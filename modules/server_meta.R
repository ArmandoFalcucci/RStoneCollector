# modules/server_meta.R - settings, export, git

setup_meta_server <- function(input, output, session, rv, shared) {
  notify_ok  <- function(msg) {
    rv$notif_ok <- msg; shinyjs::show("notif_ok")
    shinyjs::delay(2500, shinyjs::hide("notif_ok"))
  }
  notify_err <- function(msg) {
    rv$notif_err <- msg; shinyjs::show("notif_err")
    shinyjs::delay(4500, shinyjs::hide("notif_err"))
    log_warn(msg)
  }

  # ===== Settings =====
  observe({
    cfg <- rv$config; if (is.null(cfg)) return()
    updateTextInput(session, "set_site",    value = cfg$site_name     %||% "")
    updateTextInput(session, "set_analyst", value = cfg$analyst       %||% "")
    updateTextInput(session, "set_db_file", value = cfg$database_file %||% "")
    updateRadioButtons(session, "set_theme",      selected = cfg$theme      %||% "light")
    updateRadioButtons(session, "set_menu_style", selected = cfg$menu_style %||% "buttons")
    updateRadioButtons(session, "set_id_mode",    selected = cfg$id_mode    %||% "manual")
    schemas <- list_schemas(SCHEMAS_DIR)
    if (!length(schemas)) schemas <- setNames(DEFAULT_SCHEMA, basename(DEFAULT_SCHEMA))
    updateSelectInput(session, "set_schema_file",
                      choices = schemas,
                      selected = cfg$schema_file %||% DEFAULT_SCHEMA)
  })

  # Live theme switch - apply immediately when radio changes, persist to config
  observeEvent(input$set_theme, {
    theme <- input$set_theme %||% "light"
    session$sendCustomMessage("rstone_set_theme", theme)
    cfg <- rv$config %||% list()
    if (!identical(cfg$theme %||% "light", theme)) {
      cfg$theme <- theme
      save_config(cfg, CONFIG_FILE)
      rv$config <- cfg
    }
  }, ignoreInit = TRUE)

  # Persist menu style on change
  observeEvent(input$set_menu_style, {
    ms <- input$set_menu_style %||% "buttons"
    cfg <- rv$config %||% list()
    if (!identical(cfg$menu_style %||% "buttons", ms)) {
      cfg$menu_style <- ms
      save_config(cfg, CONFIG_FILE)
      rv$config <- cfg
    }
  }, ignoreInit = TRUE)

  # Persist ID numbering mode on change
  observeEvent(input$set_id_mode, {
    im <- input$set_id_mode %||% "manual"
    cfg <- rv$config %||% list()
    if (!identical(cfg$id_mode %||% "manual", im)) {
      cfg$id_mode <- im
      save_config(cfg, CONFIG_FILE)
      rv$config <- cfg
    }
  }, ignoreInit = TRUE)

  observeEvent(input$btn_save_settings, {
    tryCatch({
      cfg <- rv$config %||% list()
      cfg$site_name     <- trimws(input$set_site    %||% "RStone")
      cfg$analyst       <- trimws(input$set_analyst %||% "")
      cfg$database_file <- trimws(input$set_db_file %||% file.path(DATA_DIR, "data.csv"))
      cfg$theme         <- input$set_theme      %||% "light"
      cfg$menu_style    <- input$set_menu_style %||% "buttons"
      cfg$id_mode       <- input$set_id_mode    %||% "manual"
      save_config(cfg, CONFIG_FILE)
      rv$config <- cfg
      sch <- rv$schema
      if (!is.null(sch))
        rv$data <- load_data_for(cfg$database_file, c("DATEOFDATAENTRY", sch$fields))
      notify_ok("Settings saved.")
    }, error = function(e) notify_err(paste("Error:", e$message)))
  })

  observeEvent(input$btn_load_schema, {
    sf <- input$set_schema_file
    if (is.null(sf) || !file.exists(sf)) { notify_err("Schema file not found."); return() }
    cfg <- rv$config %||% list(); cfg$schema_file <- sf
    save_config(cfg, CONFIG_FILE)
    rv$config <- cfg
    shared$reload_schema()
    notify_ok(paste("Loaded schema:", basename(sf)))
  })

  observeEvent(input$upload_schema, {
    f <- input$upload_schema; if (is.null(f)) return()
    dest <- file.path(SCHEMAS_DIR, f$name)
    file.copy(f$datapath, dest, overwrite = TRUE)
    updateSelectInput(session, "set_schema_file",
                      choices = list_schemas(SCHEMAS_DIR), selected = dest)
    notify_ok(paste("Uploaded:", f$name))
  })

  # ===== Export =====
  output$dl_csv <- downloadHandler(
    filename = function() sprintf("%s_%s.csv",
                                  rv$config$site_name %||% "RStone",
                                  format(Sys.time(), "%Y%m%d_%H%M")),
    content = function(f) readr::write_csv(rv$data, f, na = "")
  )
  output$dl_json <- downloadHandler(
    filename = function() sprintf("%s_%s.json",
                                  rv$config$site_name %||% "RStone",
                                  format(Sys.time(), "%Y%m%d_%H%M")),
    content = function(f) jsonlite::write_json(rv$data, f, pretty = TRUE,
                                                na = "null", dataframe = "rows")
  )
  output$dl_audit <- downloadHandler(
    filename = function() sprintf("%s_audit_%s.log",
                                  rv$config$site_name %||% "RStone",
                                  format(Sys.time(), "%Y%m%d_%H%M")),
    content = function(f) {
      if (file.exists(AUDIT_FILE)) file.copy(AUDIT_FILE, f, overwrite = TRUE)
      else writeLines("", f)
    }
  )

  output$db_path_disp <- renderText({
    p <- rv$config$database_file %||% "-"
    json <- sub("\\.csv$", ".json", p, ignore.case = TRUE)
    paste0("CSV : ", p, "\nJSON: ", json,
           "\nBackups: backups/  (rolling, last 10)",
           "\nAudit log: ", AUDIT_FILE)
  })

  # ===== Git =====
  run_git <- function(...) {
    if (Sys.which("git") == "") return("git not found - install git.")
    paste(tryCatch(system2("git", c(...), stdout = TRUE, stderr = TRUE),
                   error = function(e) e$message), collapse = "\n")
  }
  observeEvent(input$git_push, {
    tryCatch({
      p <- rv$config$database_file %||% "."
      msg <- sprintf("data: %d records - %s by %s",
                     nrow(rv$data), format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                     rv$config$analyst %||% "unknown")
      json_p <- sub("\\.csv$", ".json", p, ignore.case = TRUE)
      out1 <- run_git("add", p)
      out_j <- if (file.exists(json_p)) run_git("add", json_p) else ""
      out_a <- if (file.exists(AUDIT_FILE)) run_git("add", AUDIT_FILE) else ""
      out2 <- run_git("commit", "-m", msg)
      out3 <- run_git("push")
      rv$git_log <- paste(out1, out_j, out_a, out2, out3, sep = "\n---\n")
      notify_ok("Pushed to GitHub.")
    }, error = function(e) notify_err(paste("Git:", e$message)))
  })
  observeEvent(input$git_pull, {
    tryCatch({
      rv$git_log <- run_git("pull")
      sch <- rv$schema
      if (!is.null(sch))
        rv$data <- load_data_for(rv$config$database_file, c("DATEOFDATAENTRY", sch$fields))
      notify_ok("Pulled & refreshed.")
    }, error = function(e) notify_err(paste("Git:", e$message)))
  })

  output$git_log <- renderText(rv$git_log)
}
