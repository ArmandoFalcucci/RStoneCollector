# modules/server_wizard.R
# v6: image upload for IMAGE fields (auto-renames to <RecordID>.<ext>).

setup_wizard_server <- function(input, output, session, rv, shared) {

  notify_ok  <- function(msg) {
    rv$notif_ok <- msg; shinyjs::show("notif_ok")
    shinyjs::delay(2500, shinyjs::hide("notif_ok"))
  }
  notify_err <- function(msg) {
    rv$notif_err <- msg; shinyjs::show("notif_err")
    shinyjs::delay(4500, shinyjs::hide("notif_err"))
    log_warn(msg)
  }

  # ---------- ID uniqueness ----------
  id_field_name <- function() {
    sch <- rv$schema; if (is.null(sch)) return(NULL)
    sch$UNIQUE[1] %||% sch$fields[1]
  }
  # TRUE if `val` already exists in the data for the ID field, other than the
  # record currently being edited (rv$editing_id).
  id_is_duplicate <- function(val) {
    if (is_empty_val(val)) return(FALSE)
    idf <- id_field_name()
    if (is.null(idf) || is.null(rv$data) || !idf %in% names(rv$data)) return(FALSE)
    editing <- rv$editing_id %||% ""
    if (!is_empty_val(editing) &&
        identical(as.character(val), as.character(editing))) return(FALSE)
    col <- as.character(rv$data[[idf]])
    any(!is.na(col) & col == as.character(val))
  }

  # ---------- field navigation ----------
  next_visible <- function(s) {
    sch <- rv$schema; if (is.null(sch)) return(NA_integer_)
    if (s > length(sch$fields)) return(NA_integer_)
    for (i in s:length(sch$fields)) {
      f <- sch$fields[i]
      type_ <- toupper(sch$TYPES[[f]] %||% "TEXT")
      if (type_ == "DATETIME") next
      if (!length(sch$CONDS[[f]]) || eval_conds(sch$CONDS[[f]], rv$entries))
        return(i)
    }
    NA_integer_
  }
  prev_visible <- function(s) {
    sch <- rv$schema; if (is.null(sch) || s < 1) return(NA_integer_)
    for (i in s:1) {
      f <- sch$fields[i]
      type_ <- toupper(sch$TYPES[[f]] %||% "TEXT")
      if (type_ == "DATETIME") next
      if (!length(sch$CONDS[[f]]) || eval_conds(sch$CONDS[[f]], rv$entries))
        return(i)
    }
    NA_integer_
  }

  current_field <- reactive({
    if (!isTRUE(rv$setup_done) || is.null(rv$schema)) return(NULL)
    i <- next_visible(rv$step)
    if (is.na(i)) return(NULL)
    sch <- rv$schema; f <- sch$fields[i]
    list(idx = i, field = f,
         type = toupper(sch$TYPES[[f]] %||% "TEXT"),
         opts = sch$OPTS[[f]],
         prompt = prompt_label(sch$PROMPTS[[f]] %||% f),
         required = field_is_required(f, rv$entries, sch),
         section = sch$SECTIONS[[f]] %||% "")
  })
  rv$current_field_fn <- current_field
  shared$current_field <- current_field

  # ---------- UI ----------
  output$wizard_ui <- renderUI({
    if (!isTRUE(rv$setup_done)) {
      return(div(class = "wiz-card",
                 h3(icon("hourglass-half"), " Setting up..."),
                 p("Complete the welcome dialog to begin.")))
    }
    cf <- current_field()
    if (is.null(cf)) {
      return(div(class = "wiz-card",
                 div(style = "text-align:center;padding:40px;",
                     h3(icon("check-circle", style = "color:#2e7d32;"),
                        " Record complete - saving..."),
                     p("Starting next record automatically."))))
    }
    sch <- rv$schema
    f      <- cf$field
    total  <- length(sch$fields)
    pct    <- round((cf$idx - 1) / max(total, 1) * 100)
    cur_v  <- rv$entries[[f]] %||% ""
    type_  <- cf$type

    isolate({ rv$live_field <- f; rv$live_value <- as.character(cur_v) })

    # Section breadcrumb
    sec_crumb <- if (nzchar(cf$section %||% "") && length(sch$SECTION_ORDER)) {
      idx <- match(cf$section, sch$SECTION_ORDER)
      total_sec <- length(sch$SECTION_ORDER)
      if (!is.na(idx))
        div(class = "wiz-section",
            sprintf("Section %d of %d: %s", idx, total_sec, cf$section))
      else NULL
    } else NULL

    inp <- if (type_ %in% c("MENU", "BOOLEAN")) {
      opts <- cf$opts %||% character()
      style <- rv$config$menu_style %||% "buttons"
      use_buttons <- (style == "buttons") ||
                     (style == "auto" && length(opts) > 0 && length(opts) <= 6)
      if (use_buttons) {
        div(class = "wiz-buttons",
            tags$input(type = "hidden", id = "wiz_input", name = "wiz_input", value = cur_v),
            lapply(opts, function(o) {
              cls <- paste("wiz-opt-btn", if (identical(as.character(o), cur_v)) "selected" else "")
              tags$button(type = "button", class = cls, `data-value` = o, o)
            }))
      } else {
        selectizeInput("wiz_input", label = NULL,
                       choices = c("---" = "", opts),
                       selected = cur_v, width = "100%",
                       options = list(placeholder = "Select or type to filter..."))
      }
    } else if (type_ %in% c("NUMERIC", "INSTRUMENT")) {
      v <- suppressWarnings(as.numeric(cur_v))
      numericInput("wiz_input", label = NULL,
                   value = if (length(v) && !is.na(v)) v else NA_real_,
                   min = sch$MIN[[f]], max = sch$MAX[[f]],
                   step = 0.01, width = "100%")
    } else if (type_ == "NOTE") {
      textAreaInput("wiz_input", label = NULL, value = cur_v,
                    width = "100%", height = "120px",
                    placeholder = "Type notes; press Enter to advance, Shift+Enter for a new line")
    } else if (type_ == "IMAGE") {
      # File upload + path. Upload writes to data/images/<recordID>.<ext>
      # and auto-fills the path textInput. User clicks Next to advance.
      id_field <- sch$UNIQUE[1] %||% sch$fields[1]
      id_val   <- isolate(rv$entries[[id_field]]) %||% ""
      tagList(
        if (!nzchar(id_val))
          div(style = "padding:8px 12px;background:#fff8e1;border-left:4px solid #f57f17;
                       border-radius:4px;font-size:12px;margin-bottom:8px;",
              icon("info-circle"), " Set the ", strong(id_field),
              " earlier for a cleaner filename. Without it, the upload will be ",
              "named with a timestamp.")
        else NULL,
        fileInput("wiz_image_upload", label = NULL,
                  accept = c("image/png","image/jpeg","image/jpg",
                             "image/gif","image/webp"),
                  buttonLabel = "Choose image...",
                  placeholder = "No file selected"),
        textInput("wiz_input",
                  label = "Path (auto-fills on upload, or type manually):",
                  value = cur_v, width = "100%"),
        if (nzchar(cur_v) && file.exists(cur_v))
          tags$img(src = base64enc::dataURI(file = cur_v),
                   style = "max-width:100%;max-height:220px;margin-top:10px;
                            border:1px solid #ddd;border-radius:4px;")
        else if (nzchar(cur_v))
          tags$small(style = "color:#c62828;",
                     "File not found - path will still be saved.")
        else NULL
      )
    } else {
      textInput("wiz_input", label = NULL, value = cur_v, width = "100%",
                placeholder = "Type and press Enter or click Next")
    }

    # Range hint
    range_hint <- if (type_ %in% c("NUMERIC","INSTRUMENT") &&
                       (!is.null(sch$MIN[[f]]) || !is.null(sch$MAX[[f]]))) {
      paste0(" (",
             if (!is.null(sch$MIN[[f]])) paste0("min ", sch$MIN[[f]]) else "",
             if (!is.null(sch$MIN[[f]]) && !is.null(sch$MAX[[f]])) ", " else "",
             if (!is.null(sch$MAX[[f]])) paste0("max ", sch$MAX[[f]]) else "",
             ")")
    } else ""

    div(class = "wiz-card",
      div(class = "wiz-bar", div(class = "wiz-fill",
                                  style = sprintf("width:%d%%;", pct))),
      div(class = "wiz-step",
          sprintf("Step %d of ~%d  \u00b7  %d%% complete",
                  cf$idx, total, pct)),
      sec_crumb,
      div(class = "wiz-q", cf$prompt, range_hint,
          if (cf$required) tags$span(style = "color:#c62828;", " *") else NULL),
      div(class = "wiz-input", inp),
      div(class = "wiz-hint",
          if (type_ %in% c("MENU", "BOOLEAN")) {
            style <- rv$config$menu_style %||% "buttons"
            use_buttons <- (style == "buttons") ||
                           (style == "auto" &&
                            length(cf$opts %||% character()) > 0 &&
                            length(cf$opts %||% character()) <= 6)
            if (use_buttons) "Click an option to select and advance."
            else "Type to filter; click or press Enter on an option (auto-advances)."
          }
          else if (type_ == "NOTE")
            "Press Enter to advance; Shift+Enter for a new line."
          else if (type_ == "IMAGE")
            "Upload a file or type a path, then click Next."
          else
            "Type a value, then press Enter or click Next. (Esc = Skip, Shift+Enter = Back, Ctrl+D = duplicate last record)"),
      div(class = "wiz-nav",
        actionButton("btn_back", "\u2190 Back", class = "wiz-btn btn-back"),
        div(
          if (!cf$required)
            actionButton("btn_skip", "Skip", class = "wiz-btn btn-skip",
                         style = "margin-right:8px;"),
          actionButton("btn_next", "Next \u2192", class = "wiz-btn btn-next")
        )
      )
    )
  })

  # ---------- summary panel ----------
  output$summary_ui <- renderUI({
    if (is.null(rv$schema)) return(NULL)
    e <- rv$entries; sch <- rv$schema
    e_view <- e
    live_v <- rv$live_value; live_f <- rv$live_field
    if (!is.null(live_f) && nzchar(live_v) &&
        (is_empty_val(e_view[[live_f]]) || e_view[[live_f]] != live_v)) {
      e_view[[live_f]] <- live_v
    }
    show_set <- unique(c(rv$confirmed,
                         if (!is.null(live_f) && nzchar(live_v)) live_f))
    show_set <- intersect(show_set, names(e_view))
    show_set <- show_set[vapply(show_set, function(k) !is_empty_val(e_view[[k]]),
                                logical(1))]
    show_set <- intersect(sch$fields, show_set)

    summary_card <- div(class = "summary-card", style = "margin-bottom:14px;",
      div(class = "summary-title", icon("layer-group"), " Current record"),
      if (!length(show_set))
        div(class = "summary-empty", "No entries yet - start typing above.")
      else
        lapply(show_set, function(k) {
          is_live <- !is.null(live_f) && identical(k, live_f) && nzchar(live_v)
          div(class = paste("summary-row", if (is_live) "live" else ""),
              span(class = "summary-key", prompt_label(sch$PROMPTS[[k]] %||% k)),
              span(class = "summary-val", as.character(e_view[[k]])))
        })
    )
    db_card <- div(class = "summary-card",
      div(class = "summary-title", icon("database"), " Database"),
      div(class = "summary-row",
          span(class = "summary-key", "Site"),
          span(class = "summary-val", rv$config$site_name %||% "-")),
      div(class = "summary-row",
          span(class = "summary-key", "Records"),
          span(class = "summary-val", nrow(rv$data))),
      if (nrow(rv$data) > 0)
        div(class = "summary-row",
            span(class = "summary-key", "Last entry"),
            span(class = "summary-val",
                 as.character(rv$data$DATEOFDATAENTRY[nrow(rv$data)] %||% "-")))
    )
    tagList(summary_card, db_card)
  })

  # ---------- live value tracking ----------
  observeEvent(input$wiz_live, {
    val <- input$wiz_live$v %||% ""
    rv$live_value <- if (is.null(val)) "" else as.character(val)[1]
  }, ignoreNULL = TRUE)

  # ---------- IMAGE UPLOAD ----------
  # Fires when the user picks a file in fileInput("wiz_image_upload"). Copies
  # it into data/images/ with a name derived from the unique-ID field (or a
  # timestamp if ID isn't set yet) and updates the path text input so the
  # wizard's normal save path picks it up.
  observeEvent(input$wiz_image_upload, {
    req(input$wiz_image_upload)
    sch <- rv$schema; if (is.null(sch)) return()
    cf <- current_field()
    if (is.null(cf) || cf$type != "IMAGE") return()

    upl <- input$wiz_image_upload
    id_field <- sch$UNIQUE[1] %||% sch$fields[1]
    id_val   <- rv$entries[[id_field]] %||% ""

    ext <- tolower(tools::file_ext(upl$name))
    if (!nchar(ext) || !(ext %in% c("png","jpg","jpeg","gif","webp"))) {
      notify_err("Unsupported image format (use PNG, JPG, GIF, or WEBP).")
      return()
    }

    images_dir <- "data/images"
    if (!dir.exists(images_dir))
      tryCatch(dir.create(images_dir, recursive = TRUE),
               error = function(e) { notify_err(paste("Could not create",
                                                       images_dir, ":", e$message)); return() })

    base_name <- if (nzchar(id_val))
      gsub("[^A-Za-z0-9_.-]+", "_", as.character(id_val))
    else
      sprintf("image_%s", format(Sys.time(), "%Y%m%d_%H%M%S"))

    # Per-field suffix: if a record has multiple IMAGE fields, append the
    # field name so they don't clobber each other.
    image_fields <- sch$fields[vapply(sch$fields,
                                       function(ff) toupper(sch$TYPES[[ff]] %||% "TEXT") == "IMAGE",
                                       logical(1))]
    if (length(image_fields) > 1)
      base_name <- paste0(base_name, "_", cf$field)

    dest <- file.path(images_dir, paste0(base_name, ".", ext))

    ok <- tryCatch({
      file.copy(upl$datapath, dest, overwrite = TRUE)
      TRUE
    }, error = function(e) {
      notify_err(paste("Upload failed:", e$message))
      log_error(paste("Image upload failed:", e$message))
      FALSE
    })

    if (ok) {
      updateTextInput(session, "wiz_input", value = dest)
      rv$entries[[cf$field]] <- dest
      rv$live_value <- dest
      notify_ok(sprintf("Image saved as %s. Click Next to advance.", dest))
      log_info(sprintf("Image uploaded for record %s (%s): %s",
                       id_val, cf$field, dest))
    }
  }, ignoreNULL = TRUE)

  # ---------- save ----------
  do_save <- function() {
    tryCatch({
      sch <- rv$schema; if (is.null(sch)) return(invisible())
      e <- rv$entries
      for (rf in sch$fields) {
        if (field_is_required(rf, e, sch) && is_empty_val(e[[rf]])) {
          if (!length(sch$CONDS[[rf]]) || eval_conds(sch$CONDS[[rf]], e)) {
            notify_err(sprintf("'%s' is required.",
                               prompt_label(sch$PROMPTS[[rf]] %||% rf)))
            return(invisible())
          }
        }
      }
      for (f in names(e)) {
        err <- validate_value(f, e[[f]], sch)
        if (!is.null(err)) {
          notify_err(sprintf("%s: %s",
                             prompt_label(sch$PROMPTS[[f]] %||% f), err))
          return(invisible())
        }
      }

      cols <- c("DATEOFDATAENTRY", sch$fields)
      rec  <- setNames(as.list(rep(NA_character_, length(cols))), cols)
      rec$DATEOFDATAENTRY <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      for (f in sch$fields) {
        v <- e[[f]]
        if (is_empty_val(v)) next
        if (toupper(sch$TYPES[[f]] %||% "TEXT") == "DATETIME") {
          rec[[f]] <- rec$DATEOFDATAENTRY; next
        }
        if (length(sch$CONDS[[f]]) && !eval_conds(sch$CONDS[[f]], e)) next
        rec[[f]] <- as.character(v[1])
      }
      new_row <- as.data.frame(rec, stringsAsFactors = FALSE)

      df <- rv$data
      for (c in cols) if (!c %in% names(df)) df[[c]] <- NA_character_
      df <- df[, cols, drop = FALSE]

      id_field <- sch$UNIQUE[1]
      id_val <- if (!is.null(id_field) && id_field %in% names(new_row))
                  new_row[[id_field]][1] else ""
      action <- "save"
      editing <- rv$editing_id %||% ""
      is_edit <- !is_empty_val(editing)
      if (!is_empty_val(id_val) && !is.null(id_field) && id_field %in% names(df)) {
        existing <- which(!is.na(df[[id_field]]) &
                          as.character(df[[id_field]]) == as.character(id_val))
        editing_row <- if (is_edit)
          which(!is.na(df[[id_field]]) &
                as.character(df[[id_field]]) == as.character(editing)) else integer(0)
        # A collision that is NOT the record we're editing = duplicate -> refuse.
        if (length(existing) && !(is_edit && identical(as.character(id_val),
                                                        as.character(editing)))) {
          notify_err(sprintf(
            "ID '%s' already exists - not saved. Change it to a unique value.", id_val))
          return(invisible())
        }
        if (is_edit && length(editing_row)) {
          df[editing_row[1], ] <- new_row
          action <- "update"
          notify_ok(paste0("Updated: ", id_val))
        } else {
          df <- rbind(df, new_row)
          notify_ok(paste0("Saved: ", id_val))
        }
      } else {
        df <- rbind(df, new_row); notify_ok("Saved.")
      }
      rv$data <- df
      ok <- save_data_to(df, rv$config$database_file %||% sch$suggested_db)
      if (!ok) notify_err("Save to disk failed (see log).")
      audit(action, id_val, rv$config$analyst %||% "")

      carry <- list()
      for (c in sch$CARRY) carry[[c]] <- e[[c]]
      uf <- sch$UNIQUE[1]
      if (!is.null(uf)) {
        # "increment" = consecutive auto-numbering; "manual" = keep the ID so the
        # user edits it (the duplicate check stops an unchanged ID being re-saved).
        id_mode <- rv$config$id_mode %||% "manual"
        carry[[uf]] <- if (identical(id_mode, "increment")) next_id(e[[uf]]) else e[[uf]]
      }
      rv$editing_id <- NULL
      rv$entries    <- carry
      rv$confirmed  <- character()
      rv$step       <- 1L
      rv$live_field <- NULL
      rv$live_value <- ""
    }, error = function(err) {
      log_error(paste("save crashed:", err$message))
      notify_err(paste("Save error:", err$message))
    })
  }
  rv$do_save <- do_save
  shared$do_save <- do_save

  # ---------- navigation observers ----------
  purge_unmet <- function() {
    sch <- rv$schema; if (is.null(sch)) return(invisible())
    for (iter in 1:10) {
      removed <- character()
      for (f in sch$fields) {
        if (length(sch$CONDS[[f]]) && !eval_conds(sch$CONDS[[f]], rv$entries)) {
          if (!is_empty_val(rv$entries[[f]])) {
            rv$entries[[f]] <- ""
            removed <- c(removed, f)
          }
        }
      }
      if (!length(removed)) break
      rv$confirmed <- setdiff(rv$confirmed, removed)
    }
    invisible()
  }

  observeEvent(input$wiz_pick, {
    tryCatch({
      cf <- current_field(); if (is.null(cf)) return()
      v <- input$wiz_pick$v %||% ""
      v <- if (is.null(v)) "" else as.character(v)[1]

      rv$entries[[cf$field]] <- v

      if (isTRUE(cf$required) && is_empty_val(v)) {
        notify_err(sprintf("'%s' is required.", cf$prompt)); return()
      }
      err <- validate_value(cf$field, v, rv$schema)
      if (!is.null(err)) { notify_err(sprintf("%s: %s", cf$prompt, err)); return() }

      # Block a duplicate record ID here, so the user fixes it right away rather
      # than discovering it only at save.
      if (identical(cf$field, id_field_name()) && id_is_duplicate(v)) {
        notify_err(sprintf(
          "ID '%s' already exists. Change it to a unique value before continuing.", v))
        return()
      }

      rv$confirmed <- union(rv$confirmed, cf$field)
      purge_unmet()
      nxt <- next_visible(cf$idx + 1L)
      if (is.na(nxt)) do_save() else { rv$step <- nxt; rv$live_value <- "" }
    }, error = function(e) notify_err(paste("Error:", e$message)))
  }, ignoreNULL = TRUE)

  observeEvent(input$wiz_skip, {
    tryCatch({
      cf <- current_field(); if (is.null(cf)) return()
      if (isTRUE(cf$required)) {
        notify_err(sprintf("'%s' is required - cannot skip.", cf$prompt)); return()
      }
      rv$entries[[cf$field]] <- ""
      rv$confirmed <- union(rv$confirmed, cf$field)
      purge_unmet()
      nxt <- next_visible(cf$idx + 1L)
      if (is.na(nxt)) do_save() else { rv$step <- nxt; rv$live_value <- "" }
    }, error = function(e) notify_err(paste("Error:", e$message)))
  }, ignoreNULL = TRUE)

  observeEvent(input$wiz_back, {
    tryCatch({
      cf <- current_field()
      target <- if (is.null(cf)) length(rv$schema$fields) else cf$idx - 1L
      prv <- prev_visible(target)
      if (!is.na(prv)) { rv$step <- prv; rv$live_value <- "" }
    }, error = function(e) notify_err(paste("Error:", e$message)))
  }, ignoreNULL = TRUE)

  observeEvent(input$btn_new_record, {
    rv$entries <- list(); rv$step <- 1L; rv$confirmed <- character()
    rv$editing_id <- NULL
    rv$live_field <- NULL; rv$live_value <- ""
    updateTabItems(session, "tabs", "entry")
  })

  observeEvent(input$btn_edit_last, {
    if (nrow(rv$data) == 0) { notify_err("No records yet."); return() }
    shared$open_edit_modal(rv$data[nrow(rv$data), , drop = FALSE])
  })

  observeEvent(input$wiz_dup_last, {
    if (nrow(rv$data) == 0) { notify_err("No records yet."); return() }
    load_into_wizard(rv$data[nrow(rv$data), , drop = FALSE], dup = TRUE)
  })

  load_into_wizard <- function(rec, dup = FALSE) {
    sch <- rv$schema; if (is.null(sch)) return()
    e <- list()
    for (col in sch$fields) {
      v <- rec[[col]]
      if (!is_empty_val(v)) e[[col]] <- as.character(v)
    }
    if (isTRUE(dup)) {
      uf <- sch$UNIQUE[1]
      if (!is.null(uf)) e[[uf]] <- next_id(e[[uf]])
      rv$editing_id <- NULL                 # a duplicate is a brand-new record
      notify_ok(paste("Duplicated from", rec[[sch$UNIQUE[1]]] %||% "record",
                      "- review and save."))
      rv$confirmed <- character()
    } else {
      uf <- sch$UNIQUE[1]
      rv$editing_id <- if (!is.null(uf)) as.character(e[[uf]] %||% "") else ""
      rv$confirmed <- names(e)
      notify_ok(paste("Loaded:", e[[sch$UNIQUE[1]]] %||% "record"))
    }
    rv$entries <- e; rv$step <- 1L
    rv$live_field <- NULL; rv$live_value <- ""
    updateTabItems(session, "tabs", "entry")
  }
  rv$load_into_wizard <- load_into_wizard
  shared$load_into_wizard <- load_into_wizard
}
