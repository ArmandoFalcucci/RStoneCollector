# modules/server_schema.R

setup_schema_server <- function(input, output, session, rv, shared) {
  notify_ok  <- function(msg) {
    rv$notif_ok <- msg; shinyjs::show("notif_ok")
    shinyjs::delay(2500, shinyjs::hide("notif_ok"))
  }
  notify_err <- function(msg) {
    rv$notif_err <- msg; shinyjs::show("notif_err")
    shinyjs::delay(4500, shinyjs::hide("notif_err"))
    log_warn(msg)
  }

  output$schema_info <- renderText({
    sch <- rv$schema; if (is.null(sch)) return("No schema loaded.")
    paste0(
      "Active schema:\n  ", rv$config$schema_file %||% "-", "\n\n",
      "Fields total : ", length(sch$fields), "\n",
      "Menu fields  : ", sum(toupper(unlist(sch$TYPES)) == "MENU"), "\n",
      "Numeric      : ", sum(toupper(unlist(sch$TYPES)) %in% c("NUMERIC","INSTRUMENT")), "\n",
      "Sections     : ", length(sch$SECTION_ORDER), "\n",
      "Required     : ", paste(sch$REQUIRED, collapse = ", "), "\n",
      "Carry        : ", paste(sch$CARRY, collapse = ", ")
    )
  })

  observeEvent(input$schema_validate, {
    out <- tryCatch({
      sch <- build_schema(input$schema_text)
      sprintf("OK - parsed %d fields:\n%s",
              length(sch$fields),
              paste(" - ", sch$fields, collapse = "\n"))
    }, error = function(e) paste("Error:", e$message))
    rv$schema_status <- out
  })

  observeEvent(input$schema_apply, {
    tryCatch({
      sch <- build_schema(input$schema_text)
      sf  <- rv$config$schema_file %||% DEFAULT_SCHEMA
      writeLines(input$schema_text, sf)
      shared$reload_schema()
      rv$schema_status <- sprintf("Saved & applied: %d fields (%s).",
                                  length(sch$fields), sf)
      notify_ok("Schema applied.")
      log_info(sprintf("Schema applied: %d fields", length(sch$fields)))
    }, error = function(e) {
      rv$schema_status <- paste("Parse error - not saved:", e$message)
      notify_err(e$message)
    })
  })

  observeEvent(input$schema_reload, {
    sf <- rv$config$schema_file %||% DEFAULT_SCHEMA
    if (file.exists(sf)) {
      updateTextAreaInput(session, "schema_text",
                          value = paste(readLines(sf, warn = FALSE), collapse = "\n"))
      rv$schema_status <- paste("Reloaded from:", sf)
    } else rv$schema_status <- paste("File not found:", sf)
  })

  output$schema_status <- renderText(rv$schema_status)

  # ---------- Condition rows for Add Field ----------
  # ---------- Add Field: dynamic conditions ----------
  addf_cond_count <- reactiveVal(1L)

  observeEvent(input$btn_addf_cond_more, {
    addf_cond_count(addf_cond_count() + 1L)
  })
  observeEvent(input$btn_addf_cond_less, {
    if (addf_cond_count() > 1L) addf_cond_count(addf_cond_count() - 1L)
  })

  output$cond_rows_ui <- renderUI({
    sch <- rv$schema
    fc <- if (is.null(sch)) character(0) else c("(no condition)" = "", sch$fields)
    n <- addf_cond_count()
    rows <- lapply(seq_len(n), function(i) {
      # Preserve current values when count changes by reading isolate'd input
      cur_field <- isolate(input[[paste0("cond_field_", i)]]) %||% ""
      cur_not   <- isolate(input[[paste0("cond_not_",   i)]]) %||% FALSE
      cur_vals  <- isolate(input[[paste0("cond_vals_",  i)]]) %||% ""
      cur_op    <- isolate(input[[paste0("cond_op_",    i)]]) %||% "AND"
      div(class = "cond-row",
        fluidRow(
          column(5, selectInput(paste0("cond_field_", i),
                                 paste("Condition", i, "- field:"),
                                 choices = fc, selected = cur_field)),
          column(2, checkboxInput(paste0("cond_not_", i), "NOT", value = isTRUE(cur_not))),
          column(5, textInput(paste0("cond_vals_", i),
                              "Values (comma-separated):",
                              value = cur_vals,
                              placeholder = "e.g. Tool,Flake"))
        ),
        if (i < n)
          fluidRow(column(12, selectInput(paste0("cond_op_", i), "Combine with next:",
                                          choices = c("AND" = "AND", "OR" = "OR"),
                                          selected = cur_op,
                                          width = "150px")))
      )
    })
    tagList(
      do.call(tagList, rows),
      fluidRow(
        column(6, actionButton("btn_addf_cond_more", "+ Add condition", icon = icon("plus"),
                                style = "width:100%;background:#cfd8dc;color:#37474f;border:none;font-size:12px;")),
        column(6, actionButton("btn_addf_cond_less", "− Remove last",  icon = icon("minus"),
                                style = "width:100%;background:#cfd8dc;color:#37474f;border:none;font-size:12px;"))
      )
    )
  })

  observeEvent(input$btn_add_field, {
    tryCatch({
      nm   <- trimws(input$addf_name   %||% "")
      type_ <- input$addf_type %||% "TEXT"
      prm  <- trimws(input$addf_prompt %||% nm)
      opts <- if (toupper(type_) == "MENU") {
        o <- trimws(strsplit(input$addf_options %||% "", "\n", fixed = TRUE)[[1]])
        o[nzchar(o)]
      } else NULL
      n <- addf_cond_count()
      conds <- list()
      for (i in seq_len(n)) {
        f <- input[[paste0("cond_field_", i)]] %||% ""
        if (!nzchar(f)) next
        v <- trimws(strsplit(input[[paste0("cond_vals_", i)]] %||% "", ",", fixed = TRUE)[[1]])
        v <- v[nzchar(v)]
        if (!length(v)) next
        conds[[length(conds) + 1]] <- list(
          field  = f,
          values = v,
          not    = isTRUE(input[[paste0("cond_not_", i)]]),
          op     = input[[paste0("cond_op_", i)]] %||% "AND"
        )
      }

      block <- build_field_block(
        nm, type_, prm,
        options = opts,
        required = isTRUE(input$addf_required),
        carry    = isTRUE(input$addf_carry),
        min_val  = if (nzchar(input$addf_min %||% "")) input$addf_min else NULL,
        max_val  = if (nzchar(input$addf_max %||% "")) input$addf_max else NULL,
        pattern  = if (nzchar(input$addf_pattern %||% "")) input$addf_pattern else NULL,
        conditions = conds)
      new_text <- paste0(input$schema_text, "\n\n", block)
      build_schema(new_text)
      sf <- rv$config$schema_file %||% DEFAULT_SCHEMA
      writeLines(new_text, sf)
      updateTextAreaInput(session, "schema_text", value = new_text)
      shared$reload_schema()
      notify_ok(sprintf("Added '%s'.", nm))
      updateTextInput(session, "addf_name", value = "")
      updateTextInput(session, "addf_prompt", value = "")
      updateTextAreaInput(session, "addf_options", value = "")
      for (i in seq_len(n)) {
        updateTextInput(session, paste0("cond_vals_", i), value = "")
        updateCheckboxInput(session, paste0("cond_not_", i), value = FALSE)
      }
      addf_cond_count(1L)
    }, error = function(e) notify_err(paste("Could not add:", e$message)))
  })

  # ---------- Reorder / Delete ----------
  output$reorder_ui <- renderUI({
    sch <- rv$schema
    if (is.null(sch)) return(NULL)
    selectInput("reorder_field", "Pick a field:",
                choices = setNames(sch$fields,
                                   vapply(sch$fields,
                                          function(f) paste0(f, "  -  ", prompt_label(sch$PROMPTS[[f]] %||% f)),
                                          character(1))),
                size = 12, selectize = FALSE)
  })

  move_field <- function(direction) {
    tryCatch({
      sch <- rv$schema; if (is.null(sch)) return()
      f <- input$reorder_field
      if (is.null(f) || !nzchar(f)) { notify_err("Pick a field."); return() }
      ord <- sch$fields
      i <- match(f, ord); if (is.na(i)) return()
      new_i <- if (direction == "up") max(1, i - 1) else min(length(ord), i + 1)
      if (new_i == i) return()
      ord <- ord[-i]
      ord <- append(ord, f, after = new_i - 1)
      new_text <- reorder_cfg(input$schema_text, ord)
      build_schema(new_text)
      sf <- rv$config$schema_file %||% DEFAULT_SCHEMA
      writeLines(new_text, sf)
      updateTextAreaInput(session, "schema_text", value = new_text)
      shared$reload_schema()
      updateSelectInput(session, "reorder_field", selected = f)
      notify_ok(sprintf("Moved %s.", f))
    }, error = function(e) notify_err(paste("Reorder:", e$message)))
  }
  observeEvent(input$btn_field_up,   move_field("up"))
  observeEvent(input$btn_field_down, move_field("down"))

  observeEvent(input$btn_field_del, {
    tryCatch({
      f <- input$reorder_field
      if (is.null(f) || !nzchar(f)) { notify_err("Pick a field."); return() }
      showModal(modalDialog(
        title = paste("Delete field:", f),
        p("This removes the field from the schema. Existing data in the CSV is not touched, but the field will no longer appear in the wizard."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_delete_field", "Delete", class = "btn-danger")
        ), easyClose = FALSE
      ))
    }, error = function(e) notify_err(e$message))
  })

  observeEvent(input$confirm_delete_field, {
    tryCatch({
      f <- input$reorder_field
      new_text <- delete_cfg_field(input$schema_text, f)
      build_schema(new_text)
      sf <- rv$config$schema_file %||% DEFAULT_SCHEMA
      writeLines(new_text, sf)
      updateTextAreaInput(session, "schema_text", value = new_text)
      shared$reload_schema()
      removeModal()
      notify_ok(sprintf("Deleted field %s.", f))
    }, error = function(e) notify_err(paste("Delete:", e$message)))
  })

  # ---------- Edit Field ----------
  output$editf_picker_ui <- renderUI({
    sch <- rv$schema
    if (is.null(sch)) return(p(em("Load a schema first.")))
    selectInput("editf_field", "Pick a field to edit:",
                choices = setNames(sch$fields,
                                   vapply(sch$fields,
                                          function(f) paste0(f, "  -  ", prompt_label(sch$PROMPTS[[f]] %||% f)),
                                          character(1))))
  })

  # Re-render the form whenever the user picks a different field
  # or clicks "Reset to current".
  editf_reset_trigger <- reactiveVal(0)
  observeEvent(input$btn_edit_field_reset, { editf_reset_trigger(editf_reset_trigger() + 1) })

  output$editf_form_ui <- renderUI({
    sch <- rv$schema
    f <- input$editf_field
    editf_reset_trigger()   # take dependency so Reset re-fires this
    if (is.null(sch) || is.null(f) || !nzchar(f)) return(NULL)

    type_  <- toupper(sch$TYPES[[f]] %||% "TEXT")
    prompt <- prompt_label(sch$PROMPTS[[f]] %||% f)
    opts   <- sch$OPTS[[f]] %||% character()
    mn     <- sch$MIN[[f]]
    mx     <- sch$MAX[[f]]
    pat    <- sch$PATTERN[[f]]
    req    <- f %in% sch$REQUIRED
    car    <- f %in% sch$CARRY
    conds  <- sch$CONDS[[f]] %||% list()

    tagList(
      tags$small(style = "color:#78909c;",
                 paste("Editing:", f, "- changes are validated before save.")),
      br(), br(),
      selectInput("editf_type", "Type:",
                  choices  = c("MENU","TEXT","NUMERIC","NOTE","BOOLEAN","IMAGE"),
                  selected = type_),
      textInput("editf_prompt", "Prompt:", value = prompt),
      conditionalPanel("input.editf_type == 'MENU'",
        textAreaInput("editf_options", "Menu options (one per line):",
                      value = paste(opts, collapse = "\n"),
                      width = "100%", height = "100px")),
      conditionalPanel("input.editf_type == 'NUMERIC'",
        fluidRow(
          column(6, textInput("editf_min", "Min value:",
                              value = if (!is.null(mn) && !is.na(mn)) as.character(mn) else "")),
          column(6, textInput("editf_max", "Max value:",
                              value = if (!is.null(mx) && !is.na(mx)) as.character(mx) else "")))),
      conditionalPanel("input.editf_type == 'TEXT'",
        textInput("editf_pattern", "Regex pattern (optional):",
                  value = pat %||% "")),
      checkboxInput("editf_required", "Required", value = req),
      checkboxInput("editf_carry",    "Carry over to next record", value = car),
      tags$hr(),
      p(strong("Conditions"), ":"),
      uiOutput("editf_cond_rows_ui"),
      fluidRow(
        column(6, actionButton("btn_editf_cond_more", "+ Add condition", icon = icon("plus"),
                                style = "width:100%;background:#cfd8dc;color:#37474f;border:none;font-size:12px;")),
        column(6, actionButton("btn_editf_cond_less", "− Remove last",  icon = icon("minus"),
                                style = "width:100%;background:#cfd8dc;color:#37474f;border:none;font-size:12px;"))
      )
    )
  })

  # Edit-field condition rows.
  # `editf_cond_seed` is the source of truth for what the rows should show. It is
  # reseeded from the saved schema whenever the user picks a different field or
  # hits Reset, and is updated (preserving current typing) when rows are added or
  # removed. The renderUI reads ONLY from this seed — never from persisted input
  # values — so switching fields always reloads the saved conditions, including
  # the very first one.
  editf_cond_count <- reactiveVal(1L)
  editf_cond_seed  <- reactiveVal(list())

  blank_cond_row <- function() list(field = "", not = FALSE, vals = "", op = "AND")

  # Read whatever the user currently has typed in the condition inputs, so we can
  # preserve it across +/- changes.
  capture_editf_rows <- function() {
    n <- editf_cond_count()
    lapply(seq_len(n), function(i) list(
      field = input[[paste0("editf_cond_field_", i)]] %||% "",
      not   = isTRUE(input[[paste0("editf_cond_not_", i)]]),
      vals  = input[[paste0("editf_cond_vals_", i)]] %||% "",
      op    = input[[paste0("editf_cond_op_", i)]] %||% "AND"
    ))
  }

  # Reseed from the saved schema on field change / Reset.
  observe({
    sch <- rv$schema
    f <- input$editf_field
    editf_reset_trigger()
    if (is.null(sch) || is.null(f) || !nzchar(f)) return()
    conds <- sch$CONDS[[f]] %||% list()
    rows <- lapply(conds, function(cnd) list(
      field = cnd$f %||% "",
      not   = isTRUE(cnd$not),
      vals  = paste(cnd$v, collapse = ","),
      op    = if (isTRUE(cnd$or)) "OR" else "AND"
    ))
    if (!length(rows)) rows <- list(blank_cond_row())
    editf_cond_seed(rows)
    editf_cond_count(length(rows))
  })

  observeEvent(input$btn_editf_cond_more, {
    cur <- capture_editf_rows()
    cur[[length(cur) + 1L]] <- blank_cond_row()
    editf_cond_seed(cur)
    editf_cond_count(length(cur))
  })
  observeEvent(input$btn_editf_cond_less, {
    n <- editf_cond_count()
    if (n <= 1L) return()
    cur <- capture_editf_rows()[seq_len(n - 1L)]
    editf_cond_seed(cur)
    editf_cond_count(length(cur))
  })

  output$editf_cond_rows_ui <- renderUI({
    sch <- rv$schema
    f <- input$editf_field
    if (is.null(sch) || is.null(f) || !nzchar(f)) return(NULL)
    rows <- editf_cond_seed()
    n <- max(1L, editf_cond_count())
    lapply(seq_len(n), function(i) {
      r <- if (i <= length(rows)) rows[[i]] else blank_cond_row()
      cur_field <- r$field %||% ""
      cur_not   <- isTRUE(r$not)
      cur_vals  <- r$vals %||% ""
      cur_op    <- r$op %||% "AND"

      div(class = "cond-row",
        fluidRow(
          column(5, selectInput(paste0("editf_cond_field_", i),
                                 paste("Condition", i, "- field:"),
                                 choices  = c("(no condition)" = "", sch$fields),
                                 selected = cur_field)),
          column(2, checkboxInput(paste0("editf_cond_not_", i), "NOT",
                                   value = cur_not)),
          column(5, textInput(paste0("editf_cond_vals_", i),
                              "Values (comma-separated):",
                              value = cur_vals))
        ),
        if (i < n)
          fluidRow(column(12, selectInput(paste0("editf_cond_op_", i), "Combine with next:",
                                          choices = c("AND" = "AND","OR" = "OR"),
                                          selected = cur_op,
                                          width = "150px")))
      )
    })
  })

  observeEvent(input$btn_edit_field_save, {
    tryCatch({
      sch <- rv$schema
      f <- input$editf_field
      if (is.null(sch) || is.null(f) || !nzchar(f)) {
        notify_err("Pick a field first."); return()
      }
      type_  <- input$editf_type  %||% "TEXT"
      prompt <- trimws(input$editf_prompt %||% f)
      opts <- if (toupper(type_) == "MENU") {
        o <- trimws(strsplit(input$editf_options %||% "", "\n", fixed = TRUE)[[1]])
        o[nzchar(o)]
      } else NULL
      conds <- list()
      n_edit <- editf_cond_count()
      for (i in seq_len(n_edit)) {
        cf <- input[[paste0("editf_cond_field_", i)]] %||% ""
        if (!nzchar(cf)) next
        v <- trimws(strsplit(input[[paste0("editf_cond_vals_", i)]] %||% "",
                              ",", fixed = TRUE)[[1]])
        v <- v[nzchar(v)]
        if (!length(v)) next
        conds[[length(conds) + 1]] <- list(
          field  = cf,
          values = v,
          not    = isTRUE(input[[paste0("editf_cond_not_", i)]]),
          op     = input[[paste0("editf_cond_op_", i)]] %||% "AND"
        )
      }

      new_block <- build_field_block(
        f, type_, prompt,
        options = opts,
        required = isTRUE(input$editf_required),
        carry    = isTRUE(input$editf_carry),
        min_val  = if (nzchar(input$editf_min %||% ""))     input$editf_min     else NULL,
        max_val  = if (nzchar(input$editf_max %||% ""))     input$editf_max     else NULL,
        pattern  = if (nzchar(input$editf_pattern %||% "")) input$editf_pattern else NULL,
        conditions = conds)

      new_text <- replace_cfg_field(input$schema_text, f, new_block)
      build_schema(new_text)   # validate before writing
      sf <- rv$config$schema_file %||% DEFAULT_SCHEMA
      writeLines(new_text, sf)
      updateTextAreaInput(session, "schema_text", value = new_text)
      shared$reload_schema()
      notify_ok(sprintf("Updated field '%s'.", f))
      log_info(sprintf("Schema edit: field %s updated", f))
    }, error = function(e) notify_err(paste("Edit field:", e$message)))
  })
}
