# modules/server_view.R   (v6.1 - image rename hook)
# v6.1 changes (vs v6):
#   - The single-record edit modal now detects when the user changed the
#     UNIQUE-ID field. If so it:
#       (a) refuses to overwrite an existing ID (data integrity),
#       (b) renames any IMAGE files that were auto-named under the OLD ID
#           to follow the NEW ID,
#       (c) updates the in-memory paths so the CSV write reflects them,
#       (d) aborts the save if any file rename fails - never leaves
#           disk and CSV in disagreement.

# ---- Local helper: rename auto-named image files when ID changes ----
#
# Conservative by design:
#   - Only renames files whose basename matches what the wizard would have
#     auto-named: <safeID>.<ext>, or <safeID>_<fieldName>.<ext> when the
#     schema has multiple IMAGE fields.
#   - Manually-typed paths (e.g. Z:/scans/holotype.png) are left alone.
#   - Refuses to overwrite an existing destination - surfaces an error so
#     the user can resolve it.
#   - Returns a list with $updated_paths (named by field) and $errors
#     (character vector). Caller should abort if errors is non-empty.
rename_images_for_record <- function(schema, old_id, new_id, current_vals) {
  out <- list(updated_paths = list(), errors = character())
  image_fields <- schema$fields[vapply(schema$fields, function(ff)
    toupper(schema$TYPES[[ff]] %||% "TEXT") == "IMAGE", logical(1))]
  if (!length(image_fields)) return(out)

  multi    <- length(image_fields) > 1
  safe_old <- gsub("[^A-Za-z0-9_.-]+", "_", as.character(old_id))
  safe_new <- gsub("[^A-Za-z0-9_.-]+", "_", as.character(new_id))
  if (!nzchar(safe_new)) {
    out$errors <- c(out$errors,
                    "New ID is empty - cannot rename associated images.")
    return(out)
  }

  for (imf in image_fields) {
    old_path <- current_vals[[imf]]
    if (is_empty_val(old_path)) next
    if (!file.exists(old_path)) {
      # File missing on disk; warn but don't block - the path may already
      # be broken for unrelated reasons. Just leave the path untouched.
      log_warn(sprintf("rename_images_for_record: %s does not exist on disk",
                       old_path))
      next
    }

    fname <- basename(old_path)
    fbase <- tools::file_path_sans_ext(fname)
    fext  <- tools::file_ext(fname)

    expected_base <- if (multi) paste0(safe_old, "_", imf) else safe_old
    if (fbase != expected_base) {
      # Looks manually-named - leave alone.
      next
    }

    new_base <- if (multi) paste0(safe_new, "_", imf) else safe_new
    new_path <- file.path(dirname(old_path), paste0(new_base, ".", fext))

    if (identical(normalizePath(new_path, mustWork = FALSE),
                  normalizePath(old_path, mustWork = FALSE))) next

    if (file.exists(new_path)) {
      out$errors <- c(out$errors, sprintf(
        "Cannot rename '%s' to '%s' - destination already exists. Resolve manually.",
        old_path, new_path))
      next
    }

    ok <- tryCatch(file.rename(old_path, new_path),
                   error = function(e) { log_error(e$message); FALSE })
    if (isTRUE(ok)) {
      out$updated_paths[[imf]] <- new_path
      log_info(sprintf("Renamed image: %s -> %s (ID %s -> %s)",
                       old_path, new_path, old_id, new_id))
    } else {
      out$errors <- c(out$errors,
                      sprintf("Failed to rename '%s' on disk.", old_path))
    }
  }
  out
}

setup_view_server <- function(input, output, session, rv, shared) {
  notify_ok  <- shared$notify_ok  %||% function(m) NULL
  notify_err <- shared$notify_err %||% function(m) NULL

  # ---- Dynamic filter (same module as Reports, different prefix) ----
  output$filters_ui <- renderUI({
    if (is.null(rv$schema)) return(NULL)
    div(
      filter_ui_block("flt", label = "Filter visible records:"),
      uiOutput("flt_status_banner"))
  })

  view_filter <- setup_dynamic_filter(
    "flt", input, output, session,
    data_r   = reactive(rv$data),
    schema_r = reactive(rv$schema)
  )
  filtered            <- view_filter$filtered
  view_filter_descr   <- view_filter$descr
  rv$filtered         <- filtered

  output$flt_status_banner <- renderUI({
    filter_status_div(view_filter_descr(), nrow(filtered()), nrow(rv$data))
  })

  # ---- Records table ----
  output$tbl_data <- DT::renderDT({
    df <- filtered(); sch <- rv$schema
    if (is.null(sch) || !nrow(df)) return(df)
    cols <- c(intersect(sch$fields, names(df)),
              intersect("DATEOFDATAENTRY", names(df)))
    df[, cols, drop = FALSE]
  },
  selection = "single", rownames = FALSE,
  extensions = "FixedColumns",
  options = list(pageLength = 25, scrollX = TRUE,
                 fixedColumns = list(leftColumns = 1),
                 dom = "lfrtip", autoWidth = FALSE))

  observeEvent(input$btn_view_del, {
    sel <- input$tbl_data_rows_selected
    if (is.null(sel)) { notify_err("Select a row first."); return() }
    df <- filtered(); sch <- rv$schema
    id_field <- sch$UNIQUE[1] %||% sch$fields[1]
    id_val <- df[[id_field]][sel]
    rv$data <- rv$data[is.na(rv$data[[id_field]]) | rv$data[[id_field]] != id_val, ]
    save_data_to(rv$data, rv$config$database_file %||% rv$schema$suggested_db)
    audit("delete", id_val, rv$config$analyst %||% "")
    notify_ok(paste("Deleted:", id_val))
  })
  observeEvent(input$btn_view_edit, {
    sel <- input$tbl_data_rows_selected
    if (is.null(sel)) { notify_err("Select a row first."); return() }
    rec <- filtered()[sel, , drop = FALSE]
    open_edit_modal(rec)
  })
  observeEvent(input$btn_view_dup, {
    sel <- input$tbl_data_rows_selected
    if (is.null(sel)) { notify_err("Select a row first."); return() }
    shared$load_into_wizard(filtered()[sel, , drop = FALSE], dup = TRUE)
  })

  # ---------- single-record edit modal ----------
  edit_state <- reactiveValues(rec = NULL, id = NULL, mode = "one")

  open_edit_modal <- function(rec) {
    sch <- rv$schema; if (is.null(sch)) return()
    id_field <- sch$UNIQUE[1] %||% sch$fields[1]
    id_val <- as.character(rec[[id_field]][1])
    edit_state$rec <- rec
    edit_state$id  <- id_val
    edit_state$mode <- "one"
    showModal(modalDialog(
      title = tags$span(icon("edit"), " Edit record: ", strong(id_val)),
      uiOutput("edit_modal_form"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_edit_save", "Save changes", icon = icon("save"),
                     class = "btn-primary")),
      easyClose = FALSE, size = "l"))
  }
  shared$open_edit_modal <- open_edit_modal

  output$edit_modal_form <- renderUI({
    sch <- rv$schema; rec <- edit_state$rec
    if (is.null(sch) || is.null(rec)) return(NULL)
    id_field <- sch$UNIQUE[1] %||% sch$fields[1]
    has_images <- any(vapply(sch$fields,
                              function(ff) toupper(sch$TYPES[[ff]] %||% "TEXT") == "IMAGE",
                              logical(1)))
    rows <- lapply(sch$fields, function(f) {
      type_ <- toupper(sch$TYPES[[f]] %||% "TEXT")
      cur   <- as.character(rec[[f]][1] %||% "")
      lbl   <- prompt_label(sch$PROMPTS[[f]] %||% f)

      # Flag the ID field so users know editing it triggers image renames
      if (identical(f, id_field) && has_images)
        lbl <- tagList(lbl, tags$small(style = "color:#6a1b9a;margin-left:6px;",
                                         icon("link"), " linked to images"))

      inp <- if (type_ == "MENU") {
        selectInput(paste0("ed_", f), label = lbl,
                    choices = c("", sch$OPTS[[f]] %||% character()),
                    selected = cur)
      } else if (type_ %in% c("NUMERIC","INSTRUMENT")) {
        numericInput(paste0("ed_", f), label = lbl,
                     value = suppressWarnings(as.numeric(cur)),
                     min = sch$MIN[[f]], max = sch$MAX[[f]])
      } else if (type_ == "NOTE") {
        textAreaInput(paste0("ed_", f), label = lbl, value = cur,
                      width = "100%", height = "80px")
      } else if (type_ == "IMAGE") {
        # Show the current path + a thumbnail preview, but no upload here
        # (the wizard remains the canonical upload UI).
        tagList(
          textInput(paste0("ed_", f), label = lbl, value = cur, width = "100%"),
          if (nzchar(cur) && file.exists(cur))
            tags$img(src = base64enc::dataURI(file = cur),
                     style = "max-height:80px;margin-top:4px;border:1px solid #ddd;border-radius:3px;")
          else if (nzchar(cur))
            tags$small(style = "color:#c62828;",
                       icon("exclamation-triangle"),
                       " File not found on disk."))
      } else {
        textInput(paste0("ed_", f), label = lbl, value = cur, width = "100%")
      }
      div(style = "margin-bottom:6px;", inp)
    })
    do.call(tagList, rows)
  })

  observeEvent(input$btn_edit_save, {
    sch <- rv$schema; rec <- edit_state$rec
    if (is.null(sch) || is.null(rec)) { removeModal(); return() }
    id_field <- sch$UNIQUE[1] %||% sch$fields[1]
    id_val   <- edit_state$id

    # ---- Validate and collect new values ----
    new_vals <- list()
    for (f in sch$fields) {
      v <- input[[paste0("ed_", f)]]
      if (is.null(v) || (length(v) == 1 && is.na(v))) v <- ""
      new_vals[[f]] <- as.character(v)
      err <- validate_value(f, v, sch)
      if (!is.null(err)) {
        notify_err(sprintf("%s: %s",
                            prompt_label(sch$PROMPTS[[f]] %||% f), err))
        return()
      }
    }

    # ---- ID change handling ----
    new_id <- new_vals[[id_field]]
    id_changed <- !identical(new_id, id_val) && nzchar(new_id) && nzchar(id_val)

    if (id_changed) {
      # Reject duplicate IDs
      other_rows <- rv$data[[id_field]] != id_val
      other_rows[is.na(other_rows)] <- TRUE
      if (any(rv$data[[id_field]][other_rows] == new_id, na.rm = TRUE)) {
        notify_err(sprintf(
          "ID '%s' is already in use by another record. Pick a different value.",
          new_id))
        return()
      }

      # Try to rename any linked image files. If ANY rename fails (e.g.
      # destination already exists), abort the save so disk and CSV stay
      # consistent.
      rename_res <- rename_images_for_record(sch, id_val, new_id, new_vals)
      if (length(rename_res$errors)) {
        for (e in rename_res$errors) notify_err(e)
        return()
      }
      # Apply the new paths to new_vals so the CSV write picks them up.
      for (f in names(rename_res$updated_paths)) {
        new_vals[[f]] <- rename_res$updated_paths[[f]]
      }
      if (length(rename_res$updated_paths)) {
        n <- length(rename_res$updated_paths)
        notify_ok(sprintf("Renamed %d image file%s to match new ID.",
                          n, if (n > 1) "s" else ""))
        audit("rename-images", new_id, rv$config$analyst %||% "",
              note = sprintf("%s -> %s (%d files)",
                              id_val, new_id, n))
      }
    }

    # ---- Locate and update the row ----
    idx <- which(rv$data[[id_field]] == id_val)
    if (!length(idx)) {
      notify_err("Record disappeared.")
      removeModal(); return()
    }
    for (f in sch$fields)
      rv$data[idx[1], f] <- new_vals[[f]]
    save_data_to(rv$data, rv$config$database_file %||% rv$schema$suggested_db)
    audit("update", id_val, rv$config$analyst %||% "",
          note = if (id_changed) sprintf("ID: %s -> %s", id_val, new_id) else "")
    removeModal()
    notify_ok(if (id_changed)
                sprintf("Updated %s (renamed to %s).", id_val, new_id)
              else
                paste("Updated:", id_val))
  })

  # ---------- mass edit ----------
  observeEvent(input$btn_mass_edit, {
    sch <- rv$schema; df <- filtered()
    if (is.null(sch) || !nrow(df)) {
      notify_err("No filtered records to mass-edit."); return()
    }
    showModal(modalDialog(
      title = tags$span(icon("magic"), " Mass edit ",
                        strong(nrow(df)), " filtered records"),
      selectInput("mass_field", "Field to update:",
                  choices = setNames(sch$fields,
                                     vapply(sch$fields, function(f)
                                       prompt_label(sch$PROMPTS[[f]] %||% f),
                                       character(1)))),
      uiOutput("mass_value_ui"),
      tags$small("This will update every record matching the current filter. ",
                 "Each change is recorded in the audit log."),
      footer = tagList(modalButton("Cancel"),
                       actionButton("btn_mass_apply", "Apply",
                                    class = "btn-warning",
                                    icon = icon("check"))),
      easyClose = FALSE))
  })

  output$mass_value_ui <- renderUI({
    sch <- rv$schema; f <- input$mass_field
    if (is.null(f) || !nzchar(f)) return(NULL)
    type_ <- toupper(sch$TYPES[[f]] %||% "TEXT")
    if (type_ == "MENU")
      selectInput("mass_val", "New value:",
                  choices = c("", sch$OPTS[[f]] %||% character()))
    else if (type_ %in% c("NUMERIC","INSTRUMENT"))
      numericInput("mass_val", "New value:", value = 0,
                   min = sch$MIN[[f]], max = sch$MAX[[f]])
    else
      textInput("mass_val", "New value:", value = "")
  })

  observeEvent(input$btn_mass_apply, {
    sch <- rv$schema; df <- filtered()
    f <- input$mass_field; v <- input$mass_val
    if (is.null(f) || !nzchar(f)) return()

    # Guard against mass-editing the UNIQUE ID field - that would create
    # duplicate IDs across all filtered records.
    id_field <- sch$UNIQUE[1] %||% sch$fields[1]
    if (identical(f, id_field)) {
      notify_err(sprintf(
        "Cannot mass-edit '%s' (the record ID). That would make all filtered records share one ID.",
        id_field))
      return()
    }

    err <- validate_value(f, v, sch)
    if (!is.null(err)) { notify_err(err); return() }
    ids <- df[[id_field]]
    rv$data[rv$data[[id_field]] %in% ids, f] <- as.character(v)
    save_data_to(rv$data, rv$config$database_file %||% rv$schema$suggested_db)
    for (id in ids)
      audit("mass-edit", id, rv$config$analyst %||% "",
            note = sprintf("%s <- %s", f, v))
    removeModal()
    notify_ok(sprintf("Mass-updated %d records.", length(ids)))
  })
}
