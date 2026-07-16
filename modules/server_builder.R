# modules/server_builder.R   (v6.1 - performance fix)
# Report Builder: select/reorder/include captured items and compile to a PDF
# or a single combined R script.
#
# v6.1 changes (vs v6):
#   - One delegated observer (input$builder_action) handles ALL per-item
#     actions instead of 8 observers PER item. Initial tab load and
#     subsequent re-renders are now O(1) in the number of items rather
#     than O(8N).
#   - One downloadHandler (builder_dl_selected_r) services any item's
#     R-code download via a reactive pointer, rather than N handlers.
#   - save_captured() is debounced via later::later (~500 ms). Rapid
#     reorder / toggle clicks coalesce into a single disk write.
#   - Rows are emitted as raw HTML strings (no Shiny input binding per row).
#   - Rename now opens a small modal instead of inline-editing, which means
#     the items list does NOT need to re-render while the user types.

# ---- captured-plots upgrade -------------------------------------------------

normalize_captured_entry <- function(e, idx = 1L) {
  if (is.null(e)) return(NULL)
  list(
    id              = e$id              %||% sprintf("cap_%d_%s", idx,
                                                     format(Sys.time(), "%H%M%S")),
    label           = e$label           %||% sprintf("Item %d", idx),
    type            = e$type            %||% (if (!is.null(e$plot)) "plot"
                                               else if (!is.null(e$table)) "table"
                                               else "heading"),
    plot            = e$plot,
    table           = e$table,
    filter_snapshot = e$filter_snapshot %||% list(),
    config          = e$config          %||% list(),
    r_code          = e$r_code          %||% "",
    created         = e$created         %||% Sys.time(),
    include         = if (is.null(e$include)) TRUE else isTRUE(e$include)
  )
}

normalize_captured <- function(lst) {
  if (!length(lst)) return(list())
  lapply(seq_along(lst), function(i) normalize_captured_entry(lst[[i]], i))
}

new_capture_id <- function() {
  sprintf("cap_%s_%04d", format(Sys.time(), "%H%M%S"),
          sample.int(9999, 1))
}

# ---- server -----------------------------------------------------------------

setup_builder_server <- function(input, output, session, rv, shared) {

  notify_ok  <- shared$notify_ok  %||% function(m) NULL
  notify_err <- shared$notify_err %||% function(m) NULL

  isolate({ rv$captured_plots <- normalize_captured(rv$captured_plots) })

  edit_state <- reactiveValues(
    current_rcode = NULL,
    current_view  = NULL,
    renaming_id   = NULL
  )

  # ---- DEBOUNCED save ----
  save_pending <- FALSE
  schedule_save <- function() {
    if (save_pending) return(invisible())
    save_pending <<- TRUE
    later::later(function() {
      tryCatch(save_captured(isolate(rv$captured_plots)),
               error = function(e) log_warn(paste("save_captured:", e$message)))
      save_pending <<- FALSE
    }, delay = 0.5)
  }

  # ---- Render items as plain HTML (no Shiny input bindings per row) ----
  output$builder_items_ui <- renderUI({
    items <- rv$captured_plots
    if (!length(items)) {
      return(HTML('<div style="padding:40px;text-align:center;color:#90a4ae;">
        <i class="fa fa-inbox fa-3x"></i>
        <h4>No captured items yet.</h4>
        <p>Build plots in <b>Build Your Own</b>, <b>PCA</b>, or <b>Correspondence Analysis</b> tabs,
        then click <b>Add to Report</b> to collect them here.</p></div>'))
    }

    mkbtn <- function(cid, action, icon_cls, title,
                       bg = "transparent", fg = "#37474f",
                       border = "#cfd8dc") {
      sprintf(
'<button onclick="Shiny.setInputValue(\'builder_action\',{action:\'%s\',id:\'%s\',t:Date.now()},{priority:\'event\'});" title="%s" style="background:%s;color:%s;border:1px solid %s;padding:3px 8px;font-size:11px;border-radius:4px;margin-left:2px;cursor:pointer;line-height:1;"><i class="fa %s"></i></button>',
        action, cid, title, bg, fg, border, icon_cls)
    }

    rows_html <- vapply(seq_along(items), function(i) {
      it <- items[[i]]

      type_icon_class <- switch(it$type,
        "plot" = "fa-chart-bar", "table" = "fa-table",
        "heading" = "fa-heading", "fa-file")
      type_color <- switch(it$type,
        "plot" = "#1565c0", "table" = "#2e7d32",
        "heading" = "#6a1b9a", "#37474f")

      flt_text <- if (length(it$filter_snapshot)) {
        bits <- vapply(it$filter_snapshot, function(p) {
          if (length(p$values) == 1)
            sprintf("%s=%s", p$label, p$values)
          else
            sprintf("%s\u2208{%d vals}", p$label, length(p$values))
        }, character(1))
        paste("Filters:", paste(bits, collapse = "; "))
      } else "No filter"

      include_checkbox <- sprintf(
'<input type="checkbox" %s style="cursor:pointer;width:16px;height:16px;" onchange="Shiny.setInputValue(\'builder_action\',{action:\'inc\',id:\'%s\',value:this.checked,t:Date.now()},{priority:\'event\'});">',
        if (isTRUE(it$include)) "checked" else "", it$id)

      label_html <- sprintf(
'<span style="font-weight:600;color:%s;font-size:14px;"><i class="fa %s"></i> %s</span>',
        type_color, type_icon_class,
        htmltools::htmlEscape(it$label %||% "", attribute = FALSE))

      created_str <- format(it$created %||% Sys.time(), "%Y-%m-%d %H:%M")

      rcode_btn <- if (nzchar(it$r_code %||% ""))
        mkbtn(it$id, "rcode", "fa-code",
              "View / download R code", "#37474f", "white", "#37474f")
      else ""

      sprintf('
<div style="background:white;border-left:4px solid %s;border-radius:6px;padding:12px 14px;margin-bottom:8px;box-shadow:0 1px 4px rgba(0,0,0,.06);opacity:%s;">
  <div style="display:flex;align-items:center;gap:10px;">
    <div style="width:24px;flex-shrink:0;">%s</div>
    <div style="flex:1;min-width:0;">
      %s
      <div style="font-size:11px;color:#78909c;margin-top:4px;"><em>%s</em> \u00b7 %s</div>
    </div>
    <div style="text-align:right;flex-shrink:0;">%s%s%s%s%s%s</div>
  </div>
</div>',
        type_color,
        if (isTRUE(it$include)) "1" else "0.5",
        include_checkbox,
        label_html,
        htmltools::htmlEscape(flt_text),
        created_str,
        mkbtn(it$id, "up",     "fa-arrow-up",   "Move up"),
        mkbtn(it$id, "down",   "fa-arrow-down", "Move down"),
        mkbtn(it$id, "rename", "fa-pen",        "Rename"),
        mkbtn(it$id, "view",   "fa-eye",        "Preview"),
        rcode_btn,
        mkbtn(it$id, "del", "fa-trash", "Delete",
              "#c62828", "white", "#c62828"))
    }, character(1))

    HTML(paste(rows_html, collapse = ""))
  })

  # ---- Single delegated observer for all per-item actions ----
  observeEvent(input$builder_action, {
    act <- input$builder_action
    if (is.null(act$action) || is.null(act$id)) return()
    cid <- act$id
    items <- isolate(rv$captured_plots)
    idx <- which(vapply(items, function(it) identical(it$id, cid), logical(1)))
    if (!length(idx)) return()

    switch(act$action,

      "inc" = {
        rv$captured_plots[[idx]]$include <- isTRUE(act$value)
        schedule_save()
      },

      "up" = {
        if (idx > 1) {
          o <- seq_along(rv$captured_plots)
          o[c(idx - 1, idx)] <- o[c(idx, idx - 1)]
          rv$captured_plots <- rv$captured_plots[o]
          schedule_save()
        }
      },

      "down" = {
        if (idx < length(rv$captured_plots)) {
          o <- seq_along(rv$captured_plots)
          o[c(idx, idx + 1)] <- o[c(idx + 1, idx)]
          rv$captured_plots <- rv$captured_plots[o]
          schedule_save()
        }
      },

      "rename" = {
        edit_state$renaming_id <- cid
        showModal(modalDialog(
          title = "Rename item",
          textInput("builder_rename_value", "New label:",
                    value = items[[idx]]$label, width = "100%"),
          tags$small(tags$em(
            "Only the display label changes; underlying data is untouched.")),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("builder_rename_save", "Save",
                         icon = icon("check"), class = "btn-primary")),
          size = "m", easyClose = TRUE))
      },

      "view" = {
        edit_state$current_view <- cid
        it <- items[[idx]]
        preview_id <- "builder_preview_plot"
        if (it$type == "plot" && !is.null(it$plot)) {
          local({
            cap <- it$plot
            output[[preview_id]] <- renderPlot({ print(cap) })
          })
          content <- plotOutput(preview_id, height = "500px")
        } else if (it$type == "table" && !is.null(it$table)) {
          local({
            cap <- it$table
            output[[preview_id]] <- DT::renderDT({ cap },
              options = list(pageLength = 25, scrollX = TRUE))
          })
          content <- DT::DTOutput(preview_id)
        } else {
          content <- div(tags$em("No preview available for this item type."))
        }
        showModal(modalDialog(
          title = it$label,
          content,
          if (length(it$filter_snapshot))
            div(style = "margin-top:10px;font-size:12px;color:#546e7a;",
                tags$strong("Filter snapshot: "),
                paste(vapply(it$filter_snapshot, function(p)
                  sprintf("%s = {%s}", p$label,
                          paste(p$values, collapse = ", ")),
                  character(1)), collapse = " AND "))
          else NULL,
          size = "l", easyClose = TRUE,
          footer = modalButton("Close")))
      },

      "rcode" = {
        edit_state$current_rcode <- cid
        it <- items[[idx]]
        code <- it$r_code %||% "# No R code captured for this item."
        showModal(modalDialog(
          title = paste("R code:", it$label),
          tags$pre(style = "max-height:400px;overflow:auto;background:#f5f7fa;
                            padding:12px;border-radius:4px;font-size:11px;",
                   code),
          footer = tagList(
            downloadButton("builder_dl_selected_r", "Download as .R",
                           icon = icon("download"),
                           style = "background:#1b5e20;color:white;border:none;"),
            modalButton("Close")),
          size = "l", easyClose = TRUE))
      },

      "del" = {
        rv$captured_plots <- rv$captured_plots[-idx]
        schedule_save()
        notify_ok("Item removed from report.")
      }
    )
  })

  # ---- Rename modal save ----
  observeEvent(input$builder_rename_save, {
    cid <- isolate(edit_state$renaming_id)
    new_lbl <- trimws(input$builder_rename_value %||% "")
    edit_state$renaming_id <- NULL
    removeModal()
    if (is.null(cid) || !nzchar(new_lbl)) return()
    items <- isolate(rv$captured_plots)
    idx <- which(vapply(items, function(it) identical(it$id, cid), logical(1)))
    if (length(idx)) {
      rv$captured_plots[[idx]]$label <- new_lbl
      schedule_save()
    }
  })

  # ---- ONE download handler for any item's R code ----
  output$builder_dl_selected_r <- downloadHandler(
    filename = function() {
      cid <- isolate(edit_state$current_rcode)
      if (is.null(cid)) return("captured.R")
      items <- isolate(rv$captured_plots)
      idx <- which(vapply(items, function(it) identical(it$id, cid), logical(1)))
      if (!length(idx)) return("captured.R")
      base <- gsub("[^A-Za-z0-9_-]+", "_", items[[idx]]$label)
      sprintf("%s_%s.R", base, format(Sys.time(), "%Y%m%d_%H%M%S"))
    },
    content = function(file) {
      cid <- isolate(edit_state$current_rcode)
      items <- isolate(rv$captured_plots)
      idx <- which(vapply(items, function(it) identical(it$id, cid), logical(1)))
      txt <- if (length(idx)) items[[idx]]$r_code %||% "" else ""
      if (!nzchar(txt)) txt <- "# No R code was captured for this item."
      writeLines(txt, file)
    })

  # ---- Add heading / divider ----
  observeEvent(input$builder_add_heading, {
    h <- trimws(input$builder_heading_text %||% "")
    if (!nzchar(h)) { notify_err("Type a heading first."); return() }
    rv$captured_plots[[length(rv$captured_plots) + 1]] <-
      normalize_captured_entry(list(
        id = new_capture_id(),
        label = h,
        type = "heading",
        created = Sys.time(),
        include = TRUE))
    schedule_save()
    updateTextInput(session, "builder_heading_text", value = "")
    notify_ok("Heading added.")
  })

  # ---- Clear all ----
  observeEvent(input$builder_clear_all, {
    showModal(modalDialog(
      title = "Clear the report?",
      "This removes every captured plot, table, and heading from the report.",
      tags$strong(" The underlying data is untouched."),
      footer = tagList(modalButton("Cancel"),
                       actionButton("builder_clear_confirm", "Yes, clear all",
                                    class = "btn-danger"))))
  })
  observeEvent(input$builder_clear_confirm, {
    rv$captured_plots <- list()
    schedule_save()
    removeModal()
    notify_ok("Report cleared.")
  })

  # ---- Compile PDF ----
  output$builder_dl_pdf <- downloadHandler(
    filename = function() sprintf("%s_compiled_report_%s.pdf",
                                  rv$config$site_name %||% "RStone",
                                  format(Sys.time(), "%Y%m%d_%H%M")),
    content = function(file) {
      items <- Filter(function(it) isTRUE(it$include), rv$captured_plots)
      if (!length(items)) {
        grDevices::pdf(file, width = 11, height = 8.5)
        plot.new(); text(0.5, 0.5, "No items selected for inclusion.")
        grDevices::dev.off()
        return()
      }
      tryCatch({
        grDevices::pdf(file, width = 11, height = 8.5)
        on.exit(grDevices::dev.off())

        grid::grid.newpage()
        grid::grid.text(input$builder_report_title %||%
                          sprintf("%s - Compiled Report",
                                  rv$config$site_name %||% "RStone"),
                        y = 0.72,
                        gp = grid::gpar(fontsize = 26, fontface = "bold",
                                        col = "#1565c0"))
        if (nzchar(input$builder_report_subtitle %||% ""))
          grid::grid.text(input$builder_report_subtitle, y = 0.64,
                          gp = grid::gpar(fontsize = 16, col = "#37474f"))
        grid::grid.text(paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M")),
                        y = 0.50, gp = grid::gpar(fontsize = 12))
        grid::grid.text(paste("Analyst:", rv$config$analyst %||% "(not set)"),
                        y = 0.46, gp = grid::gpar(fontsize = 12))
        grid::grid.text(sprintf("Items included: %d of %d",
                                length(items), length(rv$captured_plots)),
                        y = 0.42, gp = grid::gpar(fontsize = 12, col = "#546e7a"))

        grid::grid.newpage()
        grid::grid.text("Contents", y = 0.94,
                        gp = grid::gpar(fontsize = 22, fontface = "bold",
                                        col = "#1565c0"))
        y0 <- 0.86
        for (i in seq_along(items)) {
          it <- items[[i]]
          line <- sprintf("%d.  [%s]  %s", i, toupper(it$type), it$label)
          grid::grid.text(line, x = 0.08, y = y0 - (i - 1) * 0.035,
                          just = "left",
                          gp = grid::gpar(fontsize = 11,
                                          col = if (it$type == "heading")
                                                  "#6a1b9a" else "#37474f"))
          if (y0 - i * 0.035 < 0.05 && i < length(items)) {
            grid::grid.newpage()
            y0 <- 0.94 + i * 0.035
          }
        }

        for (it in items) {
          if (it$type == "heading") {
            grid::grid.newpage()
            grid::grid.text(it$label, y = 0.50,
                            gp = grid::gpar(fontsize = 28, fontface = "bold",
                                            col = "#1565c0"))
            if (length(it$filter_snapshot))
              grid::grid.text(
                paste("Filter:", paste(vapply(it$filter_snapshot, function(p)
                  sprintf("%s = {%s}", p$label,
                          paste(p$values, collapse = ", ")),
                  character(1)), collapse = "; ")),
                y = 0.42,
                gp = grid::gpar(fontsize = 11, col = "#546e7a"))
          } else if (it$type == "plot" && !is.null(it$plot)) {
            tryCatch(print(it$plot + ggplot2::labs(caption = it$label)),
                     error = function(e) {
                       plot.new(); text(0.5, 0.5,
                                        paste("Plot error:", e$message))
                     })
          } else if (it$type == "table" && !is.null(it$table)) {
            grid::grid.newpage()
            grid::grid.text(it$label, y = 0.95,
                            gp = grid::gpar(fontsize = 16, fontface = "bold",
                                            col = "#1565c0"))
            tbl <- head(it$table, 35)
            txt <- capture.output(print(tbl, row.names = FALSE))
            for (k in seq_along(txt))
              grid::grid.text(txt[k], x = 0.06,
                              y = 0.88 - (k - 1) * 0.022,
                              just = "left",
                              gp = grid::gpar(fontfamily = "mono",
                                              fontsize = 9))
          }
        }
      }, error = function(e) {
        plot.new(); text(0.5, 0.5, paste("Error:", e$message))
        log_error(paste("Builder PDF:", e$message))
      })
    })

  # ---- Compile combined R script ----
  output$builder_dl_rscript <- downloadHandler(
    filename = function() sprintf("rstone_combined_%s.R",
                                  format(Sys.time(), "%Y%m%d_%H%M%S")),
    content = function(file) {
      items <- Filter(function(it) isTRUE(it$include), rv$captured_plots)
      lines <- c(
        "# ============================================================",
        sprintf("# RStone combined report script - %s",
                format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        sprintf("# Site: %s   Analyst: %s",
                rv$config$site_name %||% "(unset)",
                rv$config$analyst   %||% "(unset)"),
        sprintf("# %d items", length(items)),
        "# ============================================================",
        ""
      )
      for (i in seq_along(items)) {
        it <- items[[i]]
        lines <- c(lines,
          sprintf("# --- %d. [%s] %s ---", i, toupper(it$type), it$label), "")
        if (nzchar(it$r_code %||% ""))
          lines <- c(lines, it$r_code, "", "")
        else
          lines <- c(lines, "# (No R code available for this item)", "", "")
      }
      writeLines(lines, file)
    })
}
