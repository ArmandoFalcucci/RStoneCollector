# modules/filter_module.R
# Reusable dynamic, multi-select filter UI/server.
#
# Public API
# ----------
#   filter_ui_block(prefix)
#       Returns a tagList: an output container for the dynamic rows + the
#       "+ Add Condition" / "Clear all" buttons. Drop this into any panel.
#
#   setup_dynamic_filter(prefix, input, output, session, data_r, schema_r,
#                        initial_n = 1L)
#       Installs the server-side logic and returns a list of reactives:
#         $filtered  - data_r() with all active conditions applied
#         $descr     - list of {field, label, values} for each active condition
#         $conds     - the raw active condition list (for snapshotting)
#         $reset()   - function to reset to a single empty condition
#
# Both Reports and View Records call this with their own prefix so the IDs
# don't collide (rpt_* vs flt_*). No Shiny modules — keeps the call sites
# simple and matches the existing codebase style.

# Build a single condition row.
.cond_row <- function(prefix, cid, field_choices, can_remove = TRUE) {
  div(class = "cond-row",
      id = paste0(prefix, "_row_", cid),
      fluidRow(
        column(5,
          selectInput(paste0(prefix, "_field_", cid),
                      label = NULL,
                      choices = field_choices,
                      selected = "",
                      width = "100%")),
        column(6,
          shinyWidgets::pickerInput(
            paste0(prefix, "_vals_", cid),
            label = NULL,
            choices = character(0),
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              selectedTextFormat = "count > 2",
              countSelectedText = "{0} of {1} selected",
              noneSelectedText = "(any value)",
              dropupAuto = FALSE,
              size = 12),
            width = "100%")),
        column(1,
          if (can_remove)
            tags$button(
              type = "button",
              class = "btn btn-sm",
              style = "background:#c62828;color:white;border:none;margin-top:4px;",
              title = "Remove this condition",
              onclick = sprintf(
                "Shiny.setInputValue('%s_remove_cond', '%s', {priority:'event'});",
                prefix, cid),
              icon("times"))
          else NULL)
      )
  )
}

# UI block: render container + Add/Clear buttons.
filter_ui_block <- function(prefix, label = "Filter records:") {
  tagList(
    if (nzchar(label %||% ""))
      div(style = "font-weight:600;color:#37474f;margin-bottom:6px;font-size:13px;",
          icon("filter"), " ", label),
    uiOutput(paste0(prefix, "_conds_ui")),
    div(style = "margin-top:8px;display:flex;gap:8px;flex-wrap:wrap;",
      actionButton(paste0(prefix, "_add_cond"),
                   "Add Condition",
                   icon = icon("plus"),
                   style = "background:#1565c0;color:white;border:none;"),
      actionButton(paste0(prefix, "_clear"),
                   "Clear all",
                   icon = icon("times"),
                   style = "background:#cfd8dc;color:#37474f;border:none;"))
  )
}

# Server-side setup. Returns the filtered-data reactive plus helpers.
setup_dynamic_filter <- function(prefix, input, output, session,
                                  data_r, schema_r,
                                  initial_n = 1L) {

  state <- reactiveValues(
    ids     = paste0("c", seq_len(max(initial_n, 1L))),
    counter = max(initial_n, 1L)
  )

  # --- Render rows ---
  # We re-render the whole row container whenever state$ids changes so that
  # additions/removals are atomic and consistent.
  output[[paste0(prefix, "_conds_ui")]] <- renderUI({
    sch <- schema_r()
    if (is.null(sch)) return(div(tags$em("No schema loaded.")))
    field_choices <- c("(pick a field)" = "",
                       setNames(sch$fields,
                                vapply(sch$fields,
                                       function(f) prompt_label(sch$PROMPTS[[f]] %||% f),
                                       character(1))))
    rows <- lapply(state$ids, function(cid) {
      .cond_row(prefix, cid, field_choices,
                can_remove = length(state$ids) > 1)
    })
    do.call(tagList, rows)
  })

  # --- Refresh picker choices whenever the chosen field changes ---
  # One observer per row id is created lazily as ids are seen. Observers from
  # removed rows simply no-op (the inputs are gone), so we don't track or
  # destroy them — Shiny's invalidation system handles this cleanly.
  seen_ids <- character()
  observe({
    sch <- schema_r(); df <- data_r()
    if (is.null(sch)) return()
    ids_now <- state$ids
    new_ids <- setdiff(ids_now, seen_ids)
    for (cid in new_ids) {
      local({
        cid_l <- cid
        observeEvent(input[[paste0(prefix, "_field_", cid_l)]],
                     ignoreNULL = FALSE, ignoreInit = FALSE, {
          f <- input[[paste0(prefix, "_field_", cid_l)]]
          dfx <- isolate(data_r())
          if (is.null(f) || !nzchar(f) || is.null(dfx) || !(f %in% names(dfx))) {
            shinyWidgets::updatePickerInput(session,
                                            paste0(prefix, "_vals_", cid_l),
                                            choices = character(0),
                                            selected = character(0))
            return()
          }
          vals <- sort(unique(stats::na.omit(as.character(dfx[[f]]))))
          vals <- vals[nzchar(vals)]
          shinyWidgets::updatePickerInput(session,
                                          paste0(prefix, "_vals_", cid_l),
                                          choices = vals,
                                          selected = character(0))
        })
      })
    }
    seen_ids <<- unique(c(seen_ids, ids_now))
  })

  # --- Add condition ---
  observeEvent(input[[paste0(prefix, "_add_cond")]], {
    state$counter <- state$counter + 1L
    state$ids <- c(state$ids, paste0("c", state$counter))
  })

  # --- Remove condition (triggered by per-row JS onclick) ---
  observeEvent(input[[paste0(prefix, "_remove_cond")]], {
    cid <- input[[paste0(prefix, "_remove_cond")]]
    if (!is.null(cid) && cid %in% state$ids && length(state$ids) > 1) {
      state$ids <- setdiff(state$ids, cid)
    }
  }, ignoreInit = TRUE)

  # --- Clear all (reset to one empty condition) ---
  reset_fn <- function() {
    state$counter <- 1L
    state$ids <- "c1"
  }
  observeEvent(input[[paste0(prefix, "_clear")]], { reset_fn() })

  # --- Active conditions (skip rows with no field or no values) ---
  active_conds <- reactive({
    out <- list()
    for (cid in state$ids) {
      f <- input[[paste0(prefix, "_field_", cid)]]
      v <- input[[paste0(prefix, "_vals_", cid)]]
      if (!is.null(f) && nzchar(f) &&
          !is.null(v) && length(v) > 0 && any(nzchar(v))) {
        out[[length(out) + 1]] <- list(field  = f,
                                       values = v[nzchar(v)])
      }
    }
    out
  })

  # --- Filtered data ---
  filtered <- reactive({
    df <- data_r()
    if (is.null(df) || !nrow(df)) return(df)
    for (cnd in active_conds()) {
      if (cnd$field %in% names(df)) {
        df <- df[!is.na(df[[cnd$field]]) &
                 as.character(df[[cnd$field]]) %in% as.character(cnd$values),
                 , drop = FALSE]
      }
    }
    df
  })

  # --- Human-readable description ---
  descr <- reactive({
    sch <- schema_r()
    lapply(active_conds(), function(cnd) {
      lbl <- if (!is.null(sch))
               prompt_label(sch$PROMPTS[[cnd$field]] %||% cnd$field) else cnd$field
      list(field = cnd$field, label = lbl, values = cnd$values)
    })
  })

  list(filtered = filtered,
       descr    = descr,
       conds    = active_conds,
       reset    = reset_fn)
}

# Standard "filter status" banner shown above plots/tables so the user always
# knows what subset they're looking at.
filter_status_div <- function(descr_list, n_kept, n_total) {
  if (!length(descr_list)) {
    return(div(style = "padding:8px 12px;background:#e8f5e9;border-left:4px solid #2e7d32;
                       border-radius:4px;font-size:13px;margin-bottom:10px;",
               icon("filter"), " No filters applied. Analyzing all ",
               strong(n_total), " records."))
  }
  parts_str <- vapply(descr_list, function(p) {
    if (length(p$values) == 1)
      sprintf("%s = %s", p$label, p$values)
    else
      sprintf("%s \u2208 {%s}", p$label, paste(p$values, collapse = ", "))
  }, character(1))
  div(style = "padding:8px 12px;background:#fff8e1;border-left:4px solid #f57f17;
               border-radius:4px;font-size:13px;margin-bottom:10px;",
      icon("filter"), strong(" Filters active: "),
      tags$em(paste(parts_str, collapse = "  AND  ")), ".  ",
      "Analyzing ", strong(n_kept), " of ", n_total, " records.")
}
