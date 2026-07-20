# modules/server_reports.R
# Reports: Overview, Build Your Own, PCA, CA.
# v6: dynamic multi-select filters, R-script export, enhanced captured items.

setup_reports_server <- function(input, output, session, rv, shared) {
  notify_ok  <- shared$notify_ok  %||% function(m) NULL
  notify_err <- shared$notify_err %||% function(m) NULL

  pal <- c("#1565c0","#c62828","#2e7d32","#f57f17","#6a1b9a","#00695c","#ad1457","#37474f")

  # ---------- field type helpers ----------
  cat_fields <- reactive({
    sch <- rv$schema; if (is.null(sch)) return(character(0))
    sch$fields[vapply(sch$fields, function(f)
      toupper(sch$TYPES[[f]] %||% "TEXT") %in% c("MENU","TEXT"), logical(1))]
  })
  num_fields <- reactive({
    sch <- rv$schema; if (is.null(sch)) return(character(0))
    sch$fields[vapply(sch$fields, function(f)
      toupper(sch$TYPES[[f]] %||% "TEXT") %in% c("NUMERIC","INSTRUMENT"), logical(1))]
  })
  pretty_choices <- function(fields) {
    sch <- rv$schema
    setNames(fields, vapply(fields, function(f)
      prompt_label(sch$PROMPTS[[f]] %||% f), character(1)))
  }

  # ---------- Dynamic filter wired up ONCE ----------
  # The filter_ui_block + setup_dynamic_filter pair lives in modules/filter_module.R.
  # We render the block into a placeholder inside report_filters_ui() so the
  # UI panel stays simple.
  output$report_filters_ui <- renderUI({
    if (is.null(rv$schema)) return(NULL)
    filter_ui_block("rpt", label = "Filter records (apply to all report tabs):")
  })

  rpt_filter <- setup_dynamic_filter(
    "rpt", input, output, session,
    data_r   = reactive(rv$data),
    schema_r = reactive(rv$schema)
  )
  rpt_df              <- rpt_filter$filtered
  active_filter_descr <- rpt_filter$descr
  active_filter_conds <- rpt_filter$conds

  # Shared banner shown on PCA / CA / BB tabs
  filter_status_banner <- function() {
    parts <- active_filter_descr()
    df <- rpt_df()
    filter_status_div(parts, nrow(df), nrow(rv$data))
  }
  output$pca_filter_status <- renderUI(filter_status_banner())
  output$ca_filter_status  <- renderUI(filter_status_banner())
  output$bb_filter_status  <- renderUI(filter_status_banner())

  # ---------- Overview plot factories ----------
  bar_plot <- function(df, col, title, fill = "#1565c0") {
    if (!nrow(df) || !(col %in% names(df)) || all(is.na(df[[col]])))
      return(ggplot() + annotate("text", x = .5, y = .5, label = "No data",
                                 size = 5, color = "#90a4ae") + theme_void())
    d <- df %>% filter(!is.na(.data[[col]]), .data[[col]] != "") %>%
      count(.data[[col]], name = "n") %>% rename(Cat = 1) %>% arrange(desc(n))
    if (!nrow(d))
      return(ggplot() + annotate("text", x = .5, y = .5, label = "No data",
                                 size = 5, color = "#90a4ae") + theme_void())
    ggplot(d, aes(x = reorder(Cat, n), y = n)) +
      geom_col(fill = fill, alpha = .85, width = .72) +
      geom_text(aes(label = n), hjust = -.18, size = 3.2, fontface = "bold",
                color = "#37474f") +
      coord_flip() +
      labs(title = title, x = "", y = "Count") +
      theme_minimal(base_size = 11) +
      theme(axis.text.y = element_text(size = 10),
            panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", size = 12, color = "#1a237e")) +
      expand_limits(y = max(d$n) * 1.18)
  }

  meas_plot <- function(df, mcols) {
    if (!length(mcols))
      return(ggplot() + annotate("text", x = .5, y = .5,
                                  label = "No numeric variables",
                                  size = 5, color = "#90a4ae") + theme_void())
    for (cc in mcols) df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
    dl <- df %>% select(any_of(mcols)) %>%
      pivot_longer(everything(), names_to = "M", values_to = "V") %>%
      filter(!is.na(V))
    if (!nrow(dl))
      return(ggplot() + annotate("text", x = .5, y = .5,
                                  label = "No measurement data",
                                  size = 5, color = "#90a4ae") + theme_void())
    ggplot(dl, aes(x = M, y = V, fill = M)) +
      geom_violin(alpha = .6, trim = FALSE) +
      geom_boxplot(width = .12, fill = "white", outlier.size = 1.4) +
      labs(title = "Measurement Distributions", x = "", y = "Value") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none", panel.grid.minor = element_blank(),
            axis.text.x = element_text(face = "bold", angle = 20, hjust = 1),
            plot.title = element_text(face = "bold", size = 13, color = "#1a237e")) +
      scale_fill_manual(values = rep(pal, length.out = length(mcols)))
  }

  stats_table <- function(df, mcols) {
    if (!length(mcols)) return(NULL)
    for (cc in mcols) df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
    rows <- lapply(mcols, function(cc) {
      v <- stats::na.omit(df[[cc]]); if (!length(v)) return(NULL)
      data.frame(Measurement = cc, N = length(v),
                 Mean = round(mean(v), 2), Median = round(median(v), 2),
                 SD = round(stats::sd(v), 2),
                 Min = round(min(v), 2), Max = round(max(v), 2),
                 stringsAsFactors = FALSE)
    })
    do.call(rbind, Filter(Negate(is.null), rows))
  }

  # ----- Overview: user-selectable fields -----
  ov_all_menu <- reactive({
    sch <- rv$schema; if (is.null(sch)) return(character(0))
    sch$fields[vapply(sch$fields, function(f)
      toupper(sch$TYPES[[f]] %||% "TEXT") == "MENU", logical(1))]
  })
  ov_all_num <- reactive({
    sch <- rv$schema; if (is.null(sch)) return(character(0))
    sch$fields[vapply(sch$fields, function(f)
      toupper(sch$TYPES[[f]] %||% "TEXT") %in% c("NUMERIC","INSTRUMENT"), logical(1))]
  })
  # What actually gets plotted: the user's picks, or a sensible default when empty.
  ov_bar_fields <- reactive({
    all <- ov_all_menu()
    sel <- intersect(input$ov_bar_fields, all)
    if (!length(sel)) head(all, 6) else sel
  })
  ov_meas_fields <- reactive({
    all <- ov_all_num()
    sel <- intersect(input$ov_meas_fields, all)
    if (!length(sel)) all else sel
  })

  output$ov_controls_ui <- renderUI({
    sch <- rv$schema
    if (is.null(sch)) return(tags$em("Load a schema to configure the overview."))
    menu <- ov_all_menu(); num <- ov_all_num()
    lbl <- function(f) prompt_label(sch$PROMPTS[[f]] %||% f)
    menu_choices <- setNames(menu, vapply(menu, lbl, character(1)))
    num_choices  <- setNames(num,  vapply(num,  lbl, character(1)))
    # Defaults from saved config (isolated so saving doesn't rebuild this UI).
    cfg <- isolate(rv$config %||% list())
    bar_sel  <- intersect(cfg$overview_bar_fields  %||% head(menu, 6), menu)
    if (!length(bar_sel))  bar_sel  <- head(menu, 6)
    meas_sel <- intersect(cfg$overview_meas_fields %||% num, num)
    if (!length(meas_sel)) meas_sel <- num
    fluidRow(
      column(6, shinyWidgets::pickerInput(
        "ov_bar_fields", "Category charts (one bar chart each):",
        choices = menu_choices, selected = bar_sel, multiple = TRUE,
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 2"))),
      column(6, shinyWidgets::pickerInput(
        "ov_meas_fields", "Measurement variables (distribution plot):",
        choices = num_choices, selected = meas_sel, multiple = TRUE,
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 2")))
    )
  })

  # Persist the choices per project (config.json).
  observeEvent(input$ov_bar_fields, {
    cfg <- rv$config %||% list(); cfg$overview_bar_fields <- input$ov_bar_fields
    rv$config <- cfg; save_config(cfg, CONFIG_FILE)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$ov_meas_fields, {
    cfg <- rv$config %||% list(); cfg$overview_meas_fields <- input$ov_meas_fields
    rv$config <- cfg; save_config(cfg, CONFIG_FILE)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  output$report_plots_ui <- renderUI({
    sch <- rv$schema; if (is.null(sch)) return(NULL)
    plot_fields <- ov_bar_fields()
    fr <- list()
    if (length(plot_fields)) for (i in seq(1, length(plot_fields), by = 2)) {
      pair <- plot_fields[i:min(i + 1, length(plot_fields))]
      cols <- lapply(pair, function(f) {
        column(6, box(width = 12,
                       title = prompt_label(sch$PROMPTS[[f]] %||% f),
                       status = "info",
                       plotOutput(paste0("plt_", f), height = "280px")))
      })
      fr[[length(fr) + 1]] <- do.call(fluidRow, cols)
    }
    if (length(ov_meas_fields()))
      fr[[length(fr) + 1]] <- fluidRow(column(12,
        box(width = 12, title = "Measurement Distributions", status = "success",
            plotOutput("plt_meas", height = "360px"))))
    do.call(tagList, fr)
  })

  observe({
    sch <- rv$schema; if (is.null(sch)) return()
    pick <- ov_bar_fields()
    for (i in seq_along(pick)) {
      local({
        f   <- pick[i]
        col <- pal[((i - 1) %% length(pal)) + 1]
        output[[paste0("plt_", f)]] <- renderPlot({
          bar_plot(rpt_df(), f, prompt_label(sch$PROMPTS[[f]] %||% f), col)
        })
      })
    }
  })

  output$plt_meas <- renderPlot({
    sch <- rv$schema; if (is.null(sch)) return(NULL)
    meas_plot(rpt_df(), ov_meas_fields())
  })

  stats_data <- reactive({
    sch <- rv$schema; if (is.null(sch)) return(NULL)
    mcols <- sch$fields[vapply(sch$fields, function(f)
      toupper(sch$TYPES[[f]] %||% "TEXT") %in% c("NUMERIC","INSTRUMENT"),
      logical(1))]
    stats_table(rpt_df(), mcols)
  })
  output$tbl_stats <- renderTable({ stats_data() },
                                   striped = TRUE, hover = TRUE, bordered = TRUE)

  # ===== DATA QUALITY =====
  # Whole-dataset QC. Deliberately ignores the Overview filters - you want to see
  # every problem record, not just the filtered ones. Reuses the schema's own
  # rules so the checks match what the wizard enforces on entry.
  dq_issues <- reactive({
    sch <- rv$schema; df <- rv$data
    if (is.null(sch) || is.null(df) || !nrow(df)) return(NULL)
    idf <- sch$UNIQUE[1] %||% sch$fields[1]
    ids <- if (idf %in% names(df)) as.character(df[[idf]]) else as.character(seq_len(nrow(df)))
    out <- list()
    add <- function(rows, field, value, issue) {
      if (!length(rows)) return(invisible())
      out[[length(out) + 1]] <<- data.frame(
        Record = ids[rows], Field = field,
        Value = as.character(value), Issue = issue, stringsAsFactors = FALSE)
    }
    for (f in sch$fields) {
      if (!f %in% names(df)) next
      col <- as.character(df[[f]])
      filled <- !is.na(col) & nzchar(trimws(col))
      type_ <- toupper(sch$TYPES[[f]] %||% "TEXT")
      if (f %in% sch$REQUIRED) add(which(!filled), f, "", "Required value missing")
      idx <- which(filled); if (!length(idx)) next
      v <- col[idx]
      if (type_ %in% c("NUMERIC","INSTRUMENT")) {
        n <- suppressWarnings(as.numeric(v))
        add(idx[is.na(n)], f, v[is.na(n)], "Not a number")
        mn <- sch$MIN[[f]]; mx <- sch$MAX[[f]]
        if (!is.null(mn) && !is.na(mn)) { sel <- !is.na(n) & n < mn
          add(idx[sel], f, v[sel], sprintf("Below minimum (%s)", mn)) }
        if (!is.null(mx) && !is.na(mx)) { sel <- !is.na(n) & n > mx
          add(idx[sel], f, v[sel], sprintf("Above maximum (%s)", mx)) }
      }
      if (type_ == "MENU") {
        opts <- sch$OPTS[[f]] %||% character()
        if (length(opts)) { sel <- !(v %in% opts)
          add(idx[sel], f, v[sel], "Not a valid menu option") }
      }
      pat <- sch$PATTERN[[f]]
      if (!is.null(pat) && nzchar(pat)) {
        sel <- !grepl(pat, v, perl = TRUE)
        add(idx[sel], f, v[sel], sprintf("Doesn't match pattern %s", pat))
      }
    }
    if (idf %in% names(df)) {
      dup <- which(duplicated(ids) | duplicated(ids, fromLast = TRUE))
      add(dup, idf, ids[dup], "Duplicate ID")
    }
    if (!length(out))
      return(data.frame(Record = character(0), Field = character(0),
                        Value = character(0), Issue = character(0)))
    do.call(rbind, out)
  })

  dq_completeness <- reactive({
    sch <- rv$schema; df <- rv$data
    if (is.null(sch) || is.null(df) || !nrow(df)) return(NULL)
    flds <- intersect(sch$fields, names(df)); n <- nrow(df)
    do.call(rbind, lapply(flds, function(f) {
      col <- as.character(df[[f]])
      filled <- sum(!is.na(col) & nzchar(trimws(col)))
      data.frame(Field = prompt_label(sch$PROMPTS[[f]] %||% f),
                 Missing = n - filled, Pct = round(100 * filled / n, 1),
                 stringsAsFactors = FALSE)
    }))
  })

  dq_outliers <- reactive({
    sch <- rv$schema; df <- rv$data
    if (is.null(sch) || is.null(df) || !nrow(df)) return(NULL)
    mcols <- intersect(sch$fields[vapply(sch$fields, function(f)
      toupper(sch$TYPES[[f]] %||% "TEXT") %in% c("NUMERIC","INSTRUMENT"),
      logical(1))], names(df))
    if (!length(mcols)) return(NULL)
    do.call(rbind, lapply(mcols, function(f) {
      lab <- prompt_label(sch$PROMPTS[[f]] %||% f)
      v <- suppressWarnings(as.numeric(df[[f]])); v <- v[!is.na(v)]
      if (length(v) < 4)
        return(data.frame(Field = lab, N = length(v), Min = NA, Max = NA,
                          `Outliers (1.5 IQR)` = NA, check.names = FALSE,
                          stringsAsFactors = FALSE))
      q <- stats::quantile(v, c(.25, .75)); iqr <- q[[2]] - q[[1]]
      data.frame(Field = lab, N = length(v),
                 Min = round(min(v), 2), Max = round(max(v), 2),
                 `Outliers (1.5 IQR)` = sum(v < q[[1]] - 1.5*iqr | v > q[[2]] + 1.5*iqr),
                 check.names = FALSE, stringsAsFactors = FALSE)
    }))
  })

  output$dq_summary <- renderUI({
    df <- rv$data
    if (is.null(df) || !nrow(df)) return(tags$em("No records yet."))
    iss <- dq_issues(); n_iss <- if (is.null(iss)) 0 else nrow(iss)
    n_rec <- if (n_iss) length(unique(iss$Record)) else 0
    tags$span(style = "font-size:15px;",
      sprintf("%d records checked. ", nrow(df)),
      if (n_iss == 0) tags$strong(style = "color:#2e7d32;", "No issues found.")
      else tags$strong(style = "color:#c62828;",
        sprintf("%d issue%s across %d record%s to review.",
                n_iss, if (n_iss == 1) "" else "s",
                n_rec, if (n_rec == 1) "" else "s")))
  })

  output$dq_complete_plot <- renderPlot({
    d <- dq_completeness(); if (is.null(d)) return(NULL)
    d$Field <- factor(d$Field, levels = d$Field[order(d$Pct)])
    ggplot(d, aes(x = Field, y = Pct)) +
      geom_col(fill = "#1565c0", alpha = .85) +
      geom_text(aes(label = paste0(Pct, "%")), hjust = -0.1, size = 3.2) +
      coord_flip(ylim = c(0, 112)) +
      labs(x = NULL, y = "% of records filled") +
      theme_minimal(base_size = 12)
  })

  output$dq_outliers <- renderTable({ dq_outliers() },
                                     striped = TRUE, hover = TRUE, bordered = TRUE, na = "-")

  output$dq_issues <- DT::renderDT({
    iss <- dq_issues()
    if (is.null(iss) || !nrow(iss))
      return(DT::datatable(data.frame(Message = "No issues found."),
                           rownames = FALSE, options = list(dom = "t")))
    DT::datatable(iss, rownames = FALSE, filter = "top",
                  options = list(pageLength = 15, order = list(list(0, "asc"))))
  })

  output$dq_dl_csv <- downloadHandler(
    filename = function() sprintf("data_quality_%s.csv", format(Sys.time(), "%Y%m%d_%H%M")),
    content  = function(file) {
      iss <- dq_issues(); if (is.null(iss)) iss <- data.frame()
      utils::write.csv(iss, file, row.names = FALSE)
    })

  # ===== BUILD YOUR OWN =====

  output$bb_var_ui <- renderUI({
    sch <- rv$schema; if (is.null(sch)) return(NULL)
    pt  <- input$bb_plot %||% "bar"
    cat_ch <- pretty_choices(cat_fields())
    num_ch <- pretty_choices(num_fields())
    tagList(
      if (pt %in% c("density","hist"))
        selectInput("bb_xvar", "Numeric variable (X):",
                    choices = c("(pick one)" = "", num_ch))
      else if (pt == "scatter")
        selectInput("bb_xvar", "X (numeric):",
                    choices = c("(pick one)" = "", num_ch))
      else
        selectInput("bb_xvar", "X / categorical variable:",
                    choices = c("(pick one)" = "", cat_ch)),
      if (pt == "scatter")
        selectInput("bb_yvar", "Y (numeric):",
                    choices = c("(pick one)" = "", num_ch))
      else if (pt %in% c("box","violin"))
        selectInput("bb_yvar", "Y (numeric):",
                    choices = c("(pick one)" = "", num_ch))
      else NULL,
      if (pt %in% c("stacked","grouped","proportional"))
        selectInput("bb_groupvar", "Stack / group by (required):",
                    choices = c("(pick one)" = "", cat_ch))
      else
        selectInput("bb_groupvar", "Colour / group by (optional):",
                    choices = c("(none)" = "", cat_ch)),
      selectInput("bb_facet", "Facet by:", choices = c("(none)" = "", cat_ch))
    )
  })

  pick_field <- function(candidates, type = c("cat","num")) {
    type <- match.arg(type)
    pool <- if (type == "cat") cat_fields() else num_fields()
    if (!length(pool)) return("")
    up <- toupper(pool)
    for (cand in candidates) {
      hit <- pool[which(up == toupper(cand))]
      if (length(hit)) return(hit[1])
    }
    for (cand in candidates) {
      hit <- pool[grepl(toupper(cand), up, fixed = TRUE)]
      if (length(hit)) return(hit[1])
    }
    ""
  }

  apply_preset <- function(plot, xvar = "", yvar = "", group = "",
                            facet = "", flip = FALSE) {
    updateSelectInput(session, "bb_plot", selected = plot)
    later::later(function() {
      if (nzchar(xvar))  updateSelectInput(session, "bb_xvar",     selected = xvar)
      if (nzchar(yvar))  updateSelectInput(session, "bb_yvar",     selected = yvar)
      if (nzchar(group)) updateSelectInput(session, "bb_groupvar", selected = group)
      if (nzchar(facet)) updateSelectInput(session, "bb_facet",    selected = facet)
      updateCheckboxInput(session, "bb_flip", value = isTRUE(flip))
    }, delay = 0.15)
  }

  observeEvent(input$preset_strat, {
    x <- pick_field(c("Unit","Square","Layer","Level","Spit","Stratum"), "cat")
    g <- pick_field(c("LithicArtifactClass","ArtifactClass","Class","Category"), "cat")
    apply_preset("stacked", xvar = x, group = g)
  })
  observeEvent(input$preset_length, {
    x <- pick_field(c("Unit","Layer","Level","Square","Stratum","Spit"), "cat")
    y <- pick_field(c("MaxLength","Length","TechLength"), "num")
    apply_preset("box", xvar = x, yvar = y)
  })
  observeEvent(input$preset_rawmat, {
    x <- pick_field(c("Unit","Layer","Level","Square","Stratum","Spit"), "cat")
    g <- pick_field(c("RawMaterial","Material","Raw"), "cat")
    apply_preset("proportional", xvar = x, group = g)
  })
  observeEvent(input$preset_scatter, {
    x <- pick_field(c("MaxLength","Length","TechLength"), "num")
    y <- pick_field(c("MaxWidth","Width","TechWidth"),    "num")
    g <- pick_field(c("LithicArtifactClass","ArtifactClass","Class","RawMaterial"), "cat")
    apply_preset("scatter", xvar = x, yvar = y, group = g)
  })

  build_plot <- function(df, pt, x, y, g, facet) {
    sch <- rv$schema
    if (is.null(sch) || is.null(df) || !nrow(df))
      return(ggplot() + annotate("text", x = .5, y = .5, label = "No data",
                                  size = 5, color = "#90a4ae") + theme_void())
    if (is.null(x) || !nzchar(x) || !(x %in% names(df)))
      return(ggplot() + annotate("text", x = .5, y = .5,
                                  label = "Pick an X variable",
                                  size = 5, color = "#90a4ae") + theme_void())
    needs_y <- pt %in% c("box","violin","scatter")
    if (needs_y && (is.null(y) || !nzchar(y) || !(y %in% names(df))))
      return(ggplot() + annotate("text", x = .5, y = .5,
                                  label = "Pick a Y (numeric) variable",
                                  size = 5, color = "#90a4ae") + theme_void())
    needs_g <- pt %in% c("stacked","grouped","proportional")
    if (needs_g && (is.null(g) || !nzchar(g) || !(g %in% names(df))))
      return(ggplot() + annotate("text", x = .5, y = .5,
                                  label = "Pick a Group variable",
                                  size = 5, color = "#90a4ae") + theme_void())

    d <- df
    if (pt %in% c("density","hist","scatter")) {
      d[[x]] <- suppressWarnings(as.numeric(d[[x]]))
      d <- d[!is.na(d[[x]]), , drop = FALSE]
    } else {
      d[[x]] <- as.character(d[[x]])
      d <- d[!is.na(d[[x]]) & nzchar(d[[x]]), , drop = FALSE]
    }
    if (nzchar(y %||% "")) {
      d[[y]] <- suppressWarnings(as.numeric(d[[y]]))
      d <- d[!is.na(d[[y]]), , drop = FALSE]
    }
    if (nzchar(g %||% "")) {
      d[[g]] <- as.character(d[[g]])
      d <- d[!is.na(d[[g]]) & nzchar(d[[g]]), , drop = FALSE]
    }
    if (nzchar(facet %||% "")) {
      d[[facet]] <- as.character(d[[facet]])
      d <- d[!is.na(d[[facet]]) & nzchar(d[[facet]]), , drop = FALSE]
    }
    if (!nrow(d))
      return(ggplot() + annotate("text", x = .5, y = .5,
                                  label = "No data after filtering",
                                  size = 5, color = "#90a4ae") + theme_void())

    x_lab <- prompt_label(sch$PROMPTS[[x]] %||% x)
    y_lab <- if (nzchar(y %||% "")) prompt_label(sch$PROMPTS[[y]] %||% y) else "Count"
    g_lab <- if (nzchar(g %||% "")) prompt_label(sch$PROMPTS[[g]] %||% g) else NULL

    p <- switch(pt,
      "bar" = {
        cnt <- d %>% count(.data[[x]],
                            !!!(if (nzchar(g %||% "")) rlang::syms(g) else NULL),
                            name = "n")
        if (nzchar(g %||% ""))
          ggplot(cnt, aes(x = .data[[x]], y = n, fill = .data[[g]])) +
            geom_col(position = "dodge")
        else
          ggplot(cnt, aes(x = reorder(.data[[x]], n), y = n)) +
            geom_col(fill = pal[1], width = .72) +
            geom_text(aes(label = n), hjust = -0.18, size = 3.5, fontface = "bold")
      },
      "stacked" = {
        cnt <- d %>% count(.data[[x]], .data[[g]], name = "n")
        ggplot(cnt, aes(x = .data[[x]], y = n, fill = .data[[g]])) + geom_col()
      },
      "grouped" = {
        cnt <- d %>% count(.data[[x]], .data[[g]], name = "n")
        ggplot(cnt, aes(x = .data[[x]], y = n, fill = .data[[g]])) +
          geom_col(position = "dodge")
      },
      "proportional" = {
        cnt <- d %>% count(.data[[x]], .data[[g]], name = "n")
        ggplot(cnt, aes(x = .data[[x]], y = n, fill = .data[[g]])) +
          geom_col(position = "fill") +
          scale_y_continuous(labels = scales::percent_format())
      },
      "box" = {
        p0 <- ggplot(d, aes(x = .data[[x]], y = .data[[y]]))
        if (nzchar(g %||% ""))
          p0 + geom_boxplot(aes(fill = .data[[g]]), alpha = .8, outlier.size = 1.2)
        else
          p0 + geom_boxplot(fill = pal[1], alpha = .8, outlier.size = 1.2)
      },
      "violin" = {
        p0 <- ggplot(d, aes(x = .data[[x]], y = .data[[y]]))
        if (nzchar(g %||% ""))
          p0 + geom_violin(aes(fill = .data[[g]]), alpha = .55) +
               geom_boxplot(aes(fill = .data[[g]]), width = .12,
                            position = position_dodge(.9), outlier.size = .8)
        else
          p0 + geom_violin(fill = pal[1], alpha = .55) +
               geom_boxplot(width = .12, fill = "white", outlier.size = .8)
      },
      "density" = {
        ggplot(d, aes(x = .data[[x]],
                      fill  = if (nzchar(g %||% "")) .data[[g]] else NULL,
                      color = if (nzchar(g %||% "")) .data[[g]] else NULL)) +
          geom_density(alpha = .35)
      },
      "hist" = {
        ggplot(d, aes(x = .data[[x]],
                      fill = if (nzchar(g %||% "")) .data[[g]] else NULL)) +
          geom_histogram(bins = 25, alpha = .85, color = "white",
                         position = "identity")
      },
      "scatter" = {
        ggplot(d, aes(x = .data[[x]], y = .data[[y]],
                      color = if (nzchar(g %||% "")) .data[[g]] else NULL)) +
          geom_point(size = 2.5, alpha = .75) +
          geom_smooth(method = "lm", se = FALSE, color = "#37474f",
                      linetype = "dashed", linewidth = .5)
      }
    )

    if (nzchar(g %||% "")) {
      lvl <- length(unique(d[[g]]))
      p <- p + scale_fill_manual(values  = rep(pal, length.out = lvl)) +
               scale_color_manual(values = rep(pal, length.out = lvl))
    }

    title <- paste0(y_lab, " by ", x_lab,
                    if (nzchar(g %||% "")) paste0(" (by ", g_lab, ")") else "",
                    if (nzchar(facet %||% "")) paste0(" - faceted by ",
                                                       prompt_label(sch$PROMPTS[[facet]] %||% facet)) else "")
    p <- p + labs(x = x_lab, y = y_lab, fill = g_lab, color = g_lab,
                  title = title) +
      theme_minimal(base_size = 12) +
      theme(plot.title       = element_text(face = "bold", color = "#1a237e"),
            panel.grid.minor = element_blank(),
            axis.text.x      = element_text(angle = if (nzchar(facet %||% "")) 0 else 20,
                                            hjust = 1))
    if (nzchar(facet %||% "")) p <- p + facet_wrap(stats::as.formula(paste("~", facet)))
    if (isTRUE(input$bb_flip) && pt %in% c("bar","stacked","grouped","proportional","box","violin"))
      p <- p + coord_flip()
    p
  }

  bb_plot_obj <- reactive({
    build_plot(rpt_df(), input$bb_plot %||% "bar",
               input$bb_xvar, input$bb_yvar, input$bb_groupvar, input$bb_facet)
  })

  output$bb_plot_out <- renderPlot({ bb_plot_obj() })

  # Stats annotation
  output$bb_stats_text <- renderPrint({
    if (!isTRUE(input$bb_stats)) { cat(""); return() }
    df <- rpt_df(); sch <- rv$schema
    if (is.null(sch) || !nrow(df)) { cat(""); return() }
    pt <- input$bb_plot %||% "bar"
    x  <- input$bb_xvar; y <- input$bb_yvar; g <- input$bb_groupvar

    res <- if (pt %in% c("box","violin") && !is_empty_val(x) && !is_empty_val(y)) {
      d <- df; d[[y]] <- suppressWarnings(as.numeric(d[[y]]))
      quick_test_numeric(d, x, y)
    } else if (pt == "scatter" && !is_empty_val(x) && !is_empty_val(y)) {
      quick_corr(df, x, y)
    } else if (pt %in% c("stacked","grouped","proportional","bar") &&
                !is_empty_val(x) && !is_empty_val(g)) {
      quick_test_categorical(df, x, g)
    } else NULL

    if (is.null(res)) cat("No statistical test for this combination.")
    else cat(res$label)
  })

  # Cross-tab table data - shared by the DT renderer and the CSV download
  bb_table_data <- reactive({
    df <- rpt_df(); sch <- rv$schema
    if (is.null(sch) || !nrow(df)) return(NULL)
    pt <- input$bb_plot %||% "bar"
    x  <- input$bb_xvar; y <- input$bb_yvar; g <- input$bb_groupvar
    if (is.null(x) || !nzchar(x) || !x %in% names(df)) return(NULL)
    d <- df
    if (pt %in% c("density","hist","scatter")) {
      d[[x]] <- suppressWarnings(as.numeric(d[[x]]))
    } else { d[[x]] <- as.character(d[[x]]) }
    d <- d[!is.na(d[[x]]), , drop = FALSE]
    if (!is.null(g) && nzchar(g) && g %in% names(df)) {
      d[[g]] <- as.character(d[[g]])
      d <- d[!is.na(d[[g]]) & nzchar(d[[g]]), , drop = FALSE]
    } else g <- NULL
    if (!is.null(y) && nzchar(y) && y %in% names(df)) {
      d[[y]] <- suppressWarnings(as.numeric(d[[y]]))
      d <- d[!is.na(d[[y]]), , drop = FALSE]
    } else y <- NULL
    if (!nrow(d)) return(NULL)
    if (!is.null(y) && !is.null(g)) {
      d %>% group_by(.data[[x]], .data[[g]]) %>%
        summarise(N = sum(!is.na(.data[[y]])),
                  Mean = round(mean(.data[[y]], na.rm = TRUE), 2),
                  Median = round(median(.data[[y]], na.rm = TRUE), 2),
                  SD = round(sd(.data[[y]], na.rm = TRUE), 2),
                  Min = round(min(.data[[y]], na.rm = TRUE), 2),
                  Max = round(max(.data[[y]], na.rm = TRUE), 2),
                  .groups = "drop")
    } else if (!is.null(y)) {
      d %>% group_by(.data[[x]]) %>%
        summarise(N = sum(!is.na(.data[[y]])),
                  Mean = round(mean(.data[[y]], na.rm = TRUE), 2),
                  Median = round(median(.data[[y]], na.rm = TRUE), 2),
                  SD = round(sd(.data[[y]], na.rm = TRUE), 2),
                  Min = round(min(.data[[y]], na.rm = TRUE), 2),
                  Max = round(max(.data[[y]], na.rm = TRUE), 2),
                  .groups = "drop")
    } else if (!is.null(g)) {
      d %>% count(.data[[x]], .data[[g]], name = "N") %>%
        tidyr::pivot_wider(names_from = all_of(g), values_from = N, values_fill = 0)
    } else {
      d %>% count(.data[[x]], name = "N") %>% arrange(desc(N))
    }
  })

  output$bb_table <- DT::renderDT({
    bb_table_data()
  }, options = list(pageLength = 25, scrollX = TRUE, dom = "lftip"))

  # ---- Build label, R-code, capture ----
  bb_label <- reactive({
    sch <- rv$schema
    pt  <- input$bb_plot %||% "plot"
    x   <- input$bb_xvar %||% ""
    y   <- input$bb_yvar %||% ""
    g   <- input$bb_groupvar %||% ""
    fc  <- input$bb_facet %||% ""
    lblf <- function(f) if (nzchar(f)) prompt_label(sch$PROMPTS[[f]] %||% f) else ""
    sprintf("%s: %s%s%s%s",
            pt, lblf(x),
            if (nzchar(y))  paste0(" vs ", lblf(y))  else "",
            if (nzchar(g))  paste0(" / ",  lblf(g))  else "",
            if (nzchar(fc)) paste0(" | ",  lblf(fc)) else "")
  })

  bb_r_code <- reactive({
    sch <- rv$schema
    csv <- rv$config$database_file %||% sch$suggested_db
    bb_to_r(input$bb_plot %||% "bar",
            input$bb_xvar, input$bb_yvar, input$bb_groupvar,
            input$bb_facet, isTRUE(input$bb_flip),
            active_filter_conds(), csv,
            x_label = if (nzchar(input$bb_xvar %||% ""))
                        prompt_label(sch$PROMPTS[[input$bb_xvar]] %||% input$bb_xvar) else NULL,
            y_label = if (nzchar(input$bb_yvar %||% ""))
                        prompt_label(sch$PROMPTS[[input$bb_yvar]] %||% input$bb_yvar) else NULL,
            g_label = if (nzchar(input$bb_groupvar %||% ""))
                        prompt_label(sch$PROMPTS[[input$bb_groupvar]] %||% input$bb_groupvar) else NULL)
  })

  observeEvent(input$bb_capture, {
    p <- bb_plot_obj(); if (is.null(p)) return()
    rv$captured_plots[[length(rv$captured_plots) + 1]] <-
      normalize_captured_entry(list(
        id    = new_capture_id(),
        label = bb_label(),
        type  = "plot",
        plot  = p,
        table = bb_table_data(),
        filter_snapshot = active_filter_descr(),
        config = list(plot_type = input$bb_plot,
                      x = input$bb_xvar, y = input$bb_yvar,
                      g = input$bb_groupvar, facet = input$bb_facet,
                      flip = isTRUE(input$bb_flip)),
        r_code = bb_r_code(),
        created = Sys.time(),
        include = TRUE))
    save_captured(rv$captured_plots)
    notify_ok(paste("Captured:", bb_label()))
  })

  observeEvent(input$bb_clear_captured, {
    rv$captured_plots <- list()
    save_captured(list())
    notify_ok("Captured items cleared.")
  })

  output$bb_captured_count <- renderText({
    n <- length(rv$captured_plots)
    if (!n) "No items captured yet."
    else sprintf("%d item%s in the report.", n, if (n>1) "s" else "")
  })

  # ===== PCA =====

  output$pca_var_ui <- renderUI({
    if (is.null(rv$schema)) return(NULL)
    selectInput("pca_vars", "Numeric variables (pick 2+):",
                choices = pretty_choices(num_fields()),
                multiple = TRUE)
  })

  observe({
    if (is.null(rv$schema)) return()
    updateSelectInput(session, "pca_group",
                      choices = c("(none)" = "", pretty_choices(cat_fields())))
  })

  pca_obj <- reactive({
    vs <- input$pca_vars
    if (is.null(vs) || length(vs) < 2) return(NULL)
    g <- input$pca_group; if (!nzchar(g %||% "")) g <- NULL
    do_pca(rpt_df(), vs, group_col = g)
  })

  pca_score_plot <- function(obj) {
    if (is.null(obj))
      return(ggplot() + annotate("text", x=.5, y=.5,
        label = "Pick 2+ numeric variables", size = 5, color = "#90a4ae") + theme_void())
    if (!is.null(obj$error))
      return(ggplot() + annotate("text", x=.5, y=.5,
        label = obj$error, size = 5, color = "#c62828") + theme_void())
    sc <- obj$scores
    has_group <- "Group" %in% names(sc) && length(unique(sc$Group)) > 1
    ve <- round(100 * obj$var_exp[1:2], 1)
    p <- ggplot(sc, aes(x = PC1, y = PC2,
                         color = if (has_group) Group else NULL)) +
      geom_point(size = 3, alpha = .8) +
      labs(x = sprintf("PC1 (%.1f%%)", ve[1]),
           y = sprintf("PC2 (%.1f%%)", ve[2]),
           color = if (has_group) "Group" else NULL,
           title = "PCA scores") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", color = "#1a237e"))
    if (has_group)
      p <- p + scale_color_manual(values = rep(pal,
        length.out = length(unique(sc$Group))))
    p
  }
  pca_scree_plot <- function(obj) {
    if (is.null(obj) || !is.null(obj$error)) return(ggplot() + theme_void())
    ve <- data.frame(PC = paste0("PC", seq_along(obj$var_exp)),
                     Var = obj$var_exp * 100)
    ve$PC <- factor(ve$PC, levels = ve$PC)
    ggplot(ve, aes(PC, Var)) +
      geom_col(fill = pal[1], alpha = .85) +
      geom_text(aes(label = sprintf("%.1f%%", Var)), vjust = -.3,
                size = 3, fontface = "bold") +
      labs(x = "", y = "% variance", title = "Variance explained") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold", color = "#1a237e"),
            panel.grid.minor = element_blank())
  }

  output$pca_scores <- renderPlot({ pca_score_plot(pca_obj()) })
  output$pca_scree  <- renderPlot({ pca_scree_plot(pca_obj()) })
  output$pca_loadings_tbl <- DT::renderDT({
    obj <- pca_obj(); if (is.null(obj) || !is.null(obj$error)) return(NULL)
    obj$loadings
  }, options = list(pageLength = 15, scrollX = TRUE, dom = "tip"))

  pca_r_code <- reactive({
    sch <- rv$schema
    csv <- rv$config$database_file %||% sch$suggested_db
    pca_to_r(input$pca_vars, input$pca_group, active_filter_conds(), csv)
  })

  observeEvent(input$pca_capture, {
    obj <- pca_obj()
    if (is.null(obj) || !is.null(obj$error)) {
      notify_err("Cannot capture PCA: pick valid variables first."); return()
    }
    # Single combined PCA entry (scores + scree)
    rv$captured_plots[[length(rv$captured_plots) + 1]] <-
      normalize_captured_entry(list(
        id    = new_capture_id(),
        label = sprintf("PCA: %s", paste(input$pca_vars, collapse = ", ")),
        type  = "plot",
        plot  = pca_score_plot(obj),
        table = obj$loadings,
        filter_snapshot = active_filter_descr(),
        config = list(method = "PCA", vars = input$pca_vars,
                      group = input$pca_group),
        r_code = pca_r_code(),
        created = Sys.time(),
        include = TRUE))
    rv$captured_plots[[length(rv$captured_plots) + 1]] <-
      normalize_captured_entry(list(
        id    = new_capture_id(),
        label = "PCA: variance explained (scree)",
        type  = "plot",
        plot  = pca_scree_plot(obj),
        filter_snapshot = active_filter_descr(),
        config = list(method = "PCA scree"),
        r_code = pca_r_code(),
        created = Sys.time(),
        include = TRUE))
    save_captured(rv$captured_plots)
    notify_ok("PCA captured.")
  })

  # ===== Correspondence Analysis =====
  observe({
    if (is.null(rv$schema)) return()
    updateSelectInput(session, "ca_x",
                      choices = c("(pick one)" = "", pretty_choices(cat_fields())))
    updateSelectInput(session, "ca_y",
                      choices = c("(pick one)" = "", pretty_choices(cat_fields())))
  })
  ca_obj <- reactive({
    x <- input$ca_x; y <- input$ca_y
    if (is_empty_val(x) || is_empty_val(y)) return(NULL)
    if (identical(x, y)) return(list(error = "X and Y must differ."))
    do_ca(rpt_df(), x, y)
  })
  ca_main_plot <- function(obj) {
    if (is.null(obj))
      return(ggplot() + annotate("text", x=.5, y=.5,
        label = "Pick two categorical variables", size = 5, color = "#90a4ae") +
        theme_void())
    if (!is.null(obj$error))
      return(ggplot() + annotate("text", x=.5, y=.5,
        label = obj$error, size = 5, color = "#c62828") + theme_void())
    pts <- obj$points
    ve <- round(100 * obj$var_exp[1:2], 1)
    ggplot(pts, aes(Dim1, Dim2, color = Kind, label = Label)) +
      geom_point(size = 3) +
      geom_text(vjust = -1, size = 3.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
      scale_color_manual(values = c(row = pal[1], col = pal[2])) +
      labs(x = sprintf("Dim 1 (%.1f%%)", ve[1]),
           y = sprintf("Dim 2 (%.1f%%)", ve[2]),
           title = "Correspondence Analysis") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", color = "#1a237e"))
  }
  ca_scree_plot <- function(obj) {
    if (is.null(obj) || !is.null(obj$error)) return(ggplot() + theme_void())
    ve <- data.frame(Dim = paste0("Dim", seq_along(obj$var_exp)),
                     Var = obj$var_exp * 100)
    ve$Dim <- factor(ve$Dim, levels = ve$Dim)
    ggplot(head(ve, 8), aes(Dim, Var)) +
      geom_col(fill = pal[3], alpha = .85) +
      geom_text(aes(label = sprintf("%.1f%%", Var)), vjust = -.3,
                size = 3, fontface = "bold") +
      labs(x = "", y = "% inertia", title = "Inertia per dimension") +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold", color = "#1a237e"))
  }
  output$ca_plot  <- renderPlot({ ca_main_plot(ca_obj()) })
  output$ca_scree <- renderPlot({ ca_scree_plot(ca_obj()) })

  ca_r_code <- reactive({
    sch <- rv$schema
    csv <- rv$config$database_file %||% sch$suggested_db
    ca_to_r(input$ca_x, input$ca_y, active_filter_conds(), csv)
  })

  observeEvent(input$ca_capture, {
    obj <- ca_obj()
    if (is.null(obj) || !is.null(obj$error)) {
      notify_err("Cannot capture CA: pick valid variables first."); return()
    }
    rv$captured_plots[[length(rv$captured_plots) + 1]] <-
      normalize_captured_entry(list(
        id    = new_capture_id(),
        label = sprintf("CA: %s x %s", input$ca_x, input$ca_y),
        type  = "plot",
        plot  = ca_main_plot(obj),
        filter_snapshot = active_filter_descr(),
        config = list(method = "CA", x = input$ca_x, y = input$ca_y),
        r_code = ca_r_code(),
        created = Sys.time(),
        include = TRUE))
    rv$captured_plots[[length(rv$captured_plots) + 1]] <-
      normalize_captured_entry(list(
        id    = new_capture_id(),
        label = "CA: inertia per dimension",
        type  = "plot",
        plot  = ca_scree_plot(obj),
        filter_snapshot = active_filter_descr(),
        config = list(method = "CA scree"),
        r_code = ca_r_code(),
        created = Sys.time(),
        include = TRUE))
    save_captured(rv$captured_plots)
    notify_ok("CA captured.")
  })

  # ===== plot & table exports =====
  save_plot_to <- function(p, file, fmt = c("png","jpg"), w = 8, h = 6) {
    fmt <- match.arg(fmt)
    tryCatch(
      ggplot2::ggsave(file, plot = p, device = fmt, dpi = 300,
                      width = w, height = h, units = "in"),
      error = function(e) {
        dev_fn <- if (fmt == "png") grDevices::png else grDevices::jpeg
        dev_fn(file, width = w * 300, height = h * 300, res = 300)
        on.exit(grDevices::dev.off(), add = TRUE)
        tryCatch(print(p), error = function(e2) {
          plot.new(); text(0.5, 0.5, paste("Plot error:", e2$message))
        })
      })
  }

  # Build Your Own - PNG / JPG / CSV / R-script
  output$bb_dl_png <- downloadHandler(
    filename = function() sprintf("bb_plot_%s.png",
                                  format(Sys.time(), "%Y%m%d_%H%M%S")),
    content  = function(file) {
      p <- bb_plot_obj()
      if (is.null(p)) {
        grDevices::png(file); plot.new(); text(0.5, 0.5, "No plot")
        grDevices::dev.off(); return()
      }
      save_plot_to(p, file, "png")
    })
  output$bb_dl_jpg <- downloadHandler(
    filename = function() sprintf("bb_plot_%s.jpg",
                                  format(Sys.time(), "%Y%m%d_%H%M%S")),
    content  = function(file) {
      p <- bb_plot_obj()
      if (is.null(p)) {
        grDevices::jpeg(file); plot.new(); text(0.5, 0.5, "No plot")
        grDevices::dev.off(); return()
      }
      save_plot_to(p, file, "jpg")
    })
  output$bb_dl_csv <- downloadHandler(
    filename = function() sprintf("bb_table_%s.csv",
                                  format(Sys.time(), "%Y%m%d_%H%M%S")),
    content  = function(file) {
      d <- bb_table_data()
      if (is.null(d)) d <- data.frame(message = "No table available")
      utils::write.csv(d, file, row.names = FALSE)
    })
  output$bb_dl_r <- downloadHandler(
    filename = function() sprintf("bb_plot_%s.R",
                                  format(Sys.time(), "%Y%m%d_%H%M%S")),
    content  = function(file) {
      writeLines(bb_r_code(), file)
    })

  # PCA - PNG / JPG / loadings CSV / R-script
  output$pca_dl_png <- downloadHandler(
    filename = function() sprintf("pca_scores_%s.png",
                                  format(Sys.time(), "%Y%m%d_%H%M%S")),
    content  = function(file) {
      obj <- pca_obj()
      if (is.null(obj) || !is.null(obj$error)) {
        grDevices::png(file); plot.new(); text(0.5, 0.5, "No PCA")
        grDevices::dev.off(); return()
      }
      save_plot_to(pca_score_plot(obj), file, "png")
    })
  output$pca_dl_jpg <- downloadHandler(
    filename = function() sprintf("pca_scores_%s.jpg",
                                  format(Sys.time(), "%Y%m%d_%H%M%S")),
    content  = function(file) {
      obj <- pca_obj()
      if (is.null(obj) || !is.null(obj$error)) {
        grDevices::jpeg(file); plot.new(); text(0.5, 0.5, "No PCA")
        grDevices::dev.off(); return()
      }
      save_plot_to(pca_score_plot(obj), file, "jpg")
    })
  output$pca_dl_csv <- downloadHandler(
    filename = function() sprintf("pca_loadings_%s.csv",
                                  format(Sys.time(), "%Y%m%d_%H%M%S")),
    content  = function(file) {
      obj <- pca_obj()
      if (is.null(obj) || !is.null(obj$error)) {
        utils::write.csv(data.frame(message = "No PCA"), file, row.names = FALSE)
        return()
      }
      ld <- as.data.frame(obj$loadings)
      ld$Variable <- rownames(ld)
      ld <- ld[, c("Variable", setdiff(names(ld), "Variable")), drop = FALSE]
      utils::write.csv(ld, file, row.names = FALSE)
    })
  output$pca_dl_r <- downloadHandler(
    filename = function() sprintf("pca_%s.R",
                                  format(Sys.time(), "%Y%m%d_%H%M%S")),
    content  = function(file) writeLines(pca_r_code(), file))

  # CA - PNG / JPG / R-script
  output$ca_dl_png <- downloadHandler(
    filename = function() sprintf("ca_biplot_%s.png",
                                  format(Sys.time(), "%Y%m%d_%H%M%S")),
    content  = function(file) {
      obj <- ca_obj()
      if (is.null(obj) || !is.null(obj$error)) {
        grDevices::png(file); plot.new(); text(0.5, 0.5, "No CA")
        grDevices::dev.off(); return()
      }
      save_plot_to(ca_main_plot(obj), file, "png")
    })
  output$ca_dl_jpg <- downloadHandler(
    filename = function() sprintf("ca_biplot_%s.jpg",
                                  format(Sys.time(), "%Y%m%d_%H%M%S")),
    content  = function(file) {
      obj <- ca_obj()
      if (is.null(obj) || !is.null(obj$error)) {
        grDevices::jpeg(file); plot.new(); text(0.5, 0.5, "No CA")
        grDevices::dev.off(); return()
      }
      save_plot_to(ca_main_plot(obj), file, "jpg")
    })
  output$ca_dl_r <- downloadHandler(
    filename = function() sprintf("ca_%s.R",
                                  format(Sys.time(), "%Y%m%d_%H%M%S")),
    content  = function(file) writeLines(ca_r_code(), file))

  # Overview - stats CSV + R script
  output$dl_stats_csv <- downloadHandler(
    filename = function() sprintf("summary_stats_%s.csv",
                                  format(Sys.time(), "%Y%m%d_%H%M%S")),
    content  = function(file) {
      d <- stats_data()
      if (is.null(d)) d <- data.frame(message = "No data")
      utils::write.csv(d, file, row.names = FALSE)
    })
  output$dl_overview_r <- downloadHandler(
    filename = function() sprintf("overview_%s.R",
                                  format(Sys.time(), "%Y%m%d_%H%M%S")),
    content  = function(file) {
      sch <- rv$schema
      menu_fields <- ov_bar_fields()
      mcols <- ov_meas_fields()
      csv <- rv$config$database_file %||% sch$suggested_db
      writeLines(overview_to_r(menu_fields, mcols, active_filter_conds(), csv),
                 file)
    })

  # ===== Overview PDF report (uses captured items as before) =====
  output$dl_report <- downloadHandler(
    filename = function() sprintf("%s_report_%s.pdf",
                                  rv$config$site_name %||% "RStone",
                                  format(Sys.time(), "%Y%m%d_%H%M")),
    content = function(file) {
      tryCatch({
        sch <- rv$schema; df <- rpt_df()
        menu_fields <- ov_bar_fields()
        mcols <- ov_meas_fields()
        grDevices::pdf(file, width = 11, height = 8.5)
        on.exit(grDevices::dev.off())

        # Cover
        grid::grid.newpage()
        grid::grid.text(rv$config$site_name %||% "RStone", y = 0.78,
                        gp = grid::gpar(fontsize = 30, fontface = "bold",
                                        col = "#1565c0"))
        grid::grid.text("Lithic Analysis Report", y = 0.70,
                        gp = grid::gpar(fontsize = 20, col = "#37474f"))
        grid::grid.text(paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M")),
                        y = 0.55, gp = grid::gpar(fontsize = 13))
        grid::grid.text(paste("Records analyzed:", nrow(df)),
                        y = 0.50, gp = grid::gpar(fontsize = 13))
        if (length(rv$captured_plots))
          grid::grid.text(paste("Custom items captured:",
                                length(rv$captured_plots)),
                          y = 0.46, gp = grid::gpar(fontsize = 12, col = "#546e7a"))

        for (i in seq_along(menu_fields)) {
          f <- menu_fields[i]
          col <- pal[((i - 1) %% length(pal)) + 1]
          print(bar_plot(df, f, prompt_label(sch$PROMPTS[[f]] %||% f), col))
        }
        if (length(mcols)) print(meas_plot(df, mcols))

        st <- stats_table(df, mcols)
        if (!is.null(st) && nrow(st)) {
          grid::grid.newpage()
          grid::grid.text("Summary Statistics", y = 0.94,
                          gp = grid::gpar(fontsize = 20, fontface = "bold",
                                          col = "#1565c0"))
          txt <- capture.output(print(st, row.names = FALSE))
          for (i in seq_along(txt))
            grid::grid.text(txt[i], x = 0.5, y = 0.85 - (i - 1) * 0.030,
                            gp = grid::gpar(fontfamily = "mono", fontsize = 10))
        }

        if (length(rv$captured_plots)) {
          grid::grid.newpage()
          grid::grid.text("Custom Analyses", y = 0.50,
                          gp = grid::gpar(fontsize = 26, fontface = "bold",
                                          col = "#1565c0"))
          for (cp in rv$captured_plots) {
            if (isTRUE(cp$include) && cp$type == "plot" && !is.null(cp$plot))
              tryCatch(print(cp$plot), error = function(e) NULL)
          }
        }
      }, error = function(e) {
        plot.new(); text(0.5, 0.5, paste("Error:", e$message))
        log_error(paste("PDF render:", e$message))
      })
    }
  )
}

# ===== captured plots persistence =====
CAPTURED_FILE <- "captured_plots.rds"
save_captured <- function(plots) {
  tryCatch(saveRDS(plots, CAPTURED_FILE), error = function(e) NULL)
}
load_captured <- function() {
  if (!file.exists(CAPTURED_FILE)) return(list())
  tryCatch(readRDS(CAPTURED_FILE), error = function(e) list())
}
