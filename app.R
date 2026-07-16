# ============================================================
#  RStone  -  Lithic Analysis Data Entry  (modular build, v6)
#  Run from this folder with:  shiny::runApp()
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  library(shinyWidgets)        # NEW in v6: pickerInput for multi-select filters
  library(DT)
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(jsonlite)
  library(later)
  library(rlang)
  library(scales)
})

source("helpers.R",                  local = TRUE)
source("stats_helpers.R",            local = TRUE)
source("modules/filter_module.R",    local = TRUE)   # NEW
source("modules/script_export.R",    local = TRUE)   # NEW
source("modules/ui_panels.R",        local = TRUE)
source("modules/server_wizard.R",    local = TRUE)
source("modules/server_view.R",      local = TRUE)
source("modules/server_reports.R",   local = TRUE)
source("modules/server_builder.R",   local = TRUE)   # NEW
source("modules/server_schema.R",    local = TRUE)
source("modules/server_meta.R",      local = TRUE)

options(shiny.sanitize.errors = FALSE)

# ── paths ────────────────────────────────────────────────────
CONFIG_FILE <- "config.json"
SCHEMAS_DIR <- "schemas"
DATA_DIR    <- "data"
DEFAULT_SCHEMA <- file.path(SCHEMAS_DIR, "default.cfg")
for (d in c(SCHEMAS_DIR, DATA_DIR, "log", "backups",
            file.path(DATA_DIR, "images"))) {        # NEW: images/
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}
log_info("RStone starting")

# ── CSS ──────────────────────────────────────────────────────
APP_CSS <- "
  .content-wrapper, .right-side { background:#eef2f7 !important; }
  .box { border-radius:8px !important; box-shadow:0 2px 8px rgba(0,0,0,.08) !important; }
  .main-header .logo { font-weight:700; font-family:'Helvetica Neue',sans-serif; }

  .wiz-card { background:white; border-radius:12px; padding:32px 40px;
              box-shadow:0 4px 20px rgba(0,0,0,.08); max-width:760px; margin:0 auto; }
  .wiz-bar  { background:#dde4ec; height:6px; border-radius:3px;
              overflow:hidden; margin-bottom:20px; }
  .wiz-fill { background:linear-gradient(90deg,#3c8dbc,#367fa9);
              height:100%; transition:width .35s ease; }
  .wiz-step { font-size:11px; color:#78909c; text-transform:uppercase;
              letter-spacing:1.5px; font-weight:700; margin-bottom:6px; }
  .wiz-section { font-size:13px; color:#3c8dbc; font-weight:700;
                 background:#e3f2fd; padding:6px 10px; border-radius:4px;
                 margin-bottom:10px; display:inline-block; }
  .wiz-q    { font-size:24px; color:#367fa9; font-weight:600;
              margin-bottom:16px; line-height:1.3; }
  .wiz-hint { font-size:12px; color:#78909c; margin-top:4px;
              font-style:italic; }
  .wiz-input .form-control { font-size:18px !important; padding:12px 14px !important;
                              height:auto !important; }
  .wiz-input .selectize-input { font-size:16px !important; padding:11px 14px !important;
                                 min-height:auto !important; }
  .wiz-input .selectize-dropdown { font-size:15px !important; }
  .wiz-nav { margin-top:28px; display:flex; justify-content:space-between;
             align-items:center; gap:12px; }
  .wiz-btn { padding:11px 28px !important; font-size:14px !important;
             border-radius:6px !important; font-weight:600 !important;
             border:none !important; cursor:pointer; }
  .btn-back { background:#cfd8dc !important; color:#37474f !important; }
  .btn-skip { background:#fff8e1 !important; color:#f57f17 !important;
              border:1px solid #ffd54f !important; }
  .btn-next { background:#3c8dbc !important; color:white !important; }

  .summary-card { background:white; border-radius:8px; padding:14px 16px;
                  box-shadow:0 2px 8px rgba(0,0,0,.06); }
  .summary-title { font-size:11px; color:#78909c; text-transform:uppercase;
                   letter-spacing:1px; font-weight:700; margin-bottom:10px;
                   padding-bottom:8px; border-bottom:2px solid #e0e0e0; }
  .summary-row { padding:5px 0; border-bottom:1px solid #f0f0f0;
                 font-size:12px; display:flex; justify-content:space-between;
                 gap:8px; }
  .summary-row.live { background:#fff8e1; border-radius:4px; padding:5px 8px; }
  .summary-key { color:#546e7a; font-weight:500; }
  .summary-val { color:#367fa9; font-weight:600; max-width:55%;
                 text-align:right; word-break:break-word; }
  .summary-empty { color:#90a4ae; font-style:italic; font-size:12px; }

  .alert-float { position:fixed; top:18px; right:18px; z-index:9999;
                 min-width:300px; border-radius:8px; padding:13px 20px;
                 box-shadow:0 6px 20px rgba(0,0,0,.18); font-weight:500; }

  .cond-row { background:#f5f7fa; border-radius:6px; padding:10px 12px;
              margin-bottom:8px; border-left:3px solid #78909c; }

  /* Builder row hover effect */
  .builder-row:hover { box-shadow:0 2px 8px rgba(0,0,0,.12) !important; }

  /* Option-button group (alternative to dropdown) */
  .wiz-buttons { display:grid; grid-template-columns:repeat(auto-fill, minmax(180px, 1fr));
                 gap:10px; }
  .wiz-opt-btn { padding:14px 12px; font-size:14px; font-weight:600;
                 background:#f5f7fa; color:#1a237e;
                 border:2px solid #cfd8dc; border-radius:8px;
                 cursor:pointer; transition:background .15s, border-color .15s, transform .15s;
                 text-align:center;
                 white-space:normal; line-height:1.25; word-wrap:break-word;
                 min-height:48px; display:flex; align-items:center; justify-content:center; }
  .wiz-opt-btn:hover { background:#e3f2fd; border-color:#3c8dbc;
                        transform:translateY(-1px);
                        box-shadow:0 2px 6px rgba(0,0,0,.08); }
  .wiz-opt-btn:focus { outline:none; border-color:#3c8dbc;
                        box-shadow:0 0 0 3px rgba(60,141,188,.25); }
  .wiz-opt-btn.selected { background:#3c8dbc; color:white;
                           border-color:#3c8dbc; }
  .wiz-opt-btn.selected:hover { background:#357ca5; }

  /* ===== Dark theme overrides (applied when body.dark) ===== */
  body.dark .content-wrapper, body.dark .right-side { background:#1a2230 !important; }
  body.dark .box { background:#263244 !important; color:#e0e6ed !important;
                   border-top-color:#1565c0 !important;
                   box-shadow:0 2px 8px rgba(0,0,0,.5) !important; }
  body.dark .box-header { color:#e0e6ed !important; }
  body.dark .box-title { color:#e0e6ed !important; }
  body.dark p, body.dark label, body.dark h1, body.dark h2,
  body.dark h3, body.dark h4, body.dark h5 { color:#e0e6ed !important; }
  body.dark .small-box, body.dark .info-box { background:#263244 !important;
                                              color:#e0e6ed !important; }
  body.dark .form-control, body.dark .selectize-input,
  body.dark .selectize-dropdown { background:#1a2230 !important;
                                  color:#e0e6ed !important;
                                  border-color:#3c4858 !important; }
  body.dark .selectize-dropdown .option { color:#e0e6ed !important; }
  body.dark .selectize-dropdown .active { background:#1565c0 !important; }
  body.dark .wiz-card { background:#263244 !important; color:#e0e6ed !important;
                        box-shadow:0 4px 20px rgba(0,0,0,.4) !important; }
  body.dark .wiz-q { color:#90caf9 !important; }
  body.dark .wiz-step, body.dark .wiz-hint { color:#90a4ae !important; }
  body.dark .wiz-bar { background:#3c4858 !important; }
  body.dark .summary-card { background:#263244 !important; color:#e0e6ed !important; }
  body.dark .summary-title { color:#90a4ae !important;
                              border-bottom-color:#3c4858 !important; }
  body.dark .summary-key { color:#b0bec5 !important; }
  body.dark .summary-val { color:#90caf9 !important; }
  body.dark .summary-row { border-bottom-color:#3c4858 !important; }
  body.dark .summary-row.live { background:#3a3015 !important; }
  body.dark .cond-row { background:#1a2230 !important;
                         border-left-color:#1565c0 !important; }
  body.dark .builder-row { background:#263244 !important; color:#e0e6ed !important; }
  body.dark .wiz-opt-btn { background:#1a2230 !important;
                            color:#e0e6ed !important;
                            border-color:#3c4858 !important; }
  body.dark .wiz-opt-btn:hover { background:#263244 !important;
                                  border-color:#1565c0 !important; }
  body.dark .wiz-opt-btn.selected { background:#1565c0 !important;
                                     color:white !important;
                                     border-color:#1565c0 !important; }
  body.dark table.dataTable, body.dark .dataTables_wrapper {
    color:#e0e6ed !important; }
  body.dark table.dataTable tbody tr { background:#263244 !important;
                                        color:#e0e6ed !important; }
  body.dark table.dataTable tbody tr.selected { background:#1565c0 !important; }
  body.dark .modal-content { background:#263244 !important; color:#e0e6ed !important; }
  body.dark pre { background:#1a2230 !important; color:#e0e6ed !important;
                  border-color:#3c4858 !important; }
  body.dark a { color:#90caf9 !important; }
"

# ── JS: unified pick/advance + keyboard nav (race-free) ──────
APP_JS <- "
$(function() {
  Shiny.addCustomMessageHandler('rstone_set_theme', function(theme) {
    if (theme === 'dark') document.body.classList.add('dark');
    else document.body.classList.remove('dark');
  });

  var _justRendered = false;
  var _selectizeBoundCard = null;

  function $card() { return $('.wiz-card:visible').first(); }

  function getDomVal() {
    var $c = $card(); if (!$c.length) return null;
    var $hid = $c.find('input[type=hidden]#wiz_input').first();
    if ($hid.length) return $hid.val();
    var $sel = $c.find('select.selectized').first();
    if ($sel.length && $sel[0].selectize) return $sel[0].selectize.getValue();
    var $i = $c.find('input[type=text], input[type=number], textarea').first();
    if ($i.length) return $i.val();
    return null;
  }

  function pushLive() {
    var v = getDomVal();
    Shiny.setInputValue('wiz_live', { v: (v == null ? '' : v), t: Date.now() },
                        { priority: 'event' });
  }

  function commitAndAdvance(value) {
    Shiny.setInputValue('wiz_pick',
                        { v: (value == null ? '' : value), t: Date.now() },
                        { priority: 'event' });
  }
  function commitBack() {
    Shiny.setInputValue('wiz_back', { t: Date.now() }, { priority: 'event' });
  }
  function commitSkip() {
    Shiny.setInputValue('wiz_skip', { t: Date.now() }, { priority: 'event' });
  }

  function focusCurrent() {
    var $c = $card(); if (!$c.length) return;
    var $btnSel = $c.find('.wiz-opt-btn.selected').first();
    var $btnFirst = $c.find('.wiz-opt-btn').first();
    if ($btnSel.length) { try { $btnSel[0].focus({preventScroll:true}); } catch (e) {} return; }
    if ($btnFirst.length) { try { $btnFirst[0].focus({preventScroll:true}); } catch (e) {} return; }
    var sel = $c.find('select.selectized')[0];
    if (sel && sel.selectize) {
      try { sel.selectize.focus(); sel.selectize.open(); } catch (e) {}
      return;
    }
    var $i = $c.find('input, textarea').first();
    if ($i.length) {
      var el = $i[0];
      try {
        el.focus({preventScroll:true});
        if (el.select && el.type !== 'number') el.select();
        else if (el.setSelectionRange)
          try { el.setSelectionRange(0, (el.value || '').length); } catch (e) {}
      } catch (e) {}
    }
  }

  function bindSelectize() {
    var $c = $card(); if (!$c.length) return;
    var card = $c[0];
    if (_selectizeBoundCard === card) return;
    _selectizeBoundCard = card;
    var $sel = $c.find('select.selectized').first();
    if (!$sel.length || !$sel[0].selectize) return;
    var sz = $sel[0].selectize;
    sz.off('item_add');
    sz.on('item_add', function(value) {
      if (_justRendered) return;
      if (!value || value === '') return;
      try { sz.close(); sz.blur(); } catch (e) {}
      pushLive();
      commitAndAdvance(value);
    });
  }

  $(document).on('shiny:value', function(e) {
    if (e.name === 'wizard_ui') {
      _justRendered = true;
      _selectizeBoundCard = null;
      setTimeout(function() {
        bindSelectize();
        focusCurrent();
        pushLive();
        setTimeout(function() { _justRendered = false; }, 80);
      }, 60);
    }
  });

  $(document).on('input', '.wiz-input input, .wiz-input textarea', function() {
    pushLive();
  });

  $(document).on('click', '.wiz-opt-btn', function(e) {
    e.preventDefault();
    if (_justRendered) return;
    var $b = $(this);
    var v  = $b.attr('data-value');
    $b.closest('.wiz-buttons').find('.wiz-opt-btn').removeClass('selected');
    $b.addClass('selected');
    $b.closest('.wiz-buttons').find('input[type=hidden]#wiz_input').val(v);
    pushLive();
    commitAndAdvance(v);
  });

  $(document).on('keydown', '.wiz-opt-btn', function(e) {
    var $btns = $(this).closest('.wiz-buttons').find('.wiz-opt-btn');
    var idx = $btns.index(this);
    var next = null;

    if (e.key === 'ArrowRight') {
      next = $btns.eq((idx + 1) % $btns.length);
    } else if (e.key === 'ArrowLeft') {
      next = $btns.eq((idx - 1 + $btns.length) % $btns.length);
    } else if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
      var myRect = this.getBoundingClientRect();
      var myCenterX = myRect.left + myRect.width / 2;
      var myCenterY = myRect.top + myRect.height / 2;
      var best = null, bestScore = Infinity;
      $btns.each(function() {
        if (this === e.target) return;
        var r = this.getBoundingClientRect();
        var cx = r.left + r.width / 2;
        var cy = r.top + r.height / 2;
        var dy = cy - myCenterY;
        if (e.key === 'ArrowDown' && dy <= 5) return;
        if (e.key === 'ArrowUp'   && dy >= -5) return;
        var score = Math.abs(dy) * 1.2 + Math.abs(cx - myCenterX);
        if (score < bestScore) { bestScore = score; best = this; }
      });
      if (best) next = $(best);
    } else if (e.key === 'Enter' && e.shiftKey) {
      e.preventDefault();
      commitBack();
      return;
    } else if (e.key === 'Enter' || e.key === ' ') {
      e.preventDefault();
      $(this).trigger('click');
      return;
    } else if (e.key === 'Escape') {
      e.preventDefault();
      commitSkip();
      return;
    }
    if (next && next.length) {
      e.preventDefault();
      try { next[0].focus({preventScroll:true}); } catch (err) {}
    }
  });

  $(document).on('keydown', '.wiz-input input[type=text], .wiz-input input[type=number], .wiz-input textarea', function(e) {
    var isTextarea = this.tagName.toLowerCase() === 'textarea';
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault();
      commitAndAdvance(this.value);
    } else if (e.key === 'Enter' && e.shiftKey && !isTextarea) {
      e.preventDefault();
      commitBack();
    } else if (e.key === 'Escape' && !isTextarea) {
      e.preventDefault();
      commitSkip();
    }
  });

  $(document).on('click', '#btn_next', function(e) {
    e.preventDefault();
    commitAndAdvance(getDomVal());
  });
  $(document).on('click', '#btn_back', function(e) {
    e.preventDefault();
    commitBack();
  });
  $(document).on('click', '#btn_skip', function(e) {
    e.preventDefault();
    commitSkip();
  });

  $(document).on('keydown', function(e) {
    var $t = $(e.target);
    var inField = $t.is('input, textarea, select') ||
                  $t.hasClass('wiz-opt-btn');
    if (e.key === 'Enter' && e.shiftKey && !inField) {
      e.preventDefault();
      commitBack();
      return;
    }
    if ((e.ctrlKey || e.metaKey) && (e.key === 'd' || e.key === 'D')) {
      if (inField) return;
      e.preventDefault();
      Shiny.setInputValue('wiz_dup_last', Date.now(), { priority: 'event' });
    }
  });
});
"

# Serve files in assets/ to the browser at /assets/...
if (dir.exists("assets")) addResourcePath("assets", "assets")

# Build the header title: prefer assets/logo.svg, then .png, then .jpg
app_title <- local({
  logo_tag <- NULL
  for (ext in c("svg","png","jpg","jpeg")) {
    p <- file.path("assets", paste0("logo.", ext))
    if (file.exists(p)) {
      logo_tag <- tags$img(src = paste0("assets/", basename(p)),
                            style = "height:28px;margin-right:8px;vertical-align:middle;")
      break
    }
  }
  if (is.null(logo_tag)) logo_tag <- icon("hammer")
  tags$span(logo_tag, "RStone")
})

# ── UI ───────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = app_title,
    tags$li(class = "dropdown", style = "padding:8px 15px;color:white;font-size:13px;",
            textOutput("header_info", inline = TRUE))
  ),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Data Entry",   tabName = "entry",    icon = icon("edit")),
      menuItem("View Records", tabName = "view",     icon = icon("table")),
      menuItem("Reports",      tabName = "reports",  icon = icon("chart-bar")),
      menuItem("Schema",       tabName = "schema",   icon = icon("sliders-h")),
      menuItem("Settings",     tabName = "settings", icon = icon("cog")),
      menuItem("Export & Git", tabName = "export",   icon = icon("cloud-upload-alt"))
    ),
    tags$hr(style = "border-color:#3c4858;"),
    div(style = "padding:6px 15px;",
        actionButton("btn_new_record", "New Record (n)", icon = icon("plus"),
                     style = "width:100%;background:#2e7d32;color:white;border:none;margin-bottom:6px;"),
        actionButton("btn_edit_last",  "Edit Last", icon = icon("undo"),
                     style = "width:100%;background:#e65100;color:white;border:none;margin-bottom:6px;"),
        tags$small(style = "color:#90a4ae;",
                   "Shortcuts: Enter=Next (Shift+Enter=newline in notes / Back elsewhere), Esc=Skip, Ctrl+D=duplicate last"))
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML(APP_CSS)), tags$script(HTML(APP_JS))),
    div(id = "notif_ok",  class = "alert alert-success alert-float", style = "display:none;",
        textOutput("notif_ok_txt")),
    div(id = "notif_err", class = "alert alert-danger alert-float",  style = "display:none;",
        textOutput("notif_err_txt")),
    tabItems(
      tabItem(tabName = "entry",    wizard_ui()),
      tabItem(tabName = "view",     view_ui()),
      tabItem(tabName = "reports",  reports_ui()),
      tabItem(tabName = "schema",   schema_ui()),
      tabItem(tabName = "settings", settings_ui()),
      tabItem(tabName = "export",   export_ui())
    )
  )
)

# ── SERVER ───────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(
    config        = NULL,
    schema        = NULL,
    data          = data.frame(),
    entries       = list(),
    confirmed     = character(),
    step          = 1L,
    notif_ok      = "", notif_err = "",
    git_log       = "Ready.",
    schema_status = "",
    setup_done    = FALSE,
    live_field    = NULL, live_value = "",
    captured_plots = normalize_captured(load_captured())  # NEW: normalize on load
  )

  shared <- new.env(parent = emptyenv())

  notify_ok <- function(msg) {
    rv$notif_ok <- msg
    shinyjs::show("notif_ok")
    shinyjs::delay(2500, shinyjs::hide("notif_ok"))
  }
  notify_err <- function(msg) {
    rv$notif_err <- msg
    shinyjs::show("notif_err")
    shinyjs::delay(4500, shinyjs::hide("notif_err"))
    log_warn(msg)
  }
  shared$notify_ok  <- notify_ok
  shared$notify_err <- notify_err

  output$notif_ok_txt  <- renderText(rv$notif_ok)
  output$notif_err_txt <- renderText(rv$notif_err)
  output$header_info <- renderText({
    if (is.null(rv$config)) return("")
    sprintf("%s \u00b7 %d records%s",
            rv$config$site_name %||% "RStone",
            nrow(rv$data),
            if (nzchar(rv$config$analyst %||% ""))
              paste0(" \u00b7 ", rv$config$analyst) else "")
  })

  # ----- config & schema bootstrap -----
  show_welcome <- function() {
    showModal(modalDialog(
      title = tags$span(icon("hammer"), " Welcome to RStone"),
      p("Set up your project. You can change everything later in Settings."),
      textInput("setup_site", "Site / project name:",
                value = "",
                placeholder = "e.g. Boomplaas Cave, Howiesons Poort 2024..."),
      textInput("setup_analyst", "Analyst name (for audit log):",
                value = "",
                placeholder = "Your name"),
      textInput("setup_db", "Data file (CSV path):",
                value = file.path(DATA_DIR, "data.csv")),
      selectInput("setup_schema", "Variable schema (.cfg):",
                  choices = list_schemas(SCHEMAS_DIR) %||%
                              setNames(DEFAULT_SCHEMA, basename(DEFAULT_SCHEMA)),
                  selected = DEFAULT_SCHEMA),
      tags$small(
        "Start with the generic ", code("default.cfg"),
        " and adapt it in the Schema tab, or upload your own."),
      footer = tagList(modalButton("Cancel"),
                       actionButton("setup_save", "Start", icon = icon("play"),
                                    class = "btn-primary")),
      easyClose = FALSE
    ))
  }

  apply_config <- function(cfg) {
    rv$config <- cfg
    sf <- cfg$schema_file %||% DEFAULT_SCHEMA
    if (!file.exists(sf)) sf <- DEFAULT_SCHEMA
    sch <- tryCatch(build_schema(sf, SCHEMAS_DIR),
                    error = function(e) {
                      notify_err(paste("Schema:", e$message)); NULL
                    })
    if (is.null(sch)) return(invisible())
    rv$schema <- sch
    cfg$schema_file <- sf
    rv$config <- cfg
    rv$data <- load_data_for(cfg$database_file %||% sch$suggested_db,
                              c("DATEOFDATAENTRY", sch$fields))
    rv$entries <- list()
    rv$step <- 1L
    rv$confirmed <- character()
    rv$setup_done <- TRUE
    updateTextAreaInput(session, "schema_text",
                        value = paste(readLines(sf, warn = FALSE), collapse = "\n"))
    session$sendCustomMessage("rstone_set_theme", cfg$theme %||% "light")
    log_info(sprintf("Loaded schema '%s' (%d fields), data file '%s' (%d rows)",
                     sf, length(sch$fields),
                     cfg$database_file %||% sch$suggested_db, nrow(rv$data)))
  }

  shared$reload_schema <- function() {
    cfg <- isolate(rv$config)
    apply_config(cfg)
  }

  observeEvent(input$setup_save, {
    site <- trimws(input$setup_site %||% "")
    if (!nzchar(site)) site <- "MyProject"
    cfg <- list(
      site_name     = site,
      analyst       = trimws(input$setup_analyst %||% ""),
      schema_file   = input$setup_schema %||% DEFAULT_SCHEMA,
      database_file = trimws(input$setup_db %||% file.path(DATA_DIR, paste0(site, ".csv")))
    )
    save_config(cfg, CONFIG_FILE)
    apply_config(cfg)
    removeModal()
    notify_ok(paste("Welcome,", site, "ready."))
  })

  observeEvent(input$btn_reset_config, {
    if (file.exists(CONFIG_FILE)) file.remove(CONFIG_FILE)
    show_welcome()
  })

  session$onFlushed(function() {
    setup_wizard_server(input, output, session, rv, shared)
    setup_view_server(input, output, session, rv, shared)
    setup_reports_server(input, output, session, rv, shared)
    setup_builder_server(input, output, session, rv, shared)   # NEW
    setup_schema_server(input, output, session, rv, shared)
    setup_meta_server(input, output, session, rv, shared)
    cfg <- load_config(CONFIG_FILE)
    if (is.null(cfg)) show_welcome() else apply_config(cfg)
  }, once = TRUE)
}

shinyApp(ui, server)
