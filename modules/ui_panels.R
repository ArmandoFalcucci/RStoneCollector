# modules/ui_panels.R

wizard_ui <- function() {
  fluidRow(
    column(8, uiOutput("wizard_ui")),
    column(4, uiOutput("summary_ui"))
  )
}

view_ui <- function() {
  fluidRow(column(12,
    box(width = 12, title = "Records", status = "primary", solidHeader = TRUE,
        uiOutput("filters_ui"),
        DT::DTOutput("tbl_data"),
        br(),
        actionButton("btn_view_edit", "Edit Selected", icon = icon("edit"),
                     style = "background:#e65100;color:white;border:none;margin-right:5px;"),
        actionButton("btn_view_dup", "Duplicate Selected", icon = icon("copy"),
                     style = "background:#1565c0;color:white;border:none;margin-right:5px;"),
        actionButton("btn_view_del", "Delete", icon = icon("trash"),
                     style = "background:#c62828;color:white;border:none;margin-right:15px;"),
        actionButton("btn_mass_edit", "Mass-edit filtered records", icon = icon("magic"),
                     style = "background:#6a1b9a;color:white;border:none;"))
  ))
}

reports_ui <- function() {
  tabsetPanel(id = "report_tabs",
    tabPanel("Overview",
      br(),
      fluidRow(column(12,
        box(width = 12, status = "primary",
            uiOutput("report_filters_ui"),
            br(),
            fluidRow(
              column(4, downloadButton("dl_report", "Download PDF Report",
                                       icon = icon("file-pdf"),
                                       style = "width:100%;background:#c62828;color:white;border:none;")),
              column(4, downloadButton("dl_stats_csv", "Stats as CSV",
                                       icon = icon("file-csv"),
                                       style = "width:100%;background:#37474f;color:white;border:none;")),
              column(4, downloadButton("dl_overview_r", "R Script (overview)",
                                       icon = icon("code"),
                                       style = "width:100%;background:#1b5e20;color:white;border:none;"))))
      )),
      fluidRow(column(12,
        box(width = 12, title = "Choose what to display", status = "info",
            collapsible = TRUE, collapsed = TRUE,
            uiOutput("ov_controls_ui"),
            tags$small("Pick which fields appear as charts below. Leave a box empty to use the default set. Saved per project.")))),
      uiOutput("report_plots_ui"),
      fluidRow(column(12,
        box(width = 12, title = "Summary Statistics", status = "success",
            tableOutput("tbl_stats"))))
    ),
    tabPanel("Build Your Own",
      br(),
      uiOutput("bb_filter_status"),
      fluidRow(
        column(4,
          box(width = 12, title = "Plot Controls", status = "primary", solidHeader = TRUE,
              selectInput("bb_plot", "Plot type:",
                          choices = c("Bar chart (counts)"        = "bar",
                                      "Stacked bar"               = "stacked",
                                      "Grouped bar"               = "grouped",
                                      "Proportional bar (100%)"   = "proportional",
                                      "Boxplot"                   = "box",
                                      "Violin + box"              = "violin",
                                      "Density"                   = "density",
                                      "Histogram"                 = "hist",
                                      "Scatter"                   = "scatter")),
              uiOutput("bb_var_ui"),
              checkboxInput("bb_flip", "Flip horizontal", FALSE),
              checkboxInput("bb_stats", "Show exploratory test (optional)", FALSE),
              actionButton("bb_capture", "Add to Report", icon = icon("plus"),
                           style = "width:100%;background:#1565c0;color:white;border:none;margin-top:6px;"),
              br(), br(),
              tags$strong("Export this plot/table:"),
              fluidRow(
                column(6, downloadButton("bb_dl_png", "PNG 300dpi",
                                          icon = icon("file-image"),
                                          style = "width:100%;background:#1b5e20;color:white;border:none;font-size:12px;margin-top:4px;")),
                column(6, downloadButton("bb_dl_jpg", "JPG 300dpi",
                                          icon = icon("file-image"),
                                          style = "width:100%;background:#1b5e20;color:white;border:none;font-size:12px;margin-top:4px;"))
              ),
              fluidRow(
                column(6, downloadButton("bb_dl_csv", "Table CSV",
                                          icon = icon("file-csv"),
                                          style = "width:100%;background:#37474f;color:white;border:none;font-size:12px;margin-top:4px;")),
                column(6, downloadButton("bb_dl_r", "R Script",
                                          icon = icon("code"),
                                          style = "width:100%;background:#37474f;color:white;border:none;font-size:12px;margin-top:4px;"))
              ),
              br(), br(),
              textOutput("bb_captured_count"),
              actionButton("bb_clear_captured", "Clear captured", icon = icon("trash"),
                           style = "width:100%;background:#cfd8dc;color:#37474f;border:none;margin-top:4px;font-size:12px;")
          ),
          box(width = 12, title = "Quick presets", status = "info", solidHeader = TRUE,
              actionButton("preset_strat",   "Class composition by Unit",
                           icon = icon("layer-group"),
                           style = "width:100%;margin-bottom:5px;background:#37474f;color:white;border:none;"),
              actionButton("preset_length",  "Length distribution by Unit",
                           icon = icon("ruler"),
                           style = "width:100%;margin-bottom:5px;background:#37474f;color:white;border:none;"),
              actionButton("preset_rawmat",  "Raw material by Unit",
                           icon = icon("gem"),
                           style = "width:100%;margin-bottom:5px;background:#37474f;color:white;border:none;"),
              actionButton("preset_scatter", "Length vs Width scatter",
                           icon = icon("braille"),
                           style = "width:100%;margin-bottom:5px;background:#37474f;color:white;border:none;"))
        ),
        column(8,
          box(width = 12, title = "Plot", status = "info",
              plotOutput("bb_plot_out", height = "440px"),
              verbatimTextOutput("bb_stats_text")),
          box(width = 12, title = "Cross-tabulation table", status = "info",
              DT::DTOutput("bb_table"))
        )
      )
    ),
    tabPanel("Data Quality",
      br(),
      fluidRow(column(12,
        box(width = 12, status = "warning", solidHeader = TRUE,
            title = "Data quality check",
            p(paste("A quick scan for things to fix before analysis: missing values,",
                    "entries that break the schema's rules, duplicate IDs, and numeric",
                    "outliers. Runs on the whole dataset, ignoring the Overview filters.")),
            uiOutput("dq_summary")))),
      fluidRow(
        column(6, box(width = 12, title = "Completeness by field", status = "info",
                       plotOutput("dq_complete_plot", height = "340px"))),
        column(6, box(width = 12, title = "Numeric ranges & outliers", status = "info",
                       tableOutput("dq_outliers")))),
      fluidRow(column(12,
        box(width = 12, title = "Records to review", status = "danger", solidHeader = TRUE,
            p(tags$em(paste(
              "Each row below breaks a schema rule (invalid menu value, out-of-range",
              "number, bad text pattern), is missing a required value, or duplicates an",
              "ID. Fix them in View Records."))),
            downloadButton("dq_dl_csv", "Download issues as CSV", icon = icon("file-csv"),
                           style = "background:#37474f;color:white;border:none;margin-bottom:8px;"),
            DT::DTOutput("dq_issues"))))
    ),
    tabPanel("Report Builder", icon = icon("file-pdf"),
      br(),
      fluidRow(
        column(8,
          box(width = 12, title = "Captured items", status = "primary", solidHeader = TRUE,
              p(tags$em(paste(
                "Items captured from the Build Your Own tab land here.",
                "Toggle the checkbox to include or exclude in the compiled report.",
                "Use the up/down arrows to reorder."))),
              uiOutput("builder_items_ui"))),
        column(4,
          box(width = 12, title = "Compile report", status = "success", solidHeader = TRUE,
              textInput("builder_report_title", "Report title:",
                        value = "", placeholder = "e.g. Ksar Akil - Ahmarian"),
              textInput("builder_report_subtitle", "Subtitle (optional):",
                        value = "", placeholder = "e.g. Layer XVII vs. XVI"),
              downloadButton("builder_dl_pdf", "Compile PDF report",
                             icon = icon("file-pdf"),
                             style = "width:100%;background:#c62828;color:white;border:none;margin-bottom:6px;"),
              downloadButton("builder_dl_rscript", "Combined R script",
                             icon = icon("code"),
                             style = "width:100%;background:#37474f;color:white;border:none;")),
          box(width = 12, title = "Add a heading", status = "info", solidHeader = TRUE,
              p(tags$small(
                "Insert a section heading between captures. Useful when comparing units side-by-side."
              )),
              textInput("builder_heading_text", label = NULL,
                        placeholder = "e.g. \u2014 HP Member \u2014"),
              actionButton("builder_add_heading", "Add heading",
                           icon = icon("plus"),
                           style = "width:100%;background:#6a1b9a;color:white;border:none;")),
          box(width = 12, title = "Danger zone", status = "warning", solidHeader = TRUE,
              actionButton("builder_clear_all", "Clear all captured items",
                           icon = icon("trash"),
                           style = "width:100%;background:#c62828;color:white;border:none;")))
      )
    )
  )
}

schema_ui <- function() {
  fluidRow(
    column(7,
      box(width = 12, title = "Schema Editor", status = "primary", solidHeader = TRUE,
          p("Edit the CFG file. Click ", strong("Validate"),
            " then ", strong("Apply & Save"), "."),
          textAreaInput("schema_text", label = NULL, value = "",
                        width = "100%", height = "440px", resize = "vertical"),
          fluidRow(
            column(4, actionButton("schema_validate", "Validate", icon = icon("check-circle"),
                                    style = "width:100%;background:#fb8c00;color:white;border:none;")),
            column(4, actionButton("schema_apply",    "Apply & Save", icon = icon("save"),
                                    style = "width:100%;background:#1b5e20;color:white;border:none;")),
            column(4, actionButton("schema_reload",   "Reload from File", icon = icon("sync"),
                                    style = "width:100%;background:#37474f;color:white;border:none;"))
          ),
          br(),
          verbatimTextOutput("schema_status"))),
    column(5,
      tabsetPanel(
        tabPanel("Add Field",
          br(),
          textInput("addf_name",   "Field name (no spaces):", placeholder = "e.g. EdgeAngle"),
          selectInput("addf_type", "Type:",
                      choices = c("MENU", "TEXT", "NUMERIC", "NOTE", "BOOLEAN", "IMAGE")),
          textInput("addf_prompt", "Prompt:", placeholder = "e.g. Edge angle"),
          conditionalPanel("input.addf_type == 'MENU'",
            textAreaInput("addf_options", "Menu options (one per line):",
                          placeholder = "Acute\nRight\nObtuse",
                          width = "100%", height = "100px")),
          conditionalPanel("input.addf_type == 'NUMERIC'",
            fluidRow(
              column(6, textInput("addf_min", "Min value:", placeholder = "0")),
              column(6, textInput("addf_max", "Max value:", placeholder = "100")))),
          conditionalPanel("input.addf_type == 'TEXT'",
            textInput("addf_pattern", "Regex pattern (optional):",
                      placeholder = "^[A-Z]+-[0-9]+$")),
          checkboxInput("addf_required", "Required", FALSE),
          checkboxInput("addf_carry",    "Carry over to next record", FALSE),
          tags$hr(),
          p(strong("Conditions"), " (when to show this field):"),
          uiOutput("cond_rows_ui"),
          actionButton("btn_add_field", "Add to schema", icon = icon("plus-circle"),
                       style = "width:100%;background:#1565c0;color:white;border:none;margin-top:8px;")
        ),
        tabPanel("Edit Field",
          br(),
          uiOutput("editf_picker_ui"),
          uiOutput("editf_form_ui"),
          fluidRow(
            column(6, actionButton("btn_edit_field_save", "Save changes", icon = icon("save"),
                                    style = "width:100%;background:#1b5e20;color:white;border:none;")),
            column(6, actionButton("btn_edit_field_reset", "Reset to current",
                                    icon = icon("undo"),
                                    style = "width:100%;background:#cfd8dc;color:#37474f;border:none;")))
        ),
        tabPanel("Reorder / Delete",
          br(),
          p("Pick a field, then move it up or down. Or delete it."),
          uiOutput("reorder_ui"),
          fluidRow(
            column(4, actionButton("btn_field_up",   "\u2191 Move up",   icon = icon("arrow-up"),
                                    style = "width:100%;background:#37474f;color:white;border:none;")),
            column(4, actionButton("btn_field_down", "\u2193 Move down", icon = icon("arrow-down"),
                                    style = "width:100%;background:#37474f;color:white;border:none;")),
            column(4, actionButton("btn_field_del",  "Delete field", icon = icon("trash"),
                                    style = "width:100%;background:#c62828;color:white;border:none;"))
          )),
        tabPanel("Info",
          br(),
          verbatimTextOutput("schema_info"))
      )
    )
  )
}

settings_ui <- function() {
  fluidRow(
    column(6,
      box(width = 12, title = "Site & Database", status = "primary", solidHeader = TRUE,
          textInput("set_site",    "Site name:", ""),
          textInput("set_analyst", "Analyst:", "",
                    placeholder = "Your name (saved in audit log)"),
          textInput("set_db_file", "Database file (CSV path):", ""),
          p(tags$small("A parallel ", code(".json"), " is written alongside the CSV automatically. Up to 10 timestamped backups are kept in ", code("backups/"), ".")),
          actionButton("btn_save_settings", "Save Settings", icon = icon("save"),
                       style = "background:#1565c0;color:white;border:none;"),
          tags$hr(),
          actionButton("btn_reset_config", "Run Welcome Setup", icon = icon("redo"),
                       style = "background:#fb8c00;color:white;border:none;")),
      box(width = 12, title = "Appearance", status = "info", solidHeader = TRUE,
          radioButtons("set_theme", "Theme:",
                       choices = c("Light" = "light", "Dark" = "dark"),
                       selected = "light", inline = TRUE),
          radioButtons("set_menu_style", "Menu style for short lists:",
                       choices = c("Buttons (boxes)" = "buttons",
                                   "Auto (buttons \u2264 6 options, dropdown otherwise)" = "auto",
                                   "Dropdown" = "dropdown"),
                       selected = "buttons"),
          tags$small("Saved to config.json - persists across sessions.")),
      box(width = 12, title = "Record IDs", status = "info", solidHeader = TRUE,
          radioButtons("set_id_mode", "After saving a record, the next ID is:",
                       choices = c("Kept the same - you edit it each time" = "manual",
                                   "Auto-incremented - consecutive numbering" = "increment"),
                       selected = "manual"),
          tags$small("A duplicate ID is always refused on save, so an existing record can't be overwritten by accident."))),
    column(6,
      box(width = 12, title = "Schema File", status = "primary", solidHeader = TRUE,
          selectInput("set_schema_file", "Active schema:", choices = character(0)),
          actionButton("btn_load_schema", "Load selected schema", icon = icon("download"),
                       style = "background:#1b5e20;color:white;border:none;margin-bottom:8px;"),
          br(), br(),
          fileInput("upload_schema", "Upload a new CFG file:",
                    accept = c(".cfg", ".CFG", ".txt", ".ini"))),
      box(width = 12, title = "Audit Log (recent activity)", status = "info",
          DT::DTOutput("audit_table"))
    )
  )
}

export_ui <- function() {
  fluidRow(
    column(6, box(width = 12, title = "Export Data", status = "primary", solidHeader = TRUE,
                  downloadButton("dl_csv",  "Download CSV",  style = "width:100%;margin-bottom:8px;"),
                  br(),
                  downloadButton("dl_json", "Download JSON", style = "width:100%;margin-bottom:8px;"),
                  br(),
                  downloadButton("dl_audit","Download audit log", style = "width:100%;margin-bottom:8px;"),
                  br(), br(),
                  h5("Data files in use:"),
                  verbatimTextOutput("db_path_disp"))),
    column(6, box(width = 12, title = "Git Integration", status = "success", solidHeader = TRUE,
                  actionButton("git_pull", "Pull from GitHub", icon = icon("arrow-down"),
                               style = "width:100%;background:#00695c;color:white;border:none;margin-bottom:8px;"),
                  actionButton("git_push", "Commit & Push", icon = icon("arrow-up"),
                               style = "width:100%;background:#1565c0;color:white;border:none;margin-bottom:12px;"),
                  verbatimTextOutput("git_log")))
  )
}
