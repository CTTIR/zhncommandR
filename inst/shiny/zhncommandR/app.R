# =============================================================
# zhncommandR Shiny app
# Audit / live evaluation dashboard for haematological tumour centres
# =============================================================
#
# UI is the verbatim layout from the v5 single-file app, with TWO sanctioned
# additions in the sidebar:
#   - data-source controls (cohort upload, optional tumorboard CSV upload,
#     example-data button)
#   - replacement content for the `path_info` slot (same widget id, new text)
#
# Visual identity: Hugo Coder palette via _brand.yml + bs_theme + thematic.
#
# All backend logic is in zhncommandR:: -- see ?zhn_run_app and ?zhn_example_path.

# ---- Theme: Hugo Coder design tokens via brand.yml --------------------------
.coder_theme <- function() {
  app_dir <- system.file("shiny", "zhncommandR", package = "zhncommandR")
  scss <- file.path(app_dir, "www", "custom.scss")
  theme <- bslib::bs_theme(
    version = 5,
    bg = "#fafafa",
    fg = "#212121",
    primary = "#1565c0",
    secondary = "#e0e0e0",
    success = "#00897b",
    info = "#1e88e5",
    warning = "#ffb300",
    danger = "#e53935",
    base_font = bslib::font_collection(
      "system-ui", "-apple-system", "Segoe UI", "Roboto",
      "Helvetica", "sans-serif"
    ),
    code_font = bslib::font_collection(
      "SF Mono", "Consolas", "Liberation Mono", "Menlo", "monospace"
    ),
    "border-radius" = "4px",
    "card-border-radius" = "4px",
    "card-border-color" = "#e0e0e0",
    "navbar-bg" = "#fafafa",
    "body-bg" = "#fafafa",
    "link-decoration" = "none",
    "link-hover-decoration" = "underline"
  )
  if (file.exists(scss) && requireNamespace("sass", quietly = TRUE)) {
    theme <- bslib::bs_add_rules(theme, sass::sass_file(scss))
  }
  theme
}

# ---- Plot theming: ggplot inherits app theme -------------------------------
if (requireNamespace("thematic", quietly = TRUE)) {
  thematic::thematic_shiny(font = "auto")
}
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 13))

# ragg device so on-screen Inter matches the exported PNG/PDF; register the
# bundled OFL Inter font for live display and exports (degrades gracefully).
options(shiny.useragg = TRUE)
zhncommandR::zhn_register_fonts()

# ---- i18n (DE default, EN toggle) ------------------------------------------
.app_dir <- system.file("shiny", "zhncommandR", package = "zhncommandR")
if (!nzchar(.app_dir)) .app_dir <- getwd()
.i18n_path <- file.path(.app_dir, "translations", "translation.json")
i18n <- shiny.i18n::Translator$new(translation_json_path = .i18n_path)
i18n$set_translation_language("de")

# Tiny helper: translate fallback to original if a key is missing.
.tr <- function(s) i18n$t(s)

# NULL/empty-coalescing helper for reading inputs with sane fallbacks.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# Compact PNG (600 dpi) + PDF download pair for a figure card (ids match the
# server-side .dl_handlers registration: <id>_png / <id>_pdf).
.dl_ui <- function(id) {
  shiny::div(
    class = "mt-2 d-flex gap-2",
    shiny::downloadButton(paste0(id, "_png"), "PNG (600 dpi)",
                          class = "btn-sm btn-outline-secondary"),
    shiny::downloadButton(paste0(id, "_pdf"), "PDF",
                          class = "btn-sm btn-outline-secondary")
  )
}

ui <- shiny::fluidPage(
  theme = .coder_theme(),
  title = .tr("Hämatologisches Tumorzentrum – Auditor-Auswertung"),
  shiny.i18n::usei18n(i18n),
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 390,
      # Branding: large hex on top, package name small in Hugo Coder blue
      # below it. Dark/light toggle sits top-right (client-side, no reload).
      shiny::div(
        class = "mb-3",
        shiny::div(
          class = "d-flex justify-content-end",
          bslib::input_dark_mode(id = "color_mode")
        ),
        shiny::div(
          style = paste(
            "display:flex; flex-direction:column;",
            "align-items:center; line-height:1;"
          ),
          utils::getFromNamespace(".cttir_brand_logo", "zhncommandR")(84),
          shiny::tags$span(
            "zhncommandR",
            style = paste(
              "font-family:Inter,system-ui,sans-serif; font-size:0.9rem;",
              "color:#1565C0; margin-top:6px; letter-spacing:.2px;"
            )
          )
        )
      ),

      # --- Language toggle DE / EN (no label) --------------------------
      shiny::div(
        class = "mb-2",
        shiny::radioButtons(
          "lang", label = NULL,
          choices = c("Deutsch" = "de", "English" = "en"),
          selected = "de", inline = TRUE
        )
      ),

      # --- sanctioned addition: data source -----------------------------
      bslib::card(
        bslib::card_header(.tr("Datenquelle")),
        shiny::fileInput(
          "cohort_file",
          .tr("Kohorten-Excel (.xlsx) hochladen"),
          accept = c(".xlsx", ".xls",
                     "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
        ),
        shiny::fileInput(
          "tumorboard_file",
          .tr("Optional: Tumorboardbeschlüsse CSV laden"),
          accept = c(".csv", "text/csv")
        ),
        shiny::actionButton(
          "load_example",
          .tr("Beispieldaten laden"),
          class = "btn-secondary"
        )
      ),
      # ------------------------------------------------------------------

      shiny::verbatimTextOutput("path_info"),
      shiny::actionButton("reload", .tr("Daten neu einlesen"), class = "btn-primary"),
      shiny::br(), shiny::br(),
      shiny::helpText(
        "Datenquelle: hochgeladene Excel-Datei oder bundle-Beispieldaten. ",
        "Keine Daten werden außerhalb der Session gespeichert."
      ),
      shiny::hr(),
      shiny::h5(.tr("Globale Filter")),
      shiny::checkboxInput(
        "fig_transparent",
        .tr("Transparenter Hintergrund (Abbildungen)"),
        value = FALSE
      ),
      shiny::uiOutput("global_filters"),
      shiny::hr(),
      shiny::h5(.tr("Freitextsuche")),
      shiny::textInput("search_text",
                       .tr("Suche in Name, Diagnose, Kodierung, Therapie"),
                       value = ""),
      shiny::hr(),
      shiny::downloadButton("download_filtered",
                            .tr("Gefilterte Patiententabelle CSV"))
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel(
          .tr("Auditor-Dashboard"),
          shiny::br(),
          bslib::layout_column_wrap(
            width = 1/4,
            bslib::value_box(.tr("Patienten gesamt"), shiny::textOutput("n_total"),
                             showcase = bsicons::bs_icon("people")),
            bslib::value_box(.tr("Primärfälle"), shiny::textOutput("n_primaer"),
                             showcase = bsicons::bs_icon("clipboard2-pulse")),
            bslib::value_box(.tr("Patientenfälle"), shiny::textOutput("n_patientenfall"),
                             showcase = bsicons::bs_icon("hospital")),
            bslib::value_box(.tr("Psychoonkologie"), shiny::textOutput("n_psycho"),
                             showcase = bsicons::bs_icon("heart-pulse"))
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(6, bslib::card(bslib::card_header(.tr("Fälle nach Diagnose/Kodierung")),
                                         shiny::plotOutput("plot_diagnosis", height = 430),
                                         .dl_ui("dl_diagnoses"))),
            shiny::column(6, bslib::card(bslib::card_header(.tr("Jährliche Fallzahlen")),
                                         shiny::plotOutput("plot_year", height = 430),
                                         .dl_ui("dl_year")))
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(6, bslib::card(bslib::card_header(.tr("Qualitäts-/Versorgungsindikatoren")),
                                         DT::DTOutput("indicator_table"))),
            shiny::column(6, bslib::card(bslib::card_header(.tr("Schnellfragen für Auditoren")),
                                         shiny::uiOutput("quick_questions")))
          )
        ),
        shiny::tabPanel(
          .tr("Einfache Abfragen"),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              4,
              bslib::card(
                bslib::card_header(.tr("Auditorfrage auswählen")),
                shiny::selectInput(
                  "simple_question", .tr("Vordefinierte Frage"),
                  choices = c(
                    "Psychoonkologisches Screening" = "psycho",
                    "HIV/Hepatitis Screening" = "hivhep",
                    "Diagnose gezielt auswählen" = "diagnose_select",
                    "Multiples Myelom" = "myelom",
                    "Hodgkin-Lymphom" = "hodgkin",
                    "Tumorkonferenz erfolgt" = "tumorkonferenz",
                    "Sozialdienst angebunden" = "sozialdienst",
                    "Primärfälle" = "primaerfall",
                    "Patientenfälle" = "patientenfall",
                    "Eigene Abfrage" = "custom"
                  ),
                  selected = "psycho"
                ),
                shiny::uiOutput("simple_query_ui"),
                shiny::checkboxInput("simple_only_unique",
                                     .tr("Nur eindeutige Namen zählen"), FALSE),
                shiny::actionButton("run_simple_query",
                                    .tr("Abfrage ausführen"), class = "btn-primary"),
                shiny::br(), shiny::br(),
                shiny::downloadButton("download_simple_query", .tr("Trefferliste CSV"))
              )
            ),
            shiny::column(
              8,
              bslib::value_box(.tr("Ergebnis"), shiny::textOutput("simple_query_result"),
                               showcase = bsicons::bs_icon("search")),
              shiny::br(),
              bslib::card(bslib::card_header(.tr("Zusammenfassung")),
                          DT::DTOutput("simple_query_summary")),
              shiny::br(),
              bslib::card(bslib::card_header(.tr("Trefferliste")),
                          DT::DTOutput("simple_query_table"))
            )
          )
        ),
        shiny::tabPanel(
          .tr("Patientenliste"), shiny::br(),
          shiny::helpText(.tr("Eine Zeile auswählen: Der Patient wird automatisch in den Tab 'Tumorboardbeschlüsse' übernommen.")),
          DT::DTOutput("table")
        ),
        shiny::tabPanel(
          .tr("Tumorboardbeschlüsse"), shiny::br(),
          shiny::fluidRow(
            shiny::column(
              4,
              bslib::card(
                bslib::card_header(.tr("Beschluss für Patienten erfassen")),
                shiny::verbatimTextOutput("tb_storage_info"),
                shiny::uiOutput("tb_patient_ui"),
                shiny::dateInput("tb_date", .tr("Datum Tumorboard"),
                                 value = Sys.Date(),
                                 format = "dd.mm.yyyy", language = "de"),
                shiny::textAreaInput("tb_decision",
                                     .tr("Tumorboardbeschluss / Empfehlung"),
                                     value = "", rows = 7,
                                     placeholder = .tr("z.B. Vorstellung Referenzpathologie, Therapieempfehlung, Studienprüfung, Re-Staging, supportive Maßnahmen ...")),
                shiny::textInput("tb_responsible",
                                 .tr("Verantwortlich / Eintrag durch"), value = ""),
                shiny::actionButton("save_tb",
                                    .tr("Beschluss speichern"), class = "btn-primary"),
                shiny::br(), shiny::br(),
                shiny::downloadButton("download_tb",
                                      .tr("Tumorboardbeschlüsse CSV"))
              )
            ),
            shiny::column(
              8,
              bslib::layout_column_wrap(
                width = 1/3,
                bslib::value_box(.tr("Beschlüsse gesamt"), shiny::textOutput("tb_n_total")),
                bslib::value_box(.tr("Beschlüsse Patient"), shiny::textOutput("tb_n_patient")),
                bslib::value_box(.tr("Patienten mit Beschluss"), shiny::textOutput("tb_n_patients"))
              ),
              shiny::br(),
              bslib::card(bslib::card_header(.tr("Beschlüsse des ausgewählten Patienten")),
                          DT::DTOutput("tb_patient_table")),
              shiny::br(),
              bslib::card(bslib::card_header(.tr("Alle dokumentierten Tumorboardbeschlüsse")),
                          DT::DTOutput("tb_all_table"))
            )
          )
        ),
        shiny::tabPanel(
          .tr("Kaplan–Meier"), shiny::br(),
          shiny::fluidRow(
            shiny::column(4, bslib::card(
              bslib::card_header(.tr("KM-Einstellungen")),
              shiny::uiOutput("km_ui"),
              shiny::actionButton("run_km", .tr("KM aktualisieren"), class = "btn-primary"),
              shiny::br(), shiny::br(),
              .dl_ui("dl_km")
            )),
            shiny::column(8,
              bslib::card(
                bslib::card_header(.tr("Kaplan–Meier Plot")),
                shiny::plotOutput("km_plot", height = 560)
              ),
              shiny::conditionalPanel(
                "input.km_pairwise",
                bslib::card(
                  bslib::card_header(.tr("Paarweise Log-rank (BH-korrigiert)")),
                  DT::DTOutput("km_pairwise_table")
                )
              )
            )
          )
        ),
        shiny::tabPanel(
          .tr("OPS 8-544 Therapieblöcke"), shiny::br(),
          shiny::fluidRow(
            shiny::column(4, bslib::card(
              bslib::card_header(.tr("Filter Therapieblöcke")),
              shiny::verbatimTextOutput("therapy_source_info"),
              shiny::uiOutput("therapy_filters"),
              shiny::br(),
              shiny::downloadButton("download_therapy_blocks", .tr("Therapieblöcke CSV"))
            )),
            shiny::column(8, bslib::layout_column_wrap(
              width = 1/3,
              bslib::value_box(.tr("OPS-8-544-Blöcke"), shiny::textOutput("n_therapy_blocks")),
              bslib::value_box(.tr("Patienten"), shiny::textOutput("n_therapy_patients")),
              bslib::value_box(.tr("Therapieprotokolle"), shiny::textOutput("n_therapy_protocols"))
            ))
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(6, bslib::card(bslib::card_header(.tr("Wie viele Blöcke von welcher Therapie?")),
                                         DT::DTOutput("therapy_protocol_table"))),
            shiny::column(6, bslib::card(bslib::card_header(.tr("Blöcke nach Diagnose")),
                                         DT::DTOutput("therapy_diagnosis_table")))
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(6, bslib::card(bslib::card_header(.tr("Blöcke je Therapieprotokoll")),
                                         shiny::plotOutput("therapy_protocol_plot", height = 520),
                                         .dl_ui("dl_therapy_protocol"))),
            shiny::column(6, bslib::card(bslib::card_header(.tr("Monatliche OPS-8-544-Blöcke")),
                                         shiny::plotOutput("therapy_month_plot", height = 520),
                                         .dl_ui("dl_therapy_month")))
          ),
          shiny::br(),
          bslib::card(bslib::card_header(.tr("Detailtabelle der gezählten Blöcke")),
                      DT::DTOutput("therapy_block_details"))
        ),
        shiny::tabPanel(
          .tr("OPS 1-941 Diagnostik"), shiny::br(),
          shiny::fluidRow(
            shiny::column(4, bslib::card(
              bslib::card_header(.tr("Filter komplexe Diagnostik")),
              shiny::verbatimTextOutput("diagnostic_source_info"),
              shiny::uiOutput("diagnostic_filters"),
              shiny::br(),
              shiny::downloadButton("download_diagnostic_blocks",
                                    .tr("Komplexe Diagnostik CSV"))
            )),
            shiny::column(8, bslib::layout_column_wrap(
              width = 1/3,
              bslib::value_box(.tr("OPS-1-941-Fälle"), shiny::textOutput("n_diagnostic_blocks")),
              bslib::value_box(.tr("Patienten"), shiny::textOutput("n_diagnostic_patients")),
              bslib::value_box(.tr("Diagnosen"), shiny::textOutput("n_diagnostic_diagnoses"))
            ))
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(6, bslib::card(bslib::card_header(.tr("Komplexe Diagnostik nach Bereich")),
                                         DT::DTOutput("diagnostic_component_table"))),
            shiny::column(6, bslib::card(bslib::card_header(.tr("Komplexe Diagnostik nach Diagnose")),
                                         DT::DTOutput("diagnostic_diagnosis_table")))
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(6, bslib::card(bslib::card_header(.tr("Diagnostikbereiche")),
                                         shiny::plotOutput("diagnostic_component_plot", height = 520),
                                         .dl_ui("dl_diag_component"))),
            shiny::column(6, bslib::card(bslib::card_header(.tr("Monatliche OPS-1-941-Fälle")),
                                         shiny::plotOutput("diagnostic_month_plot", height = 520),
                                         .dl_ui("dl_diag_month")))
          ),
          shiny::br(),
          bslib::card(bslib::card_header(.tr("Detailtabelle der komplexen Diagnostiken")),
                      DT::DTOutput("diagnostic_block_details"))
        ),
        shiny::tabPanel(
          .tr("Oncoprint Mutationen"), shiny::br(),
          shiny::fluidRow(
            shiny::column(4, bslib::card(
              bslib::card_header(.tr("Oncoprint-Filter")),
              shiny::helpText(.tr("Quelle: Spalte 'Krankheitsspezifische hematol Resultate'. NA/negative Befunde werden nicht geplottet. Deletionen, Zugewinne, Translokationen/Rearrangements/Brüche, Loss und komplexer Karyotyp werden nur tabellarisch aufgeführt.")),
              shiny::uiOutput("oncoprint_filters"),
              shiny::actionButton("run_oncoprint", .tr("Oncoprint aktualisieren"), class = "btn-primary"),
              shiny::br(), shiny::br(),
              shiny::downloadButton("download_oncoprint_data", .tr("Mutationsdaten CSV")),
              shiny::downloadButton("download_structural_data", .tr("Struktur-/Zytogenetik CSV")),
              .dl_ui("dl_oncoprint")
            )),
            shiny::column(8, bslib::layout_column_wrap(
              width = 1/3,
              bslib::value_box(.tr("Fälle mit Alterationen"), shiny::textOutput("n_onco_patients")),
              bslib::value_box(.tr("Alterationen"), shiny::textOutput("n_onco_alterations")),
              bslib::value_box(.tr("Entitäten"), shiny::textOutput("n_onco_entities"))
            ))
          ),
          shiny::br(),
          bslib::card(bslib::card_header(.tr("Oncoprint – nur echte Mutationen/Varianten")),
                      shiny::plotOutput("oncoprint_plot", height = 720)),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(6, bslib::card(bslib::card_header(.tr("Top-Mutationen nach Entität")),
                                         DT::DTOutput("oncoprint_summary_table"))),
            shiny::column(6, bslib::card(bslib::card_header(.tr("Detaildaten Mutationen")),
                                         DT::DTOutput("oncoprint_detail_table")))
          ),
          shiny::br(),
          bslib::card(bslib::card_header(.tr("Strukturelle Befunde aus Mutationsspalte – nur tabellarisch")),
                      DT::DTOutput("oncoprint_structural_table"))
        ),
        shiny::tabPanel(
          .tr("Zytogenetik"), shiny::br(),
          shiny::fluidRow(
            shiny::column(4, bslib::card(
              bslib::card_header(.tr("Zytogenetik-Filter")),
              shiny::helpText(.tr("Quelle: separate Spalte 'Zytogenetik'. NA und negative Befunde werden ausgeblendet. Die Darstellung ist bewusst tabellarisch/als Balkendiagramm, nicht im Oncoprint.")),
              shiny::uiOutput("cyto_filters"),
              shiny::actionButton("run_cyto", .tr("Zytogenetik aktualisieren"), class = "btn-primary"),
              shiny::br(), shiny::br(),
              shiny::downloadButton("download_cyto_data", .tr("Zytogenetik CSV"))
            )),
            shiny::column(8, bslib::layout_column_wrap(
              width = 1/3,
              bslib::value_box(.tr("Fälle mit Zytogenetik"), shiny::textOutput("n_cyto_patients")),
              bslib::value_box(.tr("Zytogenetik-Befunde"), shiny::textOutput("n_cyto_alterations")),
              bslib::value_box(.tr("Entitäten"), shiny::textOutput("n_cyto_entities"))
            ))
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(6, bslib::card(bslib::card_header(.tr("Top-Zytogenetik gesamt")),
                                         shiny::plotOutput("cyto_plot", height = 520),
                                         .dl_ui("dl_cyto"))),
            shiny::column(6, bslib::card(bslib::card_header(.tr("Zytogenetik nach Entität")),
                                         DT::DTOutput("cyto_summary_table")))
          ),
          shiny::br(),
          bslib::card(bslib::card_header(.tr("Detaildaten Zytogenetik")),
                      DT::DTOutput("cyto_detail_table"))
        ),
        shiny::tabPanel(
          .tr("Boxplots"), shiny::br(),
          shiny::fluidRow(
            shiny::column(4, bslib::card(
              bslib::card_header(.tr("Boxplot-Einstellungen")),
              shiny::uiOutput("box_ui"),
              shiny::actionButton("run_box", .tr("Boxplot aktualisieren"), class = "btn-primary"),
              shiny::br(), shiny::br(),
              .dl_ui("dl_box")
            )),
            shiny::column(8, bslib::card(bslib::card_header(.tr("Boxplot")),
                                         shiny::plotOutput("box_plot", height = 540)))
          )
        ),
        shiny::tabPanel(
          "Methoden / Methods", shiny::br(),
          shiny::uiOutput("methods_doc")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Register PNG (600 dpi) + PDF (vector) download handlers for one figure.
  # `plot_reactive` yields the on-screen ggplot; `name` is the export basename
  # (a string, or a function returning one — used for the KM custom title).
  .dl_handlers <- function(id, plot_reactive, name, width = 8, height = 5) {
    base <- function() if (is.function(name)) name() else name
    output[[paste0(id, "_png")]] <- shiny::downloadHandler(
      filename = function() paste0(base(), "_", Sys.Date(), ".png"),
      content  = function(file) zhncommandR::zhn_save_plot(
        plot_reactive(), file, format = "png", width = width, height = height,
        transparent = isTRUE(input$fig_transparent)
      )
    )
    output[[paste0(id, "_pdf")]] <- shiny::downloadHandler(
      filename = function() paste0(base(), "_", Sys.Date(), ".pdf"),
      content  = function(file) zhncommandR::zhn_save_plot(
        plot_reactive(), file, format = "pdf", width = width, height = height,
        transparent = isTRUE(input$fig_transparent)
      )
    )
  }

  # ------------------------------------------------------------------
  # Language toggle (DE / EN) -- swaps DOM strings via shiny.i18n
  # ------------------------------------------------------------------
  shiny::observeEvent(input$lang, {
    shiny.i18n::update_lang(input$lang)
  }, ignoreInit = TRUE)

  # ------------------------------------------------------------------
  # Data source: upload OR example button
  # ------------------------------------------------------------------
  current_path <- shiny::reactiveVal(NULL)
  current_label <- shiny::reactiveVal(.tr("Keine Daten geladen."))

  shiny::observeEvent(input$cohort_file, {
    f <- input$cohort_file
    if (is.null(f) || !nzchar(f$datapath)) return()
    current_path(f$datapath)
    current_label(paste0(.tr("Hochgeladen: "), f$name))
  })

  shiny::observeEvent(input$load_example, {
    p <- zhncommandR::zhn_example_path()
    current_path(p)
    current_label(.tr("Beispieldaten geladen (synthetisch)."))
  })

  output$path_info <- shiny::renderText({
    paste0("Datenquelle:\n", current_label())
  })

  # ------------------------------------------------------------------
  # Data readers — eventReactive on the reload button OR a new path
  # ------------------------------------------------------------------
  data_raw <- shiny::eventReactive(
    list(input$reload, current_path()),
    {
      p <- current_path()
      if (is.null(p)) return(data.frame())
      zhncommandR::zhn_read_cohort(p, verbose = FALSE)
    },
    ignoreNULL = FALSE
  )

  therapy_raw <- shiny::eventReactive(
    list(input$reload, current_path()),
    {
      p <- current_path()
      if (is.null(p)) {
        out <- data.frame()
        attr(out, "source_label") <- .tr("Keine Datenquelle geladen")
        return(out)
      }
      zhncommandR::zhn_read_therapy(p, verbose = FALSE)
    },
    ignoreNULL = FALSE
  )

  diagnostic_raw <- shiny::eventReactive(
    list(input$reload, current_path()),
    {
      p <- current_path()
      if (is.null(p)) {
        out <- data.frame()
        attr(out, "source_label") <- .tr("Keine Datenquelle geladen")
        return(out)
      }
      zhncommandR::zhn_read_diagnostics(p, verbose = FALSE)
    },
    ignoreNULL = FALSE
  )

  # ------------------------------------------------------------------
  # Helper aliases — keep server bodies short
  # ------------------------------------------------------------------
  find_col   <- zhncommandR:::.find_col
  as_yesno   <- zhncommandR:::.as_yesno
  as_event01 <- zhncommandR:::.as_event01
  n_distinct_nonempty <- zhncommandR:::.n_distinct_nonempty

  output$global_filters <- shiny::renderUI({
    df <- data_raw()
    shiny::req(nrow(df) > 0)
    diagnosis_col <- find_col(df, c("diagnose", "kodierung"))
    year_choices <- sort(unique(stats::na.omit(df$behandlungsjahr)))
    diag_choices <- if (!is.null(diagnosis_col)) {
      sort(unique(stats::na.omit(df[[diagnosis_col]])))
    } else character(0)
    shiny::tagList(
      shiny::selectizeInput("year_filter", .tr("Behandlungsjahr"),
                            choices = year_choices, selected = year_choices,
                            multiple = TRUE),
      shiny::selectizeInput("diagnosis_filter", .tr("Diagnose/Kodierung"),
                            choices = diag_choices, selected = diag_choices,
                            multiple = TRUE),
      shiny::checkboxInput("only_primaer", .tr("Nur Primärfälle"), FALSE),
      shiny::checkboxInput("only_patientenfall", .tr("Nur Patientenfälle"), FALSE),
      shiny::checkboxInput("only_inhouse_therapy", .tr("Nur Therapie am Haus"), FALSE)
    )
  })

  data_filtered <- shiny::reactive({
    df <- data_raw()
    if (nrow(df) == 0) return(df)
    diagnosis_col <- find_col(df, c("diagnose", "kodierung"))
    if (!is.null(input$year_filter) && length(input$year_filter) > 0) {
      df <- dplyr::filter(df, .data$behandlungsjahr %in% as.integer(input$year_filter))
    }
    if (!is.null(diagnosis_col) &&
        !is.null(input$diagnosis_filter) && length(input$diagnosis_filter) > 0) {
      df <- dplyr::filter(df, .data[[diagnosis_col]] %in% input$diagnosis_filter)
    }
    if (isTRUE(input$only_primaer) && "primaerfall" %in% names(df)) {
      df <- dplyr::filter(df, as_yesno(.data$primaerfall) %in% TRUE)
    }
    if (isTRUE(input$only_patientenfall) && "patientenfall" %in% names(df)) {
      df <- dplyr::filter(df, as_yesno(.data$patientenfall) %in% TRUE)
    }
    if (isTRUE(input$only_inhouse_therapy) && "therapie_inhouse" %in% names(df)) {
      df <- dplyr::filter(df, as_yesno(.data$therapie_inhouse) %in% TRUE)
    }
    if (!is.null(input$search_text) && nzchar(input$search_text)) {
      search_cols <- intersect(
        c("name", "diagnose", "kodierung", "therapie", "info_weitere_betreuung"),
        names(df)
      )
      pattern <- stringr::regex(input$search_text, ignore_case = TRUE)
      df <- dplyr::filter(
        df,
        dplyr::if_any(dplyr::all_of(search_cols),
                      ~ stringr::str_detect(as.character(.x), pattern))
      )
    }
    df
  })

  output$n_total <- shiny::renderText({
    format(nrow(data_filtered()), big.mark = ".")
  })
  output$n_primaer <- shiny::renderText({
    df <- data_filtered()
    if (!"primaerfall" %in% names(df)) return("n/a")
    format(sum(as_yesno(df$primaerfall) %in% TRUE, na.rm = TRUE), big.mark = ".")
  })
  output$n_patientenfall <- shiny::renderText({
    df <- data_filtered()
    if (!"patientenfall" %in% names(df)) return("n/a")
    format(sum(as_yesno(df$patientenfall) %in% TRUE, na.rm = TRUE), big.mark = ".")
  })
  output$n_psycho <- shiny::renderText({
    df <- data_filtered()
    if (!"psychoonkologie" %in% names(df)) return("n/a")
    n <- sum(as_yesno(df$psychoonkologie) %in% TRUE, na.rm = TRUE)
    denom <- nrow(df)
    paste0(n, " / ", denom, " (",
           scales::percent(n / max(denom, 1), accuracy = 0.1), ")")
  })

  diagnoses_plot <- shiny::reactive({
    df <- data_filtered()
    diagnosis_col <- find_col(df, c("diagnose", "kodierung"))
    shiny::validate(shiny::need(
      !is.null(diagnosis_col), .tr("Keine Diagnose-/Kodierungsspalte gefunden.")
    ))
    zhncommandR::zhn_plot_diagnoses(
      df, col = diagnosis_col,
      transparent = isTRUE(input$fig_transparent), ylab = .tr("Anzahl")
    )
  })
  output$plot_diagnosis <- shiny::renderPlot(diagnoses_plot(), bg = "transparent")
  .dl_handlers("dl_diagnoses", diagnoses_plot, "top_diagnosen_kodierungen")

  cases_year_plot <- shiny::reactive({
    df <- data_filtered()
    shiny::validate(shiny::need(
      "behandlungsjahr" %in% names(df), .tr("Kein Behandlungsjahr ableitbar.")
    ))
    zhncommandR::zhn_plot_cases_by_year(
      df, transparent = isTRUE(input$fig_transparent),
      xlab = .tr("Jahr"), ylab = .tr("Anzahl")
    )
  })
  output$plot_year <- shiny::renderPlot(cases_year_plot(), bg = "transparent")
  .dl_handlers("dl_year", cases_year_plot, "fallzahlen_nach_jahr")

  output$indicator_table <- DT::renderDT({
    df <- data_filtered()
    indicators <- c(
      "tumorkonferenz", "fallbesprechung", "psychoonkologie", "sozialdienst",
      "komplexe_diagnostik_nach_ops_1_940", "histologie_inhouse",
      "histologie_referenzpathologie", "studie", "zahnarzt_mkg",
      "bisphosphonate_denosumab", "hiv_hepatitis"
    )
    indicators <- intersect(indicators, names(df))
    shiny::validate(shiny::need(
      nrow(df) > 0 && length(indicators) > 0,
      .tr("Keine Daten geladen.")
    ))
    out <- lapply(indicators, function(v) {
      val <- df[[v]]
      yes <- sum(as_yesno(val) %in% TRUE, na.rm = TRUE)
      documented <- sum(!is.na(val) & trimws(as.character(val)) != "")
      data.frame(
        Indikator = v, Positiv = yes, Dokumentiert = documented,
        Gesamt = nrow(df),
        Anteil_positiv = ifelse(nrow(df) > 0, yes / nrow(df), NA_real_)
      )
    }) |> dplyr::bind_rows()
    DT::datatable(out, rownames = FALSE,
                  options = list(pageLength = 12, scrollX = TRUE)) |>
      DT::formatPercentage("Anteil_positiv", 1)
  })

  output$quick_questions <- shiny::renderUI({
    df <- data_filtered()
    diagnosis_col <- find_col(df, c("diagnose", "kodierung"))
    if (is.null(diagnosis_col)) {
      return(shiny::helpText(.tr("Keine Diagnose-/Kodierungsspalte gefunden.")))
    }
    hl_2025 <- df |>
      dplyr::filter(.data$behandlungsjahr == 2025,
                    stringr::str_detect(
                      tolower(as.character(.data[[diagnosis_col]])),
                      "hodgkin|hl"
                    )) |> nrow()
    mm <- df |>
      dplyr::filter(stringr::str_detect(
        tolower(as.character(.data[[diagnosis_col]])),
        "myelom|multiple myeloma|mm"
      )) |> nrow()
    psycho <- if ("psychoonkologie" %in% names(df)) {
      sum(as_yesno(df$psychoonkologie) %in% TRUE, na.rm = TRUE)
    } else NA_integer_
    tk <- if ("tumorkonferenz" %in% names(df)) {
      sum(as_yesno(df$tumorkonferenz) %in% TRUE, na.rm = TRUE)
    } else NA_integer_
    shiny::HTML(paste0(
      "<ul>",
      "<li><b>Hodgkin-Lymphome 2025:</b> ", hl_2025, " Fälle im aktuellen Filter.</li>",
      "<li><b>Patienten mit psychoonkologischem Screening:</b> ", psycho, " Fälle.</li>",
      "<li><b>Patienten/Fälle mit Tumorkonferenz:</b> ", tk, " Fälle.</li>",
      "<li><b>Multiple-Myelom-Fälle:</b> ", mm, " Fälle. Für PFS-Kurve Diagnosefilter auf Myelom setzen und im KM-Tab PFS auswählen.</li>",
      "</ul>"
    ))
  })

  # ------------------------------------------------------------------
  # Einfache Abfragen (server logic — verbatim from v5, namespaced)
  # ------------------------------------------------------------------
  output$simple_query_ui <- shiny::renderUI({
    df <- data_filtered()
    shiny::req(nrow(df) > 0)
    diagnosis_col <- find_col(df, c("diagnose", "kodierung"))
    if (identical(input$simple_question, "diagnose_select")) {
      shiny::validate(shiny::need(!is.null(diagnosis_col),
                                  .tr("Keine Spalte 'diagnose' oder 'kodierung' gefunden.")))
      diag_vals <- sort(unique(stats::na.omit(as.character(df[[diagnosis_col]]))))
      shiny::tagList(
        shiny::helpText(.tr("Hier können gezielt dokumentierte Diagnosen aus der Spalte 'Diagnose' ausgewählt werden. Mehrfachauswahl ist möglich.")),
        shiny::selectizeInput("diagnosis_query_values", .tr("Diagnose(n)"),
                              choices = diag_vals, selected = character(0),
                              multiple = TRUE,
                              options = list(placeholder = .tr("z.B. Multiples Myelom auswählen"))),
        shiny::checkboxInput("diagnosis_query_contains",
                             .tr("Als Textsuche verwenden statt exakter Auswahl"), FALSE)
      )
    } else if (identical(input$simple_question, "custom")) {
      text_cols <- names(df)[vapply(df, function(x) {
        is.character(x) || is.factor(x) || is.logical(x) || is.numeric(x)
      }, logical(1L))]
      shiny::tagList(
        shiny::selectInput("custom_col", .tr("Spalte"), choices = text_cols,
                           selected = if ("diagnose" %in% text_cols) "diagnose" else text_cols[1]),
        shiny::radioButtons("custom_mode", .tr("Abfragemodus"), choices = c(
          "Ja/positiv zählen" = "yesno",
          "Exakter Wert" = "exact",
          "Text enthält" = "contains",
          "Nicht leer/dokumentiert" = "documented"
        ), selected = "yesno"),
        shiny::conditionalPanel(
          condition = "input.custom_mode == 'exact' || input.custom_mode == 'contains'",
          shiny::textInput("custom_value", .tr("Suchwert/Text"), value = "")
        )
      )
    } else {
      shiny::helpText(.tr("Die Abfrage wird auf die aktuell global gefilterte Patiententabelle angewendet. Beispiel: Jahr 2025 im linken Filter auswählen, dann hier Multiples Myelom zählen."))
    }
  })

  simple_query_data <- shiny::eventReactive(input$run_simple_query, {
    df <- data_filtered()
    diagnosis_col <- find_col(df, c("diagnose", "kodierung"))
    q <- input$simple_question
    shiny::validate(shiny::need(nrow(df) > 0, .tr("Keine Daten nach globalem Filter.")))
    result <- df
    label <- ""
    if (q == "psycho") {
      shiny::validate(shiny::need("psychoonkologie" %in% names(df), .tr("Spalte 'psychoonkologie' nicht gefunden.")))
      result <- dplyr::filter(df, as_yesno(.data$psychoonkologie) %in% TRUE)
      label <- "Patienten/Fälle mit psychoonkologischem Screening"
    } else if (q == "hivhep") {
      hivhep_cols <- intersect(c("hiv_hepatitis", "hiv", "hep_b", "hep_c", "hepb", "hepc"), names(df))
      shiny::validate(shiny::need(length(hivhep_cols) > 0, .tr("Keine HIV/Hepatitis-Spalten gefunden.")))
      result <- dplyr::filter(df, dplyr::if_any(dplyr::all_of(hivhep_cols),
                                                ~ as_yesno(.x) %in% TRUE))
      label <- "Patienten/Fälle mit dokumentiert positivem HIV/Hepatitis-Screening"
    } else if (q == "diagnose_select") {
      shiny::validate(shiny::need(!is.null(diagnosis_col), .tr("Keine Diagnose-/Kodierungsspalte gefunden.")))
      shiny::validate(shiny::need(!is.null(input$diagnosis_query_values) && length(input$diagnosis_query_values) > 0,
                                  .tr("Bitte mindestens eine Diagnose auswählen.")))
      selected_diag <- input$diagnosis_query_values
      if (isTRUE(input$diagnosis_query_contains)) {
        pattern <- paste(stringr::str_escape(selected_diag), collapse = "|")
        result <- dplyr::filter(df, stringr::str_detect(
          tolower(as.character(.data[[diagnosis_col]])),
          stringr::regex(tolower(pattern), ignore_case = TRUE)
        ))
      } else {
        result <- dplyr::filter(df, as.character(.data[[diagnosis_col]]) %in% selected_diag)
      }
      label <- paste0("Patienten/Fälle mit Diagnose: ", paste(selected_diag, collapse = ", "))
    } else if (q == "myelom") {
      shiny::validate(shiny::need(!is.null(diagnosis_col), .tr("Keine Diagnose-/Kodierungsspalte gefunden.")))
      result <- dplyr::filter(df, stringr::str_detect(
        tolower(as.character(.data[[diagnosis_col]])),
        "myelom|multiple myeloma|multiples myelom|plasma"
      ))
      label <- "Patienten/Fälle mit Multiplem Myelom"
    } else if (q == "hodgkin") {
      shiny::validate(shiny::need(!is.null(diagnosis_col), .tr("Keine Diagnose-/Kodierungsspalte gefunden.")))
      result <- dplyr::filter(df, stringr::str_detect(
        tolower(as.character(.data[[diagnosis_col]])), "hodgkin|hl"
      ))
      label <- "Patienten/Fälle mit Hodgkin-Lymphom"
    } else if (q == "tumorkonferenz") {
      shiny::validate(shiny::need("tumorkonferenz" %in% names(df), .tr("Spalte 'tumorkonferenz' nicht gefunden.")))
      result <- dplyr::filter(df, as_yesno(.data$tumorkonferenz) %in% TRUE)
      label <- "Patienten/Fälle mit Tumorkonferenz"
    } else if (q == "sozialdienst") {
      shiny::validate(shiny::need("sozialdienst" %in% names(df), .tr("Spalte 'sozialdienst' nicht gefunden.")))
      result <- dplyr::filter(df, as_yesno(.data$sozialdienst) %in% TRUE)
      label <- "Patienten/Fälle mit Sozialdienst"
    } else if (q == "primaerfall") {
      shiny::validate(shiny::need("primaerfall" %in% names(df), .tr("Spalte 'primaerfall' nicht gefunden.")))
      result <- dplyr::filter(df, as_yesno(.data$primaerfall) %in% TRUE)
      label <- .tr("Primärfälle")
    } else if (q == "patientenfall") {
      shiny::validate(shiny::need("patientenfall" %in% names(df), .tr("Spalte 'patientenfall' nicht gefunden.")))
      result <- dplyr::filter(df, as_yesno(.data$patientenfall) %in% TRUE)
      label <- .tr("Patientenfälle")
    } else if (q == "custom") {
      shiny::validate(shiny::need(!is.null(input$custom_col) && input$custom_col %in% names(df), .tr("Bitte eine gültige Spalte auswählen.")))
      col <- input$custom_col; mode <- input$custom_mode; value <- input$custom_value
      if (mode == "yesno") {
        result <- dplyr::filter(df, as_yesno(.data[[col]]) %in% TRUE)
        label <- paste0("Eigene Abfrage: ", col, " = Ja/positiv")
      } else if (mode == "exact") {
        shiny::validate(shiny::need(nzchar(value), .tr("Bitte einen Suchwert eingeben.")))
        # Diacritic-insensitive comparison so "hamatologisch" matches
        # "hämatologisch". Falls back to plain tolower() if stringi missing.
        ascii_lower <- function(x) {
          if (requireNamespace("stringi", quietly = TRUE)) {
            tolower(stringi::stri_trans_general(as.character(x), "Latin-ASCII"))
          } else {
            tolower(as.character(x))
          }
        }
        result <- dplyr::filter(
          df,
          ascii_lower(trimws(as.character(.data[[col]]))) ==
            ascii_lower(trimws(value))
        )
        label <- paste0("Eigene Abfrage: ", col, " = ", value)
      } else if (mode == "contains") {
        shiny::validate(shiny::need(nzchar(value), .tr("Bitte einen Suchtext eingeben.")))
        ascii_lower <- function(x) {
          if (requireNamespace("stringi", quietly = TRUE)) {
            tolower(stringi::stri_trans_general(as.character(x), "Latin-ASCII"))
          } else {
            tolower(as.character(x))
          }
        }
        result <- dplyr::filter(df, stringr::str_detect(
          ascii_lower(.data[[col]]), stringr::fixed(ascii_lower(value))
        ))
        label <- paste0("Eigene Abfrage: ", col, " enthält '", value, "'")
      } else if (mode == "documented") {
        result <- dplyr::filter(df, !is.na(.data[[col]]) & trimws(as.character(.data[[col]])) != "")
        label <- paste0("Eigene Abfrage: ", col, " ist dokumentiert/nicht leer")
      }
    }
    if (isTRUE(input$simple_only_unique) && "name" %in% names(result)) {
      result <- dplyr::distinct(result, .data$name, .keep_all = TRUE)
    }
    attr(result, "query_label") <- label
    attr(result, "denominator") <- if (isTRUE(input$simple_only_unique) && "name" %in% names(df)) {
      dplyr::n_distinct(df$name)
    } else nrow(df)
    result
  }, ignoreNULL = FALSE)

  output$simple_query_result <- shiny::renderText({
    res <- simple_query_data(); denom <- attr(res, "denominator")
    paste0(nrow(res), " / ", denom, " (",
           scales::percent(nrow(res) / max(denom, 1), accuracy = 0.1), ")")
  })
  output$simple_query_summary <- DT::renderDT({
    res <- simple_query_data()
    label <- attr(res, "query_label"); denom <- attr(res, "denominator")
    out <- data.frame(
      Abfrage = label, Treffer = nrow(res), Grundgesamtheit = denom,
      Anteil = ifelse(denom > 0, nrow(res) / denom, NA_real_)
    )
    DT::datatable(out, rownames = FALSE,
                  options = list(dom = "t", scrollX = TRUE)) |>
      DT::formatPercentage("Anteil", 1)
  })
  output$simple_query_table <- DT::renderDT({
    res <- simple_query_data()
    cols_preferred <- intersect(c(
      "name", "geschlecht", "geb_datum", "erstvorstellung", "erstdiagnose",
      "diagnose", "kodierung", "primaerfall", "patientenfall",
      "tumorkonferenz", "psychoonkologie", "hiv_hepatitis", "hiv",
      "hep_b", "hep_c", "pfs", "os"
    ), names(res))
    if (length(cols_preferred) > 0) res <- res[, cols_preferred, drop = FALSE]
    DT::datatable(res, rownames = FALSE,
                  options = list(pageLength = 20, scrollX = TRUE,
                                 searchHighlight = TRUE))
  })
  output$download_simple_query <- shiny::downloadHandler(
    filename = function() paste0("einfache_Abfrage_", Sys.Date(), ".csv"),
    content = function(file) {
      utils::write.csv(simple_query_data(), file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    }
  )

  output$table <- DT::renderDT({
    DT::datatable(data_filtered(), rownames = FALSE, selection = "single",
                  options = list(pageLength = 25, scrollX = TRUE,
                                 searchHighlight = TRUE))
  })
  output$download_filtered <- shiny::downloadHandler(
    filename = function() paste0("gefilterte_Daten_", Sys.Date(), ".csv"),
    content = function(file) {
      utils::write.csv(data_filtered(), file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    }
  )

  # ------------------------------------------------------------------
  # Tumorboard — in-session reactiveVal, optional upload, no disk writes
  # ------------------------------------------------------------------
  tumorboard_data <- shiny::reactiveVal(zhncommandR::zhn_read_tumorboard(NULL))

  shiny::observeEvent(input$tumorboard_file, {
    f <- input$tumorboard_file
    if (is.null(f) || !nzchar(f$datapath)) return()
    tumorboard_data(zhncommandR::zhn_read_tumorboard(f$datapath))
    shiny::showNotification(paste0(.tr("Tumorboardbeschlüsse geladen: "), f$name),
                            type = "message")
  })

  output$tb_storage_info <- shiny::renderText({
    paste0("Speicherung:\nIn-Session (kein Schreibzugriff auf Festplatte). ",
           "Über 'Tumorboardbeschlüsse CSV' herunterladen oder via Upload ",
           "wiederherstellen.")
  })

  tb_patient_col <- shiny::reactive({
    df <- data_filtered()
    find_col(df, c("patient", "name", "patient_id", "patienten_id", "id"))
  })

  output$tb_patient_ui <- shiny::renderUI({
    df <- data_filtered()
    shiny::req(nrow(df) > 0)
    pcol <- tb_patient_col()
    shiny::validate(shiny::need(!is.null(pcol),
                                .tr("Keine Patientenspalte gefunden. Erwartet z.B. 'Patient', 'Name' oder 'Patient_ID'.")))
    choices <- sort(unique(trimws(as.character(df[[pcol]]))))
    choices <- choices[!is.na(choices) & choices != ""]
    shiny::selectizeInput("tb_patient", .tr("Patient aus Patientenliste"),
                          choices = choices,
                          selected = if (length(choices)) choices[1] else NULL,
                          multiple = FALSE)
  })

  shiny::observeEvent(input$table_rows_selected, {
    idx <- input$table_rows_selected
    df <- data_filtered()
    pcol <- tb_patient_col()
    if (length(idx) == 1 && !is.null(pcol) && pcol %in% names(df) && nrow(df) >= idx) {
      pat <- trimws(as.character(df[[pcol]][idx]))
      if (!is.na(pat) && nzchar(pat)) {
        shiny::updateSelectizeInput(session, "tb_patient", selected = pat)
      }
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$save_tb, {
    if (is.null(input$tb_patient) || !nzchar(input$tb_patient)) {
      shiny::showNotification(.tr("Bitte zuerst einen Patienten auswählen."), type = "error"); return()
    }
    if (is.null(input$tb_decision) || !nzchar(trimws(input$tb_decision))) {
      shiny::showNotification(.tr("Bitte einen Tumorboardbeschluss eintragen."), type = "error"); return()
    }
    entry <- data.frame(
      Patient = as.character(input$tb_patient),
      Board_Datum = as.Date(input$tb_date),
      Tumorboardbeschluss = trimws(as.character(input$tb_decision)),
      Verantwortlich = trimws(as.character(input$tb_responsible)),
      Erfasst_am = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )
    tumorboard_data(dplyr::bind_rows(tumorboard_data(), entry))
    shiny::updateTextAreaInput(session, "tb_decision", value = "")
    shiny::showNotification(.tr("Tumorboardbeschluss gespeichert (in-Session)."),
                            type = "message")
  })

  tb_current_patient <- shiny::reactive({
    df <- tumorboard_data()
    if (is.null(input$tb_patient) || !nzchar(input$tb_patient) || nrow(df) == 0) {
      return(df[0, , drop = FALSE])
    }
    df |>
      dplyr::filter(.data$Patient == input$tb_patient) |>
      dplyr::arrange(dplyr::desc(.data$Board_Datum), dplyr::desc(.data$Erfasst_am))
  })

  output$tb_n_total   <- shiny::renderText(format(nrow(tumorboard_data()), big.mark = "."))
  output$tb_n_patient <- shiny::renderText(format(nrow(tb_current_patient()), big.mark = "."))
  output$tb_n_patients <- shiny::renderText({
    df <- tumorboard_data()
    if (nrow(df) == 0 || !"Patient" %in% names(df)) return("0")
    format(n_distinct_nonempty(df$Patient), big.mark = ".")
  })
  output$tb_patient_table <- DT::renderDT({
    DT::datatable(tb_current_patient(), rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE))
  })
  output$tb_all_table <- DT::renderDT({
    df <- tumorboard_data()
    if (nrow(df) > 0) df <- dplyr::arrange(df, dplyr::desc(.data$Board_Datum),
                                           .data$Patient)
    DT::datatable(df, rownames = FALSE,
                  options = list(pageLength = 25, scrollX = TRUE,
                                 searchHighlight = TRUE))
  })
  output$download_tb <- shiny::downloadHandler(
    filename = function() paste0("Tumorboardbeschluesse_", Sys.Date(), ".csv"),
    content = function(file) {
      utils::write.csv(tumorboard_data(), file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    }
  )

  # ------------------------------------------------------------------
  # Kaplan-Meier — publication-ready, rendered with ggsurvfit (zhn_plot_km)
  # ------------------------------------------------------------------
  # Live count of levels in the selected grouping (drives stat gating).
  km_n_groups <- shiny::reactive({
    df <- data_filtered()
    g <- input$km_group
    if (is.null(g) || g == "__none__" || !g %in% names(df)) return(1L)
    length(unique(stats::na.omit(df[[g]])))
  })

  output$km_ui <- shiny::renderUI({
    df <- data_filtered(); shiny::req(nrow(df) > 0)
    num_cols <- names(df)[vapply(df, is.numeric, logical(1L))]
    cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x),
                                 logical(1L))]
    sel_time  <- if ("os" %in% num_cols) "os" else if ("pfs" %in% num_cols) "pfs"
                 else num_cols[1]
    sel_event <- if ("death_event" %in% names(df)) "death_event"
                 else if ("rezidiv_event" %in% names(df)) "rezidiv_event"
                 else names(df)[1]
    bslib::accordion(
      open = .tr("Daten/Endpunkt"),
      bslib::accordion_panel(
        .tr("Daten/Endpunkt"),
        shiny::selectInput("km_time", .tr("Zeitvariable"),
                           choices = num_cols, selected = sel_time),
        shiny::selectInput("km_event", .tr("Ereignisvariable"),
                           choices = names(df), selected = sel_event),
        shiny::selectInput("km_group", .tr("Gruppierung"),
                           choices = c(stats::setNames("__none__", .tr("(keine)")),
                                       stats::setNames(cat_cols, cat_cols)),
                           selected = "__none__")
      ),
      bslib::accordion_panel(
        .tr("Statistik"),
        shiny::checkboxInput("km_ci", .tr("Konfidenzintervall"), TRUE),
        shiny::conditionalPanel(
          "input.km_ci",
          shiny::selectInput("km_ci_level", .tr("CI-Niveau"),
                             c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99),
                             selected = 0.95),
          shiny::selectInput("km_ci_type", .tr("CI-Methode"),
                             c("log-log", "log", "plain"), selected = "log-log")
        ),
        shiny::checkboxInput("km_risktable",
                             .tr("Risikotabelle (numbers at risk)"), TRUE),
        shiny::conditionalPanel(
          "input.km_risktable",
          shiny::checkboxGroupInput(
            "km_risktable_stats", .tr("Zeilen"),
            choices = c("Unter Risiko" = "n.risk", "Zensiert" = "n.censor",
                        "Ereignisse" = "n.event"),
            selected = "n.risk"
          )
        ),
        shiny::checkboxInput("km_pvalue",
                             .tr("Log-rank p-Wert (≥ 2 Gruppen)"), TRUE),
        shiny::checkboxInput("km_hr",
                             .tr("Hazard Ratio (Cox, genau 2 Gruppen)"), FALSE),
        shiny::checkboxInput("km_median",
                             .tr("Median-Überleben markieren"), FALSE),
        shiny::checkboxInput("km_censor", .tr("Zensur-Markierungen"), TRUE),
        shiny::checkboxInput("km_pairwise",
                             .tr("Paarweise Vergleiche, BH (> 2 Gruppen)"), FALSE)
      ),
      bslib::accordion_panel(
        .tr("Darstellung"),
        shiny::radioButtons("km_yscale", .tr("Y-Skala"),
          c("Überleben (0-1)" = "survival", "Prozent" = "percent",
            "Kumulative Inzidenz" = "cuminc"), selected = "survival"),
        shiny::numericInput("km_xbreak", .tr("X-Achsen-Intervall"),
                            value = NA, min = 1),
        shiny::numericInput("km_xmax", .tr("X-Achse maximal"),
                            value = NA, min = 1),
        shiny::selectInput("km_legend", .tr("Legende"),
          c("oben" = "top", "rechts" = "right", "unten" = "bottom",
            "keine" = "none"), selected = "top"),
        shiny::textInput("km_title", .tr("Titel"),
                         value = "Kaplan-Meier-Überlebenskurve"),
        shiny::textInput("km_subtitle", .tr("Untertitel"), value = ""),
        shiny::textInput("km_caption", .tr("Fußnote"), value = "")
      )
    )
  })

  km_plot_obj <- shiny::eventReactive(input$run_km, {
    df <- data_filtered()
    shiny::validate(shiny::need(nrow(df) > 1, .tr("Zu wenige Daten nach Filter.")))
    shiny::req(input$km_time, input$km_event)
    grp <- if (!is.null(input$km_group) && input$km_group != "__none__") {
      input$km_group
    } else NULL
    n_g <- km_n_groups()
    # Conditional enabling: gate stats by group count (hard rule 5).
    show_p  <- isTRUE(input$km_pvalue)   && n_g >= 2L
    show_hr <- isTRUE(input$km_hr)       && n_g == 2L
    xbk <- if (is.null(input$km_xbreak) || is.na(input$km_xbreak)) NULL else input$km_xbreak
    xmx <- if (is.null(input$km_xmax) || is.na(input$km_xmax)) NULL else input$km_xmax

    # The plot function raises conditions; surface them as validate messages.
    res <- tryCatch(
      zhncommandR::zhn_plot_km(
        df, time_col = input$km_time, event_col = input$km_event,
        group_col = grp,
        conf_int = isTRUE(input$km_ci),
        conf_level = as.numeric(input$km_ci_level %||% 0.95),
        conf_type = input$km_ci_type %||% "log-log",
        risktable = isTRUE(input$km_risktable),
        risktable_stats = input$km_risktable_stats %||% "n.risk",
        censor_marks = isTRUE(input$km_censor),
        show_pvalue = show_p, show_hr = show_hr,
        show_median = isTRUE(input$km_median),
        y_scale = input$km_yscale %||% "survival",
        x_break = xbk, x_max = xmx,
        legend_pos = input$km_legend %||% "top",
        title = input$km_title, subtitle = input$km_subtitle,
        caption = input$km_caption,
        transparent = isTRUE(input$fig_transparent)
      ),
      error = function(e) e
    )
    shiny::validate(shiny::need(
      !inherits(res, "error"),
      if (inherits(res, "error")) conditionMessage(res) else ""
    ))
    res
  }, ignoreNULL = FALSE)

  output$km_plot <- shiny::renderPlot(
    { g <- km_plot_obj(); shiny::req(g); g },
    bg = "transparent"
  )
  # KM export basename = slugified custom title (§10); roomy for the risk table.
  .dl_handlers("dl_km", km_plot_obj, function() {
    zhncommandR:::.slugify(input$km_title, fallback = "kaplan_meier")
  }, width = 8, height = 6.5)

  # Pairwise log-rank (BH) table — only when the toggle is on and groups > 2.
  output$km_pairwise_table <- DT::renderDT({
    shiny::req(isTRUE(input$km_pairwise))
    df <- data_filtered()
    g <- input$km_group
    shiny::validate(
      shiny::need(!is.null(g) && g != "__none__", .tr("Gruppierung wählen.")),
      shiny::need(km_n_groups() > 2L,
                  .tr("Paarweise Vergleiche benötigen mehr als zwei Gruppen."))
    )
    pw <- tryCatch(
      zhncommandR::zhn_km_pairwise(df, input$km_time, input$km_event, g),
      error = function(e) e
    )
    shiny::validate(shiny::need(!inherits(pw, "error"),
      if (inherits(pw, "error")) conditionMessage(pw) else ""))
    pw$p_roh <- round(pw$p_roh, 4)
    pw$p_BH  <- round(pw$p_BH, 4)
    DT::datatable(pw, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  # ------------------------------------------------------------------
  # OPS-8-544
  # ------------------------------------------------------------------
  therapy_block_data <- shiny::reactive({
    zhncommandR::zhn_prepare_therapy_blocks(therapy_raw())
  })

  output$therapy_source_info <- shiny::renderText({
    blocks <- therapy_block_data()
    paste0(
      attr(therapy_raw(), "source_label"), "\nPatientenspalte erkannt: ",
      ifelse(is.null(attr(blocks, "patient_cols_used")) ||
               attr(blocks, "patient_cols_used") == "", "keine",
             attr(blocks, "patient_cols_used"))
    )
  })

  output$therapy_filters <- shiny::renderUI({
    blocks <- therapy_block_data()
    if (nrow(blocks) == 0) {
      return(shiny::helpText(.tr("Keine Therapieblock-Tabelle gefunden. Bitte die OPS-8-544-Tabelle als Blatt 'Komplexe Chemotherapie' (oder 'Therapie_OPS8544') in die Excel-Datei einfügen.")))
    }
    shiny::tagList(
      shiny::selectizeInput("therapy_year_filter", .tr("Jahr"),
                            choices = sort(unique(stats::na.omit(blocks$jahr))),
                            selected = sort(unique(stats::na.omit(blocks$jahr))),
                            multiple = TRUE),
      shiny::selectizeInput("therapy_protocol_filter", .tr("Therapieprotokoll"),
                            choices = sort(unique(stats::na.omit(blocks$therapieprotokoll))),
                            selected = NULL, multiple = TRUE),
      shiny::selectizeInput("therapy_diagnosis_filter", .tr("Diagnose"),
                            choices = sort(unique(stats::na.omit(blocks$diagnose))),
                            selected = NULL, multiple = TRUE),
      shiny::textInput("therapy_search",
                       .tr("Freitextsuche Patient/Therapie/Diagnose"), value = "")
    )
  })

  therapy_filtered <- shiny::reactive({
    blocks <- therapy_block_data()
    if (nrow(blocks) == 0) return(blocks)
    if (!is.null(input$therapy_year_filter) && length(input$therapy_year_filter) > 0) {
      blocks <- dplyr::filter(blocks, .data$jahr %in% as.integer(input$therapy_year_filter))
    }
    if (!is.null(input$therapy_protocol_filter) && length(input$therapy_protocol_filter) > 0) {
      blocks <- dplyr::filter(blocks, .data$therapieprotokoll %in% input$therapy_protocol_filter)
    }
    if (!is.null(input$therapy_diagnosis_filter) && length(input$therapy_diagnosis_filter) > 0) {
      blocks <- dplyr::filter(blocks, .data$diagnose %in% input$therapy_diagnosis_filter)
    }
    if (!is.null(input$therapy_search) && nzchar(input$therapy_search)) {
      pat <- tolower(input$therapy_search)
      blocks <- dplyr::filter(blocks, grepl(
        pat,
        tolower(paste(.data$patient, .data$therapieprotokoll, .data$diagnose)),
        fixed = TRUE
      ))
    }
    blocks
  })

  output$n_therapy_blocks <- shiny::renderText(format(nrow(therapy_filtered()), big.mark = "."))
  output$n_therapy_patients <- shiny::renderText({
    blocks <- therapy_filtered()
    if (!("patient" %in% names(blocks))) return("0")
    format(n_distinct_nonempty(blocks$patient), big.mark = ".")
  })
  output$n_therapy_protocols <- shiny::renderText(
    format(length(unique(stats::na.omit(therapy_filtered()$therapieprotokoll))), big.mark = ".")
  )

  output$therapy_protocol_table <- DT::renderDT({
    blocks <- therapy_filtered()
    shiny::validate(shiny::need(nrow(blocks) > 0, .tr("Keine Therapieblöcke im aktuellen Filter.")))
    tab <- blocks |>
      dplyr::group_by(.data$therapieprotokoll) |>
      dplyr::summarise(OPS_8_544_Bloecke = dplyr::n(),
                       Patienten = n_distinct_nonempty(.data$patient),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(.data$OPS_8_544_Bloecke)) |>
      dplyr::mutate(Anteil = scales::percent(.data$OPS_8_544_Bloecke / sum(.data$OPS_8_544_Bloecke)))
    DT::datatable(tab, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$therapy_diagnosis_table <- DT::renderDT({
    blocks <- therapy_filtered()
    shiny::validate(shiny::need(nrow(blocks) > 0, .tr("Keine Therapieblöcke im aktuellen Filter.")))
    tab <- blocks |>
      dplyr::group_by(.data$diagnose) |>
      dplyr::summarise(OPS_8_544_Bloecke = dplyr::n(),
                       Patienten = n_distinct_nonempty(.data$patient),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(.data$OPS_8_544_Bloecke)) |>
      dplyr::mutate(Anteil = scales::percent(.data$OPS_8_544_Bloecke / sum(.data$OPS_8_544_Bloecke)))
    DT::datatable(tab, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  therapy_protocol_plot_obj <- shiny::reactive({
    blocks <- therapy_filtered()
    shiny::validate(shiny::need(nrow(blocks) > 0, .tr("Keine Therapieblöcke im aktuellen Filter.")))
    tab <- blocks |> dplyr::count(.data$therapieprotokoll, sort = TRUE) |> dplyr::slice_head(n = 20)
    ggplot2::ggplot(tab, ggplot2::aes(x = stats::reorder(.data$therapieprotokoll, .data$n),
                                      y = .data$n)) +
      ggplot2::geom_col(fill = zhncommandR::zhn_pal$accent) + ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = .tr("OPS-8-544-Blöcke")) +
      zhncommandR::zhn_theme(transparent = isTRUE(input$fig_transparent))
  })
  output$therapy_protocol_plot <- shiny::renderPlot(therapy_protocol_plot_obj(), bg = "transparent")
  .dl_handlers("dl_therapy_protocol", therapy_protocol_plot_obj, "bloecke_nach_therapieprotokoll")

  therapy_month_plot_obj <- shiny::reactive({
    blocks <- therapy_filtered()
    shiny::validate(shiny::need(nrow(blocks) > 0, .tr("Keine Therapieblöcke im aktuellen Filter.")))
    tab <- blocks |> dplyr::filter(!is.na(.data$monat_sort)) |> dplyr::count(.data$monat_sort)
    shiny::validate(shiny::need(nrow(tab) > 0, .tr("Keine verwertbaren Datums-/Monatsangaben.")))
    ggplot2::ggplot(tab, ggplot2::aes(x = .data$monat_sort, y = .data$n, group = 1)) +
      ggplot2::geom_line(colour = zhncommandR::zhn_pal$accent, linewidth = 0.7) +
      ggplot2::geom_point(colour = zhncommandR::zhn_pal$accent) +
      ggplot2::labs(x = .tr("Monat"), y = .tr("OPS-8-544-Blöcke")) +
      zhncommandR::zhn_theme(transparent = isTRUE(input$fig_transparent)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  })
  output$therapy_month_plot <- shiny::renderPlot(therapy_month_plot_obj(), bg = "transparent")
  .dl_handlers("dl_therapy_month", therapy_month_plot_obj, "monatliche_ops_8_544_bloecke")

  output$therapy_block_details <- DT::renderDT({
    blocks <- therapy_filtered()
    show_cols <- intersect(c("datum", "patient", "therapieprotokoll", "diagnose", "zyklus", "jahr"), names(blocks))
    DT::datatable(blocks[, show_cols, drop = FALSE], rownames = FALSE,
                  options = list(pageLength = 25, scrollX = TRUE))
  })

  output$download_therapy_blocks <- shiny::downloadHandler(
    filename = function() paste0("OPS_8_544_Therapiebloecke_", Sys.Date(), ".csv"),
    content = function(file) {
      utils::write.csv(therapy_filtered(), file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    }
  )

  # ------------------------------------------------------------------
  # OPS-1-941
  # ------------------------------------------------------------------
  diagnostic_block_data <- shiny::reactive({
    zhncommandR::zhn_prepare_diagnostic_blocks(diagnostic_raw())
  })

  output$diagnostic_source_info <- shiny::renderText({
    blocks <- diagnostic_block_data()
    paste0(attr(diagnostic_raw(), "source_label"), "\nErkannte Komponenten: ",
           ifelse(is.null(attr(blocks, "component_cols")) ||
                    attr(blocks, "component_cols") == "", "keine",
                  attr(blocks, "component_cols")))
  })

  output$diagnostic_filters <- shiny::renderUI({
    blocks <- diagnostic_block_data()
    if (nrow(blocks) == 0) {
      return(shiny::helpText(.tr("Keine OPS-1-941-/Komplexe-Diagnostik-Tabelle gefunden. Bitte die Tabelle als Blatt 'Komplexe Diagnostik' in die Excel-Datei einfügen.")))
    }
    shiny::tagList(
      shiny::selectizeInput("diagnostic_year_filter", .tr("Jahr"),
                            choices = sort(unique(stats::na.omit(blocks$jahr))),
                            selected = sort(unique(stats::na.omit(blocks$jahr))),
                            multiple = TRUE),
      shiny::selectizeInput("diagnostic_diagnosis_filter", .tr("Diagnose"),
                            choices = sort(unique(stats::na.omit(blocks$diagnose))),
                            selected = NULL, multiple = TRUE),
      shiny::selectizeInput("diagnostic_component_filter", .tr("Diagnostikbereich"),
                            choices = sort(unique(zhncommandR:::.diagnostic_components_long(blocks)$diagnostik_bereich)),
                            selected = NULL, multiple = TRUE),
      shiny::textInput("diagnostic_search", .tr("Freitextsuche Patient/Diagnose"), value = "")
    )
  })

  diagnostic_filtered <- shiny::reactive({
    blocks <- diagnostic_block_data()
    if (nrow(blocks) == 0) return(blocks)
    if (!is.null(input$diagnostic_year_filter) && length(input$diagnostic_year_filter) > 0) {
      blocks <- dplyr::filter(blocks, .data$jahr %in% as.integer(input$diagnostic_year_filter))
    }
    if (!is.null(input$diagnostic_diagnosis_filter) && length(input$diagnostic_diagnosis_filter) > 0) {
      blocks <- dplyr::filter(blocks, .data$diagnose %in% input$diagnostic_diagnosis_filter)
    }
    if (!is.null(input$diagnostic_component_filter) && length(input$diagnostic_component_filter) > 0) {
      long <- zhncommandR:::.diagnostic_components_long(blocks)
      long <- dplyr::filter(long, .data$diagnostik_bereich %in% input$diagnostic_component_filter)
      keep_pat <- unique(long$patient)
      blocks <- dplyr::filter(blocks, .data$patient %in% keep_pat)
    }
    if (!is.null(input$diagnostic_search) && nzchar(input$diagnostic_search)) {
      pat <- tolower(input$diagnostic_search)
      blocks <- dplyr::filter(blocks, grepl(
        pat, tolower(paste(.data$patient, .data$diagnose)), fixed = TRUE
      ))
    }
    blocks
  })

  diagnostic_components_filtered <- shiny::reactive({
    long <- zhncommandR:::.diagnostic_components_long(diagnostic_filtered())
    if (!is.null(input$diagnostic_component_filter) &&
        length(input$diagnostic_component_filter) > 0 && nrow(long) > 0) {
      long <- dplyr::filter(long, .data$diagnostik_bereich %in% input$diagnostic_component_filter)
    }
    long
  })

  output$n_diagnostic_blocks <- shiny::renderText(format(nrow(diagnostic_filtered()), big.mark = "."))
  output$n_diagnostic_patients <- shiny::renderText({
    blocks <- diagnostic_filtered()
    if (!("patient" %in% names(blocks))) return("0")
    format(n_distinct_nonempty(blocks$patient), big.mark = ".")
  })
  output$n_diagnostic_diagnoses <- shiny::renderText(
    format(length(unique(stats::na.omit(diagnostic_filtered()$diagnose))), big.mark = ".")
  )

  output$diagnostic_component_table <- DT::renderDT({
    long <- diagnostic_components_filtered()
    shiny::validate(shiny::need(nrow(long) > 0, .tr("Keine komplexen Diagnostik-Komponenten im aktuellen Filter.")))
    tab <- long |>
      dplyr::group_by(.data$diagnostik_bereich) |>
      dplyr::summarise(Anzahl = dplyr::n(),
                       Patienten = n_distinct_nonempty(.data$patient),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(.data$Anzahl)) |>
      dplyr::mutate(Anteil = scales::percent(.data$Anzahl / sum(.data$Anzahl)))
    DT::datatable(tab, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$diagnostic_diagnosis_table <- DT::renderDT({
    blocks <- diagnostic_filtered()
    shiny::validate(shiny::need(nrow(blocks) > 0, .tr("Keine komplexen Diagnostiken im aktuellen Filter.")))
    tab <- blocks |>
      dplyr::group_by(.data$diagnose) |>
      dplyr::summarise(OPS_1_941_Faelle = dplyr::n(),
                       Patienten = n_distinct_nonempty(.data$patient),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(.data$OPS_1_941_Faelle)) |>
      dplyr::mutate(Anteil = scales::percent(.data$OPS_1_941_Faelle / sum(.data$OPS_1_941_Faelle)))
    DT::datatable(tab, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  diagnostic_component_plot_obj <- shiny::reactive({
    long <- diagnostic_components_filtered()
    shiny::validate(shiny::need(nrow(long) > 0, .tr("Keine komplexen Diagnostik-Komponenten im aktuellen Filter.")))
    tab <- long |> dplyr::count(.data$diagnostik_bereich, sort = TRUE)
    ggplot2::ggplot(tab, ggplot2::aes(x = stats::reorder(.data$diagnostik_bereich, .data$n),
                                      y = .data$n)) +
      ggplot2::geom_col(fill = zhncommandR::zhn_pal$accent) + ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = .tr("Anzahl")) +
      zhncommandR::zhn_theme(transparent = isTRUE(input$fig_transparent))
  })
  output$diagnostic_component_plot <- shiny::renderPlot(diagnostic_component_plot_obj(), bg = "transparent")
  .dl_handlers("dl_diag_component", diagnostic_component_plot_obj, "ops_1_941_komponenten")

  diagnostic_month_plot_obj <- shiny::reactive({
    blocks <- diagnostic_filtered()
    shiny::validate(shiny::need(nrow(blocks) > 0, .tr("Keine komplexen Diagnostiken im aktuellen Filter.")))
    tab <- blocks |> dplyr::filter(!is.na(.data$monat_sort)) |> dplyr::count(.data$monat_sort)
    shiny::validate(shiny::need(nrow(tab) > 0, .tr("Keine verwertbaren Datums-/Monatsangaben.")))
    ggplot2::ggplot(tab, ggplot2::aes(x = .data$monat_sort, y = .data$n, group = 1)) +
      ggplot2::geom_line(colour = zhncommandR::zhn_pal$accent, linewidth = 0.7) +
      ggplot2::geom_point(colour = zhncommandR::zhn_pal$accent) +
      ggplot2::labs(x = .tr("Monat"), y = .tr("OPS-1-941-Fälle")) +
      zhncommandR::zhn_theme(transparent = isTRUE(input$fig_transparent)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  })
  output$diagnostic_month_plot <- shiny::renderPlot(diagnostic_month_plot_obj(), bg = "transparent")
  .dl_handlers("dl_diag_month", diagnostic_month_plot_obj, "monatliche_komplexe_diagnostiken")

  output$diagnostic_block_details <- DT::renderDT({
    blocks <- diagnostic_filtered()
    show_cols <- intersect(
      c("datum", "patient", "diagnose", "primaerfall", "patientenfall",
        "morphologie", "immunphanotypisierung", "zytogenetik",
        "molekulargenetik", "jahr"),
      names(blocks)
    )
    DT::datatable(blocks[, show_cols, drop = FALSE], rownames = FALSE,
                  options = list(pageLength = 25, scrollX = TRUE))
  })

  output$download_diagnostic_blocks <- shiny::downloadHandler(
    filename = function() paste0("OPS_1_941_Komplexe_Diagnostik_", Sys.Date(), ".csv"),
    content = function(file) {
      utils::write.csv(diagnostic_filtered(), file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    }
  )

  # ------------------------------------------------------------------
  # Oncoprint
  # ------------------------------------------------------------------
  output$oncoprint_filters <- shiny::renderUI({
    df <- data_filtered(); shiny::req(nrow(df) > 0)
    onco_all <- tryCatch(
      zhncommandR::zhn_parse_oncoprint(df, remove_negative = FALSE),
      error = function(e) NULL
    )
    shiny::validate(shiny::need(!is.null(onco_all) && nrow(onco_all) > 0,
                                .tr("Keine verwertbaren Einträge in 'Krankheitsspezifische hematol Resultate'.")))
    ent_choices <- sort(unique(stats::na.omit(onco_all$diagnose_label)))
    alt_choices <- onco_all |>
      dplyr::filter(.data$oncoprint_mutation) |>
      dplyr::count(.data$alteration, sort = TRUE) |>
      dplyr::pull(.data$alteration)
    shiny::tagList(
      shiny::selectizeInput("onco_entity_filter", .tr("Entität/Diagnose"),
                            choices = ent_choices, selected = ent_choices, multiple = TRUE),
      shiny::numericInput("onco_top_n", .tr("Top-Alterationen anzeigen"),
                          value = 25, min = 5, max = 100, step = 5),
      shiny::checkboxInput("onco_remove_negative", .tr("Negative/NA-Befunde ausblenden"), TRUE),
      shiny::selectizeInput("onco_alt_filter", .tr("Optional: bestimmte Mutationen"),
                            choices = alt_choices, selected = NULL, multiple = TRUE,
                            options = list(placeholder = .tr("leer = automatisch Top-Alterationen"))),
      shiny::checkboxInput("onco_show_patient_names", .tr("Patientennamen in X-Achse anzeigen"), FALSE)
    )
  })

  oncoprint_all_filtered <- shiny::reactive({
    df <- data_filtered()
    onco <- tryCatch(
      zhncommandR::zhn_parse_oncoprint(df, remove_negative = isTRUE(input$onco_remove_negative)),
      error = function(e) zhncommandR:::.empty_alteration_table(include_oncoprint_flag = TRUE)
    )
    if (!is.null(input$onco_entity_filter) && length(input$onco_entity_filter) > 0) {
      onco <- dplyr::filter(onco, .data$diagnose_label %in% input$onco_entity_filter)
    }
    onco
  })

  oncoprint_long <- shiny::reactive({
    onco <- oncoprint_all_filtered() |> dplyr::filter(.data$oncoprint_mutation)
    if (!is.null(input$onco_alt_filter) && length(input$onco_alt_filter) > 0) {
      onco <- dplyr::filter(onco, .data$alteration %in% input$onco_alt_filter)
    } else {
      top_n <- ifelse(is.null(input$onco_top_n), 25, input$onco_top_n)
      top_alts <- onco |>
        dplyr::count(.data$alteration, sort = TRUE) |>
        dplyr::slice_head(n = top_n) |>
        dplyr::pull(.data$alteration)
      onco <- dplyr::filter(onco, .data$alteration %in% top_alts)
    }
    onco
  })

  oncoprint_structural <- shiny::reactive({
    oncoprint_all_filtered() |>
      dplyr::filter(!.data$oncoprint_mutation) |>
      dplyr::filter(!.data$alteration_class %in% c("negativ/kein Nachweis", "Nicht verwertbar/NA"))
  })

  output$n_onco_patients <- shiny::renderText(format(dplyr::n_distinct(oncoprint_long()$patient_label), big.mark = "."))
  output$n_onco_alterations <- shiny::renderText(format(dplyr::n_distinct(oncoprint_long()$alteration), big.mark = "."))
  output$n_onco_entities <- shiny::renderText(format(dplyr::n_distinct(oncoprint_long()$diagnose_label), big.mark = "."))

  oncoprint_plot_obj <- shiny::eventReactive(input$run_oncoprint, {
    onco <- oncoprint_long()
    shiny::validate(shiny::need(nrow(onco) > 0, .tr("Keine echten Mutationen/Varianten im aktuellen Filter.")))
    patient_order <- onco |>
      dplyr::distinct(.data$patient_label, .data$diagnose_label) |>
      dplyr::arrange(.data$diagnose_label, .data$patient_label) |>
      dplyr::mutate(patient_plot = if (isTRUE(input$onco_show_patient_names)) {
        .data$patient_label
      } else {
        paste0("Fall ", dplyr::row_number())
      })
    alt_order <- onco |> dplyr::count(.data$alteration, sort = TRUE) |> dplyr::pull(.data$alteration)
    plot_df <- onco |>
      dplyr::left_join(patient_order, by = c("patient_label", "diagnose_label")) |>
      dplyr::mutate(
        patient_plot   = factor(.data$patient_plot, levels = patient_order$patient_plot),
        alteration     = factor(.data$alteration, levels = rev(alt_order)),
        diagnose_label = factor(.data$diagnose_label, levels = unique(patient_order$diagnose_label))
      )
    ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$patient_plot, y = .data$alteration,
                                          fill = .data$alteration_class)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.25) +
      ggplot2::facet_grid(. ~ diagnose_label, scales = "free_x", space = "free_x") +
      ggplot2::scale_fill_viridis_d(option = "mako", begin = 0.15, end = 0.85) +
      zhncommandR::zhn_theme(base_size = 12,
                             transparent = isTRUE(input$fig_transparent)) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5,
                                            size = if (isTRUE(input$onco_show_patient_names)) 7 else 5),
        panel.grid = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_text(face = "bold", size = 10),
        legend.position = "bottom"
      ) +
      ggplot2::labs(
        x = .tr("Patient/Fall"), y = .tr("Mutation/Variante"), fill = .tr("Typ")
      )
  }, ignoreNULL = FALSE)

  output$oncoprint_plot <- shiny::renderPlot(
    { p <- oncoprint_plot_obj(); shiny::req(p); p }, bg = "transparent"
  )
  .dl_handlers("dl_oncoprint", oncoprint_plot_obj, "oncoprint")

  output$oncoprint_summary_table <- DT::renderDT({
    onco <- oncoprint_long()
    shiny::validate(shiny::need(nrow(onco) > 0, .tr("Keine echten Mutationen/Varianten im aktuellen Filter.")))
    tab <- onco |>
      dplyr::group_by(.data$diagnose_label, .data$alteration) |>
      dplyr::summarise(
        Patienten_Faelle = dplyr::n_distinct(.data$patient_label),
        Alterationstyp = paste(sort(unique(.data$alteration_class)), collapse = ", "),
        .groups = "drop"
      ) |>
      dplyr::arrange(.data$diagnose_label, dplyr::desc(.data$Patienten_Faelle), .data$alteration)
    DT::datatable(tab, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })

  output$oncoprint_detail_table <- DT::renderDT({
    onco <- oncoprint_long()
    show_cols <- intersect(
      c("patient_label", "diagnose_label", "alteration", "alteration_class", "alteration_raw"),
      names(onco)
    )
    DT::datatable(onco[, show_cols, drop = FALSE], rownames = FALSE,
                  options = list(pageLength = 20, scrollX = TRUE))
  })

  output$oncoprint_structural_table <- DT::renderDT({
    structural <- oncoprint_structural()
    shiny::validate(shiny::need(nrow(structural) > 0, .tr("Keine strukturellen/zytogenetischen Befunde im aktuellen Filter.")))
    tab <- structural |>
      dplyr::group_by(.data$diagnose_label, .data$alteration_class, .data$alteration) |>
      dplyr::summarise(
        Patienten_Faelle = dplyr::n_distinct(.data$patient_label),
        Beispiele = paste(head(sort(unique(.data$alteration_raw)), 5), collapse = " | "),
        .groups = "drop"
      ) |>
      dplyr::arrange(.data$diagnose_label, .data$alteration_class,
                     dplyr::desc(.data$Patienten_Faelle), .data$alteration)
    DT::datatable(tab, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })

  output$download_oncoprint_data <- shiny::downloadHandler(
    filename = function() paste0("Oncoprint_Mutationsdaten_", Sys.Date(), ".csv"),
    content = function(file) {
      utils::write.csv(oncoprint_long(), file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    }
  )
  output$download_structural_data <- shiny::downloadHandler(
    filename = function() paste0("Strukturelle_Zytogenetische_Befunde_", Sys.Date(), ".csv"),
    content = function(file) {
      utils::write.csv(oncoprint_structural(), file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    }
  )
  # ------------------------------------------------------------------
  # Zytogenetik
  # ------------------------------------------------------------------
  output$cyto_filters <- shiny::renderUI({
    df <- data_filtered(); shiny::req(nrow(df) > 0)
    cyto_all <- tryCatch(
      zhncommandR::zhn_parse_cytogenetics(df, remove_negative = FALSE),
      error = function(e) NULL
    )
    shiny::validate(shiny::need(!is.null(cyto_all) && nrow(cyto_all) > 0,
                                .tr("Keine verwertbaren Einträge in 'Zytogenetik'.")))
    ent_choices <- sort(unique(stats::na.omit(cyto_all$diagnose_label)))
    cyto_choices <- cyto_all |>
      dplyr::filter(!.data$alteration_class %in% c("negativ/kein Nachweis", "Nicht verwertbar/NA")) |>
      dplyr::count(.data$alteration, sort = TRUE) |>
      dplyr::pull(.data$alteration)
    shiny::tagList(
      shiny::selectizeInput("cyto_entity_filter", .tr("Entität/Diagnose"),
                            choices = ent_choices, selected = ent_choices, multiple = TRUE),
      shiny::numericInput("cyto_top_n", .tr("Top-Befunde anzeigen"),
                          value = 25, min = 5, max = 100, step = 5),
      shiny::checkboxInput("cyto_remove_negative", .tr("Negative/NA-Befunde ausblenden"), TRUE),
      shiny::selectizeInput("cyto_alt_filter", .tr("Optional: bestimmte Zytogenetik-Befunde"),
                            choices = cyto_choices, selected = NULL, multiple = TRUE,
                            options = list(placeholder = .tr("leer = automatisch Top-Befunde")))
    )
  })

  cyto_all_filtered <- shiny::reactive({
    df <- data_filtered()
    cyto <- tryCatch(
      zhncommandR::zhn_parse_cytogenetics(df, remove_negative = isTRUE(input$cyto_remove_negative)),
      error = function(e) zhncommandR:::.empty_alteration_table(include_oncoprint_flag = FALSE,
                                                              raw_name = "zytogenetik_raw")
    )
    if (!is.null(input$cyto_entity_filter) && length(input$cyto_entity_filter) > 0) {
      cyto <- dplyr::filter(cyto, .data$diagnose_label %in% input$cyto_entity_filter)
    }
    if (!is.null(input$cyto_alt_filter) && length(input$cyto_alt_filter) > 0) {
      cyto <- dplyr::filter(cyto, .data$alteration %in% input$cyto_alt_filter)
    } else {
      top_n <- ifelse(is.null(input$cyto_top_n), 25, input$cyto_top_n)
      top_alts <- cyto |>
        dplyr::count(.data$alteration, sort = TRUE) |>
        dplyr::slice_head(n = top_n) |>
        dplyr::pull(.data$alteration)
      cyto <- dplyr::filter(cyto, .data$alteration %in% top_alts)
    }
    cyto
  })

  output$n_cyto_patients <- shiny::renderText(format(dplyr::n_distinct(cyto_all_filtered()$patient_label), big.mark = "."))
  output$n_cyto_alterations <- shiny::renderText(format(dplyr::n_distinct(cyto_all_filtered()$alteration), big.mark = "."))
  output$n_cyto_entities <- shiny::renderText(format(dplyr::n_distinct(cyto_all_filtered()$diagnose_label), big.mark = "."))

  cyto_plot_obj <- shiny::eventReactive(input$run_cyto, {
    cyto <- cyto_all_filtered()
    shiny::validate(shiny::need(nrow(cyto) > 0, .tr("Keine Zytogenetik-Befunde im aktuellen Filter.")))
    tab <- cyto |>
      dplyr::count(.data$alteration, sort = TRUE) |>
      dplyr::arrange(.data$n) |>
      dplyr::mutate(alteration = factor(.data$alteration, levels = .data$alteration))
    ggplot2::ggplot(tab, ggplot2::aes(x = .data$alteration, y = .data$n)) +
      ggplot2::geom_col(fill = zhncommandR::zhn_pal$accent) + ggplot2::coord_flip() +
      ggplot2::labs(
        x = .tr("Zytogenetik-Befund"), y = .tr("Anzahl Fälle/Patienten")
      ) +
      zhncommandR::zhn_theme(transparent = isTRUE(input$fig_transparent))
  }, ignoreNULL = FALSE)

  output$cyto_plot <- shiny::renderPlot(
    { p <- cyto_plot_obj(); shiny::req(p); p }, bg = "transparent"
  )
  .dl_handlers("dl_cyto", cyto_plot_obj, "zytogenetik_befunde")

  output$cyto_summary_table <- DT::renderDT({
    cyto <- cyto_all_filtered()
    shiny::validate(shiny::need(nrow(cyto) > 0, .tr("Keine Zytogenetik-Befunde im aktuellen Filter.")))
    tab <- cyto |>
      dplyr::group_by(.data$diagnose_label, .data$alteration_class, .data$alteration) |>
      dplyr::summarise(
        Patienten_Faelle = dplyr::n_distinct(.data$patient_label),
        Beispiele = paste(head(sort(unique(.data$zytogenetik_raw)), 5), collapse = " | "),
        .groups = "drop"
      ) |>
      dplyr::arrange(.data$diagnose_label, .data$alteration_class,
                     dplyr::desc(.data$Patienten_Faelle), .data$alteration)
    DT::datatable(tab, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })

  output$cyto_detail_table <- DT::renderDT({
    cyto <- cyto_all_filtered()
    show_cols <- intersect(
      c("patient_label", "diagnose_label", "alteration", "alteration_class", "zytogenetik_raw"),
      names(cyto)
    )
    DT::datatable(cyto[, show_cols, drop = FALSE], rownames = FALSE,
                  options = list(pageLength = 25, scrollX = TRUE))
  })

  output$download_cyto_data <- shiny::downloadHandler(
    filename = function() paste0("Zytogenetik_", Sys.Date(), ".csv"),
    content = function(file) {
      utils::write.csv(cyto_all_filtered(), file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    }
  )

  # ------------------------------------------------------------------
  # Boxplots
  # ------------------------------------------------------------------
  output$box_ui <- shiny::renderUI({
    df <- data_filtered(); shiny::req(nrow(df) > 0)
    num_cols <- names(df)[vapply(df, is.numeric, logical(1L))]
    grp_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1L))]
    shiny::tagList(
      shiny::selectInput("box_y", .tr("Numerische Variable Y"), choices = num_cols,
                         selected = if ("pfs" %in% num_cols) "pfs" else num_cols[1]),
      shiny::selectInput("box_x", .tr("Gruppe X"), choices = grp_cols,
                         selected = if ("kodierung" %in% grp_cols) "kodierung" else grp_cols[1]),
      shiny::checkboxInput("box_jitter", .tr("Jitter-Punkte anzeigen"), TRUE),
      shiny::checkboxInput("box_log", .tr("Y-Achse log10"), FALSE),
      shiny::textInput("box_title", .tr("Titel"), value = .tr("Boxplot"))
    )
  })

  box_plot_obj <- shiny::eventReactive(input$run_box, {
    df <- data_filtered()
    shiny::req(input$box_y, input$box_x)
    shiny::validate(shiny::need(nrow(df) > 1, .tr("Zu wenige Daten nach Filter.")))
    plot_df <- data.frame(
      x = as.factor(df[[input$box_x]]),
      y = suppressWarnings(as.numeric(df[[input$box_y]]))
    ) |> dplyr::filter(!is.na(.data$x), !is.na(.data$y))
    shiny::validate(shiny::need(nrow(plot_df) > 1, .tr("Keine verwertbaren Daten für Boxplot.")))
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_boxplot(outlier.shape = NA,
                            colour = zhncommandR::zhn_pal$accent_dark) +
      ggplot2::labs(title = input$box_title, x = input$box_x, y = input$box_y) +
      zhncommandR::zhn_theme(transparent = isTRUE(input$fig_transparent)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    if (isTRUE(input$box_jitter)) p <- p + ggplot2::geom_jitter(width = 0.15, alpha = 0.6)
    if (isTRUE(input$box_log)) p <- p + ggplot2::scale_y_log10()
    p
  }, ignoreNULL = FALSE)

  output$box_plot <- shiny::renderPlot(
    { p <- box_plot_obj(); shiny::req(p); p }, bg = "transparent"
  )
  .dl_handlers("dl_box", box_plot_obj, function() {
    zhncommandR:::.slugify(input$box_title, fallback = "boxplot")
  })

  # ------------------------------------------------------------------
  # Methods documentation tab
  # ------------------------------------------------------------------
  output$methods_doc <- shiny::renderUI({
    is_en <- isTRUE(input$lang == "en")
    .h <- function(de, en) if (is_en) en else de
    shiny::tagList(
      shiny::h3(.h(
        "Methoden: Pakete, Berechnungen und statistische Tests",
        "Methods: packages, calculations and statistical tests"
      )),
      shiny::p(.h(
        paste0(
          "Diese App ist Teil des R-Pakets ", shiny::tags$code("zhncommandR"),
          " (CTTIR-Suite). Alle Berechnungen sind reproduzierbar und ",
          "vollständig im Quellcode auf GitHub einsehbar."
        ),
        paste0(
          "This app is part of the R package ", shiny::tags$code("zhncommandR"),
          " (CTTIR suite). All calculations are reproducible and fully ",
          "auditable in the source on GitHub."
        )
      )),

      shiny::h4(.h("Verwendete R-Pakete", "R packages used")),
      shiny::tags$table(
        class = "table table-sm",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(.h("Paket", "Package")),
          shiny::tags$th(.h("Funktion in der App", "Role in the app"))
        )),
        shiny::tags$tbody(
          shiny::tags$tr(shiny::tags$td("shiny"), shiny::tags$td(.h(
            "Reaktives Framework, UI- und Server-Logik.",
            "Reactive framework, UI and server logic."
          ))),
          shiny::tags$tr(shiny::tags$td("bslib + bsicons"), shiny::tags$td(.h(
            "Bootstrap-5-Theme (Hugo-Coder-Palette), Karten, Value-Boxes.",
            "Bootstrap 5 theme (Hugo Coder palette), cards, value boxes."
          ))),
          shiny::tags$tr(shiny::tags$td("thematic + sass"), shiny::tags$td(.h(
            "Automatische ggplot-Themen, SCSS-Kompilierung.",
            "Automatic ggplot theming, SCSS compilation."
          ))),
          shiny::tags$tr(shiny::tags$td("shiny.i18n"), shiny::tags$td(.h(
            "DE/EN-Sprachumschaltung ohne Re-Render.",
            "DE/EN language toggle without re-rendering the UI."
          ))),
          shiny::tags$tr(shiny::tags$td("readxl"), shiny::tags$td(.h(
            "Einlesen der Tumor-Dokumentations-xlsx (drei kanonische Blätter).",
            "Reads the tumour-documentation .xlsx (three canonical sheets)."
          ))),
          shiny::tags$tr(shiny::tags$td("janitor"), shiny::tags$td(.h(
            "clean_names() vereinheitlicht Spaltennamen; Duplikate werden erkannt.",
            "clean_names() normalises column names; duplicates are detected."
          ))),
          shiny::tags$tr(shiny::tags$td("dplyr + tidyr"), shiny::tags$td(.h(
            "Filter, Gruppierung, Aggregation, Pivot.",
            "Filtering, grouping, aggregation, pivoting."
          ))),
          shiny::tags$tr(shiny::tags$td("stringr"), shiny::tags$td(.h(
            "Klassifikations-Regex für Alteration / Zytogenetik.",
            "Classification regex for alteration / cytogenetics."
          ))),
          shiny::tags$tr(shiny::tags$td("lubridate"), shiny::tags$td(.h(
            "Datums-Parsing, Jahres- und Monatsableitung.",
            "Date parsing, year and month derivation."
          ))),
          shiny::tags$tr(shiny::tags$td("survival"), shiny::tags$td(.h(
            "Surv() und survfit() für Kaplan-Meier-Schätzungen.",
            "Surv() and survfit() for Kaplan-Meier estimates."
          ))),
          shiny::tags$tr(shiny::tags$td("ggsurvfit"), shiny::tags$td(.h(
            "Publikationsreife KM-Kurven inkl. Risikotabelle, KI, Zensur-Marken.",
            "Publication-ready KM curves with risk table, CI and censor marks."
          ))),
          shiny::tags$tr(shiny::tags$td("ggplot2"), shiny::tags$td(.h(
            "Alle Plots (Balken, Linien, Oncoprint-Kacheln, Boxplots).",
            "All plots (bars, lines, oncoprint tiles, box plots)."
          ))),
          shiny::tags$tr(shiny::tags$td("scales"), shiny::tags$td(.h(
            "Prozent- und Tausenderformatierung.",
            "Percent and thousand-separator formatting."
          ))),
          shiny::tags$tr(shiny::tags$td("DT"), shiny::tags$td(.h(
            "Interaktive Tabellen mit Suche, Sortierung, Pagination.",
            "Interactive tables with search, sort, pagination."
          ))),
          shiny::tags$tr(shiny::tags$td("cli + rlang"), shiny::tags$td(.h(
            "Strukturierte Fehler- und Hinweismeldungen mit Call-Tracing.",
            "Structured errors and informational messages with call tracing."
          )))
        )
      ),

      shiny::h4(.h("Deskriptive Berechnungen", "Descriptive calculations")),
      shiny::tags$ul(
        shiny::tags$li(.h(
          paste0(shiny::tags$b("Anzahl Patienten"), ": ",
                 shiny::tags$code("nrow(data_filtered())"),
                 " nach globalem Filter."),
          paste0(shiny::tags$b("Patients total"), ": ",
                 shiny::tags$code("nrow(data_filtered())"),
                 " after the global filter.")
        )),
        shiny::tags$li(.h(
          "Indikator-Anteile: 'Positiv' / 'Gesamt' (oder Dokumentiert), mit scales::percent(0.1) gerundet auf 0.1 %.",
          "Indicator shares: 'positive' / 'total' (or documented), rounded to 0.1 % via scales::percent()."
        )),
        shiny::tags$li(.h(
          paste0(shiny::tags$b("Ja/Nein-Kodierung"), ": ",
                 shiny::tags$code(".as_yesno()"),
                 " erkennt explizite Tokens (ja/nein/1/0/true/false/wahr/falsch/x) und mappt unklare Werte auf NA."),
          paste0(shiny::tags$b("Yes/No coding"), ": ",
                 shiny::tags$code(".as_yesno()"),
                 " maps explicit tokens (ja/nein/1/0/true/false/wahr/falsch/x); ambiguous values become NA.")
        )),
        shiny::tags$li(.h(
          paste0(shiny::tags$b("Distinct-Patienten"), ": ",
                 shiny::tags$code(".n_distinct_nonempty()"),
                 " (NA und leere Strings werden vorher entfernt)."),
          paste0(shiny::tags$b("Distinct patients"), ": ",
                 shiny::tags$code(".n_distinct_nonempty()"),
                 " (NA and empty strings are dropped first).")
        ))
      ),

      shiny::h4(.h("OPS-8-544 Therapieblock-Zählung",
                   "OPS-8-544 therapy-block counting")),
      shiny::p(.h(
        paste0(
          "Numerische OPS-8-544-Werte > 0 werden als positive Blöcke ",
          "gewertet (Blocknummern 1, 2, 3 ... = jeweils ein Block). ",
          "Liegen keine numerischen Werte vor, wird ",
          shiny::tags$code(".as_event01()"),
          " auf Tokens angewendet. Wenn keine OPS-Spalte existiert, ",
          "zählt jede eindeutige Kombination aus ",
          "Patient/Datum/Zyklus/Protokoll als Block ",
          "(",
          shiny::tags$code("dplyr::distinct()"), ")."
        ),
        paste0(
          "Numeric OPS-8-544 values > 0 are counted as positive blocks ",
          "(numbers 1, 2, 3 ... each = one block). Without numeric values, ",
          shiny::tags$code(".as_event01()"),
          " falls back to token matching. With no OPS column at all, ",
          "every distinct Patient/Date/Cycle/Protocol combination is ",
          "treated as one block (",
          shiny::tags$code("dplyr::distinct()"), ")."
        )
      )),

      shiny::h4(.h("OPS-1-941 Komplexe Diagnostik",
                   "OPS-1-941 complex diagnostics")),
      shiny::p(.h(
        paste0(
          "Analoge Zähl-Logik wie OPS 8-544. Komponenten ",
          "(Morphologie, Immunphänotypisierung, Zytogenetik, ",
          "Molekulargenetik) werden anhand der Spaltennamen erkannt ",
          "(", shiny::tags$code("grepl"), "-Regex). Long-Format über ",
          shiny::tags$code("tidyr::pivot_longer()"), "."
        ),
        paste0(
          "Same counting logic as OPS 8-544. Components ",
          "(Morphology, Immunophenotyping, Cytogenetics, Molecular ",
          "genetics) are detected via column-name regex (",
          shiny::tags$code("grepl"), "). Long form via ",
          shiny::tags$code("tidyr::pivot_longer()"), "."
        )
      )),

      shiny::h4(.h("Kaplan-Meier-Schätzung", "Kaplan-Meier estimation")),
      shiny::p(.h(
        paste0(
          shiny::tags$b("Modell"),
          ": Nicht-parametrische Überlebenswahrscheinlichkeit ",
          "via ", shiny::tags$code("survival::survfit(Surv(time, event) ~ 1)"),
          " (oder ~ grp für Stratifizierung). Punktweise ",
          "Konfidenzintervalle (Default log-log). ",
          "Visualisierung mit ", shiny::tags$code("ggsurvfit"),
          " (zhn_plot_km): Risikotabelle, Zensur-Marken, Log-rank-p, ",
          "Cox-HR mit cox.zph-Prüfung, paarweise BH-Vergleiche."
        ),
        paste0(
          shiny::tags$b("Model"),
          ": non-parametric survival via ",
          shiny::tags$code("survival::survfit(Surv(time, event) ~ 1)"),
          " (or ~ grp for stratification). Point-wise confidence intervals ",
          "(default log-log). Visualised with ",
          shiny::tags$code("ggsurvfit"),
          " (zhn_plot_km): risk table, censor marks, log-rank p, Cox HR with ",
          "cox.zph check, and pairwise BH comparisons."
        )
      )),
      shiny::p(.h(
        paste0(
          shiny::tags$b("Event-Kodierung"), " (",
          shiny::tags$code(".as_event01()"), "): ",
          "1 = Ereignis, 0 = zensiert, NA verworfen. Date-/POSIXct-Werte: ",
          "vorhanden = 1, fehlend = 0. Strings werden auf einer expliziten ",
          "Tokenliste verglichen (ja, j, yes, y, 1, x, true, wahr, tod, ",
          "verstorben → 1; nein, n, no, 0, false, falsch, lebt, alive ",
          "→ 0; alles andere → NA). Ein bewusst restriktiver Regex ",
          "ohne Präfix-Match: „10 Monate“ wird NICHT als Event ",
          "klassifiziert (Bugfix gegenüber Legacy-v5)."
        ),
        paste0(
          shiny::tags$b("Event coding"), " (",
          shiny::tags$code(".as_event01()"), "): ",
          "1 = event, 0 = censored, NA dropped. Date / POSIXct: present = 1, ",
          "missing = 0. Strings are compared against an explicit token list ",
          "(ja, j, yes, y, 1, x, true, wahr, tod, verstorben → 1; ",
          "nein, n, no, 0, false, falsch, lebt, alive → 0; everything ",
          "else → NA). Deliberately strict; \"10 months\" is NOT ",
          "classified as an event (legacy v5 regression fixed)."
        )
      )),

      shiny::h4(.h("Oncoprint-Klassifikation", "Oncoprint classification")),
      shiny::p(.h(
        paste0(
          "Die Freitext-Mutationsspalte wird an Kommas und Zeilenumbrüchen ",
          "gesplittet (Semikolons innerhalb von Klammern, z.B. ",
          shiny::tags$code("t(11;14)"),
          ", bleiben intakt). Jeder Eintrag wird klassifiziert via ",
          shiny::tags$code("zhn_alteration_type()"),
          " in: „Mutation/Variante“, ",
          "„negativ/kein Nachweis“, ",
          "„Strukturell: Deletion/Loss“, ",
          "„Zugewinn/Amplifikation“, ",
          "„Translokation/Rearrangement/Bruch“, ",
          "„Komplexer Karyotyp“ oder ",
          "„Nicht verwertbar/NA“. ",
          "Nur „Mutation/Variante“ geht in den Oncoprint-Kachelplot; ",
          "strukturelle Befunde landen im separaten Zytogenetik-Tab."
        ),
        paste0(
          "The free-text mutation column is split on commas and newlines ",
          "(semicolons inside parentheses, e.g. ",
          shiny::tags$code("t(11;14)"),
          ", remain intact). Each entry is classified via ",
          shiny::tags$code("zhn_alteration_type()"),
          " into: \"Mutation/Variante\", \"negativ/kein Nachweis\", ",
          "\"Strukturell: Deletion/Loss\", \"Zugewinn/Amplifikation\", ",
          "\"Translokation/Rearrangement/Bruch\", \"Komplexer Karyotyp\", ",
          "or \"Nicht verwertbar/NA\". Only true mutations populate the ",
          "oncoprint tile plot; structural findings show in a separate ",
          "table and in the dedicated cytogenetics tab."
        )
      )),

      shiny::h4(.h("Boxplots", "Box plots")),
      shiny::p(.h(
        paste0(
          shiny::tags$code("ggplot2::geom_boxplot()"),
          " mit Tukey-Definitionen (Whiskers = 1.5 × IQR; ",
          "Median als Linie; Outlier ausgeblendet, optional ",
          shiny::tags$code("geom_jitter()"),
          " für Einzelwerte; optional log10-Y-Achse). ",
          shiny::tags$b("Hinweis"),
          ": kein statistischer Test im aktuellen Tab; ",
          "Gruppenvergleiche bitte außerhalb der App durchführen."
        ),
        paste0(
          shiny::tags$code("ggplot2::geom_boxplot()"),
          " using Tukey definitions (whiskers = 1.5 × IQR; median as a ",
          "line; outliers hidden, optional ",
          shiny::tags$code("geom_jitter()"),
          " for raw points; optional log10 Y axis). ",
          shiny::tags$b("Note"),
          ": no statistical test is computed in this tab; ",
          "do group comparisons outside the app."
        )
      )),

      shiny::h4(.h("Reproduzierbarkeit", "Reproducibility")),
      shiny::tags$ul(
        shiny::tags$li(.h(
          paste0("R-Paket: ", shiny::tags$code("zhncommandR"),
                 " mit 0/0/0 in R CMD check --as-cran und Testabdeckung ",
                 "via covr."),
          paste0("R package: ", shiny::tags$code("zhncommandR"),
                 " — 0/0/0 in R CMD check --as-cran and covr coverage.")
        )),
        shiny::tags$li(.h(
          paste0("Beispiel-Datensatz: 100 % synthetisch, gebundled unter ",
                 shiny::tags$code("inst/extdata/zhn_example.xlsx"),
                 " (",
                 shiny::tags$code("zhn_example_path()"), ")."),
          paste0("Example data: 100 % synthetic, bundled at ",
                 shiny::tags$code("inst/extdata/zhn_example.xlsx"),
                 " (",
                 shiny::tags$code("zhn_example_path()"), ").")
        )),
        shiny::tags$li(.h(
          paste0("Quelle und Issue-Tracker: ",
                 shiny::tags$a(
                   href = "https://github.com/CTTIR/zhncommandR",
                   "github.com/CTTIR/zhncommandR")),
          paste0("Source and issue tracker: ",
                 shiny::tags$a(
                   href = "https://github.com/CTTIR/zhncommandR",
                   "github.com/CTTIR/zhncommandR"))
        ))
      )
    )
  })
}

shiny::shinyApp(ui, server)
