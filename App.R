# =============================================================
# Shiny App: Hämatologisches Tumorzentrum – Auditor Live-Auswertung
# Erwartete Excel-Datei: Daten.xlsx, bevorzugt auf dem Desktop
# =============================================================

# Einmalig ausführen, falls Pakete fehlen:
# install.packages(c(
#   "shiny", "readxl", "dplyr", "tidyr", "ggplot2", "survival",
#   "survminer", "DT", "bslib", "scales", "janitor", "stringr", "lubridate"
# ))

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(survival)
library(survminer)
library(DT)
library(bslib)
library(scales)
library(janitor)
library(stringr)
library(lubridate)

# -------------------------------------------------------------
# 1) Pfad zur Excel-Datei
# -------------------------------------------------------------
# Standard: Desktop. Falls Datei dort nicht liegt, wird App-Verzeichnis geprüft.
# Bevorzugt wird die aktualisierte Datei Daten(3).xlsx, sonst Daten(2).xlsx/Daten.xlsx.
excel_candidates <- c(
  normalizePath("~/Desktop/Daten(3).xlsx", mustWork = FALSE),
  normalizePath("~/Desktop/Daten(2).xlsx", mustWork = FALSE),
  normalizePath("~/Desktop/Daten.xlsx", mustWork = FALSE),
  file.path(getwd(), "Daten(3).xlsx"),
  file.path(getwd(), "Daten(2).xlsx"),
  file.path(getwd(), "Daten.xlsx")
)
excel_path <- excel_candidates[file.exists(excel_candidates)][1]
if (is.na(excel_path)) excel_path <- excel_candidates[3]


# Separate CSV-Datei für Tumorboardbeschlüsse.
# Sie wird neben der Excel-Datei gespeichert, damit die Rohdaten-Excel nicht überschrieben wird.
tumorboard_csv_path <- file.path(dirname(excel_path), "Tumorboardbeschluesse.csv")
if (is.na(tumorboard_csv_path) || !nzchar(tumorboard_csv_path)) {
  tumorboard_csv_path <- file.path(getwd(), "Tumorboardbeschluesse.csv")
}


# Optionales zweites Tabellenblatt bzw. separate Datei mit OPS-8-544-Therapieblöcken
therapy_fallback_candidates <- c(
  normalizePath("~/Desktop/Therapien Haematologische Neoplasien.xlsx", mustWork = FALSE),
  normalizePath("~/Desktop/Therapien Hämatologische Neoplasien.xlsx", mustWork = FALSE),
  file.path(getwd(), "Therapien Haematologische Neoplasien.xlsx"),
  file.path(getwd(), "Therapien Hämatologische Neoplasien.xlsx")
)
therapy_fallback_path <- therapy_fallback_candidates[file.exists(therapy_fallback_candidates)][1]
if (is.na(therapy_fallback_path)) therapy_fallback_path <- therapy_fallback_candidates[1]

# -------------------------------------------------------------
# 2) Hilfsfunktionen
# -------------------------------------------------------------
find_col <- function(df, candidates) {
  hits <- candidates[candidates %in% names(df)]
  if (length(hits) == 0) return(NULL)
  hits[1]
}

as_yesno <- function(x) {
  # Liefert TRUE/FALSE/NA für typische Dokumentationsvarianten
  if (is.logical(x)) return(x)
  x_chr <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    x_chr %in% c("1", "ja", "yes", "y", "true", "wahr", "x") ~ TRUE,
    x_chr %in% c("0", "nein", "no", "n", "false", "falsch") ~ FALSE,
    is.na(x) | x_chr %in% c("", "na", "n/a", "unbekannt", "nicht bekannt") ~ NA,
    TRUE ~ NA
  )
}


n_distinct_nonempty <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr <- x_chr[!is.na(x_chr) & x_chr != ""]
  dplyr::n_distinct(x_chr)
}


read_tumorboard_data <- function(path) {
  if (!file.exists(path)) {
    return(data.frame(
      Patient = character(),
      Board_Datum = as.Date(character()),
      Tumorboardbeschluss = character(),
      Verantwortlich = character(),
      Erfasst_am = character(),
      stringsAsFactors = FALSE
    ))
  }
  df <- tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
  if (nrow(df) == 0) {
    return(data.frame(
      Patient = character(),
      Board_Datum = as.Date(character()),
      Tumorboardbeschluss = character(),
      Verantwortlich = character(),
      Erfasst_am = character(),
      stringsAsFactors = FALSE
    ))
  }
  if ("Board_Datum" %in% names(df)) df$Board_Datum <- suppressWarnings(as.Date(df$Board_Datum))
  df
}

write_tumorboard_data <- function(df, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8")
}

first_nonempty_col <- function(data, cols) {
  # Gibt pro Zeile den ersten nicht-leeren Wert aus mehreren möglichen Spalten zurück.
  if (length(cols) == 0 || nrow(data) == 0) return(rep(NA_character_, nrow(data)))
  out <- rep(NA_character_, nrow(data))
  for (col in cols) {
    if (!col %in% names(data)) next
    val <- trimws(as.character(data[[col]]))
    val[is.na(val) | val == ""] <- NA_character_
    idx <- is.na(out) & !is.na(val)
    out[idx] <- val[idx]
  }
  out
}


as_event01 <- function(x, mode = c("auto", "date_event", "death_event")) {
  # Robuste Event-Kodierung für Kaplan-Meier:
  # - Datum vorhanden = 1, Datum fehlt = 0 (z.B. Rezidivdatum)
  # - 1/0, Ja/Nein, TRUE/FALSE, X werden erkannt
  # - Texte wie "1 (muss angenommen werden)" werden als 1 gewertet
  mode <- match.arg(mode)

  if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(ifelse(is.na(x), 0, 1))
  }

  x_chr <- trimws(as.character(x))
  x_low <- tolower(x_chr)

  out <- dplyr::case_when(
    is.na(x) | x_low %in% c("", "na", "n/a", "?", "unbekannt", "nicht bekannt") ~ NA_real_,
    grepl("^1", x_low) | x_low %in% c("ja", "j", "yes", "y", "true", "wahr", "x", "tod", "verstorben") ~ 1,
    grepl("^0", x_low) | x_low %in% c("nein", "n", "no", "false", "falsch", "lebt", "alive") ~ 0,
    TRUE ~ NA_real_
  )

  # Falls readxl Datumswerte als Excel-Seriennummern oder Strings liefert: nicht automatisch als Event werten,
  # außer der Modus ist explizit date_event.
  if (mode == "date_event") {
    # Alles, was nicht leer/? ist und noch nicht klassifiziert wurde, zählt als vorhandenes Datum/Ereignis.
    out[is.na(out) & !(is.na(x) | x_low %in% c("", "na", "n/a", "?", "unbekannt", "nicht bekannt"))] <- 1
    out[is.na(x) | x_low %in% c("", "na", "n/a", "?", "unbekannt", "nicht bekannt")] <- 0
  }

  out
}

safe_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  suppressWarnings(as.Date(x))
}

add_year <- function(df) {
  # Behandlungsjahr bevorzugt aus Erstvorstellung, sonst Erstdiagnose, sonst Therapiebeginn
  date_col <- find_col(df, c("erstvorstellung", "erstdiagnose", "therapie_beginn", "tumorkonferenz_datum", "last_follow_up"))
  if (!is.null(date_col)) {
    df$behandlungsjahr <- lubridate::year(safe_date(df[[date_col]]))
  } else {
    df$behandlungsjahr <- NA_integer_
  }
  df
}

read_main_data <- function(path) {
  validate(need(file.exists(path), paste0("Excel-Datei nicht gefunden: ", path)))
  sheets <- readxl::excel_sheets(path)
  sheet_to_use <- if ("Faelle" %in% sheets) "Faelle" else if ("Tabelle1" %in% sheets) "Tabelle1" else sheets[1]
  df <- readxl::read_excel(path, sheet = sheet_to_use)
  df <- as.data.frame(df)
  df <- janitor::clean_names(df)
  df <- add_year(df)
  attr(df, "sheet_to_use") <- sheet_to_use
  df
}


read_therapy_data <- function(main_path, fallback_path) {
  # Bevorzugt: zweites Blatt in der Hauptdatei, z.B. "Therapie_OPS8544".
  # Fallback: separate Therapie-Excel-Datei im App-Ordner oder auf dem Desktop.
  if (file.exists(main_path)) {
    sheets <- readxl::excel_sheets(main_path)
    therapy_sheet <- sheets[grepl("therapie|ops|8544|8[-_ ]?544", sheets, ignore.case = TRUE)]
    therapy_sheet <- setdiff(therapy_sheet, c("Faelle", "Tabelle1"))
    if (length(therapy_sheet) > 0) {
      df <- readxl::read_excel(main_path, sheet = therapy_sheet[1])
      df <- as.data.frame(df)
      df <- janitor::clean_names(df)
      attr(df, "source_label") <- paste0("Quelle: ", basename(main_path), " / Blatt: ", therapy_sheet[1])
      return(df)
    }
  }

  if (!is.na(fallback_path) && file.exists(fallback_path)) {
    sheets <- readxl::excel_sheets(fallback_path)
    df <- readxl::read_excel(fallback_path, sheet = sheets[1])
    df <- as.data.frame(df)
    df <- janitor::clean_names(df)
    attr(df, "source_label") <- paste0("Quelle: ", basename(fallback_path), " / Blatt: ", sheets[1])
    return(df)
  }

  out <- data.frame()
  attr(out, "source_label") <- "Keine OPS-8-544-Tabelle gefunden"
  out
}

prepare_therapy_blocks <- function(df) {
  if (nrow(df) == 0) return(df)

  ops_col     <- find_col(df, c("ops8544", "ops_8544", "ops8_544", "ops_8_544", "ops_8_544_komplexe_chemotherapie"))
  prot_col    <- find_col(df, c("therapieprotokoll", "therapie_protokoll", "protokoll", "therapie", "schema", "regime"))
  diag_col    <- find_col(df, c("diagnose", "kodierung", "entitaet", "entitaet_onkozert"))
  patient_col <- find_col(df, c("patient", "patient_1", "patient_2", "name", "patient_name", "patient_id", "patienten_id", "kis_patienten_id", "id"))
  date_col    <- find_col(df, c("datum", "herstellungsdatum", "therapiedatum", "applikationsdatum", "beginn"))
  cycle_col   <- find_col(df, c("zyklusnr", "zyklus", "zyklus_nr", "block", "block_nr"))

  # Wenn eine OPS-Spalte vorhanden ist: nur positiv dokumentierte OPS-8-544-Blöcke zählen.
  # Wenn keine OPS-Spalte vorhanden ist: jede eindeutige Kombination aus Patient/Datum/Zyklus/Protokoll zählt als Block.
  if (!is.null(ops_col)) {
    # Wichtig: In der OPS8544-Spalte können nicht nur 1/0 stehen,
    # sondern auch Blocknummern wie 1, 2, 3 usw.
    # Daher zählt jeder numerische Wert > 0 als OPS-8-544-Block.
    if (is.numeric(df[[ops_col]])) {
      df$.ops_event <- ifelse(is.na(df[[ops_col]]), NA_real_, ifelse(df[[ops_col]] > 0, 1, 0))
    } else {
      ops_chr <- trimws(as.character(df[[ops_col]]))
      ops_num <- suppressWarnings(as.numeric(gsub(",", ".", ops_chr)))
      df$.ops_event <- ifelse(!is.na(ops_num), ifelse(ops_num > 0, 1, 0), as_event01(df[[ops_col]], mode = "auto"))
    }
    if (any(df$.ops_event == 1, na.rm = TRUE)) {
      blocks <- df %>% filter(.ops_event == 1)
    } else {
      keep <- unique(na.omit(c(patient_col, date_col, cycle_col, prot_col, diag_col)))
      blocks <- df[, keep, drop = FALSE] %>% distinct()
    }
  } else {
    keep <- unique(na.omit(c(patient_col, date_col, cycle_col, prot_col, diag_col)))
    if (length(keep) == 0) return(data.frame())
    blocks <- df[, keep, drop = FALSE] %>% distinct()
  }

  get_chr <- function(data, col, default = NA_character_) {
    if (is.null(col) || !(col %in% names(data))) rep(default, nrow(data)) else as.character(data[[col]])
  }

  blocks$therapieprotokoll <- get_chr(blocks, prot_col, "Nicht angegeben")
  blocks$diagnose <- get_chr(blocks, diag_col, "Nicht angegeben")

  # Patientenerkennung robust:
  # In der Therapie-Excel gibt es meist „Patient“, teils zusätzlich „Patient.1“,
  # nach janitor::clean_names() z.B. patient und patient_2.
  # Wir nutzen bevorzugt die Namensspalten, danach IDs als Fallback.
  patient_name_cols <- intersect(c("patient", "patient_1", "patient_2", "name", "patient_name"), names(blocks))
  patient_id_cols   <- intersect(c("patienten_id", "kis_patienten_id", "patient_id", "id"), names(blocks))
  blocks$patient <- first_nonempty_col(blocks, c(patient_name_cols, patient_id_cols))
  attr(blocks, "patient_cols_used") <- paste(c(patient_name_cols, patient_id_cols), collapse = ", ")

  blocks$datum <- get_chr(blocks, date_col, NA_character_)
  blocks$zyklus <- get_chr(blocks, cycle_col, NA_character_)

  blocks$therapieprotokoll[is.na(blocks$therapieprotokoll) | trimws(blocks$therapieprotokoll) == ""] <- "Nicht angegeben"
  blocks$diagnose[is.na(blocks$diagnose) | trimws(blocks$diagnose) == ""] <- "Nicht angegeben"

  # Jahr robust aus Datum oder Text extrahieren.
  d <- blocks$datum
  parsed_date <- suppressWarnings(lubridate::as_date(d))
  year_from_date <- suppressWarnings(lubridate::year(parsed_date))
  year_from_text <- suppressWarnings(as.integer(sub(".*(20[0-9]{2}).*", "\\1", d)))
  year_from_text[is.na(year_from_text) | year_from_text < 2000 | year_from_text > 2100] <- NA_integer_
  blocks$jahr <- ifelse(!is.na(year_from_date), year_from_date, year_from_text)

  month_from_date <- ifelse(!is.na(parsed_date), format(parsed_date, "%Y-%m"), NA_character_)
  month_from_text <- ifelse(!is.na(blocks$jahr), paste0(blocks$jahr, "-", sprintf("%02d", suppressWarnings(as.integer(substr(d, 4, 5))))), NA_character_)
  blocks$monat_sort <- ifelse(!is.na(month_from_date), month_from_date, month_from_text)

  blocks
}


# -------------------------------------------------------------
# OPS 1-941 Komplexe Diagnostik
# -------------------------------------------------------------
read_diagnostic_data <- function(main_path) {
  # Bevorzugt: eigenes Blatt in der Hauptdatei, z.B. "Komplexe Diagnostik".
  if (file.exists(main_path)) {
    sheets <- readxl::excel_sheets(main_path)
    diag_sheet <- sheets[grepl("diagnostik|diagnostic|1[-_ ]?941|1941", sheets, ignore.case = TRUE)]
    diag_sheet <- setdiff(diag_sheet, c("Faelle", "Tabelle1", "Basisdaten", "Komplexe Chemotherapie"))
    if (length(diag_sheet) > 0) {
      df <- readxl::read_excel(main_path, sheet = diag_sheet[1])
      df <- as.data.frame(df)
      df <- janitor::clean_names(df)
      attr(df, "source_label") <- paste0("Quelle: ", basename(main_path), " / Blatt: ", diag_sheet[1])
      return(df)
    }
  }

  out <- data.frame()
  attr(out, "source_label") <- "Keine OPS-1-941-/Komplexe-Diagnostik-Tabelle gefunden"
  out
}

prepare_diagnostic_blocks <- function(df) {
  if (nrow(df) == 0) return(df)

  patient_col <- find_col(df, c("patient", "name", "patient_name", "patient_id", "patienten_id", "kis_patienten_id", "id"))
  diag_col    <- find_col(df, c("diagnose", "kodierung", "entitaet", "entitaet_onkozert", "diagnose_kategorie"))
  ops_col     <- find_col(df, c("komplexe_diagnostik", "ops_1_941", "ops1941", "ops_1941", "ops1_941"))
  date_col    <- find_col(df, c("tumorkonferenz", "tumorkonferenz_datum", "erstvorstellung", "erstdiagnose", "datum"))
  primary_col <- find_col(df, c("primarfall", "primaerfall"))
  case_col    <- find_col(df, c("patientenfall"))

  # Nur positiv dokumentierte OPS-1-941-Fälle zählen. Wenn keine OPS-Spalte existiert,
  # wird jede Zeile des Diagnostik-Blattes als komplexe Diagnostik interpretiert.
  if (!is.null(ops_col)) {
    if (is.numeric(df[[ops_col]])) {
      df$.diag_event <- ifelse(is.na(df[[ops_col]]), NA_real_, ifelse(df[[ops_col]] > 0, 1, 0))
    } else {
      ops_chr <- trimws(as.character(df[[ops_col]]))
      ops_num <- suppressWarnings(as.numeric(gsub(",", ".", ops_chr)))
      df$.diag_event <- ifelse(!is.na(ops_num), ifelse(ops_num > 0, 1, 0), as_event01(df[[ops_col]], mode = "auto"))
    }
    blocks <- df %>% filter(.diag_event == 1)
  } else {
    blocks <- df
  }

  if (nrow(blocks) == 0) return(blocks)

  get_chr <- function(data, col, default = NA_character_) {
    if (is.null(col) || !(col %in% names(data))) rep(default, nrow(data)) else as.character(data[[col]])
  }

  blocks$patient <- get_chr(blocks, patient_col, NA_character_)
  blocks$diagnose <- get_chr(blocks, diag_col, "Nicht angegeben")
  blocks$diagnose[is.na(blocks$diagnose) | trimws(blocks$diagnose) == ""] <- "Nicht angegeben"
  blocks$datum <- get_chr(blocks, date_col, NA_character_)
  blocks$primaerfall <- if (!is.null(primary_col)) as_yesno(blocks[[primary_col]]) else NA
  blocks$patientenfall <- if (!is.null(case_col)) as_yesno(blocks[[case_col]]) else NA

  # Komponenten der komplexen Diagnostik automatisch erkennen.
  # Derzeit typischerweise: Morphologie, Immunphänotypisierung, Zytogenetik, Molekulargenetik.
  comp_candidates <- names(blocks)[
    grepl("morph|immun|zyto|cyto|molekular|molecular|genetik|diagnostik", names(blocks), ignore.case = TRUE)
  ]
  comp_candidates <- setdiff(comp_candidates, c(ops_col, ".diag_event"))
  attr(blocks, "component_cols") <- paste(comp_candidates, collapse = ", ")

  d <- blocks$datum
  parsed_date <- suppressWarnings(lubridate::as_date(d))
  year_from_date <- suppressWarnings(lubridate::year(parsed_date))
  year_from_text <- suppressWarnings(as.integer(sub(".*(20[0-9]{2}).*", "\\1", d)))
  year_from_text[is.na(year_from_text) | year_from_text < 2000 | year_from_text > 2100] <- NA_integer_
  blocks$jahr <- ifelse(!is.na(year_from_date), year_from_date, year_from_text)

  month_from_date <- ifelse(!is.na(parsed_date), format(parsed_date, "%Y-%m"), NA_character_)
  month_from_text <- ifelse(!is.na(blocks$jahr), paste0(blocks$jahr, "-", sprintf("%02d", suppressWarnings(as.integer(substr(d, 4, 5))))), NA_character_)
  blocks$monat_sort <- ifelse(!is.na(month_from_date), month_from_date, month_from_text)

  blocks
}

diagnostic_components_long <- function(blocks) {
  if (nrow(blocks) == 0) return(data.frame())
  comp_cols <- names(blocks)[
    grepl("morph|immun|zyto|cyto|molekular|molecular|genetik", names(blocks), ignore.case = TRUE)
  ]
  if (length(comp_cols) == 0) return(data.frame())

  blocks %>%
    select(any_of(c("patient", "diagnose", "jahr", "monat_sort", comp_cols))) %>%
    pivot_longer(cols = all_of(comp_cols), names_to = "diagnostik_bereich", values_to = "wert") %>%
    mutate(
      positiv = dplyr::case_when(
        is.numeric(wert) ~ !is.na(wert) & wert > 0,
        TRUE ~ as_yesno(wert) %in% TRUE
      ),
      diagnostik_bereich = dplyr::case_when(
        grepl("morph", diagnostik_bereich, ignore.case = TRUE) ~ "Morphologie",
        grepl("immun", diagnostik_bereich, ignore.case = TRUE) ~ "Immunphänotypisierung",
        grepl("zyto|cyto", diagnostik_bereich, ignore.case = TRUE) ~ "Zytogenetik",
        grepl("molekular|molecular|genetik", diagnostik_bereich, ignore.case = TRUE) ~ "Molekulargenetik",
        TRUE ~ diagnostik_bereich
      )
    ) %>%
    filter(positiv)
}


# -------------------------------------------------------------
# Oncoprint / Mutationsprofil aus Freitext-Spalte
# -------------------------------------------------------------
normalize_alteration_label <- function(x) {
  x <- trimws(as.character(x))
  x <- stringr::str_replace_all(x, "\\s+", " ")
  x <- stringr::str_replace_all(x, regex("Mutation$|Mut$", ignore_case = TRUE), "")
  x <- trimws(x)
  x
}

# Befunde, die NICHT als Mutation in den Oncoprint sollen.
# Diese werden im Tab separat tabellarisch ausgegeben.
alteration_type <- function(x) {
  xl <- tolower(as.character(x))
  dplyr::case_when(
    is.na(x) | trimws(xl) == "" | xl %in% c("na", "n.a.", "n/a", "nan", "null") ~ "Nicht verwertbar/NA",
    stringr::str_detect(xl, "negativ|kein\\b|keine\\b|ohne\\b|nicht nachweis|kein nachweis|wt\\b|wildtyp|wild type") ~ "negativ/kein Nachweis",
    stringr::str_detect(xl, "del\\b|deletion|verlust|loss|monosomie|minus|\\-") ~ "Strukturell/Zytogenetik: Deletion/Loss",
    stringr::str_detect(xl, "gain|zugewinn|zugewin|amplifikation|amplification|amp\\b|trisomie|plus|\\+") ~ "Strukturell/Zytogenetik: Zugewinn/Amplifikation",
    stringr::str_detect(xl, "translokation|translocation|translation|rearrangement|bruch|break|fusion|t\\(") ~ "Strukturell/Zytogenetik: Translokation/Rearrangement/Bruch",
    stringr::str_detect(xl, "karyotyp|komplex|complex") ~ "Strukturell/Zytogenetik: Komplexer Karyotyp",
    TRUE ~ "Mutation/Variante"
  )
}

is_mutation_for_oncoprint <- function(alteration_class) {
  alteration_class == "Mutation/Variante"
}

parse_oncoprint_data <- function(df, remove_negative = TRUE) {
  result_col <- find_col(df, c(
    "krankheitsspezifische_hematol_resultate",
    "krankheitsspezifische_haematol_resultate",
    "hematol_resultate", "haematol_resultate", "resultate", "mutation", "mutationen"
  ))
  diag_col <- find_col(df, c("diagnose", "diagnose_kategorie", "kodierung", "entitaet", "entitaet_onkozert"))
  patient_col <- find_col(df, c("name", "patient", "patient_id", "patienten_id", "id"))

  validate(need(!is.null(result_col), "Spalte 'Krankheitsspezifische hematol Resultate' wurde nicht gefunden."))
  validate(need(!is.null(diag_col), "Keine Diagnose-/Entitätsspalte gefunden."))

  tmp <- df %>%
    mutate(
      .row_id = dplyr::row_number(),
      patient_label = if (!is.null(patient_col)) as.character(.data[[patient_col]]) else paste0("Fall_", .row_id),
      diagnose_label = as.character(.data[[diag_col]]),
      alteration_raw = as.character(.data[[result_col]])
    ) %>%
    filter(!is.na(alteration_raw), trimws(alteration_raw) != "") %>%
    mutate(alteration_raw = stringr::str_replace_all(alteration_raw, "\\n|\\r|/", ",")) %>%
    tidyr::separate_rows(alteration_raw, sep = ",|;") %>%
    mutate(
      alteration_raw = trimws(alteration_raw),
      alteration = normalize_alteration_label(alteration_raw),
      alteration_class = alteration_type(alteration_raw),
      oncoprint_mutation = is_mutation_for_oncoprint(alteration_class)
    ) %>%
    filter(
      !is.na(alteration), alteration != "",
      !tolower(alteration) %in% c("na", "n.a.", "n/a", "nan", "null"),
      alteration_class != "Nicht verwertbar/NA"
    )

  if (isTRUE(remove_negative)) {
    tmp <- tmp %>% filter(alteration_class != "negativ/kein Nachweis")
  }

  tmp %>% distinct(patient_label, diagnose_label, alteration, alteration_class, oncoprint_mutation, .keep_all = TRUE)
}



# -------------------------------------------------------------
# Zytogenetik aus separater Spalte
# -------------------------------------------------------------
parse_cytogenetics_data <- function(df, remove_negative = TRUE) {
  cyto_col <- find_col(df, c(
    "zytogenetik", "cytogenetik", "cytogenetics", "karyotyp", "fish", "chromosomenanalyse"
  ))
  diag_col <- find_col(df, c("diagnose", "diagnose_kategorie", "kodierung", "entitaet", "entitaet_onkozert"))
  patient_col <- find_col(df, c("name", "patient", "patient_id", "patienten_id", "id"))

  validate(need(!is.null(cyto_col), "Spalte 'Zytogenetik' wurde nicht gefunden."))
  validate(need(!is.null(diag_col), "Keine Diagnose-/Entitätsspalte gefunden."))

  tmp <- df %>%
    mutate(
      .row_id = dplyr::row_number(),
      patient_label = if (!is.null(patient_col)) as.character(.data[[patient_col]]) else paste0("Fall_", .row_id),
      diagnose_label = as.character(.data[[diag_col]]),
      zytogenetik_raw = as.character(.data[[cyto_col]])
    ) %>%
    filter(!is.na(zytogenetik_raw), trimws(zytogenetik_raw) != "") %>%
    mutate(zytogenetik_raw = stringr::str_replace_all(zytogenetik_raw, "\\n|\\r|/", ",")) %>%
    tidyr::separate_rows(zytogenetik_raw, sep = ",|;") %>%
    mutate(
      zytogenetik_raw = trimws(zytogenetik_raw),
      alteration = normalize_alteration_label(zytogenetik_raw),
      alteration_class = alteration_type(zytogenetik_raw)
    ) %>%
    filter(
      !is.na(alteration), alteration != "",
      !tolower(alteration) %in% c("na", "n.a.", "n/a", "nan", "null"),
      alteration_class != "Nicht verwertbar/NA"
    )

  if (isTRUE(remove_negative)) {
    tmp <- tmp %>% filter(alteration_class != "negativ/kein Nachweis")
  }

  tmp %>% distinct(patient_label, diagnose_label, alteration, alteration_class, zytogenetik_raw, .keep_all = TRUE)
}

# -------------------------------------------------------------
# 3) UI
# -------------------------------------------------------------
# Resolve branding from this standalone application's source directory.
.brand_source <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(.brand_source)) {
  .brand_arg <- grep("^--file=", commandArgs(), value = TRUE)
  if (length(.brand_arg)) .brand_source <- sub("^--file=", "", .brand_arg[[1]])
}
.brand_root <- if (!is.null(.brand_source)) {
  dirname(normalizePath(.brand_source))
} else getwd()
.brand_env <- new.env(parent = globalenv())
.brand_env$system.file <- function(..., package) {
  file.path(.brand_root, "inst", ...)
}
sys.source(file.path(.brand_root, "R", "cttir-branding.R"), envir = .brand_env)

ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Hämatologisches Tumorzentrum – Auditor-Auswertung",
  layout_sidebar(
    sidebar = sidebar(
      width = 390,
      h4(.brand_env$.cttir_brand_logo(84), "Auditor-App"),
      verbatimTextOutput("path_info"),
      actionButton("reload", "Excel neu einlesen", class = "btn-primary"),
      br(), br(),
      helpText("Die App liest standardmäßig ~/Desktop/Daten.xlsx ein. Alternativ kann Daten.xlsx im App-Ordner liegen."),
      hr(),
      h5("Globale Filter"),
      uiOutput("global_filters"),
      hr(),
      h5("Freitextsuche"),
      textInput("search_text", "Suche in Name, Diagnose, Kodierung, Therapie", value = ""),
      hr(),
      downloadButton("download_filtered", "Gefilterte Patiententabelle CSV")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Auditor-Dashboard",
          br(),
          layout_column_wrap(
            width = 1/4,
            value_box("Patienten gesamt", textOutput("n_total"), showcase = bsicons::bs_icon("people")),
            value_box("Primärfälle", textOutput("n_primaer"), showcase = bsicons::bs_icon("clipboard2-pulse")),
            value_box("Patientenfälle", textOutput("n_patientenfall"), showcase = bsicons::bs_icon("hospital")),
            value_box("Psychoonkologie", textOutput("n_psycho"), showcase = bsicons::bs_icon("heart-pulse"))
          ),
          br(),
          fluidRow(
            column(6, card(card_header("Fälle nach Diagnose/Kodierung"), plotOutput("plot_diagnosis", height = 430))),
            column(6, card(card_header("Jährliche Fallzahlen"), plotOutput("plot_year", height = 430)))
          ),
          br(),
          fluidRow(
            column(6, card(card_header("Qualitäts-/Versorgungsindikatoren"), DTOutput("indicator_table"))),
            column(6, card(card_header("Schnellfragen für Auditoren"), uiOutput("quick_questions")))
          )
        ),
        tabPanel(
          "Einfache Abfragen",
          br(),
          fluidRow(
            column(
              4,
              card(
                card_header("Auditorfrage auswählen"),
                selectInput(
                  "simple_question",
                  "Vordefinierte Frage",
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
                uiOutput("simple_query_ui"),
                checkboxInput("simple_only_unique", "Nur eindeutige Namen zählen", FALSE),
                actionButton("run_simple_query", "Abfrage ausführen", class = "btn-primary"),
                br(), br(),
                downloadButton("download_simple_query", "Trefferliste CSV")
              )
            ),
            column(
              8,
              value_box("Ergebnis", textOutput("simple_query_result"), showcase = bsicons::bs_icon("search")),
              br(),
              card(card_header("Zusammenfassung"), DTOutput("simple_query_summary")),
              br(),
              card(card_header("Trefferliste"), DTOutput("simple_query_table"))
            )
          )
        ),
        tabPanel(
          "Patientenliste",
          br(),
          helpText("Eine Zeile auswählen: Der Patient wird automatisch in den Tab 'Tumorboardbeschlüsse' übernommen."),
          DTOutput("table")
        ),
        tabPanel(
          "Tumorboardbeschlüsse",
          br(),
          fluidRow(
            column(
              4,
              card(
                card_header("Beschluss für Patienten erfassen"),
                verbatimTextOutput("tb_storage_info"),
                uiOutput("tb_patient_ui"),
                dateInput("tb_date", "Datum Tumorboard", value = Sys.Date(), format = "dd.mm.yyyy", language = "de"),
                textAreaInput("tb_decision", "Tumorboardbeschluss / Empfehlung", value = "", rows = 7, placeholder = "z.B. Vorstellung Referenzpathologie, Therapieempfehlung, Studienprüfung, Re-Staging, supportive Maßnahmen ..."),
                textInput("tb_responsible", "Verantwortlich / Eintrag durch", value = ""),
                actionButton("save_tb", "Beschluss speichern", class = "btn-primary"),
                br(), br(),
                downloadButton("download_tb", "Tumorboardbeschlüsse CSV")
              )
            ),
            column(
              8,
              layout_column_wrap(
                width = 1/3,
                value_box("Beschlüsse gesamt", textOutput("tb_n_total")),
                value_box("Beschlüsse Patient", textOutput("tb_n_patient")),
                value_box("Patienten mit Beschluss", textOutput("tb_n_patients"))
              ),
              br(),
              card(card_header("Beschlüsse des ausgewählten Patienten"), DTOutput("tb_patient_table")),
              br(),
              card(card_header("Alle dokumentierten Tumorboardbeschlüsse"), DTOutput("tb_all_table"))
            )
          )
        ),
        tabPanel(
          "Kaplan–Meier",
          br(),
          fluidRow(
            column(
              4,
              card(
                card_header("KM-Einstellungen"),
                uiOutput("km_ui"),
                actionButton("run_km", "KM aktualisieren", class = "btn-primary"),
                br(), br(),
                downloadButton("download_km_plot", "KM Plot PNG")
              )
            ),
            column(
              8,
              card(card_header("Kaplan–Meier Plot"), plotOutput("km_plot", height = 560))
            )
          )
        ),
        tabPanel(
          "OPS 8-544 Therapieblöcke",
          br(),
          fluidRow(
            column(
              4,
              card(
                card_header("Filter Therapieblöcke"),
                verbatimTextOutput("therapy_source_info"),
                uiOutput("therapy_filters"),
                br(),
                downloadButton("download_therapy_blocks", "Therapieblöcke CSV")
              )
            ),
            column(
              8,
              layout_column_wrap(
                width = 1/3,
                value_box("OPS-8-544-Blöcke", textOutput("n_therapy_blocks")),
                value_box("Patienten", textOutput("n_therapy_patients")),
                value_box("Therapieprotokolle", textOutput("n_therapy_protocols"))
              )
            )
          ),
          br(),
          fluidRow(
            column(6, card(card_header("Wie viele Blöcke von welcher Therapie?"), DTOutput("therapy_protocol_table"))),
            column(6, card(card_header("Blöcke nach Diagnose"), DTOutput("therapy_diagnosis_table")))
          ),
          br(),
          fluidRow(
            column(6, card(card_header("Blöcke je Therapieprotokoll"), plotOutput("therapy_protocol_plot", height = 520))),
            column(6, card(card_header("Monatliche OPS-8-544-Blöcke"), plotOutput("therapy_month_plot", height = 520)))
          ),
          br(),
          card(card_header("Detailtabelle der gezählten Blöcke"), DTOutput("therapy_block_details"))
        ),

        tabPanel(
          "OPS 1-941 Diagnostik",
          br(),
          fluidRow(
            column(
              4,
              card(
                card_header("Filter komplexe Diagnostik"),
                verbatimTextOutput("diagnostic_source_info"),
                uiOutput("diagnostic_filters"),
                br(),
                downloadButton("download_diagnostic_blocks", "Komplexe Diagnostik CSV")
              )
            ),
            column(
              8,
              layout_column_wrap(
                width = 1/3,
                value_box("OPS-1-941-Fälle", textOutput("n_diagnostic_blocks")),
                value_box("Patienten", textOutput("n_diagnostic_patients")),
                value_box("Diagnosen", textOutput("n_diagnostic_diagnoses"))
              )
            )
          ),
          br(),
          fluidRow(
            column(6, card(card_header("Komplexe Diagnostik nach Bereich"), DTOutput("diagnostic_component_table"))),
            column(6, card(card_header("Komplexe Diagnostik nach Diagnose"), DTOutput("diagnostic_diagnosis_table")))
          ),
          br(),
          fluidRow(
            column(6, card(card_header("Diagnostikbereiche"), plotOutput("diagnostic_component_plot", height = 520))),
            column(6, card(card_header("Monatliche OPS-1-941-Fälle"), plotOutput("diagnostic_month_plot", height = 520)))
          ),
          br(),
          card(card_header("Detailtabelle der komplexen Diagnostiken"), DTOutput("diagnostic_block_details"))
        ),

        tabPanel(
          "Oncoprint Mutationen",
          br(),
          fluidRow(
            column(
              4,
              card(
                card_header("Oncoprint-Filter"),
                helpText("Quelle: Spalte 'Krankheitsspezifische hematol Resultate'. NA/negative Befunde werden nicht geplottet. Deletionen, Zugewinne, Translokationen/Rearrangements/Brüche, Loss und komplexer Karyotyp werden nur tabellarisch aufgeführt."),
                uiOutput("oncoprint_filters"),
                actionButton("run_oncoprint", "Oncoprint aktualisieren", class = "btn-primary"),
                br(), br(),
                downloadButton("download_oncoprint_data", "Mutationsdaten CSV"),
                downloadButton("download_structural_data", "Struktur-/Zytogenetik CSV"),
                downloadButton("download_oncoprint_plot", "Oncoprint PNG")
              )
            ),
            column(
              8,
              layout_column_wrap(
                width = 1/3,
                value_box("Fälle mit Alterationen", textOutput("n_onco_patients")),
                value_box("Alterationen", textOutput("n_onco_alterations")),
                value_box("Entitäten", textOutput("n_onco_entities"))
              )
            )
          ),
          br(),
          card(card_header("Oncoprint – nur echte Mutationen/Varianten"), plotOutput("oncoprint_plot", height = 720)),
          br(),
          fluidRow(
            column(6, card(card_header("Top-Mutationen nach Entität"), DTOutput("oncoprint_summary_table"))),
            column(6, card(card_header("Detaildaten Mutationen"), DTOutput("oncoprint_detail_table")))
          ),
          br(),
          card(card_header("Strukturelle Befunde aus Mutationsspalte – nur tabellarisch"), DTOutput("oncoprint_structural_table"))
        ),

        tabPanel(
          "Zytogenetik",
          br(),
          fluidRow(
            column(
              4,
              card(
                card_header("Zytogenetik-Filter"),
                helpText("Quelle: separate Spalte 'Zytogenetik'. NA und negative Befunde werden ausgeblendet. Die Darstellung ist bewusst tabellarisch/als Balkendiagramm, nicht im Oncoprint."),
                uiOutput("cyto_filters"),
                actionButton("run_cyto", "Zytogenetik aktualisieren", class = "btn-primary"),
                br(), br(),
                downloadButton("download_cyto_data", "Zytogenetik CSV")
              )
            ),
            column(
              8,
              layout_column_wrap(
                width = 1/3,
                value_box("Fälle mit Zytogenetik", textOutput("n_cyto_patients")),
                value_box("Zytogenetik-Befunde", textOutput("n_cyto_alterations")),
                value_box("Entitäten", textOutput("n_cyto_entities"))
              )
            )
          ),
          br(),
          fluidRow(
            column(6, card(card_header("Top-Zytogenetik gesamt"), plotOutput("cyto_plot", height = 520))),
            column(6, card(card_header("Zytogenetik nach Entität"), DTOutput("cyto_summary_table")))
          ),
          br(),
          card(card_header("Detaildaten Zytogenetik"), DTOutput("cyto_detail_table"))
        ),
        tabPanel(
          "Boxplots",
          br(),
          fluidRow(
            column(
              4,
              card(
                card_header("Boxplot-Einstellungen"),
                uiOutput("box_ui"),
                actionButton("run_box", "Boxplot aktualisieren", class = "btn-primary"),
                br(), br(),
                downloadButton("download_box_plot", "Boxplot PNG")
              )
            ),
            column(8, card(card_header("Boxplot"), plotOutput("box_plot", height = 540)))
          )
        )
      )
    )
  )
)

# -------------------------------------------------------------
# 4) Server
# -------------------------------------------------------------
server <- function(input, output, session) {

  output$path_info <- renderText({
    paste0("Excel-Datei:\n", excel_path,
           "\n\nExistiert: ", file.exists(excel_path))
  })

  data_raw <- eventReactive(input$reload, {
    read_main_data(excel_path)
  }, ignoreNULL = FALSE)


  therapy_raw <- eventReactive(input$reload, {
    read_therapy_data(excel_path, therapy_fallback_path)
  }, ignoreNULL = FALSE)

  diagnostic_raw <- eventReactive(input$reload, {
    read_diagnostic_data(excel_path)
  }, ignoreNULL = FALSE)

  output$global_filters <- renderUI({
    df <- data_raw()
    req(nrow(df) > 0)

    diagnosis_col <- find_col(df, c("diagnose", "kodierung"))
    year_choices <- sort(unique(na.omit(df$behandlungsjahr)))

    diag_choices <- if (!is.null(diagnosis_col)) sort(unique(na.omit(df[[diagnosis_col]]))) else character(0)

    tagList(
      selectizeInput("year_filter", "Behandlungsjahr", choices = year_choices,
                     selected = year_choices, multiple = TRUE),
      selectizeInput("diagnosis_filter", "Diagnose/Kodierung", choices = diag_choices,
                     selected = diag_choices, multiple = TRUE),
      checkboxInput("only_primaer", "Nur Primärfälle", FALSE),
      checkboxInput("only_patientenfall", "Nur Patientenfälle", FALSE),
      checkboxInput("only_inhouse_therapy", "Nur Therapie am Haus", FALSE)
    )
  })

  data_filtered <- reactive({
    df <- data_raw()
    diagnosis_col <- find_col(df, c("diagnose", "kodierung"))

    if (!is.null(input$year_filter) && length(input$year_filter) > 0) {
      df <- df %>% filter(behandlungsjahr %in% as.integer(input$year_filter))
    }

    if (!is.null(diagnosis_col) && !is.null(input$diagnosis_filter) && length(input$diagnosis_filter) > 0) {
      df <- df %>% filter(.data[[diagnosis_col]] %in% input$diagnosis_filter)
    }

    if (isTRUE(input$only_primaer) && "primaerfall" %in% names(df)) {
      df <- df %>% filter(as_yesno(.data$primaerfall) %in% TRUE)
    }

    if (isTRUE(input$only_patientenfall) && "patientenfall" %in% names(df)) {
      df <- df %>% filter(as_yesno(.data$patientenfall) %in% TRUE)
    }

    if (isTRUE(input$only_inhouse_therapy) && "therapie_inhouse" %in% names(df)) {
      df <- df %>% filter(as_yesno(.data$therapie_inhouse) %in% TRUE)
    }

    if (!is.null(input$search_text) && nzchar(input$search_text)) {
      search_cols <- intersect(c("name", "diagnose", "kodierung", "therapie", "info_weitere_betreuung"), names(df))
      pattern <- regex(input$search_text, ignore_case = TRUE)
      df <- df %>% filter(if_any(all_of(search_cols), ~ str_detect(as.character(.x), pattern)))
    }

    df
  })

  output$n_total <- renderText({ format(nrow(data_filtered()), big.mark = ".") })

  output$n_primaer <- renderText({
    df <- data_filtered()
    if (!"primaerfall" %in% names(df)) return("n/a")
    format(sum(as_yesno(df$primaerfall) %in% TRUE, na.rm = TRUE), big.mark = ".")
  })

  output$n_patientenfall <- renderText({
    df <- data_filtered()
    if (!"patientenfall" %in% names(df)) return("n/a")
    format(sum(as_yesno(df$patientenfall) %in% TRUE, na.rm = TRUE), big.mark = ".")
  })

  output$n_psycho <- renderText({
    df <- data_filtered()
    if (!"psychoonkologie" %in% names(df)) return("n/a")
    n <- sum(as_yesno(df$psychoonkologie) %in% TRUE, na.rm = TRUE)
    denom <- nrow(df)
    paste0(n, " / ", denom, " (", percent(n / max(denom, 1), accuracy = 0.1), ")")
  })

  output$plot_diagnosis <- renderPlot({
    df <- data_filtered()
    diagnosis_col <- find_col(df, c("diagnose", "kodierung"))
    validate(need(!is.null(diagnosis_col), "Keine Diagnose-/Kodierungsspalte gefunden."))

    df %>%
      count(.data[[diagnosis_col]], sort = TRUE) %>%
      slice_head(n = 20) %>%
      ggplot(aes(x = reorder(.data[[diagnosis_col]], n), y = n)) +
      geom_col() +
      coord_flip() +
      theme_minimal(base_size = 13) +
      labs(x = NULL, y = "Anzahl", title = "Top-Diagnosen/Kodierungen")
  })

  output$plot_year <- renderPlot({
    df <- data_filtered()
    validate(need("behandlungsjahr" %in% names(df), "Kein Behandlungsjahr ableitbar."))

    df %>%
      filter(!is.na(behandlungsjahr)) %>%
      count(behandlungsjahr) %>%
      ggplot(aes(x = factor(behandlungsjahr), y = n)) +
      geom_col() +
      theme_minimal(base_size = 13) +
      labs(x = "Jahr", y = "Anzahl", title = "Fallzahlen nach Jahr")
  })

  output$indicator_table <- renderDT({
    df <- data_filtered()
    indicators <- c(
      "tumorkonferenz", "fallbesprechung", "psychoonkologie", "sozialdienst",
      "komplexe_diagnostik_nach_ops_1_940", "histologie_inhouse",
      "histologie_referenzpathologie", "studie", "zahnarzt_mkg",
      "bisphosphonate_denosumab", "hiv_hepatitis"
    )
    indicators <- intersect(indicators, names(df))

    out <- lapply(indicators, function(v) {
      val <- df[[v]]
      yes <- sum(as_yesno(val) %in% TRUE, na.rm = TRUE)
      documented <- sum(!is.na(val) & trimws(as.character(val)) != "")
      data.frame(
        Indikator = v,
        Positiv = yes,
        Dokumentiert = documented,
        Gesamt = nrow(df),
        Anteil_positiv = ifelse(nrow(df) > 0, yes / nrow(df), NA_real_)
      )
    }) %>% bind_rows()

    datatable(out, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE)) %>%
      formatPercentage("Anteil_positiv", 1)
  })

  output$quick_questions <- renderUI({
    df <- data_filtered()
    diagnosis_col <- find_col(df, c("diagnose", "kodierung"))
    if (is.null(diagnosis_col)) return(helpText("Keine Diagnose-/Kodierungsspalte gefunden."))

    hl_2025 <- df %>%
      filter(behandlungsjahr == 2025,
             str_detect(tolower(as.character(.data[[diagnosis_col]])), "hodgkin|hl")) %>%
      nrow()

    mm <- df %>% filter(str_detect(tolower(as.character(.data[[diagnosis_col]])), "myelom|multiple myeloma|mm")) %>% nrow()
    psycho <- if ("psychoonkologie" %in% names(df)) sum(as_yesno(df$psychoonkologie) %in% TRUE, na.rm = TRUE) else NA_integer_
    tk <- if ("tumorkonferenz" %in% names(df)) sum(as_yesno(df$tumorkonferenz) %in% TRUE, na.rm = TRUE) else NA_integer_

    HTML(paste0(
      "<ul>",
      "<li><b>Hodgkin-Lymphome 2025:</b> ", hl_2025, " Fälle im aktuellen Filter.</li>",
      "<li><b>Patienten mit psychoonkologischem Screening:</b> ", psycho, " Fälle.</li>",
      "<li><b>Patienten/Fälle mit Tumorkonferenz:</b> ", tk, " Fälle.</li>",
      "<li><b>Multiple-Myelom-Fälle:</b> ", mm, " Fälle. Für PFS-Kurve Diagnosefilter auf Myelom setzen und im KM-Tab PFS auswählen.</li>",
      "</ul>"
    ))
  })



  # -----------------------------------------------------------
  # Einfache Abfragen für Auditorfragen
  # -----------------------------------------------------------
  output$simple_query_ui <- renderUI({
    df <- data_filtered()
    req(nrow(df) > 0)

    diagnosis_col <- find_col(df, c("diagnose", "kodierung"))

    if (identical(input$simple_question, "diagnose_select")) {
      validate(need(!is.null(diagnosis_col), "Keine Spalte 'diagnose' oder 'kodierung' gefunden."))
      diag_vals <- sort(unique(na.omit(as.character(df[[diagnosis_col]]))))
      tagList(
        helpText("Hier können gezielt dokumentierte Diagnosen aus der Spalte 'Diagnose' ausgewählt werden. Mehrfachauswahl ist möglich."),
        selectizeInput(
          "diagnosis_query_values",
          "Diagnose(n)",
          choices = diag_vals,
          selected = character(0),
          multiple = TRUE,
          options = list(placeholder = "z.B. Multiples Myelom auswählen")
        ),
        checkboxInput("diagnosis_query_contains", "Als Textsuche verwenden statt exakter Auswahl", FALSE)
      )
    } else if (identical(input$simple_question, "custom")) {
      text_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x) || is.logical(x) || is.numeric(x))]
      tagList(
        selectInput("custom_col", "Spalte", choices = text_cols, selected = if ("diagnose" %in% text_cols) "diagnose" else text_cols[1]),
        radioButtons(
          "custom_mode", "Abfragemodus",
          choices = c(
            "Ja/positiv zählen" = "yesno",
            "Exakter Wert" = "exact",
            "Text enthält" = "contains",
            "Nicht leer/dokumentiert" = "documented"
          ),
          selected = "yesno"
        ),
        conditionalPanel(
          condition = "input.custom_mode == 'exact' || input.custom_mode == 'contains'",
          textInput("custom_value", "Suchwert/Text", value = "")
        )
      )
    } else {
      helpText("Die Abfrage wird auf die aktuell global gefilterte Patiententabelle angewendet. Beispiel: Jahr 2025 im linken Filter auswählen, dann hier Multiples Myelom zählen.")
    }
  })

  simple_query_data <- eventReactive(input$run_simple_query, {
    df <- data_filtered()
    diagnosis_col <- find_col(df, c("diagnose", "kodierung"))
    q <- input$simple_question

    validate(need(nrow(df) > 0, "Keine Daten nach globalem Filter."))

    result <- df
    label <- ""

    if (q == "psycho") {
      validate(need("psychoonkologie" %in% names(df), "Spalte 'psychoonkologie' nicht gefunden."))
      result <- df %>% filter(as_yesno(.data$psychoonkologie) %in% TRUE)
      label <- "Patienten/Fälle mit psychoonkologischem Screening"
    } else if (q == "hivhep") {
      hivhep_cols <- intersect(c("hiv_hepatitis", "hiv", "hep_b", "hep_c", "hepb", "hepc"), names(df))
      validate(need(length(hivhep_cols) > 0, "Keine HIV/Hepatitis-Spalten gefunden."))
      result <- df %>% filter(if_any(all_of(hivhep_cols), ~ as_yesno(.x) %in% TRUE))
      label <- "Patienten/Fälle mit dokumentiert positivem HIV/Hepatitis-Screening"
    } else if (q == "diagnose_select") {
      validate(need(!is.null(diagnosis_col), "Keine Diagnose-/Kodierungsspalte gefunden."))
      validate(need(!is.null(input$diagnosis_query_values) && length(input$diagnosis_query_values) > 0, "Bitte mindestens eine Diagnose auswählen."))
      selected_diag <- input$diagnosis_query_values
      if (isTRUE(input$diagnosis_query_contains)) {
        pattern <- paste(stringr::str_escape(selected_diag), collapse = "|")
        result <- df %>% filter(str_detect(tolower(as.character(.data[[diagnosis_col]])), regex(tolower(pattern), ignore_case = TRUE)))
      } else {
        result <- df %>% filter(as.character(.data[[diagnosis_col]]) %in% selected_diag)
      }
      label <- paste0("Patienten/Fälle mit Diagnose: ", paste(selected_diag, collapse = ", "))
    } else if (q == "myelom") {
      validate(need(!is.null(diagnosis_col), "Keine Diagnose-/Kodierungsspalte gefunden."))
      result <- df %>% filter(str_detect(tolower(as.character(.data[[diagnosis_col]])), "myelom|multiple myeloma|multiples myelom|plasma"))
      label <- "Patienten/Fälle mit Multiplem Myelom"
    } else if (q == "hodgkin") {
      validate(need(!is.null(diagnosis_col), "Keine Diagnose-/Kodierungsspalte gefunden."))
      result <- df %>% filter(str_detect(tolower(as.character(.data[[diagnosis_col]])), "hodgkin|hl"))
      label <- "Patienten/Fälle mit Hodgkin-Lymphom"
    } else if (q == "tumorkonferenz") {
      validate(need("tumorkonferenz" %in% names(df), "Spalte 'tumorkonferenz' nicht gefunden."))
      result <- df %>% filter(as_yesno(.data$tumorkonferenz) %in% TRUE)
      label <- "Patienten/Fälle mit Tumorkonferenz"
    } else if (q == "sozialdienst") {
      validate(need("sozialdienst" %in% names(df), "Spalte 'sozialdienst' nicht gefunden."))
      result <- df %>% filter(as_yesno(.data$sozialdienst) %in% TRUE)
      label <- "Patienten/Fälle mit Sozialdienst"
    } else if (q == "primaerfall") {
      validate(need("primaerfall" %in% names(df), "Spalte 'primaerfall' nicht gefunden."))
      result <- df %>% filter(as_yesno(.data$primaerfall) %in% TRUE)
      label <- "Primärfälle"
    } else if (q == "patientenfall") {
      validate(need("patientenfall" %in% names(df), "Spalte 'patientenfall' nicht gefunden."))
      result <- df %>% filter(as_yesno(.data$patientenfall) %in% TRUE)
      label <- "Patientenfälle"
    } else if (q == "custom") {
      validate(need(!is.null(input$custom_col) && input$custom_col %in% names(df), "Bitte eine gültige Spalte auswählen."))
      col <- input$custom_col
      mode <- input$custom_mode
      value <- input$custom_value

      if (mode == "yesno") {
        result <- df %>% filter(as_yesno(.data[[col]]) %in% TRUE)
        label <- paste0("Eigene Abfrage: ", col, " = Ja/positiv")
      } else if (mode == "exact") {
        validate(need(nzchar(value), "Bitte einen Suchwert eingeben."))
        result <- df %>% filter(tolower(trimws(as.character(.data[[col]]))) == tolower(trimws(value)))
        label <- paste0("Eigene Abfrage: ", col, " = ", value)
      } else if (mode == "contains") {
        validate(need(nzchar(value), "Bitte einen Suchtext eingeben."))
        result <- df %>% filter(str_detect(tolower(as.character(.data[[col]])), fixed(tolower(value))))
        label <- paste0("Eigene Abfrage: ", col, " enthält '", value, "'")
      } else if (mode == "documented") {
        result <- df %>% filter(!is.na(.data[[col]]) & trimws(as.character(.data[[col]])) != "")
        label <- paste0("Eigene Abfrage: ", col, " ist dokumentiert/nicht leer")
      }
    }

    if (isTRUE(input$simple_only_unique) && "name" %in% names(result)) {
      result <- result %>% distinct(name, .keep_all = TRUE)
    }

    attr(result, "query_label") <- label
    attr(result, "denominator") <- if (isTRUE(input$simple_only_unique) && "name" %in% names(df)) n_distinct(df$name) else nrow(df)
    result
  }, ignoreNULL = FALSE)

  output$simple_query_result <- renderText({
    res <- simple_query_data()
    denom <- attr(res, "denominator")
    paste0(nrow(res), " / ", denom, " (", percent(nrow(res) / max(denom, 1), accuracy = 0.1), ")")
  })

  output$simple_query_summary <- renderDT({
    res <- simple_query_data()
    label <- attr(res, "query_label")
    denom <- attr(res, "denominator")

    out <- data.frame(
      Abfrage = label,
      Treffer = nrow(res),
      Grundgesamtheit = denom,
      Anteil = ifelse(denom > 0, nrow(res) / denom, NA_real_)
    )

    datatable(out, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) %>%
      formatPercentage("Anteil", 1)
  })

  output$simple_query_table <- renderDT({
    res <- simple_query_data()
    cols_preferred <- intersect(c("name", "geschlecht", "geb_datum", "erstvorstellung", "erstdiagnose", "diagnose", "kodierung", "primaerfall", "patientenfall", "tumorkonferenz", "psychoonkologie", "hiv_hepatitis", "hiv", "hep_b", "hep_c", "pfs", "os"), names(res))
    if (length(cols_preferred) > 0) res <- res[, cols_preferred, drop = FALSE]
    datatable(res, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE, searchHighlight = TRUE))
  })

  output$download_simple_query <- downloadHandler(
    filename = function() paste0("einfache_Abfrage_", Sys.Date(), ".csv"),
    content = function(file) write.csv(simple_query_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )

    output$table <- renderDT({
    datatable(
      data_filtered(),
      rownames = FALSE,
      selection = "single",
      options = list(pageLength = 25, scrollX = TRUE, searchHighlight = TRUE)
    )
  })

  output$download_filtered <- downloadHandler(
    filename = function() paste0("gefilterte_Daten_", Sys.Date(), ".csv"),
    content = function(file) write.csv(data_filtered(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )

  # -----------------------------------------------------------
  # Tumorboardbeschlüsse
  # -----------------------------------------------------------
  tumorboard_data <- reactiveVal(read_tumorboard_data(tumorboard_csv_path))

  output$tb_storage_info <- renderText({
    paste0("Speicherdatei:\n", tumorboard_csv_path,
           "\n\nExistiert: ", file.exists(tumorboard_csv_path))
  })

  tb_patient_col <- reactive({
    df <- data_filtered()
    find_col(df, c("patient", "name", "patient_id", "patienten_id", "id"))
  })

  output$tb_patient_ui <- renderUI({
    df <- data_filtered()
    req(nrow(df) > 0)
    pcol <- tb_patient_col()
    validate(need(!is.null(pcol), "Keine Patientenspalte gefunden. Erwartet z.B. 'Patient', 'Name' oder 'Patient_ID'."))
    choices <- sort(unique(trimws(as.character(df[[pcol]]))))
    choices <- choices[!is.na(choices) & choices != ""]
    selectizeInput("tb_patient", "Patient aus Patientenliste", choices = choices, selected = choices[1], multiple = FALSE)
  })

  # Wenn in der Patientenliste eine Zeile markiert wird, den Patienten automatisch übernehmen.
  observeEvent(input$table_rows_selected, {
    idx <- input$table_rows_selected
    df <- data_filtered()
    pcol <- tb_patient_col()
    if (length(idx) == 1 && !is.null(pcol) && pcol %in% names(df) && nrow(df) >= idx) {
      pat <- trimws(as.character(df[[pcol]][idx]))
      if (!is.na(pat) && nzchar(pat)) {
        updateSelectizeInput(session, "tb_patient", selected = pat)
      }
    }
  }, ignoreInit = TRUE)

  observeEvent(input$save_tb, {
    if (is.null(input$tb_patient) || !nzchar(input$tb_patient)) {
      showNotification("Bitte zuerst einen Patienten auswählen.", type = "error")
      return()
    }
    if (is.null(input$tb_decision) || !nzchar(trimws(input$tb_decision))) {
      showNotification("Bitte einen Tumorboardbeschluss eintragen.", type = "error")
      return()
    }

    entry <- data.frame(
      Patient = as.character(input$tb_patient),
      Board_Datum = as.Date(input$tb_date),
      Tumorboardbeschluss = trimws(as.character(input$tb_decision)),
      Verantwortlich = trimws(as.character(input$tb_responsible)),
      Erfasst_am = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )

    updated <- bind_rows(tumorboard_data(), entry)
    tumorboard_data(updated)
    write_tumorboard_data(updated, tumorboard_csv_path)
    updateTextAreaInput(session, "tb_decision", value = "")
    showNotification("Tumorboardbeschluss gespeichert.", type = "message")
  })

  tb_current_patient <- reactive({
    df <- tumorboard_data()
    if (is.null(input$tb_patient) || !nzchar(input$tb_patient) || nrow(df) == 0) return(df[0, , drop = FALSE])
    df %>% filter(.data$Patient == input$tb_patient) %>% arrange(desc(.data$Board_Datum), desc(.data$Erfasst_am))
  })

  output$tb_n_total <- renderText({
    format(nrow(tumorboard_data()), big.mark = ".")
  })

  output$tb_n_patient <- renderText({
    format(nrow(tb_current_patient()), big.mark = ".")
  })

  output$tb_n_patients <- renderText({
    df <- tumorboard_data()
    if (nrow(df) == 0 || !"Patient" %in% names(df)) return("0")
    format(n_distinct_nonempty(df$Patient), big.mark = ".")
  })

  output$tb_patient_table <- renderDT({
    datatable(tb_current_patient(), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$tb_all_table <- renderDT({
    df <- tumorboard_data()
    if (nrow(df) > 0) df <- df %>% arrange(desc(.data$Board_Datum), .data$Patient)
    datatable(df, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE, searchHighlight = TRUE))
  })

  output$download_tb <- downloadHandler(
    filename = function() paste0("Tumorboardbeschluesse_", Sys.Date(), ".csv"),
    content = function(file) write.csv(tumorboard_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )


  # -----------------------------------------------------------
  # Kaplan-Meier
  # -----------------------------------------------------------
  output$km_ui <- renderUI({
    df <- data_filtered()
    req(nrow(df) > 0)
    num_cols <- names(df)[sapply(df, is.numeric)]
    cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]

    diagnose_choices <- if ("diagnose" %in% names(df)) sort(unique(na.omit(as.character(df$diagnose)))) else character(0)

    tagList(
      selectInput(
        "km_endpoint",
        "Analyse",
        choices = c(
          "PFS automatisch: PFS + Rezidiv Event" = "pfs_auto",
          "OS automatisch: OS + Death Event" = "os_auto",
          "Manuell" = "manual"
        ),
        selected = "pfs_auto"
      ),
      selectizeInput(
        "km_diagnosis",
        "Diagnose/Entität für KM-Kurve",
        choices = c("Alle Diagnosen" = "__all__", diagnose_choices),
        selected = "__all__",
        multiple = FALSE
      ),
      selectInput("km_time", "Zeitvariable manuell", choices = num_cols, selected = if ("pfs" %in% num_cols) "pfs" else num_cols[1]),
      selectInput("km_event", "Eventvariable manuell", choices = names(df), selected = if ("rezidiv_event" %in% names(df)) "rezidiv_event" else if ("rezidiv" %in% names(df)) "rezidiv" else if ("death_event" %in% names(df)) "death_event" else names(df)[1]),
      selectInput("km_group", "Gruppe/Stratum optional", choices = c("— keine —", cat_cols), selected = "— keine —"),
      checkboxInput("km_confint", "Konfidenzintervall", TRUE),
      checkboxInput("km_risktable", "Risk Table", TRUE),
      numericInput("km_time_div", "Zeit-Skalierung: 1 = Monate, 12 = Jahre", value = 1, min = 0.0001),
      textInput("km_title", "Titel", value = "Kaplan–Meier-Kurve"),
      textInput("km_xlab", "X-Achse", value = "Monate"),
      textInput("km_ylab", "Y-Achse", value = "Wahrscheinlichkeit")
    )
  })

  km_plot_obj <- eventReactive(input$run_km, {
    df <- data_filtered()
    validate(need(nrow(df) > 1, "Zu wenige Daten nach Filter."))

    # Entitätsspezifischer Filter direkt im KM-Modul
    if (!is.null(input$km_diagnosis) && input$km_diagnosis != "__all__" && "diagnose" %in% names(df)) {
      df <- df %>% filter(as.character(.data$diagnose) == input$km_diagnosis)
    }

    endpoint <- input$km_endpoint
    if (endpoint == "pfs_auto") {
      time_col <- "pfs"
      event_col <- if ("rezidiv_event" %in% names(df)) "rezidiv_event" else "rezidiv"
      event_mode <- "auto"
      default_title <- paste0("PFS", if (!is.null(input$km_diagnosis) && input$km_diagnosis != "__all__") paste0(" – ", input$km_diagnosis) else "")
    } else if (endpoint == "os_auto") {
      time_col <- "os"
      event_col <- "death_event"
      event_mode <- "auto"
      default_title <- paste0("OS", if (!is.null(input$km_diagnosis) && input$km_diagnosis != "__all__") paste0(" – ", input$km_diagnosis) else "")
    } else {
      req(input$km_time, input$km_event)
      time_col <- input$km_time
      event_col <- input$km_event
      event_mode <- if (event_col == "rezidiv") "date_event" else "auto"
      default_title <- input$km_title
    }

    validate(
      need(time_col %in% names(df), paste0("Zeitspalte '", time_col, "' nicht gefunden.")),
      need(event_col %in% names(df), paste0("Eventspalte '", event_col, "' nicht gefunden."))
    )

    time <- suppressWarnings(as.numeric(df[[time_col]])) / input$km_time_div
    event <- as_event01(df[[event_col]], mode = event_mode)

    keep <- !is.na(time) & !is.na(event) & time >= 0 & event %in% c(0, 1)

    validate(
      need(sum(keep) >= 2, paste0(
        "Zu wenige verwertbare KM-Daten. Prüfen: ", time_col, " muss Monate enthalten; ",
        event_col, " muss Ereignis/Zensierung enthalten."
      )),
      need(sum(event[keep] == 1, na.rm = TRUE) >= 1, paste0(
        "Keine Ereignisse in der Auswahl. Für PFS bitte prüfen, ob rezidiv_event 1/0 bzw. Ja/Nein enthält. "
      ))
    )

    # Expliziter KM-Datensatz: verhindert Fehler wie
    # "The `data` argument should be provided either to ggsurvfit or survfit."
    km_df <- data.frame(
      time = time[keep],
      event = as.integer(event[keep])
    )

    if (input$km_group != "— keine —") {
      km_df$grp <- as.factor(df[[input$km_group]][keep])
      km_df <- km_df[!is.na(km_df$grp), , drop = FALSE]
      fit <- survfit(Surv(time, event) ~ grp, data = km_df)
    } else {
      fit <- survfit(Surv(time, event) ~ 1, data = km_df)
    }

    title_to_use <- if (!is.null(input$km_title) && nzchar(input$km_title) && input$km_title != "Kaplan–Meier-Kurve") input$km_title else default_title

    ggsurvplot(
      fit,
      data = km_df,
      conf.int = isTRUE(input$km_confint),
      risk.table = isTRUE(input$km_risktable),
      ggtheme = theme_minimal(base_size = 13),
      title = title_to_use,
      xlab = input$km_xlab,
      ylab = input$km_ylab,
      censor = TRUE,
      risk.table.height = 0.25
    )
  }, ignoreNULL = FALSE)

  output$km_plot <- renderPlot({
    g <- km_plot_obj()
    req(g)
    print(g)
  })

  output$download_km_plot <- downloadHandler(
    filename = function() paste0("KM_", Sys.Date(), ".png"),
    content = function(file) {
      g <- km_plot_obj()
      png(file, width = 1500, height = 1000, res = 150)
      print(g)
      dev.off()
    }
  )


  # -----------------------------------------------------------
  # OPS 8-544 Therapieblöcke
  # -----------------------------------------------------------
  output$therapy_source_info <- renderText({
    blocks <- therapy_block_data()
    paste0(
      attr(therapy_raw(), "source_label"),
      "
Patientenspalte erkannt: ",
      ifelse(is.null(attr(blocks, "patient_cols_used")) || attr(blocks, "patient_cols_used") == "", "keine", attr(blocks, "patient_cols_used"))
    )
  })

  therapy_block_data <- reactive({
    prepare_therapy_blocks(therapy_raw())
  })

  output$therapy_filters <- renderUI({
    blocks <- therapy_block_data()
    if (nrow(blocks) == 0) {
      return(helpText("Keine Therapieblock-Tabelle gefunden. Bitte die OPS-8-544-Tabelle als zweites Blatt in Daten.xlsx einfügen. Empfohlener Blattname: Therapie_OPS8544."))
    }

    tagList(
      selectizeInput("therapy_year_filter", "Jahr", choices = sort(unique(na.omit(blocks$jahr))), selected = sort(unique(na.omit(blocks$jahr))), multiple = TRUE),
      selectizeInput("therapy_protocol_filter", "Therapieprotokoll", choices = sort(unique(na.omit(blocks$therapieprotokoll))), selected = NULL, multiple = TRUE),
      selectizeInput("therapy_diagnosis_filter", "Diagnose", choices = sort(unique(na.omit(blocks$diagnose))), selected = NULL, multiple = TRUE),
      textInput("therapy_search", "Freitextsuche Patient/Therapie/Diagnose", value = "")
    )
  })

  therapy_filtered <- reactive({
    blocks <- therapy_block_data()
    if (nrow(blocks) == 0) return(blocks)

    if (!is.null(input$therapy_year_filter) && length(input$therapy_year_filter) > 0) {
      blocks <- blocks %>% filter(jahr %in% as.integer(input$therapy_year_filter))
    }
    if (!is.null(input$therapy_protocol_filter) && length(input$therapy_protocol_filter) > 0) {
      blocks <- blocks %>% filter(therapieprotokoll %in% input$therapy_protocol_filter)
    }
    if (!is.null(input$therapy_diagnosis_filter) && length(input$therapy_diagnosis_filter) > 0) {
      blocks <- blocks %>% filter(diagnose %in% input$therapy_diagnosis_filter)
    }
    if (!is.null(input$therapy_search) && nzchar(input$therapy_search)) {
      pat <- tolower(input$therapy_search)
      blocks <- blocks %>% filter(grepl(pat, tolower(paste(patient, therapieprotokoll, diagnose)), fixed = TRUE))
    }

    blocks
  })

  output$n_therapy_blocks <- renderText(format(nrow(therapy_filtered()), big.mark = "."))
  output$n_therapy_patients <- renderText({
    blocks <- therapy_filtered()
    if (!("patient" %in% names(blocks))) return("0")
    n_pat <- n_distinct_nonempty(blocks$patient)
    format(n_pat, big.mark = ".")
  })
  output$n_therapy_protocols <- renderText(format(length(unique(na.omit(therapy_filtered()$therapieprotokoll))), big.mark = "."))

  output$therapy_protocol_table <- renderDT({
    blocks <- therapy_filtered()
    validate(need(nrow(blocks) > 0, "Keine Therapieblöcke im aktuellen Filter."))
    tab <- blocks %>%
      group_by(therapieprotokoll) %>%
      summarise(
        OPS_8_544_Bloecke = n(),
        Patienten = n_distinct_nonempty(patient),
        .groups = "drop"
      ) %>%
      arrange(desc(OPS_8_544_Bloecke)) %>%
      mutate(Anteil = scales::percent(OPS_8_544_Bloecke / sum(OPS_8_544_Bloecke)))
    datatable(tab, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$therapy_diagnosis_table <- renderDT({
    blocks <- therapy_filtered()
    validate(need(nrow(blocks) > 0, "Keine Therapieblöcke im aktuellen Filter."))
    tab <- blocks %>%
      group_by(diagnose) %>%
      summarise(
        OPS_8_544_Bloecke = n(),
        Patienten = n_distinct_nonempty(patient),
        .groups = "drop"
      ) %>%
      arrange(desc(OPS_8_544_Bloecke)) %>%
      mutate(Anteil = scales::percent(OPS_8_544_Bloecke / sum(OPS_8_544_Bloecke)))
    datatable(tab, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$therapy_protocol_plot <- renderPlot({
    blocks <- therapy_filtered()
    validate(need(nrow(blocks) > 0, "Keine Therapieblöcke im aktuellen Filter."))
    tab <- blocks %>% count(therapieprotokoll, sort = TRUE) %>% slice_head(n = 20)
    ggplot(tab, aes(x = reorder(therapieprotokoll, n), y = n)) +
      geom_col() +
      coord_flip() +
      theme_minimal(base_size = 13) +
      labs(x = NULL, y = "OPS-8-544-Blöcke", title = "Blöcke nach Therapieprotokoll")
  })

  output$therapy_month_plot <- renderPlot({
    blocks <- therapy_filtered()
    validate(need(nrow(blocks) > 0, "Keine Therapieblöcke im aktuellen Filter."))
    tab <- blocks %>% filter(!is.na(monat_sort)) %>% count(monat_sort)
    validate(need(nrow(tab) > 0, "Keine verwertbaren Datums-/Monatsangaben."))
    ggplot(tab, aes(x = monat_sort, y = n, group = 1)) +
      geom_line() +
      geom_point() +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Monat", y = "OPS-8-544-Blöcke", title = "Monatliche OPS-8-544-Blöcke")
  })

  output$therapy_block_details <- renderDT({
    blocks <- therapy_filtered()
    show_cols <- intersect(c("datum", "patient", "therapieprotokoll", "diagnose", "zyklus", "jahr"), names(blocks))
    datatable(blocks[, show_cols, drop = FALSE], rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })

  output$download_therapy_blocks <- downloadHandler(
    filename = function() paste0("OPS_8_544_Therapiebloecke_", Sys.Date(), ".csv"),
    content = function(file) write.csv(therapy_filtered(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )


  # -----------------------------------------------------------
  # OPS 1-941 Komplexe Diagnostik
  # -----------------------------------------------------------
  diagnostic_block_data <- reactive({
    prepare_diagnostic_blocks(diagnostic_raw())
  })

  output$diagnostic_source_info <- renderText({
    blocks <- diagnostic_block_data()
    paste0(
      attr(diagnostic_raw(), "source_label"),
      "\nErkannte Komponenten: ",
      ifelse(is.null(attr(blocks, "component_cols")) || attr(blocks, "component_cols") == "", "keine", attr(blocks, "component_cols"))
    )
  })

  output$diagnostic_filters <- renderUI({
    blocks <- diagnostic_block_data()
    if (nrow(blocks) == 0) {
      return(helpText("Keine OPS-1-941-/Komplexe-Diagnostik-Tabelle gefunden. Bitte die Tabelle als Blatt 'Komplexe Diagnostik' in Daten.xlsx einfügen."))
    }

    tagList(
      selectizeInput("diagnostic_year_filter", "Jahr", choices = sort(unique(na.omit(blocks$jahr))), selected = sort(unique(na.omit(blocks$jahr))), multiple = TRUE),
      selectizeInput("diagnostic_diagnosis_filter", "Diagnose", choices = sort(unique(na.omit(blocks$diagnose))), selected = NULL, multiple = TRUE),
      selectizeInput("diagnostic_component_filter", "Diagnostikbereich", choices = sort(unique(diagnostic_components_long(blocks)$diagnostik_bereich)), selected = NULL, multiple = TRUE),
      textInput("diagnostic_search", "Freitextsuche Patient/Diagnose", value = "")
    )
  })

  diagnostic_filtered <- reactive({
    blocks <- diagnostic_block_data()
    if (nrow(blocks) == 0) return(blocks)

    if (!is.null(input$diagnostic_year_filter) && length(input$diagnostic_year_filter) > 0) {
      blocks <- blocks %>% filter(jahr %in% as.integer(input$diagnostic_year_filter))
    }
    if (!is.null(input$diagnostic_diagnosis_filter) && length(input$diagnostic_diagnosis_filter) > 0) {
      blocks <- blocks %>% filter(diagnose %in% input$diagnostic_diagnosis_filter)
    }
    if (!is.null(input$diagnostic_component_filter) && length(input$diagnostic_component_filter) > 0) {
      long <- diagnostic_components_long(blocks) %>% filter(diagnostik_bereich %in% input$diagnostic_component_filter)
      keep_pat <- unique(long$patient)
      blocks <- blocks %>% filter(patient %in% keep_pat)
    }
    if (!is.null(input$diagnostic_search) && nzchar(input$diagnostic_search)) {
      pat <- tolower(input$diagnostic_search)
      blocks <- blocks %>% filter(grepl(pat, tolower(paste(patient, diagnose)), fixed = TRUE))
    }

    blocks
  })

  diagnostic_components_filtered <- reactive({
    long <- diagnostic_components_long(diagnostic_filtered())
    if (!is.null(input$diagnostic_component_filter) && length(input$diagnostic_component_filter) > 0 && nrow(long) > 0) {
      long <- long %>% filter(diagnostik_bereich %in% input$diagnostic_component_filter)
    }
    long
  })

  output$n_diagnostic_blocks <- renderText(format(nrow(diagnostic_filtered()), big.mark = "."))
  output$n_diagnostic_patients <- renderText({
    blocks <- diagnostic_filtered()
    if (!("patient" %in% names(blocks))) return("0")
    format(n_distinct_nonempty(blocks$patient), big.mark = ".")
  })
  output$n_diagnostic_diagnoses <- renderText(format(length(unique(na.omit(diagnostic_filtered()$diagnose))), big.mark = "."))

  output$diagnostic_component_table <- renderDT({
    long <- diagnostic_components_filtered()
    validate(need(nrow(long) > 0, "Keine komplexen Diagnostik-Komponenten im aktuellen Filter."))
    tab <- long %>%
      group_by(diagnostik_bereich) %>%
      summarise(
        Anzahl = n(),
        Patienten = n_distinct_nonempty(patient),
        .groups = "drop"
      ) %>%
      arrange(desc(Anzahl)) %>%
      mutate(Anteil = scales::percent(Anzahl / sum(Anzahl)))
    datatable(tab, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$diagnostic_diagnosis_table <- renderDT({
    blocks <- diagnostic_filtered()
    validate(need(nrow(blocks) > 0, "Keine komplexen Diagnostiken im aktuellen Filter."))
    tab <- blocks %>%
      group_by(diagnose) %>%
      summarise(
        OPS_1_941_Faelle = n(),
        Patienten = n_distinct_nonempty(patient),
        .groups = "drop"
      ) %>%
      arrange(desc(OPS_1_941_Faelle)) %>%
      mutate(Anteil = scales::percent(OPS_1_941_Faelle / sum(OPS_1_941_Faelle)))
    datatable(tab, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$diagnostic_component_plot <- renderPlot({
    long <- diagnostic_components_filtered()
    validate(need(nrow(long) > 0, "Keine komplexen Diagnostik-Komponenten im aktuellen Filter."))
    tab <- long %>% count(diagnostik_bereich, sort = TRUE)
    ggplot(tab, aes(x = reorder(diagnostik_bereich, n), y = n)) +
      geom_col() +
      coord_flip() +
      theme_minimal(base_size = 13) +
      labs(x = NULL, y = "Anzahl", title = "OPS-1-941-Komponenten")
  })

  output$diagnostic_month_plot <- renderPlot({
    blocks <- diagnostic_filtered()
    validate(need(nrow(blocks) > 0, "Keine komplexen Diagnostiken im aktuellen Filter."))
    tab <- blocks %>% filter(!is.na(monat_sort)) %>% count(monat_sort)
    validate(need(nrow(tab) > 0, "Keine verwertbaren Datums-/Monatsangaben."))
    ggplot(tab, aes(x = monat_sort, y = n, group = 1)) +
      geom_line() +
      geom_point() +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Monat", y = "OPS-1-941-Fälle", title = "Monatliche komplexe Diagnostiken")
  })

  output$diagnostic_block_details <- renderDT({
    blocks <- diagnostic_filtered()
    show_cols <- intersect(c("datum", "patient", "diagnose", "primaerfall", "patientenfall", "morphologie", "immunphanotypisierung", "zytogenetik", "molekulargenetik", "jahr"), names(blocks))
    datatable(blocks[, show_cols, drop = FALSE], rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })

  output$download_diagnostic_blocks <- downloadHandler(
    filename = function() paste0("OPS_1_941_Komplexe_Diagnostik_", Sys.Date(), ".csv"),
    content = function(file) write.csv(diagnostic_filtered(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )



  # -----------------------------------------------------------
  # Oncoprint / Mutationsprofil
  # -----------------------------------------------------------
  output$oncoprint_filters <- renderUI({
    df <- data_filtered()
    req(nrow(df) > 0)

    onco_all <- parse_oncoprint_data(df, remove_negative = FALSE)
    validate(need(nrow(onco_all) > 0, "Keine verwertbaren Einträge in 'Krankheitsspezifische hematol Resultate'."))

    ent_choices <- sort(unique(na.omit(onco_all$diagnose_label)))
    alt_choices <- onco_all %>%
      filter(oncoprint_mutation) %>%
      count(alteration, sort = TRUE) %>%
      pull(alteration)

    tagList(
      selectizeInput("onco_entity_filter", "Entität/Diagnose", choices = ent_choices, selected = ent_choices, multiple = TRUE),
      numericInput("onco_top_n", "Top-Alterationen anzeigen", value = 25, min = 5, max = 100, step = 5),
      checkboxInput("onco_remove_negative", "Negative/NA-Befunde ausblenden", TRUE),
      selectizeInput("onco_alt_filter", "Optional: bestimmte Mutationen", choices = alt_choices, selected = NULL, multiple = TRUE,
                     options = list(placeholder = "leer = automatisch Top-Alterationen")),
      checkboxInput("onco_show_patient_names", "Patientennamen in X-Achse anzeigen", FALSE)
    )
  })

  oncoprint_all_filtered <- reactive({
    df <- data_filtered()
    onco <- parse_oncoprint_data(df, remove_negative = isTRUE(input$onco_remove_negative))

    if (!is.null(input$onco_entity_filter) && length(input$onco_entity_filter) > 0) {
      onco <- onco %>% filter(diagnose_label %in% input$onco_entity_filter)
    }

    onco
  })

  oncoprint_long <- reactive({
    # Für den Oncoprint werden nur echte Mutationen/Varianten verwendet.
    # NA, negative Befunde sowie strukturelle/zytogenetische Alterationen bleiben draußen.
    onco <- oncoprint_all_filtered() %>%
      filter(oncoprint_mutation)

    if (!is.null(input$onco_alt_filter) && length(input$onco_alt_filter) > 0) {
      onco <- onco %>% filter(alteration %in% input$onco_alt_filter)
    } else {
      top_n <- ifelse(is.null(input$onco_top_n), 25, input$onco_top_n)
      top_alts <- onco %>% count(alteration, sort = TRUE) %>% slice_head(n = top_n) %>% pull(alteration)
      onco <- onco %>% filter(alteration %in% top_alts)
    }

    onco
  })

  oncoprint_structural <- reactive({
    oncoprint_all_filtered() %>%
      filter(!oncoprint_mutation) %>%
      filter(!alteration_class %in% c("negativ/kein Nachweis", "Nicht verwertbar/NA"))
  })

  output$n_onco_patients <- renderText({
    onco <- oncoprint_long()
    format(dplyr::n_distinct(onco$patient_label), big.mark = ".")
  })

  output$n_onco_alterations <- renderText({
    onco <- oncoprint_long()
    format(dplyr::n_distinct(onco$alteration), big.mark = ".")
  })

  output$n_onco_entities <- renderText({
    onco <- oncoprint_long()
    format(dplyr::n_distinct(onco$diagnose_label), big.mark = ".")
  })

  oncoprint_plot_obj <- eventReactive(input$run_oncoprint, {
    onco <- oncoprint_long()
    validate(need(nrow(onco) > 0, "Keine echten Mutationen/Varianten im aktuellen Filter."))

    patient_order <- onco %>%
      distinct(patient_label, diagnose_label) %>%
      arrange(diagnose_label, patient_label) %>%
      mutate(patient_plot = if (isTRUE(input$onco_show_patient_names)) patient_label else paste0("Fall ", row_number()))

    alt_order <- onco %>% count(alteration, sort = TRUE) %>% pull(alteration)

    plot_df <- onco %>%
      left_join(patient_order, by = c("patient_label", "diagnose_label")) %>%
      mutate(
        patient_plot = factor(patient_plot, levels = patient_order$patient_plot),
        alteration = factor(alteration, levels = rev(alt_order)),
        diagnose_label = factor(diagnose_label, levels = unique(patient_order$diagnose_label))
      )

    ggplot(plot_df, aes(x = patient_plot, y = alteration, fill = alteration_class)) +
      geom_tile(color = "white", linewidth = 0.25) +
      facet_grid(. ~ diagnose_label, scales = "free_x", space = "free_x") +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = if (isTRUE(input$onco_show_patient_names)) 7 else 5),
        panel.grid = element_blank(),
        strip.text.x = element_text(face = "bold", size = 10),
        legend.position = "bottom"
      ) +
      labs(
        title = "Oncoprint: echte Mutationen/Varianten",
        subtitle = "NA, negative Befunde und strukturelle/zytogenetische Alterationen sind aus dem Plot ausgeschlossen",
        x = "Patient/Fall",
        y = "Mutation/Variante",
        fill = "Typ"
      )
  }, ignoreNULL = FALSE)

  output$oncoprint_plot <- renderPlot({
    p <- oncoprint_plot_obj()
    req(p)
    print(p)
  })

  output$oncoprint_summary_table <- renderDT({
    onco <- oncoprint_long()
    validate(need(nrow(onco) > 0, "Keine echten Mutationen/Varianten im aktuellen Filter."))
    tab <- onco %>%
      group_by(diagnose_label, alteration) %>%
      summarise(
        Patienten_Faelle = n_distinct(patient_label),
        Alterationstyp = paste(sort(unique(alteration_class)), collapse = ", "),
        .groups = "drop"
      ) %>%
      arrange(diagnose_label, desc(Patienten_Faelle), alteration)
    datatable(tab, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })

  output$oncoprint_detail_table <- renderDT({
    onco <- oncoprint_long()
    show_cols <- intersect(c("patient_label", "diagnose_label", "alteration", "alteration_class", "alteration_raw"), names(onco))
    datatable(onco[, show_cols, drop = FALSE], rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })


  output$oncoprint_structural_table <- renderDT({
    structural <- oncoprint_structural()
    validate(need(nrow(structural) > 0, "Keine strukturellen/zytogenetischen Befunde im aktuellen Filter."))
    tab <- structural %>%
      group_by(diagnose_label, alteration_class, alteration) %>%
      summarise(
        Patienten_Faelle = n_distinct(patient_label),
        Beispiele = paste(head(sort(unique(alteration_raw)), 5), collapse = " | "),
        .groups = "drop"
      ) %>%
      arrange(diagnose_label, alteration_class, desc(Patienten_Faelle), alteration)
    datatable(tab, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })

  output$download_oncoprint_data <- downloadHandler(
    filename = function() paste0("Oncoprint_Mutationsdaten_", Sys.Date(), ".csv"),
    content = function(file) write.csv(oncoprint_long(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )

  output$download_structural_data <- downloadHandler(
    filename = function() paste0("Strukturelle_Zytogenetische_Befunde_", Sys.Date(), ".csv"),
    content = function(file) write.csv(oncoprint_structural(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )

  output$download_oncoprint_plot <- downloadHandler(
    filename = function() paste0("Oncoprint_", Sys.Date(), ".png"),
    content = function(file) {
      p <- oncoprint_plot_obj()
      ggsave(file, p, width = 15, height = 9, dpi = 150)
    }
  )


  # -----------------------------------------------------------
  # Zytogenetik aus separater Spalte
  # -----------------------------------------------------------
  output$cyto_filters <- renderUI({
    df <- data_filtered()
    req(nrow(df) > 0)

    cyto_all <- parse_cytogenetics_data(df, remove_negative = FALSE)
    validate(need(nrow(cyto_all) > 0, "Keine verwertbaren Einträge in 'Zytogenetik'."))

    ent_choices <- sort(unique(na.omit(cyto_all$diagnose_label)))
    cyto_choices <- cyto_all %>%
      filter(!alteration_class %in% c("negativ/kein Nachweis", "Nicht verwertbar/NA")) %>%
      count(alteration, sort = TRUE) %>%
      pull(alteration)

    tagList(
      selectizeInput("cyto_entity_filter", "Entität/Diagnose", choices = ent_choices, selected = ent_choices, multiple = TRUE),
      numericInput("cyto_top_n", "Top-Befunde anzeigen", value = 25, min = 5, max = 100, step = 5),
      checkboxInput("cyto_remove_negative", "Negative/NA-Befunde ausblenden", TRUE),
      selectizeInput("cyto_alt_filter", "Optional: bestimmte Zytogenetik-Befunde", choices = cyto_choices, selected = NULL, multiple = TRUE,
                     options = list(placeholder = "leer = automatisch Top-Befunde"))
    )
  })

  cyto_all_filtered <- reactive({
    df <- data_filtered()
    cyto <- parse_cytogenetics_data(df, remove_negative = isTRUE(input$cyto_remove_negative))

    if (!is.null(input$cyto_entity_filter) && length(input$cyto_entity_filter) > 0) {
      cyto <- cyto %>% filter(diagnose_label %in% input$cyto_entity_filter)
    }

    if (!is.null(input$cyto_alt_filter) && length(input$cyto_alt_filter) > 0) {
      cyto <- cyto %>% filter(alteration %in% input$cyto_alt_filter)
    } else {
      top_n <- ifelse(is.null(input$cyto_top_n), 25, input$cyto_top_n)
      top_alts <- cyto %>% count(alteration, sort = TRUE) %>% slice_head(n = top_n) %>% pull(alteration)
      cyto <- cyto %>% filter(alteration %in% top_alts)
    }

    cyto
  })

  output$n_cyto_patients <- renderText({
    cyto <- cyto_all_filtered()
    format(dplyr::n_distinct(cyto$patient_label), big.mark = ".")
  })

  output$n_cyto_alterations <- renderText({
    cyto <- cyto_all_filtered()
    format(dplyr::n_distinct(cyto$alteration), big.mark = ".")
  })

  output$n_cyto_entities <- renderText({
    cyto <- cyto_all_filtered()
    format(dplyr::n_distinct(cyto$diagnose_label), big.mark = ".")
  })

  cyto_plot_obj <- eventReactive(input$run_cyto, {
    cyto <- cyto_all_filtered()
    validate(need(nrow(cyto) > 0, "Keine Zytogenetik-Befunde im aktuellen Filter."))

    tab <- cyto %>%
      count(alteration, sort = TRUE) %>%
      arrange(n) %>%
      mutate(alteration = factor(alteration, levels = alteration))

    ggplot(tab, aes(x = alteration, y = n)) +
      geom_col() +
      coord_flip() +
      theme_minimal(base_size = 13) +
      labs(
        title = "Top-Zytogenetik-Befunde",
        subtitle = "Quelle: separate Spalte 'Zytogenetik'",
        x = "Zytogenetik-Befund",
        y = "Anzahl Fälle/Patienten"
      )
  }, ignoreNULL = FALSE)

  output$cyto_plot <- renderPlot({
    p <- cyto_plot_obj()
    req(p)
    print(p)
  })

  output$cyto_summary_table <- renderDT({
    cyto <- cyto_all_filtered()
    validate(need(nrow(cyto) > 0, "Keine Zytogenetik-Befunde im aktuellen Filter."))

    tab <- cyto %>%
      group_by(diagnose_label, alteration_class, alteration) %>%
      summarise(
        Patienten_Faelle = n_distinct(patient_label),
        Beispiele = paste(head(sort(unique(zytogenetik_raw)), 5), collapse = " | "),
        .groups = "drop"
      ) %>%
      arrange(diagnose_label, alteration_class, desc(Patienten_Faelle), alteration)

    datatable(tab, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })

  output$cyto_detail_table <- renderDT({
    cyto <- cyto_all_filtered()
    show_cols <- intersect(c("patient_label", "diagnose_label", "alteration", "alteration_class", "zytogenetik_raw"), names(cyto))
    datatable(cyto[, show_cols, drop = FALSE], rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })

  output$download_cyto_data <- downloadHandler(
    filename = function() paste0("Zytogenetik_", Sys.Date(), ".csv"),
    content = function(file) write.csv(cyto_all_filtered(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )

  # -----------------------------------------------------------
  # Boxplots
  # -----------------------------------------------------------
  output$box_ui <- renderUI({
    df <- data_filtered()
    req(nrow(df) > 0)
    num_cols <- names(df)[sapply(df, is.numeric)]
    grp_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]

    tagList(
      selectInput("box_y", "Numerische Variable Y", choices = num_cols, selected = if ("pfs" %in% num_cols) "pfs" else num_cols[1]),
      selectInput("box_x", "Gruppe X", choices = grp_cols, selected = if ("kodierung" %in% grp_cols) "kodierung" else grp_cols[1]),
      checkboxInput("box_jitter", "Jitter-Punkte anzeigen", TRUE),
      checkboxInput("box_log", "Y-Achse log10", FALSE),
      textInput("box_title", "Titel", value = "Boxplot")
    )
  })

  box_plot_obj <- eventReactive(input$run_box, {
    df <- data_filtered()
    req(input$box_y, input$box_x)
    validate(need(nrow(df) > 1, "Zu wenige Daten nach Filter."))

    plot_df <- data.frame(
      x = as.factor(df[[input$box_x]]),
      y = suppressWarnings(as.numeric(df[[input$box_y]]))
    ) %>% filter(!is.na(x), !is.na(y))

    validate(need(nrow(plot_df) > 1, "Keine verwertbaren Daten für Boxplot."))

    p <- ggplot(plot_df, aes(x = x, y = y)) +
      geom_boxplot(outlier.shape = NA) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = input$box_title, x = input$box_x, y = input$box_y)

    if (isTRUE(input$box_jitter)) p <- p + geom_jitter(width = 0.15, alpha = 0.6)
    if (isTRUE(input$box_log)) p <- p + scale_y_log10()
    p
  }, ignoreNULL = FALSE)

  output$box_plot <- renderPlot({
    p <- box_plot_obj()
    req(p)
    print(p)
  })

  output$download_box_plot <- downloadHandler(
    filename = function() paste0("Boxplot_", Sys.Date(), ".png"),
    content = function(file) {
      p <- box_plot_obj()
      ggsave(file, p, width = 10, height = 7, dpi = 150)
    }
  )
}

shinyApp(ui, server)
