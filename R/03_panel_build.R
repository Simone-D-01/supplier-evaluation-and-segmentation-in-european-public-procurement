# 03_panel_build.R
# Purpose: aggregate ted_awards_clean.csv to supplier-year level
# and build the panel variables used for regression and clustering.

rm(list = ls())

options(stringsAsFactors = FALSE, scipen = 999)

# -----------------------------------------------------------
# 1) Package management
# -----------------------------------------------------------
required_pkgs <- c(
  "here", "data.table", "dplyr", "stringr", "readr",
  "fs", "tibble", "countrycode"
)

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_pkgs, library, character.only = TRUE))

# -----------------------------------------------------------
# 2) Project paths
# -----------------------------------------------------------
root_dir <- here::here()
message("Project root: ", root_dir)

input_file <- here::here("data", "processed", "ted_awards_clean.csv")
output_dir <- here::here("data", "processed")
log_dir <- here::here("logs")

fs::dir_create(output_dir)
fs::dir_create(log_dir)

if (!fs::file_exists(input_file)) {
  stop(
    "Missing cleaned input file: ", input_file,
    ". Run 02_cleaning.R first."
  )
}

# -----------------------------------------------------------
# 3) Read cleaned award-level data
# -----------------------------------------------------------
message("Reading ted_awards_clean.csv ...")
awards <- data.table::fread(input_file, encoding = "UTF-8", showProgress = TRUE)

message("Rows read: ", nrow(awards))
message("Columns available: ", ncol(awards))

required_cols <- c(
  "WIN_NAME_CLEAN",
  "award_year",
  "award_value",
  "ISO_COUNTRY_CODE",
  "WIN_COUNTRY_CODE",
  "CAE_NAME",
  "CPV_2digit",
  "TOP_TYPE",
  "CRIT_CODE"
)

missing_cols <- setdiff(required_cols, names(awards))

if (length(missing_cols) > 0) {
  stop(
    "The following required columns are missing from ted_awards_clean.csv: ",
    paste(missing_cols, collapse = ", ")
  )
}

# -----------------------------------------------------------
# 4) Helper functions
# -----------------------------------------------------------
mode_non_missing <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  tab <- sort(table(x), decreasing = TRUE)
  names(tab)[1]
}

safe_mean <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_sum <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

safe_median <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  median(x, na.rm = TRUE)
}

safe_price_share <- function(x) {
  x <- stringr::str_to_upper(as.character(x))
  x <- x[!is.na(x) & x != "" & x %in% c("L", "M")]
  if (length(x) == 0) return(NA_real_)
  mean(x == "L")
}

parse_supplier_country <- function(x) {
  x <- stringr::str_squish(stringr::str_to_upper(as.character(x)))
  x[x == ""] <- NA_character_
  
  vapply(
    x,
    function(val) {
      if (is.na(val)) return(NA_character_)
      
      parts <- stringr::str_split(val, stringr::fixed("---"))[[1]]
      parts <- stringr::str_squish(parts)
      parts <- parts[parts != ""]
      parts <- unique(parts)
      
      if (length(parts) == 0) return(NA_character_)
      if (!all(stringr::str_detect(parts, "^[A-Z]{2}$"))) return(NA_character_)
      
      if (length(unique(parts)) == 1) {
        return(parts[1])
      }
      
      NA_character_
    },
    character(1)
  )
}

# -----------------------------------------------------------
# 5) Award-level variables
# -----------------------------------------------------------
awards <- awards %>%
  dplyr::mutate(
    WIN_NAME_CLEAN = stringr::str_squish(as.character(WIN_NAME_CLEAN)),
    award_year = as.integer(award_year),
    award_value = suppressWarnings(as.numeric(award_value)),
    ISO_COUNTRY_CODE = dplyr::na_if(
      stringr::str_squish(stringr::str_to_upper(as.character(ISO_COUNTRY_CODE))), ""
    ),
    WIN_COUNTRY_CODE_RAW = dplyr::na_if(
      stringr::str_squish(stringr::str_to_upper(as.character(WIN_COUNTRY_CODE))), ""
    ),
    WIN_COUNTRY_CODE_PARSED = parse_supplier_country(WIN_COUNTRY_CODE_RAW),
    WIN_COUNTRY_CODE_LOOKUP = dplyr::case_when(
      WIN_COUNTRY_CODE_PARSED == "UK" ~ "GB",
      TRUE ~ WIN_COUNTRY_CODE_PARSED
    ),
    WIN_COUNTRY_ISO_NAME = countrycode::countrycode(
      WIN_COUNTRY_CODE_LOOKUP,
      origin = "iso2c",
      destination = "country.name"
    ),
    win_country_status = dplyr::case_when(
      is.na(WIN_COUNTRY_CODE_RAW) ~ "missing_blank",
      is.na(WIN_COUNTRY_CODE_PARSED) ~ "invalid_unresolved",
      WIN_COUNTRY_CODE_PARSED == "UK" ~ "accepted_special_case",
      !is.na(WIN_COUNTRY_ISO_NAME) ~ "accepted_iso2",
      TRUE ~ "invalid_unresolved"
    ),
    WIN_COUNTRY_CODE_ANALYSIS = dplyr::case_when(
      win_country_status %in% c("accepted_iso2", "accepted_special_case") ~ WIN_COUNTRY_CODE_PARSED,
      TRUE ~ NA_character_
    ),
    CAE_NAME = dplyr::na_if(stringr::str_squish(as.character(CAE_NAME)), ""),
    CPV_2digit = dplyr::na_if(stringr::str_squish(as.character(CPV_2digit)), ""),
    TOP_TYPE = dplyr::na_if(
      stringr::str_squish(stringr::str_to_upper(as.character(TOP_TYPE))), ""
    ),
    CRIT_CODE = dplyr::na_if(
      stringr::str_squish(stringr::str_to_upper(as.character(CRIT_CODE))), ""
    ),
    cross_border = dplyr::if_else(
      !is.na(WIN_COUNTRY_CODE_ANALYSIS) & !is.na(ISO_COUNTRY_CODE),
      as.integer(WIN_COUNTRY_CODE_ANALYSIS != ISO_COUNTRY_CODE),
      NA_integer_
    ),
    is_consortium = as.integer(
      stringr::str_detect(WIN_NAME_CLEAN, stringr::fixed("---"))
    )
  ) %>%
  dplyr::filter(
    !is.na(WIN_NAME_CLEAN) & WIN_NAME_CLEAN != "",
    !is.na(award_year) & award_year >= 2018 & award_year <= 2023
  )

message("Rows after final award-level checks: ", nrow(awards))
message("Consortia identified: ", sum(awards$is_consortium, na.rm = TRUE))
message(
  "Consortia share: ",
  round(mean(awards$is_consortium, na.rm = TRUE) * 100, 2),
  "%"
)

message(
  "Rows with raw supplier-country strings collapsed to one unique country: ",
  sum(
    !is.na(awards$WIN_COUNTRY_CODE_RAW) &
      !is.na(awards$WIN_COUNTRY_CODE_PARSED) &
      awards$WIN_COUNTRY_CODE_RAW != awards$WIN_COUNTRY_CODE_PARSED,
    na.rm = TRUE
  )
)

message(
  "Rows with accepted ISO alpha-2 supplier-country codes: ",
  sum(awards$win_country_status == "accepted_iso2", na.rm = TRUE)
)

message(
  "Rows with accepted special-case supplier-country codes (UK): ",
  sum(awards$win_country_status == "accepted_special_case", na.rm = TRUE)
)

message(
  "Rows with invalid / unresolved supplier-country codes set to NA for analysis: ",
  sum(awards$win_country_status == "invalid_unresolved", na.rm = TRUE)
)

message(
  "Rows with missing blank supplier-country codes: ",
  sum(awards$win_country_status == "missing_blank", na.rm = TRUE)
)

# -----------------------------------------------------------
# 6) Supplier-country validation output
# -----------------------------------------------------------
message("Building WIN_COUNTRY_CODE validation tables ...")

win_country_validation <- awards %>%
  dplyr::count(
    WIN_COUNTRY_CODE_RAW,
    WIN_COUNTRY_CODE_PARSED,
    WIN_COUNTRY_CODE_ANALYSIS,
    win_country_status,
    sort = TRUE,
    name = "n_rows"
  )

win_country_validation_summary <- awards %>%
  dplyr::count(win_country_status, sort = TRUE, name = "n_rows") %>%
  dplyr::mutate(share = n_rows / sum(n_rows))

win_country_validation_file <- here::here("logs", "win_country_code_validation.csv")
win_country_validation_summary_file <- here::here("logs", "win_country_code_validation_summary.csv")

readr::write_csv(win_country_validation, win_country_validation_file)
readr::write_csv(win_country_validation_summary, win_country_validation_summary_file)

message("WIN_COUNTRY_CODE validation saved to: ", win_country_validation_file)
message("WIN_COUNTRY_CODE validation summary saved to: ", win_country_validation_summary_file)

# -----------------------------------------------------------
# 7) CRIT_CODE validation output
# -----------------------------------------------------------
message("Building CRIT_CODE validation table ...")

crit_code_validation <- awards %>%
  dplyr::mutate(
    crit_group = dplyr::case_when(
      is.na(CRIT_CODE) ~ "Missing",
      CRIT_CODE == "L" ~ "Lowest price",
      CRIT_CODE == "M" ~ "Most economically advantageous tender",
      TRUE ~ "Other / unexpected code"
    )
  ) %>%
  dplyr::count(CRIT_CODE, crit_group, name = "n") %>%
  dplyr::mutate(share = n / sum(n)) %>%
  dplyr::arrange(dplyr::desc(n), CRIT_CODE)

crit_code_validation_file <- here::here("logs", "crit_code_validation.csv")
readr::write_csv(crit_code_validation, crit_code_validation_file)

message("CRIT_CODE validation saved to: ", crit_code_validation_file)
message(
  "Unique non-missing CRIT_CODE values observed: ",
  paste(sort(unique(stats::na.omit(awards$CRIT_CODE))), collapse = ", ")
)

# -----------------------------------------------------------
# 8) CRIT_CODE by-year validation output
#    (replaces the orphaned crit_code_by_year.csv /
#    crit_code_frequency.csv, which were not produced by any
#    script and were 10 rows out of step with the deduplicated
#    awards table used everywhere else. This block is derived
#    from the same `awards` object as crit_code_validation
#    above, so the Overall row and the by-year rows can never
#    disagree again.)
# -----------------------------------------------------------
message("Building CRIT_CODE by-year validation table ...")

crit_code_by_year_validation <- awards %>%
  dplyr::mutate(
    crit_group = dplyr::case_when(
      is.na(CRIT_CODE) ~ "Missing",
      CRIT_CODE == "L" ~ "Lowest price",
      CRIT_CODE == "M" ~ "Most economically advantageous tender",
      TRUE ~ "Other / unexpected code"
    )
  ) %>%
  dplyr::count(award_year, CRIT_CODE, crit_group, name = "n") %>%
  dplyr::group_by(award_year) %>%
  dplyr::mutate(share_within_year = n / sum(n)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(award_year, dplyr::desc(n), CRIT_CODE)

crit_code_by_year_file <- here::here("logs", "crit_code_by_year_validation.csv")
readr::write_csv(crit_code_by_year_validation, crit_code_by_year_file)

message("CRIT_CODE by-year validation saved to: ", crit_code_by_year_file)

# -----------------------------------------------------------
# 9) CRIT_CODE by-procedure validation output
#    (replaces the orphaned crit_code_by_procedure.csv, same
#    stale-generation issue as above: not produced by any
#    script and summing to the pre-dedup row count.)
# -----------------------------------------------------------
message("Building CRIT_CODE by-procedure validation table ...")

crit_code_by_procedure_validation <- awards %>%
  dplyr::mutate(
    crit_group = dplyr::case_when(
      is.na(CRIT_CODE) ~ "Missing",
      CRIT_CODE == "L" ~ "Lowest price",
      CRIT_CODE == "M" ~ "Most economically advantageous tender",
      TRUE ~ "Other / unexpected code"
    )
  ) %>%
  dplyr::count(TOP_TYPE, CRIT_CODE, crit_group, name = "n") %>%
  dplyr::group_by(TOP_TYPE) %>%
  dplyr::mutate(share_within_proc = n / sum(n)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(TOP_TYPE, dplyr::desc(n), CRIT_CODE)

crit_code_by_procedure_file <- here::here("logs", "crit_code_by_procedure_validation.csv")
readr::write_csv(crit_code_by_procedure_validation, crit_code_by_procedure_file)

message("CRIT_CODE by-procedure validation saved to: ", crit_code_by_procedure_file)

# -----------------------------------------------------------
# 10) Supplier-year aggregation
# -----------------------------------------------------------
message("Aggregating to supplier-year level ...")

panel <- awards %>%
  dplyr::group_by(WIN_NAME_CLEAN, award_year) %>%
  dplyr::summarise(
    WIN_COUNTRY_CODE = mode_non_missing(WIN_COUNTRY_CODE_ANALYSIS),
    
    awards_count = dplyr::n(),
    total_award_value = safe_sum(award_value),
    avg_award_value = safe_mean(award_value),
    median_award_value = safe_median(award_value),
    
    distinct_buyers = dplyr::n_distinct(CAE_NAME, na.rm = TRUE),
    distinct_buyer_countries = dplyr::n_distinct(ISO_COUNTRY_CODE, na.rm = TRUE),
    
    cross_border_share = safe_mean(cross_border),
    
    distinct_cpv_divisions = dplyr::n_distinct(CPV_2digit, na.rm = TRUE),
    top_cpv = {
      cpv_vec <- CPV_2digit[!is.na(CPV_2digit) & CPV_2digit != ""]
      if (length(cpv_vec) == 0) {
        NA_character_
      } else {
        names(sort(table(cpv_vec), decreasing = TRUE))[1]
      }
    },
    specialization_share = {
      cpv_vec <- CPV_2digit[!is.na(CPV_2digit) & CPV_2digit != ""]
      if (length(cpv_vec) == 0) {
        NA_real_
      } else {
        cpv_tab <- table(cpv_vec)
        max(cpv_tab) / sum(cpv_tab)
      }
    },
    hhi_cpv = {
      cpv_vec <- CPV_2digit[!is.na(CPV_2digit) & CPV_2digit != ""]
      if (length(cpv_vec) == 0) {
        NA_real_
      } else {
        cpv_tab <- table(cpv_vec)
        shares <- as.numeric(cpv_tab) / sum(cpv_tab)
        sum(shares^2)
      }
    },
    
    top_procedure = mode_non_missing(TOP_TYPE),
    price_criteria_share = safe_price_share(CRIT_CODE),
    
    is_consortium = dplyr::if_else(any(is_consortium == 1, na.rm = TRUE), 1L, 0L),
    
    n_missing_value = sum(is.na(award_value)),
    n_missing_win_country = sum(is.na(WIN_COUNTRY_CODE_ANALYSIS)),
    n_missing_crit_code = sum(is.na(CRIT_CODE)),
    n_unexpected_crit_code = sum(!is.na(CRIT_CODE) & !(CRIT_CODE %in% c("L", "M"))),
    
    supplier_country_conflict = dplyr::if_else(
      dplyr::n_distinct(WIN_COUNTRY_CODE_ANALYSIS, na.rm = TRUE) > 1, 1L, 0L
    ),
    
    .groups = "drop"
  )

message("Supplier-year rows: ", nrow(panel))
message("Unique suppliers: ", dplyr::n_distinct(panel$WIN_NAME_CLEAN))
message(
  "Years covered: ",
  paste(sort(unique(panel$award_year)), collapse = ", ")
)

# -----------------------------------------------------------
# 11) Longitudinal variables
# -----------------------------------------------------------
panel <- panel %>%
  dplyr::group_by(WIN_NAME_CLEAN) %>%
  dplyr::arrange(award_year, .by_group = TRUE) %>%
  dplyr::mutate(
    years_active = dplyr::n(),
    first_year = min(award_year, na.rm = TRUE),
    last_year = max(award_year, na.rm = TRUE),
    award_frequency = sum(awards_count, na.rm = TRUE) / dplyr::n()
  ) %>%
  dplyr::ungroup()

# -----------------------------------------------------------
# 12) Log transformations
# -----------------------------------------------------------
panel <- panel %>%
  dplyr::mutate(
    log_total_value = log1p(total_award_value),
    log_avg_value = log1p(avg_award_value),
    log_awards_count = log1p(awards_count)
  )

# -----------------------------------------------------------
# 13) Save panel output
# -----------------------------------------------------------
output_file <- here::here("data", "processed", "supplier_year_panel.csv")
readr::write_csv(panel, output_file)

message("Saved supplier-year panel to: ", output_file)

# -----------------------------------------------------------
# 14) Save panel log
# -----------------------------------------------------------
panel_log <- tibble::tibble(
  metric = c(
    "panel_rows",
    "unique_suppliers",
    "years_covered",
    "consortium_rows",
    "missing_total_award_value",
    "missing_avg_award_value",
    "missing_distinct_buyers",
    "missing_distinct_buyer_countries",
    "missing_specialization_share",
    "missing_win_country_code",
    "missing_price_criteria_share",
    "rows_with_supplier_country_conflict",
    "rows_with_unexpected_crit_code",
    "award_rows_win_country_missing_blank",
    "award_rows_win_country_accepted_iso2",
    "award_rows_win_country_accepted_special_case",
    "award_rows_win_country_invalid_unresolved"
  ),
  value = c(
    nrow(panel),
    dplyr::n_distinct(panel$WIN_NAME_CLEAN),
    dplyr::n_distinct(panel$award_year),
    sum(panel$is_consortium == 1, na.rm = TRUE),
    sum(is.na(panel$total_award_value)),
    sum(is.na(panel$avg_award_value)),
    sum(is.na(panel$distinct_buyers)),
    sum(is.na(panel$distinct_buyer_countries)),
    sum(is.na(panel$specialization_share)),
    sum(is.na(panel$WIN_COUNTRY_CODE)),
    sum(is.na(panel$price_criteria_share)),
    sum(panel$supplier_country_conflict == 1, na.rm = TRUE),
    sum(panel$n_unexpected_crit_code > 0, na.rm = TRUE),
    sum(awards$win_country_status == "missing_blank", na.rm = TRUE),
    sum(awards$win_country_status == "accepted_iso2", na.rm = TRUE),
    sum(awards$win_country_status == "accepted_special_case", na.rm = TRUE),
    sum(awards$win_country_status == "invalid_unresolved", na.rm = TRUE)
  )
)

panel_log_file <- here::here("logs", "panel_log.csv")
readr::write_csv(panel_log, panel_log_file)

message("Panel log saved to: ", panel_log_file)
message("03_panel_build.R completed successfully.")
message("Ready for 04_regression.R")
