# 02_cleaning.R
# Purpose: read TED CAN 2018-2023 raw files, clean award-level records,
# and produce the base dataset for panel construction.

rm(list = ls())

options(stringsAsFactors = FALSE, scipen = 999)

# -----------------------------------------------------------
# 1) Package management
# -----------------------------------------------------------
required_pkgs <- c("here", "data.table", "dplyr", "stringr", "readr", "fs", "tibble")

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

input_dir  <- here::here("data", "raw", "tedcan")
output_dir <- here::here("data", "processed")
log_dir    <- here::here("logs")

fs::dir_create(output_dir)
fs::dir_create(log_dir)

output_file   <- here::here("data", "processed", "ted_awards_clean.csv")
log_file      <- here::here("logs", "cleaning_log.csv")
conflict_file <- here::here("logs", "id_award_conflicts.csv")

if (fs::file_exists(conflict_file)) {
  fs::file_delete(conflict_file)
}

years <- 2018:2023
expected_files <- paste0("TED_CAN_", years, ".csv")
expected_paths <- fs::path(input_dir, expected_files)

missing_files <- expected_files[!fs::file_exists(expected_paths)]

if (length(missing_files) > 0) {
  stop(
    "Missing raw input files: ",
    paste(missing_files, collapse = ", "),
    ". Run 01_download.R first and verify the raw-data folder."
  )
}

# -----------------------------------------------------------
# 3) Helpers
# -----------------------------------------------------------
clean_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  x
}

clean_upper <- function(x) {
  x <- clean_text(x)
  stringr::str_to_upper(x)
}

parse_ted_numeric <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\s+", "")
  x <- stringr::str_replace_all(x, ",", ".")
  x <- stringr::str_replace_all(x, "[^0-9.\\-]", "")
  x[x %in% c("", ".", "-", "-.", ".-")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

resolve_crit_code <- function(x) {
  vals <- clean_upper(x)
  vals <- vals[!is.na(vals)]
  
  if (length(vals) == 0) {
    return(NA_character_)
  }
  
  vals <- unique(vals)
  
  if (length(vals) == 1) {
    return(vals)
  }
  
  NA_character_
}

# -----------------------------------------------------------
# 4) Read and merge annual raw files
# -----------------------------------------------------------
ted_raw <- data.table::rbindlist(
  lapply(years, function(y) {
    f <- fs::path(input_dir, paste0("TED_CAN_", y, ".csv"))
    message("Reading: ", fs::path_file(f))
    
    dt <- data.table::fread(
      f,
      encoding = "UTF-8",
      fill = TRUE,
      showProgress = TRUE
    )
    
    dt[, source_year := y]
    dt
  }),
  use.names = TRUE,
  fill = TRUE
)

message("Total rows after merge: ", nrow(ted_raw))
message("Total columns available: ", ncol(ted_raw))

n_raw_rows <- nrow(ted_raw)

# -----------------------------------------------------------
# 5) Select required columns
# -----------------------------------------------------------
target_columns <- c(
  "ID_NOTICE_CAN",
  "ID_AWARD",
  "WIN_NAME",
  "WIN_COUNTRY_CODE",
  "CAE_NAME",
  "ISO_COUNTRY_CODE",
  "DT_AWARD",
  "CPV",
  "AWARD_VALUE_EURO",
  "AWARD_VALUE_EURO_FIN_1",
  "TOP_TYPE",
  "CRIT_CODE",
  "CANCELLED",
  "source_year"
)

available_columns <- intersect(target_columns, names(ted_raw))
missing_columns <- setdiff(target_columns, names(ted_raw))

if (length(missing_columns) > 0) {
  warning(
    "The following expected columns were not found: ",
    paste(missing_columns, collapse = ", ")
  )
}

ted_sel <- ted_raw %>%
  dplyr::select(dplyr::all_of(available_columns)) %>%
  tibble::as_tibble()

# -----------------------------------------------------------
# 6) Base cleaning
# -----------------------------------------------------------
ted_clean <- ted_sel %>%
  dplyr::mutate(
    ID_NOTICE_CAN    = clean_text(ID_NOTICE_CAN),
    ID_AWARD         = clean_text(ID_AWARD),
    WIN_NAME         = clean_text(WIN_NAME),
    WIN_COUNTRY_CODE = clean_upper(WIN_COUNTRY_CODE),
    CAE_NAME         = clean_text(CAE_NAME),
    ISO_COUNTRY_CODE = clean_upper(ISO_COUNTRY_CODE),
    DT_AWARD         = clean_text(DT_AWARD),
    CPV              = clean_text(CPV),
    TOP_TYPE         = clean_upper(TOP_TYPE),
    CRIT_CODE        = clean_upper(CRIT_CODE),
    CANCELLED        = clean_upper(CANCELLED)
  )

n_cancelled_removed <- 0L

if ("CANCELLED" %in% names(ted_clean)) {
  before_n <- nrow(ted_clean)
  
  ted_clean <- ted_clean %>%
    dplyr::filter(
      is.na(CANCELLED) |
        !(CANCELLED %in% c("1", "Y", "YES", "TRUE", "T"))
    )
  
  n_cancelled_removed <- before_n - nrow(ted_clean)
}

before_supplier_filter <- nrow(ted_clean)
ted_clean <- ted_clean %>%
  dplyr::filter(!is.na(WIN_NAME))
n_missing_supplier_removed <- before_supplier_filter - nrow(ted_clean)

before_date_filter <- nrow(ted_clean)
ted_clean <- ted_clean %>%
  dplyr::filter(!is.na(DT_AWARD))
n_missing_date_removed <- before_date_filter - nrow(ted_clean)

ted_clean <- ted_clean %>%
  dplyr::mutate(
    DT_AWARD_RAW = DT_AWARD,
    DT_AWARD = as.Date(DT_AWARD, format = "%d/%m/%y"),
    award_year = as.integer(format(DT_AWARD, "%Y"))
  )

n_invalid_date_parse <- sum(is.na(ted_clean$DT_AWARD) & !is.na(ted_clean$DT_AWARD_RAW))

before_period_filter <- nrow(ted_clean)
ted_clean <- ted_clean %>%
  dplyr::filter(!is.na(award_year) & award_year >= 2018 & award_year <= 2023)
n_out_of_scope_or_invalid_year_removed <- before_period_filter - nrow(ted_clean)

ted_clean <- ted_clean %>%
  dplyr::mutate(
    WIN_NAME_CLEAN = stringr::str_squish(stringr::str_to_upper(WIN_NAME)),
    AWARD_VALUE_EURO_FIN_1 = if ("AWARD_VALUE_EURO_FIN_1" %in% names(.)) parse_ted_numeric(AWARD_VALUE_EURO_FIN_1) else NA_real_,
    AWARD_VALUE_EURO       = if ("AWARD_VALUE_EURO" %in% names(.)) parse_ted_numeric(AWARD_VALUE_EURO) else NA_real_,
    award_value = dplyr::coalesce(AWARD_VALUE_EURO_FIN_1, AWARD_VALUE_EURO),
    award_value = dplyr::if_else(!is.na(award_value) & award_value <= 0, NA_real_, award_value),
    CPV = stringr::str_replace_all(CPV, "[^0-9]", ""),
    CPV_2digit = dplyr::if_else(!is.na(CPV) & nchar(CPV) >= 2, stringr::str_sub(CPV, 1, 2), NA_character_)
  )

n_after_cleaning <- nrow(ted_clean)
message("Rows after base cleaning: ", n_after_cleaning)

# -----------------------------------------------------------
# 6b) CPV division validation
# -----------------------------------------------------------
# Purpose: Table 2.2 (row 4) claims "All 45 official CPV divisions present;
# no malformed or out-of-range codes detected," but nothing in this script
# or in cleaning_log.csv previously supported that claim. This block adds
# the missing validation:
#   (i)  confirms every non-missing CPV_2digit value is a well-formed
#        two-digit numeric string -- guaranteed by construction above
#        (anything shorter than 2 digits is already set to NA), but
#        verified explicitly here rather than assumed;
#   (ii) counts how many distinct divisions are actually observed in the
#        cleaned data, so the Table 2.2 wording can be tied to a real
#        number either way. This does not require an external CPV
#        codelist -- it only reports what is actually present.
cpv_2digit_nonmissing <- ted_clean$CPV_2digit[!is.na(ted_clean$CPV_2digit)]

n_cpv_2digit_malformed <- sum(!grepl("^[0-9]{2}$", cpv_2digit_nonmissing))

cpv_division_validation <- tibble::tibble(CPV_2digit = cpv_2digit_nonmissing) %>%
  dplyr::count(CPV_2digit, name = "n") %>%
  dplyr::mutate(share = n / sum(n)) %>%
  dplyr::arrange(CPV_2digit)

n_distinct_cpv_divisions <- nrow(cpv_division_validation)

message(
  "CPV division validation: ", n_distinct_cpv_divisions,
  " distinct two-digit divisions observed; ",
  n_cpv_2digit_malformed, " malformed CPV_2digit values (expect 0)."
)

cpv_division_validation_file <- here::here("logs", "cpv_division_validation.csv")
readr::write_csv(cpv_division_validation, cpv_division_validation_file)

message("CPV division validation saved to: ", cpv_division_validation_file)

# -----------------------------------------------------------
# 7) Deduplication with diagnostics
# -----------------------------------------------------------
n_exact_dup_rows <- sum(duplicated(ted_clean))

if (n_exact_dup_rows > 0) {
  ted_clean <- ted_clean %>%
    dplyr::distinct()
}

message("Exact duplicate rows removed: ", n_exact_dup_rows)

n_dup_id_award <- NA_integer_
n_safe_groups <- 0L
n_conflict_groups <- 0L
n_rows_removed_safe_dedup <- 0L
n_crit_code_ambiguous_groups <- 0L

if ("ID_AWARD" %in% names(ted_clean)) {
  dup_groups <- ted_clean %>%
    dplyr::filter(!is.na(ID_AWARD)) %>%
    dplyr::group_by(ID_AWARD) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()
  
  n_dup_id_award <- dup_groups %>%
    dplyr::distinct(ID_AWARD) %>%
    nrow()
  
  if (n_dup_id_award > 0) {
    key_fields <- intersect(
      c(
        "ID_AWARD",
        "WIN_NAME_CLEAN",
        "WIN_COUNTRY_CODE",
        "CAE_NAME",
        "ISO_COUNTRY_CODE",
        "DT_AWARD",
        "award_year",
        "award_value",
        "CPV",
        "CPV_2digit",
        "TOP_TYPE"
      ),
      names(ted_clean)
    )
    
    dup_profile <- dup_groups %>%
      dplyr::group_by(ID_AWARD) %>%
      dplyr::summarise(
        n_rows = dplyr::n(),
        dplyr::across(
          dplyr::all_of(setdiff(key_fields, "ID_AWARD")),
          ~ dplyr::n_distinct(.x, na.rm = FALSE),
          .names = "ndist_{.col}"
        ),
        n_distinct_crit_nonmissing = dplyr::n_distinct(CRIT_CODE[!is.na(CRIT_CODE)]),
        resolved_crit_code = resolve_crit_code(CRIT_CODE),
        .groups = "drop"
      )
    
    consistency_cols <- grep("^ndist_", names(dup_profile), value = TRUE)
    
    safe_ids <- dup_profile %>%
      dplyr::filter(
        dplyr::if_all(dplyr::all_of(consistency_cols), ~ .x <= 1)
      ) %>%
      dplyr::pull(ID_AWARD)
    
    conflict_ids <- dup_profile %>%
      dplyr::filter(
        !dplyr::if_all(dplyr::all_of(consistency_cols), ~ .x <= 1)
      ) %>%
      dplyr::pull(ID_AWARD)
    
    n_safe_groups <- length(safe_ids)
    n_conflict_groups <- length(conflict_ids)
    
    safe_resolution <- dup_profile %>%
      dplyr::filter(ID_AWARD %in% safe_ids) %>%
      dplyr::transmute(
        ID_AWARD,
        resolved_crit_code,
        crit_code_ambiguous = n_distinct_crit_nonmissing > 1
      )
    
    n_crit_code_ambiguous_groups <- sum(safe_resolution$crit_code_ambiguous, na.rm = TRUE)
    
    if (n_conflict_groups > 0 || n_crit_code_ambiguous_groups > 0) {
      ids_to_log <- c(
        conflict_ids,
        safe_resolution %>%
          dplyr::filter(crit_code_ambiguous) %>%
          dplyr::pull(ID_AWARD)
      ) %>% unique()
      
      ted_clean %>%
        dplyr::filter(ID_AWARD %in% ids_to_log) %>%
        dplyr::arrange(ID_AWARD) %>%
        readr::write_csv(conflict_file)
      
      message("Conflicting / ambiguous duplicated ID_AWARD groups saved to: ", conflict_file)
    }
    
    before_safe_dedup <- nrow(ted_clean)
    
    safe_deduped <- ted_clean %>%
      dplyr::filter(ID_AWARD %in% safe_ids) %>%
      dplyr::group_by(ID_AWARD) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(safe_resolution, by = "ID_AWARD") %>%
      dplyr::mutate(
        CRIT_CODE = resolved_crit_code
      ) %>%
      dplyr::select(-resolved_crit_code, -crit_code_ambiguous)
    
    ted_clean <- ted_clean %>%
      dplyr::filter(!(ID_AWARD %in% safe_ids)) %>%
      dplyr::bind_rows(safe_deduped)
    
    n_rows_removed_safe_dedup <- before_safe_dedup - nrow(ted_clean)
  }
}

n_after_dedup <- nrow(ted_clean)

message("Duplicated ID_AWARD groups detected: ", ifelse(is.na(n_dup_id_award), "NA", n_dup_id_award))
message("Safe duplicated ID_AWARD groups deduplicated: ", n_safe_groups)
message("Conflicting duplicated ID_AWARD groups flagged only: ", n_conflict_groups)
message("Safe duplicated groups with ambiguous CRIT_CODE set to NA: ", n_crit_code_ambiguous_groups)
message("Rows removed by safe ID_AWARD deduplication: ", n_rows_removed_safe_dedup)
message("Rows after deduplication step: ", n_after_dedup)

# -----------------------------------------------------------
# 8) Save cleaned output
# -----------------------------------------------------------
ted_clean <- ted_clean %>%
  dplyr::arrange(award_year, WIN_NAME_CLEAN, ID_AWARD)

readr::write_csv(ted_clean, output_file)

message("Saved cleaned award-level dataset to: ", output_file)

# -----------------------------------------------------------
# 9) Save cleaning log
# -----------------------------------------------------------
log_clean <- tibble::tibble(
  metric = c(
    "raw_rows",
    "cancelled_rows_removed",
    "missing_supplier_rows_removed",
    "missing_award_date_rows_removed",
    "invalid_date_parse_rows",
    "out_of_scope_or_invalid_year_rows_removed",
    "after_cleaning",
    "exact_duplicate_rows_removed",
    "duplicated_id_award_groups",
    "safe_id_award_groups_deduped",
    "conflicting_id_award_groups_flagged",
    "safe_groups_with_ambiguous_crit_code",
    "rows_removed_safe_id_award_dedup",
    "after_dedup",
    "n_missing_award_value",
    "n_missing_supplier_clean",
    "n_missing_crit_code_after_dedup",
    "n_distinct_cpv_divisions",
    "n_cpv_2digit_malformed"
  ),
  value = c(
    n_raw_rows,
    n_cancelled_removed,
    n_missing_supplier_removed,
    n_missing_date_removed,
    n_invalid_date_parse,
    n_out_of_scope_or_invalid_year_removed,
    n_after_cleaning,
    n_exact_dup_rows,
    ifelse(is.na(n_dup_id_award), NA, n_dup_id_award),
    n_safe_groups,
    n_conflict_groups,
    n_crit_code_ambiguous_groups,
    n_rows_removed_safe_dedup,
    n_after_dedup,
    sum(is.na(ted_clean$award_value)),
    sum(is.na(ted_clean$WIN_NAME_CLEAN) | ted_clean$WIN_NAME_CLEAN == ""),
    sum(is.na(ted_clean$CRIT_CODE)),
    n_distinct_cpv_divisions,
    n_cpv_2digit_malformed
  )
)

readr::write_csv(log_clean, log_file)

message("Cleaning log saved to: ", log_file)
message("02_cleaning.R completed successfully.")
message("Ready for 03_panel_build.R")