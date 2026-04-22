# 04_regression.R
# Purpose: estimate the determinants of the number and total value
# of contract awards at supplier-year level (RQ1)

rm(list = ls())

options(stringsAsFactors = FALSE, scipen = 999)

# -----------------------------------------------------------
# 1) Package management
# -----------------------------------------------------------
required_pkgs <- c(
  "here", "readr", "dplyr", "stringr", "fs", "tibble", "tidyr",
  "ggplot2", "fixest", "modelsummary", "corrplot"
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

input_file <- here::here("data", "processed", "supplier_year_panel.csv")
output_dir <- here::here("output")
fig_dir    <- here::here("output", "figures")
tab_dir    <- here::here("output", "tables")
log_dir    <- here::here("logs")

invisible(lapply(c(output_dir, fig_dir, tab_dir, log_dir), fs::dir_create))

if (!fs::file_exists(input_file)) {
  stop(
    "Missing panel input file: ", input_file,
    ". Run 03_panel_build.R first."
  )
}

# -----------------------------------------------------------
# 3) Read supplier-year panel
# -----------------------------------------------------------
message("Reading supplier_year_panel.csv ...")
panel <- readr::read_csv(input_file, show_col_types = FALSE)

message("Rows read: ", nrow(panel))
message("Columns available: ", ncol(panel))

required_cols <- c(
  "WIN_NAME_CLEAN",
  "award_year",
  "WIN_COUNTRY_CODE",
  "top_procedure",
  "awards_count",
  "total_award_value",
  "avg_award_value",
  "log_awards_count",
  "log_total_value",
  "distinct_buyers",
  "distinct_buyer_countries",
  "cross_border_share",
  "distinct_cpv_divisions",
  "specialization_share",
  "hhi_cpv",
  "years_active",
  "award_frequency",
  "is_consortium",
  "price_criteria_share"
)

missing_cols <- setdiff(required_cols, names(panel))
if (length(missing_cols) > 0) {
  stop(
    "The following required columns are missing from supplier_year_panel.csv: ",
    paste(missing_cols, collapse = ", ")
  )
}

# -----------------------------------------------------------
# 4) Helpers
# -----------------------------------------------------------
clean_procedure <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x[is.na(x) | x == ""] <- "UNKNOWN"
  x
}

common_filters <- function(df) {
  df %>%
    dplyr::filter(
      !is.na(z_distinct_buyers),
      !is.na(z_distinct_buyer_countries),
      !is.na(z_cross_border_share),
      !is.na(z_hhi_cpv),
      !is.na(z_years_active),
      !is.na(is_consortium),
      !is.na(country_fe),
      !is.na(year_fe),
      !is.na(WIN_NAME_CLEAN),
      WIN_NAME_CLEAN != ""
    )
}

# -----------------------------------------------------------
# 5) Descriptive statistics
# -----------------------------------------------------------
desc_vars <- c(
  "awards_count",
  "total_award_value",
  "avg_award_value",
  "distinct_buyers",
  "distinct_buyer_countries",
  "cross_border_share",
  "distinct_cpv_divisions",
  "specialization_share",
  "hhi_cpv",
  "years_active",
  "award_frequency",
  "price_criteria_share",
  "is_consortium"
)

desc_stats <- panel %>%
  dplyr::select(dplyr::all_of(desc_vars)) %>%
  dplyr::summarise(
    dplyr::across(
      everything(),
      list(
        n = ~ sum(!is.na(.)),
        mean = ~ mean(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE),
        median = ~ median(., na.rm = TRUE),
        min = ~ min(., na.rm = TRUE),
        max = ~ max(., na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    )
  ) %>%
  tidyr::pivot_longer(
    everything(),
    names_to = c("variable", "stat"),
    names_sep = "__"
  ) %>%
  tidyr::pivot_wider(
    names_from = stat,
    values_from = value
  )

readr::write_csv(desc_stats, here::here("output", "tables", "descriptive_stats.csv"))
message("Descriptive statistics saved.")

# -----------------------------------------------------------
# 6) Regression sample preparation
# -----------------------------------------------------------
panel_model <- panel %>%
  dplyr::mutate(
    WIN_NAME_CLEAN = stringr::str_squish(as.character(WIN_NAME_CLEAN)),
    top_procedure = clean_procedure(top_procedure),
    country_fe = as.factor(WIN_COUNTRY_CODE),
    year_fe = as.factor(award_year),
    proc_fe = as.factor(top_procedure),
    z_distinct_buyers = as.numeric(scale(distinct_buyers)),
    z_distinct_buyer_countries = as.numeric(scale(distinct_buyer_countries)),
    z_cross_border_share = as.numeric(scale(cross_border_share)),
    z_hhi_cpv = as.numeric(scale(hhi_cpv)),
    z_years_active = as.numeric(scale(years_active)),
    z_price_criteria_share = as.numeric(scale(price_criteria_share))
  )

panel_count_base <- panel_model %>%
  common_filters() %>%
  dplyr::filter(!is.na(log_awards_count))

panel_value_base <- panel_model %>%
  common_filters() %>%
  dplyr::filter(!is.na(log_total_value), !is.na(total_award_value))

panel_count_ext <- panel_count_base %>%
  dplyr::filter(!is.na(z_price_criteria_share))

panel_value_ext <- panel_value_base %>%
  dplyr::filter(!is.na(z_price_criteria_share))

panel_count_rob2 <- panel_count_ext %>%
  dplyr::filter(awards_count >= 2)

panel_value_rob2 <- panel_value_ext %>%
  dplyr::filter(awards_count >= 2)

value_trim_quantile <- 0.99
value_trim_cutoff <- as.numeric(
  stats::quantile(
    panel_value_ext$total_award_value,
    probs = value_trim_quantile,
    na.rm = TRUE,
    names = FALSE,
    type = 7
  )
)

if (!is.finite(value_trim_cutoff)) {
  stop("Could not compute the value-trimming cutoff.")
}

panel_value_trim <- panel_value_ext %>%
  dplyr::filter(total_award_value <= value_trim_cutoff)

message("Observations in count baseline sample: ", nrow(panel_count_base))
message("Observations in value baseline sample: ", nrow(panel_value_base))
message("Observations in count extended sample: ", nrow(panel_count_ext))
message("Observations in value extended sample: ", nrow(panel_value_ext))
message("Observations in count robustness sample (>=2 awards): ", nrow(panel_count_rob2))
message("Observations in value robustness sample (>=2 awards): ", nrow(panel_value_rob2))
message("Observations in value trimmed sample (top 1% removed): ", nrow(panel_value_trim))
message("Value trim cutoff (99th percentile): ", round(value_trim_cutoff, 2))
message("Procedure categories in count extended sample: ", dplyr::n_distinct(panel_count_ext$proc_fe))
message("Procedure categories in value extended sample: ", dplyr::n_distinct(panel_value_ext$proc_fe))

# -----------------------------------------------------------
# 7) Correlation matrix
# -----------------------------------------------------------
corr_df <- panel_value_ext %>%
  dplyr::select(
    log_awards_count,
    log_total_value,
    z_distinct_buyers,
    z_distinct_buyer_countries,
    z_cross_border_share,
    z_hhi_cpv,
    z_years_active,
    z_price_criteria_share,
    is_consortium
  )

corr_mat <- cor(corr_df, use = "complete.obs")

readr::write_csv(
  tibble::as_tibble(corr_mat, rownames = "variable"),
  here::here("output", "tables", "correlation_matrix.csv")
)

png(
  filename = here::here("output", "figures", "correlation_matrix.png"),
  width = 900,
  height = 900
)

corrplot::corrplot(
  corr_mat,
  method = "color",
  type = "upper",
  tl.cex = 0.8,
  addCoef.col = "black",
  number.cex = 0.7
)

dev.off()

message("Correlation matrix saved.")

# -----------------------------------------------------------
# 8) Regression models
# -----------------------------------------------------------
modA1 <- fixest::feols(
  log_awards_count ~
    z_distinct_buyers +
    z_distinct_buyer_countries +
    z_cross_border_share +
    z_hhi_cpv +
    z_years_active +
    is_consortium | year_fe,
  cluster = ~ WIN_NAME_CLEAN,
  data = panel_count_base
)

modB1 <- fixest::feols(
  log_total_value ~
    z_distinct_buyers +
    z_distinct_buyer_countries +
    z_cross_border_share +
    z_hhi_cpv +
    z_years_active +
    is_consortium | year_fe,
  cluster = ~ WIN_NAME_CLEAN,
  data = panel_value_base
)

modA2 <- fixest::feols(
  log_awards_count ~
    z_distinct_buyers +
    z_distinct_buyer_countries +
    z_cross_border_share +
    z_hhi_cpv +
    z_years_active | year_fe + country_fe,
  cluster = ~ WIN_NAME_CLEAN,
  data = panel_count_base
)

modB2 <- fixest::feols(
  log_total_value ~
    z_distinct_buyers +
    z_distinct_buyer_countries +
    z_cross_border_share +
    z_hhi_cpv +
    z_years_active | year_fe + country_fe,
  cluster = ~ WIN_NAME_CLEAN,
  data = panel_value_base
)

modA2m <- fixest::feols(
  log_awards_count ~
    z_distinct_buyers +
    z_distinct_buyer_countries +
    z_cross_border_share +
    z_hhi_cpv +
    z_years_active | year_fe + country_fe,
  cluster = ~ WIN_NAME_CLEAN,
  data = panel_count_ext
)

modB2m <- fixest::feols(
  log_total_value ~
    z_distinct_buyers +
    z_distinct_buyer_countries +
    z_cross_border_share +
    z_hhi_cpv +
    z_years_active | year_fe + country_fe,
  cluster = ~ WIN_NAME_CLEAN,
  data = panel_value_ext
)

modA3 <- fixest::feols(
  log_awards_count ~
    z_distinct_buyers +
    z_distinct_buyer_countries +
    z_cross_border_share +
    z_hhi_cpv +
    z_years_active +
    z_price_criteria_share | year_fe + country_fe,
  cluster = ~ WIN_NAME_CLEAN,
  data = panel_count_ext
)

modB3 <- fixest::feols(
  log_total_value ~
    z_distinct_buyers +
    z_distinct_buyer_countries +
    z_cross_border_share +
    z_hhi_cpv +
    z_years_active +
    z_price_criteria_share | year_fe + country_fe,
  cluster = ~ WIN_NAME_CLEAN,
  data = panel_value_ext
)

modA4 <- fixest::feols(
  log_awards_count ~
    z_distinct_buyers +
    z_distinct_buyer_countries +
    z_cross_border_share +
    z_hhi_cpv +
    z_years_active +
    z_price_criteria_share | year_fe + country_fe + proc_fe,
  cluster = ~ WIN_NAME_CLEAN,
  data = panel_count_ext
)

modB4 <- fixest::feols(
  log_total_value ~
    z_distinct_buyers +
    z_distinct_buyer_countries +
    z_cross_border_share +
    z_hhi_cpv +
    z_years_active +
    z_price_criteria_share | year_fe + country_fe + proc_fe,
  cluster = ~ WIN_NAME_CLEAN,
  data = panel_value_ext
)

modA5 <- fixest::feols(
  log_awards_count ~
    z_distinct_buyers +
    z_distinct_buyer_countries +
    z_cross_border_share +
    z_hhi_cpv +
    z_years_active +
    z_price_criteria_share | year_fe + country_fe + proc_fe,
  cluster = ~ WIN_NAME_CLEAN,
  data = panel_count_rob2
)

modB5 <- fixest::feols(
  log_total_value ~
    z_distinct_buyers +
    z_distinct_buyer_countries +
    z_cross_border_share +
    z_hhi_cpv +
    z_years_active +
    z_price_criteria_share | year_fe + country_fe + proc_fe,
  cluster = ~ WIN_NAME_CLEAN,
  data = panel_value_rob2
)

modB6 <- fixest::feols(
  log_total_value ~
    z_distinct_buyers +
    z_distinct_buyer_countries +
    z_cross_border_share +
    z_hhi_cpv +
    z_years_active +
    z_price_criteria_share | year_fe + country_fe + proc_fe,
  cluster = ~ WIN_NAME_CLEAN,
  data = panel_value_trim
)

# -----------------------------------------------------------
# 9) Save regression tables
# -----------------------------------------------------------
coef_map_common <- c(
  "z_distinct_buyers" = "Buyer diversification",
  "z_distinct_buyer_countries" = "Buyer-country diversification",
  "z_cross_border_share" = "Cross-border share",
  "z_hhi_cpv" = "Sector concentration (HHI)",
  "z_years_active" = "Years active",
  "is_consortium" = "Consortium",
  "z_price_criteria_share" = "Price criterion share"
)

gof_map_common <- tibble::tribble(
  ~raw, ~clean, ~fmt,
  "nobs", "N", 0,
  "adj.r.squared", "Adj. R2", 3,
  "within.r.squared", "Within R2", 3,
  "rmse", "RMSE", 3
)

modelsummary::modelsummary(
  list(
    "Awards Count (1) Year FE" = modA1,
    "Awards Count (2) Baseline FE" = modA2,
    "Awards Count (2m) Baseline FE Matched Sample" = modA2m,
    "Awards Count (3) + Price Criterion" = modA3,
    "Awards Count (4) + Procedure FE" = modA4,
    "Total Value (1) Year FE" = modB1,
    "Total Value (2) Baseline FE" = modB2,
    "Total Value (2m) Baseline FE Matched Sample" = modB2m,
    "Total Value (3) + Price Criterion" = modB3,
    "Total Value (4) + Procedure FE" = modB4
  ),
  coef_map = coef_map_common,
  gof_map = gof_map_common,
  stars = TRUE,
  output = here::here("output", "tables", "regression_main.csv")
)

modelsummary::modelsummary(
  list(
    "Awards Count Robustness (>=2 awards, + Procedure FE)" = modA5,
    "Total Value Robustness (>=2 awards, + Procedure FE)" = modB5,
    "Total Value Robustness (top 1% trimmed, + Procedure FE)" = modB6
  ),
  coef_map = coef_map_common,
  gof_map = gof_map_common,
  stars = TRUE,
  output = here::here("output", "tables", "regression_robustness.csv")
)

message("Regression tables saved.")

# -----------------------------------------------------------
# 9b) Incremental comparison table
# -----------------------------------------------------------
coef_map_inc <- c(
  "z_distinct_buyers" = "Buyer diversification",
  "z_distinct_buyer_countries" = "Buyer-country diversification",
  "z_cross_border_share" = "Cross-border share",
  "z_hhi_cpv" = "Sector concentration (HHI)",
  "z_years_active" = "Years active",
  "z_price_criteria_share" = "Price criterion share"
)

gof_map_inc <- tibble::tribble(
  ~raw, ~clean, ~fmt,
  "nobs", "N", 0,
  "adj.r.squared", "Adj. R2", 3,
  "within.r.squared", "Within R2", 3,
  "rmse", "RMSE", 3
)

modelsummary::modelsummary(
  list(
    "Awards: Baseline matched" = modA2m,
    "Awards: + Price criterion" = modA3,
    "Awards: + Procedure FE" = modA4,
    "Value: Baseline matched" = modB2m,
    "Value: + Price criterion" = modB3,
    "Value: + Procedure FE" = modB4
  ),
  coef_map = coef_map_inc,
  gof_map = gof_map_inc,
  stars = TRUE,
  output = here::here("output", "tables", "regression_incremental.csv")
)

message("Incremental comparison table saved.")

# -----------------------------------------------------------
# 10) Save regression log
# -----------------------------------------------------------
reg_log <- tibble::tibble(
  metric = c(
    "panel_rows_input",
    "rows_count_baseline",
    "rows_value_baseline",
    "rows_count_extended",
    "rows_value_extended",
    "rows_count_robustness_ge2",
    "rows_value_robustness_ge2",
    "rows_value_trimmed_p99",
    "unique_suppliers_count_baseline",
    "unique_suppliers_value_baseline",
    "unique_suppliers_count_extended",
    "unique_suppliers_value_extended",
    "unique_suppliers_count_robustness_ge2",
    "unique_suppliers_value_robustness_ge2",
    "unique_suppliers_value_trimmed_p99",
    "countries_count_baseline",
    "countries_value_baseline",
    "countries_count_extended",
    "countries_value_extended",
    "years_count_baseline",
    "years_value_baseline",
    "procedures_count_extended",
    "procedures_value_extended",
    "unknown_procedure_share_count_extended",
    "unknown_procedure_share_value_extended",
    "missing_total_award_value_input",
    "missing_price_criteria_share_input",
    "value_trim_quantile",
    "value_trim_cutoff"
  ),
  value = c(
    nrow(panel),
    nrow(panel_count_base),
    nrow(panel_value_base),
    nrow(panel_count_ext),
    nrow(panel_value_ext),
    nrow(panel_count_rob2),
    nrow(panel_value_rob2),
    nrow(panel_value_trim),
    dplyr::n_distinct(panel_count_base$WIN_NAME_CLEAN),
    dplyr::n_distinct(panel_value_base$WIN_NAME_CLEAN),
    dplyr::n_distinct(panel_count_ext$WIN_NAME_CLEAN),
    dplyr::n_distinct(panel_value_ext$WIN_NAME_CLEAN),
    dplyr::n_distinct(panel_count_rob2$WIN_NAME_CLEAN),
    dplyr::n_distinct(panel_value_rob2$WIN_NAME_CLEAN),
    dplyr::n_distinct(panel_value_trim$WIN_NAME_CLEAN),
    dplyr::n_distinct(panel_count_base$country_fe),
    dplyr::n_distinct(panel_value_base$country_fe),
    dplyr::n_distinct(panel_count_ext$country_fe),
    dplyr::n_distinct(panel_value_ext$country_fe),
    dplyr::n_distinct(panel_count_base$year_fe),
    dplyr::n_distinct(panel_value_base$year_fe),
    dplyr::n_distinct(panel_count_ext$proc_fe),
    dplyr::n_distinct(panel_value_ext$proc_fe),
    mean(panel_count_ext$top_procedure == "UNKNOWN", na.rm = TRUE),
    mean(panel_value_ext$top_procedure == "UNKNOWN", na.rm = TRUE),
    sum(is.na(panel$total_award_value)),
    sum(is.na(panel$price_criteria_share)),
    value_trim_quantile,
    value_trim_cutoff
  )
)

readr::write_csv(reg_log, here::here("logs", "regression_log.csv"))
message("Regression log saved.")

# -----------------------------------------------------------
# 11) Console summaries
# -----------------------------------------------------------
cat("\n--- MODEL A1: log awards count, year FE ---\n")
print(summary(modA1))

cat("\n--- MODEL A2: log awards count, baseline with year + country FE ---\n")
print(summary(modA2))

cat("\n--- MODEL A2m: log awards count, matched-sample baseline ---\n")
print(summary(modA2m))

cat("\n--- MODEL A3: log awards count, extended with price criterion ---\n")
print(summary(modA3))

cat("\n--- MODEL A4: log awards count, + procedure FE ---\n")
print(summary(modA4))

cat("\n--- MODEL A5: log awards count, robustness >=2 awards + procedure FE ---\n")
print(summary(modA5))

cat("\n--- MODEL B1: log total value, year FE ---\n")
print(summary(modB1))

cat("\n--- MODEL B2: log total value, baseline with year + country FE ---\n")
print(summary(modB2))

cat("\n--- MODEL B2m: log total value, matched-sample baseline ---\n")
print(summary(modB2m))

cat("\n--- MODEL B3: log total value, extended with price criterion ---\n")
print(summary(modB3))

cat("\n--- MODEL B4: log total value, + procedure FE ---\n")
print(summary(modB4))

cat("\n--- MODEL B5: log total value, robustness >=2 awards + procedure FE ---\n")
print(summary(modB5))

cat("\n--- MODEL B6: log total value, robustness top 1% trimmed + procedure FE ---\n")
print(summary(modB6))

message("04_regression.R completed successfully.")
message("Ready for 05_clustering.R")