# 05_clustering.R
# Purpose: build supplier-level profiles and segment suppliers with K-means (RQ2)

rm(list = ls())

options(stringsAsFactors = FALSE, scipen = 999)

# -----------------------------------------------------------
# 1) Package management
# -----------------------------------------------------------
required_pkgs <- c(
  "here", "readr", "dplyr", "tidyr", "tibble",
  "ggplot2", "fs", "cluster", "stringr"
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
proc_dir   <- here::here("data", "processed")
log_dir    <- here::here("logs")

invisible(lapply(c(output_dir, fig_dir, tab_dir, proc_dir, log_dir), fs::dir_create))

if (!fs::file_exists(input_file)) {
  stop(
    "Missing panel input file: ", input_file,
    ". Run 03_panel_build.R first."
  )
}

# -----------------------------------------------------------
# 3) Clustering settings
# -----------------------------------------------------------
final_k <- 5
robust_same_k <- final_k
robust_alt_k <- 4
k_grid <- 2:8

silhouette_sample_n <- 5000
silhouette_reps <- 3

kmeans_nstart_diag <- 10
kmeans_nstart_final <- 50
kmeans_iter_max <- 200
kmeans_algorithm <- "Lloyd"

diag_retry_max_tries <- 5
diag_retry_seed_step <- 1000

low_value_threshold <- 100
plot_n_max <- 50000

# -----------------------------------------------------------
# 4) Helper functions
# -----------------------------------------------------------
na_mean <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

na_median <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  median(x, na.rm = TRUE)
}

na_sum <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

na_max <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

mode_char <- function(x) {
  x <- x[!is.na(x) & trimws(x) != ""]
  if (length(x) == 0) return(NA_character_)
  tab <- sort(table(x), decreasing = TRUE)
  names(tab)[1]
}

weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  weighted.mean(x[ok], w[ok], na.rm = TRUE)
}

prepare_cluster_matrix <- function(df, vars) {
  out <- df %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    scale() %>%
    as.matrix()
  
  if (!all(is.finite(out))) {
    stop("Non-finite values found in the standardized clustering matrix.")
  }
  
  out
}

make_cluster_summary <- function(df, cluster_var = "cluster") {
  df %>%
    dplyr::filter(!is.na(.data[[cluster_var]])) %>%
    dplyr::group_by(.data[[cluster_var]]) %>%
    dplyr::summarise(
      n_suppliers = dplyr::n(),
      median_awards_per_year = round(na_median(avg_awards_per_year), 2),
      median_contract_value = round(na_median(avg_contract_value), 2),
      median_buyer_countries = round(na_median(avg_buyer_countries), 2),
      median_cross_border = round(na_median(avg_cross_border_share), 3),
      median_hhi_cpv = round(na_median(avg_hhi_cpv), 3),
      median_years_active = round(na_median(years_active), 2),
      pct_consortium = round(mean(supplier_is_consortium == 1, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    dplyr::rename(cluster_id = 1) %>%
    dplyr::arrange(cluster_id)
}

make_cluster_summary_means <- function(df, cluster_var = "cluster") {
  df %>%
    dplyr::filter(!is.na(.data[[cluster_var]])) %>%
    dplyr::group_by(.data[[cluster_var]]) %>%
    dplyr::summarise(
      n_suppliers = dplyr::n(),
      avg_awards_per_year = round(na_mean(avg_awards_per_year), 2),
      avg_contract_value = round(na_mean(avg_contract_value), 2),
      avg_buyer_countries = round(na_mean(avg_buyer_countries), 2),
      avg_cross_border = round(na_mean(avg_cross_border_share), 3),
      avg_hhi_cpv = round(na_mean(avg_hhi_cpv), 3),
      avg_years_active = round(na_mean(years_active), 2),
      pct_consortium = round(mean(supplier_is_consortium == 1, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    dplyr::rename(cluster_id = 1) %>%
    dplyr::arrange(cluster_id)
}

make_cluster_size_table <- function(cluster_vec, cluster_name = "cluster") {
  tibble::tibble(!!cluster_name := cluster_vec) %>%
    dplyr::count(.data[[cluster_name]], name = "n_suppliers") %>%
    dplyr::mutate(share_suppliers = n_suppliers / sum(n_suppliers)) %>%
    dplyr::arrange(.data[[cluster_name]])
}

run_kmeans <- function(x, k, seed, nstart, iter.max, algorithm) {
  set.seed(seed)
  stats::kmeans(
    x,
    centers = k,
    nstart = nstart,
    iter.max = iter.max,
    algorithm = algorithm
  )
}

run_kmeans_retry <- function(
    x, k, seed, nstart, iter.max, algorithm,
    max_tries = 5, seed_step = 1000
) {
  last_warning <- NA_character_
  
  for (attempt in seq_len(max_tries)) {
    current_seed <- seed + (attempt - 1) * seed_step
    warning_msg <- NULL
    
    km_fit <- withCallingHandlers(
      {
        set.seed(current_seed)
        stats::kmeans(
          x,
          centers = k,
          nstart = nstart,
          iter.max = iter.max,
          algorithm = algorithm
        )
      },
      warning = function(w) {
        warning_msg <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
    
    has_empty_cluster_warning <- !is.null(warning_msg) &&
      grepl("empty cluster", warning_msg, ignore.case = TRUE)
    
    has_all_clusters <- length(unique(km_fit$cluster)) == k
    
    if (!has_empty_cluster_warning && has_all_clusters) {
      attr(km_fit, "used_seed") <- current_seed
      attr(km_fit, "attempts") <- attempt
      attr(km_fit, "warning_msg") <- warning_msg
      return(km_fit)
    }
    
    last_warning <- if (is.null(warning_msg)) NA_character_ else warning_msg
  }
  
  stop(
    "K-means failed after ", max_tries,
    " attempts for k = ", k,
    ". Last warning: ", last_warning
  )
}

make_cluster_labels <- function(summary_df, cluster_col = "cluster_id") {
  out <- summary_df %>%
    dplyr::mutate(
      provisional_label = dplyr::case_when(
        pct_consortium > 50 & median_cross_border > 0.5 ~ "cross-border consortium suppliers",
        median_awards_per_year > 2.5 & median_hhi_cpv < 0.9 ~ "higher-volume diversified suppliers",
        median_contract_value <= low_value_threshold ~ "anomalous low-value suppliers",
        median_cross_border > 0.5 ~ "cross-border suppliers",
        median_hhi_cpv >= 0.95 & median_awards_per_year <= 1.5 ~ "local mono-sector suppliers",
        TRUE ~ "mixed supplier profile"
      ),
      interpretation_note = dplyr::case_when(
        provisional_label == "anomalous low-value suppliers" ~
          "Likely influenced by data-quality issues in TED contract values.",
        provisional_label == "cross-border consortium suppliers" ~
          "High consortium incidence and cross-border orientation.",
        provisional_label == "higher-volume diversified suppliers" ~
          "More active suppliers with lower sector concentration.",
        provisional_label == "local mono-sector suppliers" ~
          "Primarily domestic and highly concentrated by sector.",
        TRUE ~
          "Interpret together with centroids and summary tables."
      )
    )
  
  names(out)[names(out) == cluster_col] <- "cluster_id"
  out
}

# -----------------------------------------------------------
# 5) Read panel input
# -----------------------------------------------------------
message("Reading supplier_year_panel.csv ...")
panel <- readr::read_csv(input_file, show_col_types = FALSE)

message("Rows read: ", nrow(panel))
message("Columns available: ", ncol(panel))

required_cols <- c(
  "WIN_NAME_CLEAN",
  "award_year",
  "WIN_COUNTRY_CODE",
  "awards_count",
  "total_award_value",
  "avg_award_value",
  "distinct_buyer_countries",
  "cross_border_share",
  "hhi_cpv",
  "years_active",
  "is_consortium"
)

missing_cols <- setdiff(required_cols, names(panel))

if (length(missing_cols) > 0) {
  stop(
    "The following required columns are missing from supplier_year_panel.csv: ",
    paste(missing_cols, collapse = ", ")
  )
}

# -----------------------------------------------------------
# 6) Build supplier-level profiles
# -----------------------------------------------------------
message("Building supplier-level profiles ...")

panel <- panel %>%
  dplyr::mutate(
    WIN_NAME_CLEAN = stringr::str_squish(as.character(WIN_NAME_CLEAN)),
    WIN_COUNTRY_CODE = dplyr::na_if(stringr::str_squish(as.character(WIN_COUNTRY_CODE)), ""),
    award_year = as.integer(award_year),
    awards_count = suppressWarnings(as.numeric(awards_count)),
    total_award_value = suppressWarnings(as.numeric(total_award_value)),
    avg_award_value = suppressWarnings(as.numeric(avg_award_value)),
    distinct_buyer_countries = suppressWarnings(as.numeric(distinct_buyer_countries)),
    cross_border_share = suppressWarnings(as.numeric(cross_border_share)),
    hhi_cpv = suppressWarnings(as.numeric(hhi_cpv)),
    years_active = suppressWarnings(as.numeric(years_active)),
    is_consortium = suppressWarnings(as.integer(is_consortium))
  ) %>%
  dplyr::filter(!is.na(WIN_NAME_CLEAN), WIN_NAME_CLEAN != "")

supplier_profile <- panel %>%
  dplyr::group_by(WIN_NAME_CLEAN) %>%
  dplyr::summarise(
    WIN_COUNTRY_CODE = mode_char(WIN_COUNTRY_CODE),
    first_year = min(award_year, na.rm = TRUE),
    last_year = max(award_year, na.rm = TRUE),
    years_active = na_max(years_active),
    n_supplier_years = dplyr::n(),
    total_awards = na_sum(awards_count),
    total_award_value = na_sum(total_award_value),
    
    n_valid_value_years = sum(!is.na(avg_award_value) & !is.na(awards_count) & awards_count > 0),
    n_valid_cross_border_years = sum(!is.na(cross_border_share) & !is.na(awards_count) & awards_count > 0),
    n_valid_hhi_years = sum(!is.na(hhi_cpv) & !is.na(awards_count) & awards_count > 0),
    n_valid_buyer_country_years = sum(!is.na(distinct_buyer_countries) & !is.na(awards_count) & awards_count > 0),
    
    avg_awards_per_year = na_mean(awards_count),
    avg_contract_value = weighted_mean_safe(avg_award_value, awards_count),
    avg_buyer_countries = weighted_mean_safe(distinct_buyer_countries, awards_count),
    avg_cross_border_share = weighted_mean_safe(cross_border_share, awards_count),
    avg_hhi_cpv = weighted_mean_safe(hhi_cpv, awards_count),
    
    avg_buyer_countries_unweighted = na_mean(distinct_buyer_countries),
    avg_cross_border_share_unweighted = na_mean(cross_border_share),
    avg_hhi_cpv_unweighted = na_mean(hhi_cpv),
    
    supplier_is_consortium = as.integer(any(is_consortium == 1, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    log_avg_awards_per_year = log1p(avg_awards_per_year),
    log_avg_contract_value = log1p(avg_contract_value),
    coverage_value = n_valid_value_years / n_supplier_years,
    coverage_cross_border = n_valid_cross_border_years / n_supplier_years,
    coverage_hhi = n_valid_hhi_years / n_supplier_years,
    coverage_buyer_countries = n_valid_buyer_country_years / n_supplier_years
  )

message("Suppliers in supplier profile: ", nrow(supplier_profile))

# -----------------------------------------------------------
# 7) Clustering input
# -----------------------------------------------------------
cluster_vars <- c(
  "log_avg_awards_per_year",
  "log_avg_contract_value",
  "avg_buyer_countries",
  "avg_cross_border_share",
  "avg_hhi_cpv",
  "years_active",
  "supplier_is_consortium"
)

cluster_data <- supplier_profile %>%
  dplyr::select(WIN_NAME_CLEAN, WIN_COUNTRY_CODE, dplyr::all_of(cluster_vars), dplyr::everything()) %>%
  dplyr::filter(dplyr::if_all(dplyr::all_of(cluster_vars), ~ !is.na(.)))

if (nrow(cluster_data) < max(k_grid)) {
  stop("Too few complete supplier profiles for clustering.")
}

cluster_matrix <- prepare_cluster_matrix(cluster_data, cluster_vars)

message("Suppliers with complete clustering features: ", nrow(cluster_data))

# -----------------------------------------------------------
# 8) Elbow diagnostics
# -----------------------------------------------------------
message("Running elbow diagnostics ...")

elbow_tbl <- tibble::tibble(
  k = k_grid,
  tot_withinss = sapply(
    k_grid,
    function(k) {
      run_kmeans(
        x = cluster_matrix,
        k = k,
        seed = 42 + k,
        nstart = kmeans_nstart_diag,
        iter.max = kmeans_iter_max,
        algorithm = kmeans_algorithm
      )$tot.withinss
    }
  )
)

readr::write_csv(elbow_tbl, here::here("output", "tables", "clustering_elbow.csv"))

# -----------------------------------------------------------
# 9) Silhouette diagnostics
# -----------------------------------------------------------
message("Running silhouette diagnostics on repeated subsamples ...")

sil_n <- min(silhouette_sample_n, nrow(cluster_matrix))
sil_results <- vector("list", silhouette_reps)

for (rep_id in seq_len(silhouette_reps)) {
  set.seed(1000 + rep_id)
  sample_idx <- sample(seq_len(nrow(cluster_matrix)), size = sil_n, replace = FALSE)
  sil_matrix <- cluster_matrix[sample_idx, , drop = FALSE]
  sil_dist <- stats::dist(sil_matrix)
  
  rep_tbl <- tibble::tibble(
    rep = integer(),
    k = integer(),
    mean_silhouette = double(),
    kmeans_seed_used = integer(),
    kmeans_attempts = integer(),
    retry_used = logical()
  )
  
  for (k in k_grid) {
    km_tmp <- run_kmeans_retry(
      x = sil_matrix,
      k = k,
      seed = 5000 + rep_id * 100 + k,
      nstart = kmeans_nstart_diag,
      iter.max = kmeans_iter_max,
      algorithm = kmeans_algorithm,
      max_tries = diag_retry_max_tries,
      seed_step = diag_retry_seed_step
    )
    
    sil_obj <- cluster::silhouette(km_tmp$cluster, sil_dist)
    
    rep_tbl <- dplyr::bind_rows(
      rep_tbl,
      tibble::tibble(
        rep = rep_id,
        k = k,
        mean_silhouette = mean(sil_obj[, 3]),
        kmeans_seed_used = attr(km_tmp, "used_seed"),
        kmeans_attempts = attr(km_tmp, "attempts"),
        retry_used = attr(km_tmp, "attempts") > 1
      )
    )
  }
  
  sil_results[[rep_id]] <- rep_tbl
}

silhouette_tbl <- dplyr::bind_rows(sil_results)

silhouette_summary <- silhouette_tbl %>%
  dplyr::group_by(k) %>%
  dplyr::summarise(
    mean_silhouette_avg = mean(mean_silhouette),
    sd_silhouette = sd(mean_silhouette),
    min_silhouette = min(mean_silhouette),
    max_silhouette = max(mean_silhouette),
    reps = dplyr::n(),
    retries_used = sum(retry_used),
    avg_kmeans_attempts = mean(kmeans_attempts),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(mean_silhouette_avg), k)

k_comparison_tbl <- elbow_tbl %>%
  dplyr::left_join(silhouette_summary %>% dplyr::arrange(k), by = "k") %>%
  dplyr::mutate(
    silhouette_rank = rank(-mean_silhouette_avg, ties.method = "min"),
    elbow_rank = rank(tot_withinss, ties.method = "min")
  ) %>%
  dplyr::arrange(k)

readr::write_csv(silhouette_tbl, here::here("output", "tables", "clustering_silhouette_reps.csv"))
readr::write_csv(silhouette_summary, here::here("output", "tables", "clustering_silhouette_summary.csv"))
readr::write_csv(k_comparison_tbl, here::here("output", "tables", "clustering_k_comparison.csv"))

# -----------------------------------------------------------
# 10) Diagnostic figures
# -----------------------------------------------------------
p_elbow <- ggplot2::ggplot(elbow_tbl, ggplot2::aes(x = k, y = tot_withinss)) +
  ggplot2::geom_line(color = "#1b5e20", linewidth = 0.8) +
  ggplot2::geom_point(color = "#1b5e20", size = 2) +
  ggplot2::scale_x_continuous(breaks = k_grid) +
  ggplot2::labs(
    title = "Elbow diagnostic for K-means",
    x = "Number of clusters K",
    y = "Total within-cluster sum of squares"
  ) +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
  filename = here::here("output", "figures", "clustering_elbow.png"),
  plot = p_elbow,
  width = 8,
  height = 5,
  dpi = 300
)

p_sil <- ggplot2::ggplot(
  silhouette_summary %>% dplyr::arrange(k),
  ggplot2::aes(x = k, y = mean_silhouette_avg)
) +
  ggplot2::geom_line(color = "#0d47a1", linewidth = 0.8) +
  ggplot2::geom_point(color = "#0d47a1", size = 2) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = mean_silhouette_avg - sd_silhouette,
      ymax = mean_silhouette_avg + sd_silhouette
    ),
    width = 0.15,
    color = "#0d47a1"
  ) +
  ggplot2::scale_x_continuous(breaks = k_grid) +
  ggplot2::labs(
    title = "Silhouette diagnostic on repeated subsamples",
    subtitle = paste0(
      "Sample size per repetition: ", sil_n,
      " | repetitions: ", silhouette_reps
    ),
    x = "Number of clusters K",
    y = "Mean silhouette width"
  ) +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
  filename = here::here("output", "figures", "clustering_silhouette.png"),
  plot = p_sil,
  width = 8,
  height = 5,
  dpi = 300
)

# -----------------------------------------------------------
# 11) Final K-means
# -----------------------------------------------------------
message("Estimating final K-means with K = ", final_k, " ...")

km_final <- run_kmeans(
  x = cluster_matrix,
  k = final_k,
  seed = 42,
  nstart = kmeans_nstart_final,
  iter.max = kmeans_iter_max,
  algorithm = kmeans_algorithm
)

cluster_data_main <- cluster_data %>%
  dplyr::mutate(cluster = factor(km_final$cluster))

supplier_profile_main <- supplier_profile %>%
  dplyr::left_join(
    cluster_data_main %>% dplyr::select(WIN_NAME_CLEAN, cluster),
    by = "WIN_NAME_CLEAN"
  )

cluster_summary <- make_cluster_summary(supplier_profile_main, "cluster")
cluster_summary_means <- make_cluster_summary_means(supplier_profile_main, "cluster")
cluster_sizes <- make_cluster_size_table(cluster_data_main$cluster, "cluster")
cluster_labels_main <- make_cluster_labels(cluster_summary, "cluster_id")

readr::write_csv(cluster_summary, here::here("output", "tables", "cluster_summary.csv"))
readr::write_csv(cluster_summary_means, here::here("output", "tables", "cluster_summary_means.csv"))
readr::write_csv(cluster_sizes, here::here("output", "tables", "cluster_sizes.csv"))
readr::write_csv(cluster_labels_main, here::here("output", "tables", "cluster_labels_main.csv"))
readr::write_csv(supplier_profile_main, here::here("data", "processed", "supplier_profiles.csv"))

centroids_tbl <- tibble::as_tibble(km_final$centers) %>%
  dplyr::mutate(cluster = factor(seq_len(nrow(km_final$centers))), .before = 1)

readr::write_csv(centroids_tbl, here::here("output", "tables", "cluster_centroids_scaled.csv"))

message("Final clustering completed.")
message("Cluster distribution:")
print(table(cluster_data_main$cluster))
print(cluster_summary)

# -----------------------------------------------------------
# 12) Main clustering plots
# -----------------------------------------------------------
plot_n <- min(plot_n_max, nrow(cluster_data_main))

set.seed(42)
plot_df <- cluster_data_main %>%
  dplyr::slice_sample(n = plot_n)

p1 <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(x = log_avg_awards_per_year, y = log_avg_contract_value, color = cluster)
) +
  ggplot2::geom_point(alpha = 0.25, size = 0.8) +
  ggplot2::labs(
    title = "Supplier segmentation: volume vs contract value",
    x = "Log average awards per year",
    y = "Log average contract value",
    color = "Cluster"
  ) +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
  filename = here::here("output", "figures", "cluster_scatter.png"),
  plot = p1,
  width = 9,
  height = 6,
  dpi = 300
)

p2 <- ggplot2::ggplot(
  plot_df,
  ggplot2::aes(x = avg_hhi_cpv, y = avg_cross_border_share, color = cluster)
) +
  ggplot2::geom_point(alpha = 0.25, size = 0.8) +
  ggplot2::labs(
    title = "Supplier segmentation: specialization vs cross-border orientation",
    x = "Average HHI sector concentration",
    y = "Average cross-border share",
    color = "Cluster"
  ) +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
  filename = here::here("output", "figures", "cluster_specialization.png"),
  plot = p2,
  width = 9,
  height = 6,
  dpi = 300
)

# -----------------------------------------------------------
# 13) Robustness: exclude very low-value suppliers, same K
# -----------------------------------------------------------
message(
  "Running robustness clustering excluding avg_contract_value <= ",
  low_value_threshold,
  ", with K = ", robust_same_k, " ..."
)

cluster_data_robust <- cluster_data %>%
  dplyr::filter(avg_contract_value > low_value_threshold)

if (nrow(cluster_data_robust) < robust_same_k) {
  stop("Too few observations in the low-value filtered sample for robustness clustering.")
}

cluster_matrix_robust <- prepare_cluster_matrix(cluster_data_robust, cluster_vars)

km_robust <- run_kmeans(
  x = cluster_matrix_robust,
  k = robust_same_k,
  seed = 4242,
  nstart = kmeans_nstart_final,
  iter.max = kmeans_iter_max,
  algorithm = kmeans_algorithm
)

cluster_data_robust <- cluster_data_robust %>%
  dplyr::mutate(cluster_robust = factor(km_robust$cluster))

supplier_profile_robust <- supplier_profile %>%
  dplyr::left_join(
    cluster_data_robust %>% dplyr::select(WIN_NAME_CLEAN, cluster_robust),
    by = "WIN_NAME_CLEAN"
  )

cluster_summary_robust <- make_cluster_summary(supplier_profile_robust, "cluster_robust")
cluster_summary_robust_means <- make_cluster_summary_means(supplier_profile_robust, "cluster_robust")
cluster_sizes_robust <- make_cluster_size_table(cluster_data_robust$cluster_robust, "cluster_robust")

readr::write_csv(cluster_data_robust, here::here("data", "processed", "supplier_profiles_robust_input.csv"))
readr::write_csv(supplier_profile_robust, here::here("data", "processed", "supplier_profiles_robust.csv"))
readr::write_csv(cluster_summary_robust, here::here("output", "tables", "cluster_summary_robust.csv"))
readr::write_csv(cluster_summary_robust_means, here::here("output", "tables", "cluster_summary_robust_means.csv"))
readr::write_csv(cluster_sizes_robust, here::here("output", "tables", "cluster_sizes_robust.csv"))

robust_centroids_tbl <- tibble::as_tibble(km_robust$centers) %>%
  dplyr::mutate(cluster_robust = factor(seq_len(nrow(km_robust$centers))), .before = 1)

readr::write_csv(
  robust_centroids_tbl,
  here::here("output", "tables", "cluster_centroids_scaled_robust.csv")
)

# -----------------------------------------------------------
# 14) Robustness: filtered exploratory K = 4
# -----------------------------------------------------------
message("Running exploratory filtered clustering with K = ", robust_alt_k, " ...")

km_robust_k4 <- run_kmeans(
  x = cluster_matrix_robust,
  k = robust_alt_k,
  seed = 4343,
  nstart = kmeans_nstart_final,
  iter.max = kmeans_iter_max,
  algorithm = kmeans_algorithm
)

cluster_data_robust_k4 <- cluster_data_robust %>%
  dplyr::select(-cluster_robust) %>%
  dplyr::mutate(cluster_robust_k4 = factor(km_robust_k4$cluster))

supplier_profile_robust_k4 <- supplier_profile %>%
  dplyr::left_join(
    cluster_data_robust_k4 %>% dplyr::select(WIN_NAME_CLEAN, cluster_robust_k4),
    by = "WIN_NAME_CLEAN"
  )

cluster_summary_robust_k4 <- make_cluster_summary(supplier_profile_robust_k4, "cluster_robust_k4")
cluster_summary_robust_k4_means <- make_cluster_summary_means(supplier_profile_robust_k4, "cluster_robust_k4")
cluster_sizes_robust_k4 <- make_cluster_size_table(cluster_data_robust_k4$cluster_robust_k4, "cluster_robust_k4")
cluster_labels_robust_k4 <- make_cluster_labels(cluster_summary_robust_k4, "cluster_id")

readr::write_csv(cluster_data_robust_k4, here::here("data", "processed", "supplier_profiles_robust_k4_input.csv"))
readr::write_csv(supplier_profile_robust_k4, here::here("data", "processed", "supplier_profiles_robust_k4.csv"))
readr::write_csv(cluster_summary_robust_k4, here::here("output", "tables", "cluster_summary_robust_k4.csv"))
readr::write_csv(cluster_summary_robust_k4_means, here::here("output", "tables", "cluster_summary_robust_k4_means.csv"))
readr::write_csv(cluster_sizes_robust_k4, here::here("output", "tables", "cluster_sizes_robust_k4.csv"))
readr::write_csv(cluster_labels_robust_k4, here::here("output", "tables", "cluster_labels_robust_k4.csv"))

robust_k4_centroids_tbl <- tibble::as_tibble(km_robust_k4$centers) %>%
  dplyr::mutate(cluster_robust_k4 = factor(seq_len(nrow(km_robust_k4$centers))), .before = 1)

readr::write_csv(
  robust_k4_centroids_tbl,
  here::here("output", "tables", "cluster_centroids_scaled_robust_k4.csv")
)

# -----------------------------------------------------------
# 15) Transition table
# -----------------------------------------------------------
cluster_transition_tbl <- supplier_profile %>%
  dplyr::select(WIN_NAME_CLEAN, avg_contract_value) %>%
  dplyr::left_join(
    cluster_data_main %>% dplyr::select(WIN_NAME_CLEAN, cluster),
    by = "WIN_NAME_CLEAN"
  ) %>%
  dplyr::left_join(
    cluster_data_robust_k4 %>% dplyr::select(WIN_NAME_CLEAN, cluster_robust_k4),
    by = "WIN_NAME_CLEAN"
  ) %>%
  dplyr::mutate(
    filtered_in_k4 = !is.na(cluster_robust_k4),
    cluster = as.character(cluster),
    cluster_robust_k4 = as.character(cluster_robust_k4)
  ) %>%
  dplyr::count(cluster, cluster_robust_k4, filtered_in_k4, name = "n_suppliers") %>%
  dplyr::arrange(cluster, cluster_robust_k4, filtered_in_k4)

readr::write_csv(
  cluster_transition_tbl,
  here::here("output", "tables", "cluster_transition_main_vs_robust_k4.csv")
)

# -----------------------------------------------------------
# 16) Anomaly diagnostics
# -----------------------------------------------------------
anomaly_tbl <- supplier_profile %>%
  dplyr::summarise(
    suppliers_profile_total = dplyr::n(),
    suppliers_complete_clustering = sum(
      !is.na(log_avg_awards_per_year) &
        !is.na(log_avg_contract_value) &
        !is.na(avg_buyer_countries) &
        !is.na(avg_cross_border_share) &
        !is.na(avg_hhi_cpv) &
        !is.na(years_active) &
        !is.na(supplier_is_consortium)
    ),
    suppliers_avg_contract_value_eq1 = sum(avg_contract_value == 1, na.rm = TRUE),
    suppliers_avg_contract_value_le100 = sum(avg_contract_value <= 100, na.rm = TRUE),
    share_avg_contract_value_eq1 = mean(avg_contract_value == 1, na.rm = TRUE),
    share_avg_contract_value_le100 = mean(avg_contract_value <= 100, na.rm = TRUE)
  )

readr::write_csv(
  anomaly_tbl,
  here::here("output", "tables", "clustering_value_anomalies.csv")
)

# -----------------------------------------------------------
# 17) Clustering log
# -----------------------------------------------------------
clustering_log <- tibble::tibble(
  metric = c(
    "suppliers_total_profile",
    "suppliers_complete_for_clustering",
    "share_complete_for_clustering",
    "silhouette_sample_n",
    "silhouette_reps",
    "final_k",
    "robust_same_k",
    "robust_alt_k",
    "low_value_threshold",
    "kmeans_algorithm",
    "kmeans_nstart_diag",
    "kmeans_nstart_final",
    "kmeans_iter_max",
    "diag_retry_max_tries",
    "diag_retry_seed_step",
    "diag_total_retry_events",
    "diag_share_runs_with_retry",
    "suppliers_excluded_low_value_robustness",
    "suppliers_in_robustness_clustering",
    "share_low_value_excluded",
    "suppliers_avg_contract_value_eq1",
    "suppliers_avg_contract_value_le100",
    "share_avg_contract_value_eq1",
    "share_avg_contract_value_le100",
    "mean_coverage_value",
    "mean_coverage_cross_border",
    "mean_coverage_hhi",
    "mean_coverage_buyer_countries"
  ),
  value = c(
    nrow(supplier_profile),
    nrow(cluster_data),
    nrow(cluster_data) / nrow(supplier_profile),
    sil_n,
    silhouette_reps,
    final_k,
    robust_same_k,
    robust_alt_k,
    low_value_threshold,
    kmeans_algorithm,
    kmeans_nstart_diag,
    kmeans_nstart_final,
    kmeans_iter_max,
    diag_retry_max_tries,
    diag_retry_seed_step,
    sum(silhouette_tbl$retry_used),
    mean(silhouette_tbl$retry_used),
    nrow(cluster_data) - nrow(cluster_data_robust),
    nrow(cluster_data_robust),
    (nrow(cluster_data) - nrow(cluster_data_robust)) / nrow(cluster_data),
    anomaly_tbl$suppliers_avg_contract_value_eq1,
    anomaly_tbl$suppliers_avg_contract_value_le100,
    anomaly_tbl$share_avg_contract_value_eq1,
    anomaly_tbl$share_avg_contract_value_le100,
    mean(supplier_profile$coverage_value, na.rm = TRUE),
    mean(supplier_profile$coverage_cross_border, na.rm = TRUE),
    mean(supplier_profile$coverage_hhi, na.rm = TRUE),
    mean(supplier_profile$coverage_buyer_countries, na.rm = TRUE)
  )
)

readr::write_csv(clustering_log, here::here("logs", "clustering_log.csv"))

message("05_clustering.R completed successfully.")
message("Main outputs saved in output/tables, output/figures, data/processed, and logs.")