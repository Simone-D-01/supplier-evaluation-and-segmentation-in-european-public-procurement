# 01_download.R
# Purpose: create project folders, optionally download TED CAN annual source files
# for 2018-2023, extract CSVs when archives are provided, and validate the local
# raw-data inventory.

rm(list = ls())

options(stringsAsFactors = FALSE, scipen = 999)

# -----------------------------------------------------------
# 1) Package management
# -----------------------------------------------------------
required_pkgs <- c("here", "fs", "readr", "dplyr", "tibble", "httr")

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_pkgs, library, character.only = TRUE))

# -----------------------------------------------------------
# 2) User options
# -----------------------------------------------------------
download_if_missing <- TRUE
force_download <- FALSE
verify_existing <- TRUE

# These URLs currently point to ZIP archives. The script downloads them,
# extracts the relevant CSV, and saves it locally with the expected filename.
source_urls <- tibble::tibble(
  year = 2018:2023,
  file_name = paste0("TED_CAN_", year, ".csv"),
  url = c(
    "https://data.europa.eu/api/hub/store/data/ted-contract-award-notices-2018.zip",
    "https://data.europa.eu/api/hub/store/data/ted-contract-award-notices-2019.zip",
    "https://data.europa.eu/api/hub/store/data/ted-contract-award-notices-2020.zip",
    "https://data.europa.eu/api/hub/store/data/ted-contract-award-notices-2021.zip",
    "https://data.europa.eu/api/hub/store/data/ted-contract-award-notices-2022.zip",
    "https://data.europa.eu/api/hub/store/data/ted-contract-award-notices-2023.zip"
  )
)

# -----------------------------------------------------------
# 3) Project root
# -----------------------------------------------------------
root_dir <- here::here()
message("Project root: ", root_dir)

# -----------------------------------------------------------
# 4) Folder structure
# -----------------------------------------------------------
dirs_to_create <- c(
  here::here("data"),
  here::here("data", "raw"),
  here::here("data", "raw", "tedcan"),
  here::here("data", "processed"),
  here::here("logs"),
  here::here("R")
)

invisible(lapply(dirs_to_create, fs::dir_create))

input_dir <- here::here("data", "raw", "tedcan")
log_file <- here::here("logs", "download_log_can.csv")

files_tbl <- source_urls |>
  dplyr::mutate(
    dest_path = fs::path(input_dir, file_name)
  )

# -----------------------------------------------------------
# 5) Helpers
# -----------------------------------------------------------
safe_head_check <- function(file_path) {
  out <- tryCatch(
    {
      hdr <- suppressMessages(
        readr::read_csv(
          file_path,
          n_max = 5,
          show_col_types = FALSE,
          progress = FALSE
        )
      )
      
      list(
        readable = TRUE,
        n_columns = ncol(hdr),
        read_error = NA_character_
      )
    },
    error = function(e) {
      list(
        readable = FALSE,
        n_columns = NA_integer_,
        read_error = conditionMessage(e)
      )
    }
  )
  
  tibble::tibble(
    readable = out$readable,
    n_columns = out$n_columns,
    read_error = out$read_error
  )
}

pick_best_csv <- function(paths, year) {
  if (length(paths) == 0) {
    return(NA_character_)
  }
  
  info <- fs::file_info(paths)
  
  ranking_tbl <- tibble::tibble(
    path = paths,
    name = tolower(fs::path_file(paths)),
    size_bytes = as.numeric(info$size)
  ) |>
    dplyr::mutate(
      has_year = grepl(as.character(year), name),
      looks_ted = grepl("ted|award|contract|can", name)
    ) |>
    dplyr::arrange(
      dplyr::desc(has_year),
      dplyr::desc(looks_ted),
      dplyr::desc(size_bytes),
      name
    )
  
  ranking_tbl$path[1]
}

download_one_file <- function(url, dest_path, year) {
  tmp_dir <- tempfile(pattern = paste0("tedcan_", year, "_"))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  
  url_clean <- sub("\\?.*$", "", url)
  url_base <- basename(url_clean)
  
  if (is.na(url_base) || !nzchar(url_base)) {
    url_base <- paste0("tedcan_", year, ".zip")
  }
  
  tmp_download <- fs::path(tmp_dir, url_base)
  is_zip <- grepl("\\.zip$", tolower(url_clean))
  
  tryCatch(
    {
      utils::download.file(
        url = url,
        destfile = tmp_download,
        mode = "wb",
        quiet = FALSE
      )
      
      if (!fs::file_exists(tmp_download)) {
        stop("Downloaded file was not created.")
      }
      
      if (is_zip) {
        unzip_dir <- fs::path(tmp_dir, "unzipped")
        fs::dir_create(unzip_dir)
        
        extracted_files <- utils::unzip(
          zipfile = tmp_download,
          exdir = unzip_dir
        )
        
        csv_candidates <- extracted_files[
          grepl("\\.csv$", extracted_files, ignore.case = TRUE)
        ]
        
        if (length(csv_candidates) == 0) {
          stop("ZIP archive does not contain any CSV file.")
        }
        
        chosen_csv <- pick_best_csv(csv_candidates, year)
        
        if (is.na(chosen_csv) || !fs::file_exists(chosen_csv)) {
          stop("Could not identify the extracted CSV file.")
        }
        
        fs::file_copy(chosen_csv, dest_path, overwrite = TRUE)
        
        return(
          list(
            downloaded = TRUE,
            download_status = "downloaded_zip_extracted",
            download_error = NA_character_
          )
        )
      }
      
      fs::file_copy(tmp_download, dest_path, overwrite = TRUE)
      
      list(
        downloaded = TRUE,
        download_status = "downloaded_csv",
        download_error = NA_character_
      )
    },
    error = function(e) {
      list(
        downloaded = FALSE,
        download_status = "download_failed",
        download_error = conditionMessage(e)
      )
    }
  )
}

# -----------------------------------------------------------
# 6) Download logic
# -----------------------------------------------------------
download_results <- vector("list", nrow(files_tbl))

for (i in seq_len(nrow(files_tbl))) {
  file_exists_now <- fs::file_exists(files_tbl$dest_path[i])
  
  if (file_exists_now && !force_download) {
    download_results[[i]] <- tibble::tibble(
      year = files_tbl$year[i],
      file_name = files_tbl$file_name[i],
      url = files_tbl$url[i],
      dest_path = as.character(files_tbl$dest_path[i]),
      action = "kept_existing",
      download_status = "skipped_existing",
      download_error = NA_character_
    )
    next
  }
  
  if (!file_exists_now && !download_if_missing) {
    download_results[[i]] <- tibble::tibble(
      year = files_tbl$year[i],
      file_name = files_tbl$file_name[i],
      url = files_tbl$url[i],
      dest_path = as.character(files_tbl$dest_path[i]),
      action = "not_downloaded",
      download_status = "missing_not_downloaded",
      download_error = NA_character_
    )
    next
  }
  
  if (grepl("^PUT_EXACT_TED_CAN_", files_tbl$url[i])) {
    download_results[[i]] <- tibble::tibble(
      year = files_tbl$year[i],
      file_name = files_tbl$file_name[i],
      url = files_tbl$url[i],
      dest_path = as.character(files_tbl$dest_path[i]),
      action = "not_downloaded",
      download_status = "missing_url_placeholder",
      download_error = "Replace placeholder URL in source_urls before running automated download."
    )
    next
  }
  
  dl <- download_one_file(
    url = files_tbl$url[i],
    dest_path = files_tbl$dest_path[i],
    year = files_tbl$year[i]
  )
  
  download_results[[i]] <- tibble::tibble(
    year = files_tbl$year[i],
    file_name = files_tbl$file_name[i],
    url = files_tbl$url[i],
    dest_path = as.character(files_tbl$dest_path[i]),
    action = if (file_exists_now) "re_downloaded" else "downloaded_missing",
    download_status = dl$download_status,
    download_error = dl$download_error
  )
}

download_log_tbl <- dplyr::bind_rows(download_results)

# -----------------------------------------------------------
# 7) Validation
# -----------------------------------------------------------
validation_tbl <- files_tbl |>
  dplyr::rowwise() |>
  dplyr::do({
    yr <- .$year
    fn <- .$file_name
    url <- .$url
    path <- .$dest_path
    
    exists_flag <- fs::file_exists(path)
    
    if (!exists_flag) {
      return(
        tibble::tibble(
          year = yr,
          file_name = fn,
          url = url,
          file_path = as.character(path),
          exists = FALSE,
          size_bytes = NA_real_,
          size_mb = NA_real_,
          last_modified = NA_character_,
          readable = FALSE,
          n_columns = NA_integer_,
          read_error = "File not found"
        )
      )
    }
    
    info <- fs::file_info(path)
    
    head_chk <- if (verify_existing) {
      safe_head_check(path)
    } else {
      tibble::tibble(
        readable = NA,
        n_columns = NA_integer_,
        read_error = NA_character_
      )
    }
    
    tibble::tibble(
      year = yr,
      file_name = fn,
      url = url,
      file_path = as.character(path),
      exists = TRUE,
      size_bytes = as.numeric(info$size),
      size_mb = round(as.numeric(info$size) / (1024^2), 2),
      last_modified = as.character(info$modification_time),
      readable = head_chk$readable,
      n_columns = head_chk$n_columns,
      read_error = head_chk$read_error
    )
  }) |>
  dplyr::ungroup()

log_tbl <- validation_tbl |>
  dplyr::left_join(
    download_log_tbl |>
      dplyr::select(year, action, download_status, download_error),
    by = "year"
  ) |>
  dplyr::mutate(
    zero_bytes = dplyr::if_else(!is.na(size_bytes), size_bytes == 0, NA),
    validation_status = dplyr::case_when(
      !exists ~ "missing",
      isTRUE(zero_bytes) ~ "empty",
      verify_existing & !is.na(readable) & !readable ~ "unreadable",
      TRUE ~ "ok"
    ),
    checked_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) |>
  dplyr::arrange(year)

readr::write_csv(log_tbl, log_file)

# -----------------------------------------------------------
# 8) Final checks
# -----------------------------------------------------------
problem_tbl <- log_tbl |>
  dplyr::filter(validation_status != "ok")

if (nrow(problem_tbl) > 0) {
  message("Validation log saved to: ", log_file)
  stop(
    paste0(
      "01_download.R did not pass validation for: ",
      paste(problem_tbl$file_name, collapse = ", "),
      ". Check logs/download_log_can.csv."
    )
  )
}

message("All TED CAN files for 2018-2023 are available and validated.")
message("Validation log saved to: ", log_file)
message("01_download.R completed successfully.")
message("Ready for 02_cleaning.R")