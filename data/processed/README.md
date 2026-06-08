# Processed TED data

This folder contains the intermediate datasets produced by the empirical pipeline.

These files are kept private because they are derived from the raw TED CAN source files and are mainly used as inputs for later stages of the workflow. They are generated automatically by the scripts in `R/` and should not be edited manually.

## Folder contents

The main processed files are created in two stages:

- `R/02_cleaning.R` produces the cleaned award-level dataset.
- `R/03_panel_build.R` produces the supplier-year panel.
- `R/05_clustering.R` produces supplier-level profiles and robustness variants used for clustering.

The processed files include:

- `ted_awards_clean.csv`
- `supplier_year_panel.csv`
- `supplier_profiles.csv`
- `supplier_profiles_robust.csv`
- `supplier_profiles_robust_input.csv`
- `supplier_profiles_robust_k4.csv`
- `supplier_profiles_robust_k4_input.csv`

## How the files are used

- `ted_awards_clean.csv` is the cleaned award-level input for panel construction.
- `supplier_year_panel.csv` is the core dataset used for regression and clustering.
- `supplier_profiles*.csv` files are derived supplier-level datasets used for segmentation and robustness checks.

The processed datasets are part of the reproducibility chain, but they are not the final outputs shown in the repository. The final figures and tables are stored in `output/`.

## Notes

- These files are generated automatically and should be recreated by rerunning the pipeline rather than edited manually.
- The folder is documented here so that the intermediate data flow is easy to understand even though the files themselves are not public.
- If a processed file is missing, rerun the relevant script in `R/` in sequence.