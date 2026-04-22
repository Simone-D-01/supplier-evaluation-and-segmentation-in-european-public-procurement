# Supplier evaluation and segmentation in European public procurement: empirical evidence from TED data

This repository contains the empirical workflow for a master's thesis on supplier performance and supplier segmentation in European public procurement using Tenders Electronic Daily (TED) contract award data.

The project is organized as a sequential R pipeline. It starts from raw TED Contract Award Notice (CAN) files, builds a cleaned award-level dataset, aggregates it to the supplier-year level, estimates regression models for supplier performance, and then constructs supplier profiles for clustering and segmentation.

## Research scope

The empirical analysis focuses on TED Contract Award Notice (CAN) data with actual award dates between 2018 and 2023.

The thesis addresses two main research questions:

- **RQ1:** What factors are associated with supplier performance in European public procurement, measured through the number and total value of awarded contracts at supplier-year level?
- **RQ2:** Can suppliers be grouped into meaningful segments on the basis of their observed procurement behavior and performance patterns?

## Data source

- **Source:** Tenders Electronic Daily (TED) CSV subset
- **Portal:** https://data.europa.eu/data/datasets/ted-csv
- **Original coverage:** 2006–2023
- **Thesis focus:** 2018–2023, based on actual award date
- **Notice type used:** Contract Award Notice (CAN)
- **Source distribution:** Annual TED CAN source archives from the TED data portal
- **Local working format:** Annual CSV files stored locally and validated through the preparation pipeline. Raw TED CAN files are not versioned in this repository because of their size; instead, they are reconstructed locally by 01_download.R, which downloads, extracts, and validates the required annual source files for 2018–2023.

## Workflow overview

The empirical workflow is implemented through five scripts that should be run in numerical order:

1. `01_download.R`
2. `02_cleaning.R`
3. `03_panel_build.R`
4. `04_regression.R`
5. `05_clustering.R`

The scripts are designed as a reproducible pipeline. Each stage reads the output of the previous stage, validates required inputs, and writes structured outputs to the `data/processed`, `logs`, and `output` folders.

## Repository structure

```text
.
├── 01_download.R
├── 02_cleaning.R
├── 03_panel_build.R
├── 04_regression.R
├── 05_clustering.R
├── data/
│   ├── raw/
│   │   └── tedcan/
│   └── processed/
├── logs/
└── output/
    ├── figures/
    └── tables/
```

## Script description

### 01_download.R

Purpose: initialize the project folder structure and prepare the raw TED CAN input files used in the empirical pipeline.

Main operations:
- Detects the project root automatically.
- Creates the required folders (`data/raw/tedcan`, `data/processed`, `logs`, and `scripts`) if they do not already exist.
- Defines the six annual TED CAN source files for the period 2018–2023.
- Downloads missing source files when automatic download is enabled.
- Uses annual TED CAN source URLs that currently point to ZIP archives.
- When the source is distributed as a ZIP archive, downloads the archive, extracts the relevant CSV file, and saves it locally with the standardized filename `TED_CAN_YYYY.csv`.
- Validates the presence and readability of each local raw file.
- Writes a validation log to `logs/download_log_can.csv`.

Expected raw-data inventory:
- `data/raw/tedcan/TED_CAN_2018.csv`
- `data/raw/tedcan/TED_CAN_2019.csv`
- `data/raw/tedcan/TED_CAN_2020.csv`
- `data/raw/tedcan/TED_CAN_2021.csv`
- `data/raw/tedcan/TED_CAN_2022.csv`
- `data/raw/tedcan/TED_CAN_2023.csv`

Operational note:
By default, the script is configured to download missing files automatically (`download_if_missing <- TRUE`). If the required raw files are already stored locally in the expected folder, they can be retained and validated without forcing a new download.

Validation note:
The script stops with an error if one or more required files are missing, empty, or unreadable. A successful run confirms that the raw TED CAN files are available and ready for `02_cleaning.R`.

### 02_cleaning.R

Purpose: read TED CAN raw files for 2018–2023, clean award-level observations, and create the base dataset used for supplier-year panel construction.

Main operations:
- Merge the six annual TED CAN raw CSV files into one harmonized dataset.
- Retain the core award-level variables needed for panel construction.
- Convert `DT_AWARD` from TED format (`DD/MM/YY`) to R date format and restrict the sample to awards effectively assigned in 2018–2023.
- Standardize supplier names into `WIN_NAME_CLEAN`.
- Harmonize award values by prioritizing `AWARD_VALUE_EURO_FIN_1` and otherwise using `AWARD_VALUE_EURO`.
- Set non-positive award values to missing.
- Derive 2-digit CPV divisions for later specialization measures.
- Remove exact duplicate rows.
- Deduplicate repeated `ID_AWARD` records at award level so that the same award is not counted more than once.
- Retain `CRIT_CODE` as an award attribute, but treat duplicated `ID_AWARD` rows with contradictory non-missing criterion codes as ambiguous, setting the deduplicated `CRIT_CODE` to missing rather than assigning `L` or `M` arbitrarily.
- Save a diagnostic file for duplicated or ambiguous `ID_AWARD` groups.

Outputs:
- `data/processed/ted_awards_clean.csv`
- `logs/cleaning_log.csv`
- `logs/id_award_conflicts.csv`

Methodological note:
The cleaning step defines the empirical foundation of the thesis by transforming raw TED notice files into a consistent award-level dataset. The time window is enforced using the actual award date rather than the notice publication year, because the research question concerns when contracts were awarded.

Data-quality note:
Supplier names are standardized before aggregation in order to reduce artificial fragmentation of the same supplier across multiple observations. Award values are harmonized into a single monetary variable, and implausible non-positive values are set to missing so that later monetary aggregates are not mechanically distorted.

Deduplication note:
At the cleaning stage, `CRIT_CODE` is retained for downstream analysis but is no longer allowed to generate multiple award-level records. When duplicated rows differ only because `CRIT_CODE` is coded once as `L` and once as `M`, the award is kept only once and the deduplicated criterion code is set to missing.

### 03_panel_build.R

Purpose: aggregate the cleaned TED award-level dataset to supplier-year level and construct the panel variables used for regression and clustering.

Main operations:
- Read `data/processed/ted_awards_clean.csv` and validate the columns required for panel construction.
- Normalize supplier, buyer-country, procedure, CPV, and TED criterion-code fields.
- Parse award-level supplier-country strings from `WIN_COUNTRY_CODE` before constructing supplier-country and cross-border variables.
- Collapse consortium-style supplier-country strings to a single code when all country tokens are identical.
- Treat genuinely multi-country or otherwise non-parsable supplier-country strings as non-unique and set them to missing for award-level cross-border classification.
- Assign supplier-year `WIN_COUNTRY_CODE` as the modal non-missing parsed supplier-country code within each supplier-year.
- Compute supplier-year indicators for award volume, contract value, buyer diversification, buyer-country diversification, sector scope, dominant CPV division, procedure type, consortium participation, and longitudinal activity.
- Compute `price_criteria_share` from valid TED `CRIT_CODE` values only, where `L` indicates lowest price and `M` indicates most economically advantageous tender.
- Generate a validation log for observed `CRIT_CODE` values and a panel log for missingness and aggregation diagnostics.
- Create log-transformed outcome variables for downstream regression models.

Outputs:
- `data/processed/supplier_year_panel.csv`
- `logs/panel_log.csv`
- `logs/crit_code_validation.csv`

Methodological note:
The panel unit of analysis is supplier-year. This level is appropriate because RQ1 focuses on the determinants of supplier performance over time, while RQ2 later uses the same panel as the basis for constructing longer-run supplier profiles.

Variable-construction note:
Supplier country is integrated directly during aggregation rather than recovered through a later external join. Cross-border status is computed only where both buyer country and a unique parsed supplier-country code are observed.

Criterion-code note:
`price_criteria_share` is defined as the share of awards within each supplier-year for which the TED criterion code is valid and equal to `L`. Awards with missing or unexpected criterion codes are excluded from the denominator rather than treated as non-price awards.

### 04_regression.R

Purpose: estimate the determinants of the number and total value of contract awards at supplier-year level.

Main operations:
- Read `data/processed/supplier_year_panel.csv`.
- Generate descriptive statistics and a correlation matrix automatically.
- Standardize regressors using `as.numeric(scale())`.
- Retain `hhi_cpv` as the main specialization measure and exclude `specialization_share` from the regressions because of severe collinearity.
- Include `is_consortium` only in models without supplier-country fixed effects when the variable is collinear with country fixed effects.
- Estimate year fixed-effects models, baseline year plus supplier-country fixed-effects models, matched-sample baseline fixed-effects models, and extended models including `price_criteria_share`.
- Add an enhanced final specification with procedure fixed effects based on `top_procedure`, the modal procedure observed within each supplier-year.
- Estimate robustness models restricted to supplier-years with at least two awards.
- Estimate an additional robustness model for total contract value trimming the top 1% of the value distribution.
- Save a regression log documenting sample sizes, supplier coverage, country coverage, procedure coverage, and the trimming threshold.

Outputs:
- `output/tables/descriptive_stats.csv`
- `output/tables/correlation_matrix.csv`
- `output/figures/correlation_matrix.png`
- `output/tables/regression_main.csv`
- `output/tables/regression_robustness.csv`
- `output/tables/regression_incremental.csv`
- `logs/regression_log.csv`

Methodological note:
The regression analysis is conducted at supplier-year level because the objective is to explain variation in supplier performance over time, measured both in terms of award count and total contract value. The script estimates a sequence of comparable models rather than a single specification, allowing comparison across baseline, extended, and robustness designs.

Identification note:
The matched-sample baseline models are used as the correct benchmark for evaluating the incremental contribution of `price_criteria_share`, because they are estimated on the same supplier-year observations as the extended models.

### 05_clustering.R

Purpose: construct supplier-level profiles from the supplier-year panel and segment suppliers using K-means clustering.

Main operations:
- Read `data/processed/supplier_year_panel.csv`.
- Aggregate supplier-year observations into one supplier-level profile for segmentation.
- Use a standardized set of supplier features for clustering: average awards per year, average contract value, buyer-country diversification, cross-border orientation, sector concentration, years active, and consortium status.
- Estimate elbow diagnostics on the full clustering sample.
- Estimate silhouette diagnostics on repeated random subsamples of 5,000 suppliers to avoid infeasible full-sample distance matrices.
- Use the Lloyd algorithm for K-means estimation to improve computational stability.
- Estimate the final K-means model with `K = 5`.
- Save cluster summaries using medians as the main descriptive statistics and means as complementary evidence.
- Add cluster-size tables, scaled centroid tables, provisional interpretation tables, and robustness solutions.
- Estimate a robustness clustering excluding suppliers with `avg_contract_value < 100`.
- Estimate an additional exploratory filtered clustering with `K = 4`.
- Create a transition table mapping the main `K = 5` solution to the filtered `K = 4` solution.
- Report anomaly diagnostics for suppliers with extremely low average contract values.

Outputs:
- `data/processed/supplier_profiles.csv`
- `data/processed/supplier_profiles_robust.csv`
- `data/processed/supplier_profiles_robust_input.csv`
- `data/processed/supplier_profiles_robust_k4.csv`
- `data/processed/supplier_profiles_robust_k4_input.csv`
- `output/tables/clustering_elbow.csv`
- `output/tables/clustering_silhouette_reps.csv`
- `output/tables/clustering_silhouette_summary.csv`
- `output/tables/clustering_k_comparison.csv`
- `output/tables/cluster_summary.csv`
- `output/tables/cluster_summary_means.csv`
- `output/tables/cluster_sizes.csv`
- `output/tables/cluster_labels_main.csv`
- `output/tables/cluster_centroids_scaled.csv`
- `output/tables/cluster_summary_robust.csv`
- `output/tables/cluster_summary_robust_means.csv`
- `output/tables/cluster_sizes_robust.csv`
- `output/tables/cluster_centroids_scaled_robust.csv`
- `output/tables/cluster_summary_robust_k4.csv`
- `output/tables/cluster_summary_robust_k4_means.csv`
- `output/tables/cluster_sizes_robust_k4.csv`
- `output/tables/cluster_labels_robust_k4.csv`
- `output/tables/cluster_centroids_scaled_robust_k4.csv`
- `output/tables/cluster_transition_main_vs_robust_k4.csv`
- `output/tables/clustering_value_anomalies.csv`
- `output/figures/clustering_elbow.png`
- `output/figures/clustering_silhouette.png`
- `output/figures/cluster_scatter.png`
- `output/figures/cluster_specialization.png`
- `logs/clustering_log.csv`

Methodological note:
The clustering stage moves from the supplier-year panel to a supplier-level representation in order to identify persistent supplier profiles rather than year-specific observations. This is appropriate because the segmentation exercise concerns longer-run supplier typologies rather than short-run annual variation.

Diagnostic note:
The choice of `K = 5` is supported by a combination of elbow and silhouette diagnostics rather than a single mechanical rule. Because full-sample silhouette computation is infeasible at this scale, the script estimates repeated silhouette diagnostics on random subsamples and summarizes their distribution across repetitions.

## Reproduction steps

To reproduce the empirical workflow from scratch:

1. Clone the repository.
2. Open the project in R or RStudio.
3. Install any missing packages required by the scripts.
4. Run the scripts in numerical order from `01_download.R` to `05_clustering.R`.

The pipeline is sequential, so each script depends on outputs generated by the previous step. If the raw TED CAN files are not already available locally, `01_download.R` prepares the raw-data inventory needed for the rest of the analysis.

## Public repository note

This repository is intended to document the empirical workflow in a transparent and reproducible way. The code, folder structure, and expected outputs can be inspected without immediately downloading the full raw TED files, while full replication requires running the download and preparation stages locally.

## Thesis relation

This repository contains the empirical workflow only. The thesis manuscript, appendix drafting files, and related writing materials may be maintained separately from the public replication repository.