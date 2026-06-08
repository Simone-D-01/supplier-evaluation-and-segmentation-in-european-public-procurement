# Raw TED CAN data

This folder contains the raw annual TED Contract Award Notice (CAN) files used as input for the empirical pipeline.

The raw files are kept private in the repository because they are large and are not intended to be edited manually. They are reconstructed locally by `R/01_download.R`, which downloads, extracts, and validates the required annual source files.

## Folder contents

The expected raw files are stored in the `tedcan/` subfolder and follow this naming convention:

- `TED_CAN_2018.csv`
- `TED_CAN_2019.csv`
- `TED_CAN_2020.csv`
- `TED_CAN_2021.csv`
- `TED_CAN_2022.csv`
- `TED_CAN_2023.csv`

## Source and coverage

- **Source:** Tenders Electronic Daily (TED) CSV subset.
- **Notice type:** Contract Award Notice (CAN).
- **Coverage used in the thesis:** 2018–2023, based on actual award date.
- **Local format:** annual CSV files stored locally after download and extraction.

## How the files are used

The raw files are read by the first two stages of the pipeline:

1. `R/01_download.R` prepares or validates the local raw-data inventory.
2. `R/02_cleaning.R` merges the annual files and creates the cleaned award-level dataset.

The raw data are the starting point for the full empirical workflow. They should remain unchanged once downloaded, so that the downstream cleaning and aggregation steps are reproducible.

## Notes

- The raw files are not versioned in the public repository.
- The folder is documented here so that the expected structure is clear even when the data themselves are stored locally.
- Any changes to the raw files should be made only by re-running the download stage rather than manual editing.