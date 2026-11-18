# Inadequate Collection Systems Methodology

An R pipeline for identifying wastewater collection systems that fail to meet
performance and reporting standards under California's Sanitary Sewer System
General Order (SSSGO). Developed for the California State Water Resources Control Board's Wastewater Needs Assessment by staff at UCLA's Luskin Center for Innovation.

## Overview

Collection systems (sanitary sewer systems) are evaluated against two
independent criteria:

### Criteria 1 — Spill Performance: 
  A system is flagged as Low Performing
  if its spill count (only includes Cat 1 & Cat 2 spill) per scaled system length falls above the 75th
  percentile within its size category on both the 5-year rolling average and
  the 12-month spill rate. The 12-month rate is the total number of spills in
  the final 12 months of the analysis period (January-December 2025), scaled as
  described below, so it is an annual rate in the same units as the 5-year
  average.

#### Spill Rate Scaling

Spill counts are not compared as a flat rate per 100 miles for every
system. Instead, each system's rolling average spill count is divided by
its own length relative to a size-tiered scale, so that smaller systems
aren't penalized with an artificially inflated rate simply because a
fraction of a mile produces an outsized ratio:

| System length | Scaled per |
|---|---|
| < 5 miles | 5 miles |
| 5 to < 25 miles | 25 miles |
| ≥ 25 miles | 100 miles |

This is implemented in `03_criteria1_spills.R` as:

```r
scale_miles = case_when(
  TOTAL_SYS_LENGTH_COMBINED < 5 ~ 5,
  TOTAL_SYS_LENGTH_COMBINED >= 5 & TOTAL_SYS_LENGTH_COMBINED < 25 ~ 25,
  TOTAL_SYS_LENGTH_COMBINED >= 25 ~ 100,
  TRUE ~ NA_real_
),
rolling_avg_5yr_scaled = rolling_avg_5yr / (TOTAL_SYS_LENGTH_COMBINED / scale_miles)
```

(The 12-month rate uses the identical scaling logic, applied to the total spills
in January-December 2025.)

**Example:** A system with 15 miles of sewer that averaged 8.2 spills/year
over the 5-year window falls in the 5 to < 25 mile tier, so its rate is scaled
per 25 miles: 8.2 / (15 / 25) = 13.7 scaled spills per year. (Illustrative
numbers only.)

  Medium and Large systems are additionally evaluated
  against an absolute numeric standard — a system meeting both of the
  following is exempted from the list regardless of its percentile
  ranking:
  - 5-year rolling average ≤ 2 spills per 100 miles
  - 12-month spill rate (January-December 2025) ≤ 1 spill per 100 miles

#### GESD Statistical Outlier Test

  A Generalized Extreme Studentized Deviate (GESD) test is also run as an
  informational marker, flagging systems whose spill rate is a statistical
  outlier compared with systems of similar length. It does not affect which
  systems are flagged as inadequate.

### Criteria 2 — Reporting: 
  A system is flagged if it has not submitted a required Annual Report and has not filed a No Spill Certification.

  Systems with active enforcement orders are identified separately as "Under
  Enforcement" and are not included in the combined Inadequacy List produced
  by this pipeline.

## Repository Structure

Scripts are numbered and intended to be run in order:

| Script | Purpose |
|---|---|
| `00_setup.R` | Loads required libraries, sets global variables (analysis period, snapshot controls), and defines helper functions. |
| `01_load_data.R` | Loads raw data; deduplicates Annual Report and SSO Questionnaire submissions to one record per system before joining to spill data; applies data quality corrections (see "Data Quality Corrections" below). |
| `apply_annual_report_corrections.R` | Sourced by `01_load_data.R`. Automated correction for implausible year-over-year jumps in reported population served. |
| `apply_manual_length_corrections.R` | Sourced by `01_load_data.R`. Manually reviewed corrections for implausible year-over-year jumps in reported system length. |
| `02_clean_data.R` | Cleans and standardizes source datasets; derives system size categories and attributes. |
| `03_criteria1_spills.R` | Applies the spill-performance criteria described above (percentile bands, numeric threshold, GESD). |
| `04_criteria2_reporting.R` | Applies the reporting criteria (missing Annual Report and No Spill Certification). |
| `05_enforcement.R` | Loads enforcement order data and builds the combined Inadequacy List from Criteria 1 and 2. |
| `06_inadequacy_export.R` | Assembles the final comprehensive dataset and exports the Inadequacy List to Excel. |
| `07_enforcement_export.R` | Exports the separate "Under Enforcement" list. |
| `run_all.R` | Runs `00`–`07` in sequence and sets a dated output folder automatically. See "Running the Pipeline" below. |

## Requirements

This pipeline uses the following R packages:

```
dplyr, readr, lubridate, purrr, writexl, tidyr, flextable, officer, zoo, ggplot2, here
```

## Data

This repository includes an archived data snapshot so the pipeline can be
run exactly as originally executed. `01_load_data.R` supports two modes,
controlled by `USE_SNAPSHOT` in `00_setup.R`:

- **Snapshot mode** (`USE_SNAPSHOT <- "YYYY-MM-DD"`) loads the archived
  data included in this repository, reproducing the original run exactly.
- **Live mode** (`USE_SNAPSHOT <- NA`) downloads current data from source
  instead of using the snapshot, for re-running the analysis on up-to-date
  data.

# Setup and Running the Pipeline

## Directory Structure for Users

When you clone or download this repository, set it up like this:

```
project-root/
├── 00_setup.R
├── 01_load_data.R
├── 02_clean_data.R
├── 03_criteria1_spills.R
├── 04_criteria2_reporting.R
├── 05_enforcement.R
├── 06_inadequacy_export.R
├── 07_enforcement_export.R
├── run_all.R
├── README.md
├── data/
│   ├── local/                    # Raw input files (you must provide)
│   │   ├── CSV_CIWQS_Facilities_List_OWP_4_1_26.csv
│   │   ├── CIWQS_Enforcement_Actions_4_14_26.csv
│   └── snapshots/                # Versioned data snapshots (optional)
│       └── 2026-04-13/
│           ├── AnnualReports.csv
│           ├── Cat_Spills.csv
│           └── ...
├── outputs/                      # Created by the pipeline
│   ├── Inadequate_Systems/
│   ├── Enforcement_Orders/
│   └── OUTPUTS_README.md
└── .gitignore
```

## Running the Pipeline

### Option 1: Run Scripts in Sequence
Open R or RStudio in the project root directory and run:

```r
source("00_setup.R")
source("01_load_data.R")
source("02_clean_data.R")
source("03_criteria1_spills.R")
source("04_criteria2_reporting.R")
source("05_enforcement.R")
source("06_inadequacy_export.R")
source("07_enforcement_export.R")
```

Console output will show progress and system counts at each step.

### Option 2: Run All Scripts at Once

This repository includes `run_all.R`, which runs the full pipeline in
sequence and automatically creates a dated output folder
(`outputs/inadequacy_revisions_MM_DD_YY/`) based on the date it's run.

```bash
Rscript run_all.R
```

Or from within R/RStudio:
```r
source("run_all.R")
```

```

Then run from command line:

```bash
Rscript run_all.R
```

## Data Requirements

### Required Input Files (in `data/local/`)

1. **`CSV_CIWQS_Facilities_List_OWP_4_1_26.csv`**
   - Complete list of all facilities with their collection system details
   - Must include columns: WDID, FACILITY.NAME, CS.ASSESSMENT, PROGRAM.CATEGORY, etc.
   - Date in filename should match the analysis period end date

2. **`CIWQS_Enforcement_Actions_4_14_2026.csv`**
   - Active enforcement orders for SSO systems
   - Must include: FACILITY.ID, PROGRAM.CATEGORY, STATUS, ENFORCEMENT.ACTION.TYPE, EFFECTIVE.DATE.1, etc.

### Data Snapshots (in `data/snapshots/2026-04-13/`)

This repository includes a data snapshot from **April 13, 2026**. By default, the pipeline uses this snapshot (`USE_SNAPSHOT <- "2026-04-13"` in `00_setup.R`), which ensures reproducible results.
 
**To use the included snapshot (recommended):**
- No changes needed — `USE_SNAPSHOT <- "2026-04-13"` is the default

**To use different data:**
- Update to a newer snapshot: `USE_SNAPSHOT <- DATE"` (after creating a fresh snapshot)
- Or download live data: `USE_SNAPSHOT <- NA` (only when you have a specific reason)
```r

### Analysis Period (Date Range)
 
```r
ANALYSIS_PERIOD_START <- as.Date("2021-01-01")
ANALYSIS_PERIOD_END   <- as.Date("2026-01-01")
```
 
This defines the **time window** for analyzing spills and reports (currently: 2021–2026, a 5-year period). The pipeline only looks at:
- Spill events that occurred between these dates
- Annual reports and no-spill certifications filed between these dates
---

## Output Files Generated

Running `run_all.R` creates a single dated output folder:
`outputs/inadequacy_revisions_MM_DD_YY/`, containing:

- The comprehensive Inadequacy List (Excel)
- `Under_Enforcement_Systems_<date>.xlsx` — the separate Under Enforcement list

## Contact

Grace Harrison, Project Manager at UCLA Luskin Center for Innovation
gharrison@luskin.ucla.edu

Greg Pierce, Senior Director at UCLA Luskin Center for Innovation
gpierce@luskin.ucla.edu