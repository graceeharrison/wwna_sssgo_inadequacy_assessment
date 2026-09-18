# ============================================================================
# run_all.R
# Master script to run full analysis pipeline in order
# ============================================================================

# Set version-specific output folder BEFORE sourcing setup
# Uses the date the pipeline is actually run, so each run gets its own
# dated output folder automatically.
OUTPUT_PATH <- here::here("outputs", paste0("inadequacy_revisions_", format(Sys.Date(), "%m_%d_%y")))

# Create the folder if it doesn't exist
if (!dir.exists(OUTPUT_PATH)) {
  dir.create(OUTPUT_PATH, recursive = TRUE)
  message("Created output folder: ", OUTPUT_PATH)
}

source("00_setup.R")
source("01_load_data.R")
source("02_clean_data.R")
source("03_criteria1_spills.R")              # Inadequacy: system length spill metrics only
source("04_criteria2_reporting.R")            # Inadequacy: reporting criteria
source("05_enforcement.R")                   # Prepare enforcement data; build inadequacy list
source("06_inadequacy_export.R")                        # Export Inadequacy List
source("07_enforcement_export.R")             # Export separate Under Enforcement list