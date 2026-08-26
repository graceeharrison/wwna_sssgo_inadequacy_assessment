# ============================================================================
# 01_load_data_FIXED.R
#
# Fixed version: deduplicates AnnualReports_txt and Questionnaire_txt to one
# row per WDID (most recent submission) BEFORE joining to spill data, so
# each spill is matched to exactly one submission instead of every
# submission on file for that system.
# ============================================================================

# ============================================================================
# 1. Load Remote Data (or snapshot)
# ============================================================================

if (!is.na(USE_SNAPSHOT)) {
  
  snap_path <- file.path(SNAPSHOT_DIR, USE_SNAPSHOT)
  
  if (!dir.exists(snap_path)) {
    stop("Snapshot directory not found: ", snap_path,
         "\nCheck that SNAPSHOT_DIR and USE_SNAPSHOT are set correctly in 00_setup.R")
  }
  
  message("Loading data from snapshot: ", USE_SNAPSHOT)
  
  SSO_txt <- read.csv(
    file.path(snap_path, "SSO.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  message("  SSO data loaded: ", nrow(SSO_txt), " rows")
  
  Questionnaire_txt <- read.csv(
    file.path(snap_path, "Questionnaire.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  message("  Questionnaire data loaded: ", nrow(Questionnaire_txt), " rows")
  
  Cat1_2_3_txt <- read.csv(
    file.path(snap_path, "Cat1_2_3.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  message("  Cat1-2-3 spills loaded: ", nrow(Cat1_2_3_txt), " rows")
  
  AnnualReports_txt <- read.csv(
    file.path(snap_path, "AnnualReports.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  message("  Annual reports loaded: ", nrow(AnnualReports_txt), " rows")
  
  neworder_no_spills <- read.csv(
    file.path(snap_path, "neworder_no_spills.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  message("  New order monthly counts loaded: ", nrow(neworder_no_spills), " rows")
  
  message("Snapshot data loaded successfully from: ", snap_path)
  
} else {
  
  message("Loading remote data...")
  
  SSO_txt <- read.delim(SSO_URL)
  message("  SSO data loaded: ", nrow(SSO_txt), " rows")
  
  Questionnaire_txt <- read.delim(QUESTIONNAIRE_URL)
  message("  Questionnaire data loaded: ", nrow(Questionnaire_txt), " rows")
  
  Cat1_2_3_txt <- read.delim(CAT1_2_3_URL)
  message("  Cat1-2-3 spills loaded: ", nrow(Cat1_2_3_txt), " rows")
  
  AnnualReports_txt <- read.delim(ANNUAL_REPORTS_URL)
  message("  Annual reports loaded: ", nrow(AnnualReports_txt), " rows")
  
  neworder_no_spills <- read.delim(NEWORDER_URL)
  message("  New order monthly counts loaded: ", nrow(neworder_no_spills), " rows")
  
  snap_path <- file.path(SNAPSHOT_DIR, as.character(Sys.Date()))
  
  if (dir.exists(snap_path)) {
    message("Snapshot already exists for today (", Sys.Date(), ") — overwriting.")
  } else {
    dir.create(snap_path, recursive = TRUE)
  }
  
  write.csv(SSO_txt, file.path(snap_path, "SSO.csv"), row.names = FALSE)
  write.csv(Questionnaire_txt, file.path(snap_path, "Questionnaire.csv"), row.names = FALSE)
  write.csv(Cat1_2_3_txt, file.path(snap_path, "Cat1_2_3.csv"), row.names = FALSE)
  write.csv(AnnualReports_txt, file.path(snap_path, "AnnualReports.csv"), row.names = FALSE)
  write.csv(neworder_no_spills, file.path(snap_path, "neworder_no_spills.csv"), row.names = FALSE)
  
  message("Remote data downloaded and snapshot saved to: ", snap_path)
  
}

# ============================================================================
# 2. Load Local Data
# ============================================================================
message("Loading local data...")

Facilities_List <- read.csv(FACILITIES_LIST_PATH,
                            na.strings = c("", "NA", "#N/A"))

message("Facilities list loaded: ", nrow(Facilities_List), " rows, ",
        n_distinct(Facilities_List$WDID), " unique WDIDs")

enforcement_orders_CIWQS <- read.csv(ENFORCEMENT_ORDERS_PATH)
message("  Enforcement orders loaded: ", nrow(enforcement_orders_CIWQS), " rows")

# ============================================================================
# 3. Initial Combines  --  FIXED VERSION
# ============================================================================
message("Combining datasets...")

# Standardize Questionnaire column names
names(Questionnaire_txt) <- toupper(names(Questionnaire_txt))

# --------------------------------------------------------------------------
# Correct implausible year-over-year jumps in AnnualReports_txt (system
# length, population served) BEFORE deduplicating to the most recent year.
# See apply_annual_report_corrections.R for the full rule and rationale.
# Writes a full audit log so every correction is traceable.
# --------------------------------------------------------------------------
source(here("R", "apply_annual_report_corrections.R"))
correction_result <- correct_annual_reports(AnnualReports_txt, Questionnaire_txt)
AnnualReports_txt <- correction_result$corrected_data
annual_report_correction_log <- correction_result$correction_log

if (nrow(annual_report_correction_log) > 0) {
  write.csv(annual_report_correction_log,
            here("outputs", "annual_report_correction_log.csv"),
            row.names = FALSE)
  message("  Correction log saved to: ", here("outputs", "annual_report_correction_log.csv"))
}

# Each WDID can have multiple annual report / questionnaire submissions on
# file (one per year). Joining on WDID alone would match every spill to
# every submission for that system, duplicating spill records. Keep only
# the most recent submission per WDID before joining.
AnnualReports_latest <- AnnualReports_txt %>%
  arrange(WDID, desc(ANNUAL_REPORT_YEAR)) %>%
  distinct(WDID, .keep_all = TRUE)

Questionnaire_latest <- Questionnaire_txt %>%
  mutate(LAST_UPDATED_PARSED = as.Date(LAST.UPDATED.ON, format = "%m/%d/%Y")) %>%
  arrange(WDID, desc(LAST_UPDATED_PARSED)) %>%
  distinct(WDID, .keep_all = TRUE) %>%
  select(-LAST_UPDATED_PARSED)

# Combine SSO and Questionnaire
SSO_Questionnaire_combined <- left_join(SSO_txt, Questionnaire_latest, by = "WDID")

# Combine Cat1-2-3 and Annual Reports
Cat_spills_AnnualReports_combined <- left_join(Cat1_2_3_txt, AnnualReports_latest, by = "WDID")

message("Data loading complete (fixed version).")