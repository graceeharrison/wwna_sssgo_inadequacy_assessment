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

# ----------------------------------------------------------------------------
# Manual facility resolution
#
# A small number of WDIDs have more than one FACILITY.ID row in the source
# data - typically because the physical facility was re-registered under a
# new FACILITY.ID at some point, leaving both the old and current records
# in the export under the same WDID. Left unresolved, any downstream join
# on WDID fans out to every FACILITY.ID/FACILITY.NAME combination for that
# WDID.
#
# There is no reliable general rule to pick the correct record automatically
# (e.g. higher FACILITY.ID is not guaranteed to mean "more current," and
# Facilities_List has no explicit active/historical status field), so each
# case is resolved manually and documented here.
#
# Confirmed case: WDID 9SSO10710 has two FACILITY.ID rows -
#   256444  "MCB Camp Pendleton Sewage Collection System (Historic)"
#   631390  "USMC Base, Camp Pendleton CS"
# 631390 (USMC Base, Camp Pendleton CS) is the current registration to use;
# 256444 is an old/superseded record for the same WDID.
# ----------------------------------------------------------------------------
manual_facility_id_resolution <- tribble(
  ~WDID,        ~Correct_FACILITY.ID,
  "9SSO10710",  "631390"
)

n_facility_dupes_before <- Facilities_List %>%
  count(WDID) %>%
  filter(n > 1) %>%
  nrow()

Facilities_List <- Facilities_List %>%
  filter(
    !(WDID %in% manual_facility_id_resolution$WDID) |
      FACILITY.ID %in% manual_facility_id_resolution$Correct_FACILITY.ID
  )

n_facility_dupes_after <- Facilities_List %>%
  count(WDID) %>%
  filter(n > 1) %>%
  nrow()

message("  Facilities list: ", n_facility_dupes_before,
        " WDID(s) had multiple FACILITY.ID rows before manual resolution; ",
        n_facility_dupes_after, " remain unresolved (review manual_facility_id_resolution if > 0)")

enforcement_orders_CIWQS <- read.csv(ENFORCEMENT_ORDERS_PATH,
                                     na.strings = c("", "NA", "#N/A"))
message("  Enforcement orders loaded: ", nrow(enforcement_orders_CIWQS), " rows")

# ============================================================================
# 3. Initial Combines  --  FIXED VERSION
# ============================================================================
message("Combining datasets...")

# Standardize Questionnaire column names
names(Questionnaire_txt) <- toupper(names(Questionnaire_txt))

# --------------------------------------------------------------------------
# Correct implausible year-over-year jumps in AnnualReports_txt.
#
# POPULATION SERVED: uses the automated rule (closest to nearest-year
# SSOQ data, or smallest non-zero if SSOQ unavailable) — see
# apply_annual_report_corrections.R for full rationale.
#
# SYSTEM LENGTH: uses MANUALLY REVIEWED values instead of the automated
# rule. The automated rule was checked against direct manual review of
# all flagged systems; in most cases the two agreed, but in a few cases
# (including WDID 2SSO10189) manual review overrode the automated pick —
# see apply_manual_length_corrections.R for the full reviewed list and
# reasoning per system.
#
# Both steps run BEFORE deduplicating to the most recent year, and both
# write a full audit log so every correction is traceable.
# --------------------------------------------------------------------------
source(here("apply_annual_report_corrections.R"))
source(here("apply_manual_length_corrections.R"))

# Population correction (automated)
pop_correction_result <- correct_annual_reports(AnnualReports_txt, Questionnaire_txt)
AnnualReports_txt <- pop_correction_result$corrected_data
annual_report_pop_correction_log <- pop_correction_result$correction_log %>%
  filter(Field == "POP_SERVED_AR")

# System length correction (manual review)
length_correction_result <- apply_manual_length_corrections(AnnualReports_txt)
AnnualReports_txt <- length_correction_result$corrected_data
annual_report_length_correction_log <- length_correction_result$correction_log

if (nrow(annual_report_pop_correction_log) > 0) {
  write.csv(annual_report_pop_correction_log,
            here("outputs", "annual_report_population_correction_log.csv"),
            row.names = FALSE)
  message("  Population correction log saved to: ",
          here("outputs", "annual_report_population_correction_log.csv"))
}

if (nrow(annual_report_length_correction_log) > 0) {
  write.csv(annual_report_length_correction_log,
            here("outputs", "annual_report_length_correction_log.csv"),
            row.names = FALSE)
  message("  Length correction log saved to: ",
          here("outputs", "annual_report_length_correction_log.csv"))
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