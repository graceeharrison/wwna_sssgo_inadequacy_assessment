# ============================================================================
# apply_manual_length_corrections.R
#
# Applies MANUALLY REVIEWED system length corrections to AnnualReports_txt.
#
# This replaces the automated "closest to SSOQ, else smallest non-zero"
# rule for Total_System_Length_AR with values chosen by direct manual
# review of flagged_length_changes_for_review.xlsx. Population corrections
# are NOT affected by this file and continue to use the automated rule in
# apply_annual_report_corrections.R.
#
# The lookup table below (manual_length_review) is a direct transcription
# of the Review_Verdict column from the reviewed spreadsheet. In most
# rows, the reviewer's chosen value matches either the prior year's or
# current year's actual filed report — those are applied by copying that
# year's real raw MILES_FORCED_MAINS / MILES_GRAVITY_SEWERS values forward,
# preserving traceability to an actual state filing.
#
# FOUR rows do not match either the prior or current year's raw value —
# the reviewer instead entered the SSO Questionnaire's own reported figure
# directly (WDID 3SSO10336, 5SSO10868), or explicitly chose to trust the
# current year's report over the automated fallback rule (WDID 5SSO20047,
# 2SSO10189, including reversing the Small->Micro reclassification that
# the automated rule would have produced for 2SSO10189). For these four,
# there is no single annual report year to copy raw values from, so the
# corrected total is applied directly: the full corrected value is placed
# in MILES_GRAVITY_SEWERS and MILES_FORCED_MAINS is set to 0. This is a
# synthetic split done only to preserve the correct COMBINED total feeding
# Total_System_Length_AR — the two raw fields are not used independently
# anywhere else in this pipeline, only summed, so this split has no other
# downstream effect. The original, unmodified raw values remain visible
# in the correction log for full traceability.
# ============================================================================

library(dplyr)

manual_length_review <- tribble(
  ~WDID,        ~ANNUAL_REPORT_YEAR, ~Corrected_Length,
  "5SSO10964",  2024,                1.3,
  "5SSO11292",  2024,                0.7,
  "5SSO11320",  2025,                1,
  "5SSO11604",  2025,                1.9,
  "6SSO11378",  2024,                10,
  "8SSO10611",  2024,                1,
  "8SSO11146",  2024,                9,
  "9SSO20059",  2025,                0.1,
  "5SSO11224",  2025,                73.1,
  "5SSO10994",  2024,                41,
  "6SSO11108",  2024,                2,
  "9SSO10695",  2025,                43.5,
  "8SSO10554",  2025,                74.9,
  "8SSO10613",  2024,                18.7,
  "9SSO11514",  2025,                4,
  "5SSO10949",  2025,                14,
  "9SSO11530",  2024,                40,
  "5SSO11311",  2025,                9,
  "3SSO10284",  2025,                112,
  "5SSO11068",  2024,                10.5,
  "3SSO10336",  2025,                33,     # manual: SSOQ value used directly
  "9SSO10667",  2024,                83.7,
  "5SSO10908",  2025,                12,
  "5SSO20047",  2025,                6,      # manual: kept current-year value over fallback rule
  "5SSO10991",  2024,                13,
  "5SSO10991",  2025,                13,
  "3SSO10329",  2025,                15,
  "5SSO10868",  2024,                18.3,   # manual: SSOQ value used directly
  "9SSO11203",  2024,                17.8,
  "2SSO10189",  2025,                33.2    # manual: kept current-year value, reverses Small->Micro flip
)

apply_manual_length_corrections <- function(AnnualReports_txt) {

  corrected_data <- AnnualReports_txt
  correction_log <- tibble(
    WDID = character(), ANNUAL_REPORT_YEAR = numeric(),
    Original_Value = numeric(), Corrected_Value = numeric(),
    Method = character()
  )

  raw_lookup <- AnnualReports_txt %>%
    mutate(Total_System_Length_AR = clean_numeric(MILES_FORCED_MAINS) + clean_numeric(MILES_GRAVITY_SEWERS)) %>%
    select(WDID, ANNUAL_REPORT_YEAR, MILES_FORCED_MAINS, MILES_GRAVITY_SEWERS, Total_System_Length_AR)

  for (i in seq_len(nrow(manual_length_review))) {
    wdid <- manual_length_review$WDID[i]
    yr   <- manual_length_review$ANNUAL_REPORT_YEAR[i]
    target_value <- manual_length_review$Corrected_Length[i]

    row_idx <- which(corrected_data$WDID == wdid & corrected_data$ANNUAL_REPORT_YEAR == yr)
    if (length(row_idx) == 0) next

    current_value <- raw_lookup %>% filter(WDID == wdid, ANNUAL_REPORT_YEAR == yr) %>% pull(Total_System_Length_AR)
    prior_row <- raw_lookup %>%
      filter(WDID == wdid, ANNUAL_REPORT_YEAR < yr) %>%
      arrange(desc(ANNUAL_REPORT_YEAR)) %>%
      slice(1)

    if (!is.na(current_value) && isTRUE(all.equal(current_value, target_value))) {
      # Reviewer confirmed the current year's value as-is — no change needed
      method <- "current_year_confirmed"

    } else if (nrow(prior_row) == 1 && isTRUE(all.equal(prior_row$Total_System_Length_AR, target_value))) {
      # Reviewer's chosen value matches the prior year's actual filed report —
      # copy that year's real raw values forward
      corrected_data$MILES_FORCED_MAINS[row_idx]   <- prior_row$MILES_FORCED_MAINS
      corrected_data$MILES_GRAVITY_SEWERS[row_idx] <- prior_row$MILES_GRAVITY_SEWERS
      method <- paste0("copied_from_prior_year_", prior_row$ANNUAL_REPORT_YEAR)

    } else {
      # Reviewer's value doesn't match either year on file (e.g. SSOQ value
      # used directly) — apply the total directly; see file header comment
      corrected_data$MILES_FORCED_MAINS[row_idx]   <- 0
      corrected_data$MILES_GRAVITY_SEWERS[row_idx] <- target_value
      method <- "manual_value_applied_directly"
    }

    correction_log <- correction_log %>% add_row(
      WDID = wdid, ANNUAL_REPORT_YEAR = yr,
      Original_Value = current_value, Corrected_Value = target_value,
      Method = method
    )
  }

  message("Manual length corrections applied: ", nrow(correction_log), " row(s)")
  list(corrected_data = corrected_data, correction_log = correction_log)
}
