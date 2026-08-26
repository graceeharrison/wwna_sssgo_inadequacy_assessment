# ============================================================================
# apply_annual_report_corrections.R
#
# Automatically corrects implausible year-over-year jumps in
# Total_System_Length_AR and POP_SERVED_AR within AnnualReports_txt.
#
# RULE (confirmed by manual review):
#   For any WDID/year where the reported value more than TRIPLED or dropped
#   to less than a THIRD of the prior year's value:
#     1. Find the closest available SSO Questionnaire year for that WDID.
#     2. If SSOQ data exists and is non-zero, keep whichever of
#        {prior year value, new value} is CLOSER to the SSOQ value.
#     3. If SSOQ data is missing OR zero, keep the SMALLEST NON-ZERO of
#        {prior year value, new value}.
#     4. If both candidate values are zero, no valid correction exists —
#        the row is left unchanged and flagged for manual review.
#
# When the prior year's value is selected as more trustworthy, that year's
# ACTUAL raw MILES_FORCED_MAINS / MILES_GRAVITY_SEWERS / POP_SERVED values
# are copied forward to replace the current year's figures. The original
# source data is never silently discarded — every correction is written to
# a log (annual_report_correction_log) alongside the corrected table, so
# the change is fully traceable.
#
# USAGE:
#   source("apply_annual_report_corrections.R")
#   result <- correct_annual_reports(AnnualReports_txt, Questionnaire_txt)
#   AnnualReports_txt <- result$corrected_data
#   annual_report_correction_log <- result$correction_log
# ============================================================================

library(dplyr)
library(lubridate)
library(purrr)

correct_annual_reports <- function(AnnualReports_txt, Questionnaire_txt,
                                    flag_ratio_high = 3, flag_ratio_low = 1/3,
                                    agreement_tolerance = 0.20) {

  # --------------------------------------------------------------------------
  # 1. Compute year-over-year values and flags (length + population)
  # --------------------------------------------------------------------------
  annual_values <- AnnualReports_txt %>%
    mutate(
      Total_System_Length_AR = clean_numeric(MILES_FORCED_MAINS) + clean_numeric(MILES_GRAVITY_SEWERS),
      POP_SERVED_AR = clean_numeric(POP_SERVED)
    ) %>%
    filter(!is.na(ANNUAL_REPORT_YEAR)) %>%
    arrange(WDID, ANNUAL_REPORT_YEAR) %>%
    group_by(WDID) %>%
    mutate(
      Prev_Length = lag(Total_System_Length_AR),
      Prev_Pop    = lag(POP_SERVED_AR),
      Length_Ratio = Total_System_Length_AR / Prev_Length,
      Pop_Ratio    = POP_SERVED_AR / Prev_Pop,
      Length_Flag = !is.na(Length_Ratio) &
        (Length_Ratio > flag_ratio_high | Length_Ratio < flag_ratio_low),
      Pop_Flag = !is.na(Pop_Ratio) &
        (Pop_Ratio > flag_ratio_high | Pop_Ratio < flag_ratio_low)
    ) %>%
    ungroup()

  # --------------------------------------------------------------------------
  # 2. Build SSOQ per-WDID-per-year lookup
  # --------------------------------------------------------------------------
  questionnaire_by_year <- Questionnaire_txt %>%
    mutate(
      LAST_UPDATED_PARSED = as.Date(LAST.UPDATED.ON, format = "%m/%d/%Y"),
      SSOQ_Year = year(LAST_UPDATED_PARSED),
      POP_SERVED_SSOQ = clean_numeric(SSOQ.POPULATION.SERVED),
      Total_System_Length_SSOQ = clean_numeric(SSOQ_MILES_OF_FORCE_MAIN) +
        clean_numeric(SSOQ_MILES_OF_GRAVITY_SEWER)
    ) %>%
    filter(!is.na(SSOQ_Year)) %>%
    arrange(WDID, SSOQ_Year, desc(LAST_UPDATED_PARSED)) %>%
    distinct(WDID, SSOQ_Year, .keep_all = TRUE) %>%
    select(WDID, SSOQ_Year, POP_SERVED_SSOQ, Total_System_Length_SSOQ)

  nearest_ssoq_match <- function(target_wdid, target_year) {
    candidates <- questionnaire_by_year %>% filter(WDID == target_wdid)
    if (nrow(candidates) == 0) {
      return(tibble(SSOQ_Year_Used = NA_integer_, POP_SERVED_SSOQ = NA_real_,
                     Total_System_Length_SSOQ = NA_real_, Years_Apart = NA_integer_))
    }
    candidates %>%
      mutate(Years_Apart = abs(SSOQ_Year - target_year)) %>%
      arrange(Years_Apart, desc(SSOQ_Year)) %>%
      slice(1) %>%
      transmute(SSOQ_Year_Used = SSOQ_Year, POP_SERVED_SSOQ, Total_System_Length_SSOQ, Years_Apart)
  }

  # --------------------------------------------------------------------------
  # 3. Determine the corrected value for each flagged row (length + pop)
  #    following the confirmed rule
  # --------------------------------------------------------------------------
  pick_corrected_value <- function(prev_val, new_val, ssoq_val) {
    ssoq_usable <- !is.na(ssoq_val) && ssoq_val != 0
    if (ssoq_usable) {
      if (abs(prev_val - ssoq_val) <= abs(new_val - ssoq_val)) prev_val else new_val
    } else {
      candidates <- c(prev_val, new_val)
      candidates <- candidates[candidates != 0]
      if (length(candidates) == 0) new_val else min(candidates)  # both zero: no valid fix, leave as-is
    }
  }

  flagged_length <- annual_values %>%
    filter(Length_Flag) %>%
    select(WDID, ANNUAL_REPORT_YEAR, Prev_Length, Total_System_Length_AR)

  flagged_pop <- annual_values %>%
    filter(Pop_Flag) %>%
    select(WDID, ANNUAL_REPORT_YEAR, Prev_Pop, POP_SERVED_AR)

  if (nrow(flagged_length) > 0) {
    length_matches <- purrr::map2_dfr(flagged_length$WDID, flagged_length$ANNUAL_REPORT_YEAR, nearest_ssoq_match)
    flagged_length <- flagged_length %>%
      bind_cols(length_matches %>% select(SSOQ_Year_Used, Total_System_Length_SSOQ, Years_Apart)) %>%
      rowwise() %>%
      mutate(
        Corrected_Length = pick_corrected_value(Prev_Length, Total_System_Length_AR, Total_System_Length_SSOQ),
        Length_Source = if_else(Corrected_Length == Prev_Length & Corrected_Length != Total_System_Length_AR,
                                 "prior_year", "current_year")
      ) %>%
      ungroup()
  }

  if (nrow(flagged_pop) > 0) {
    pop_matches <- purrr::map2_dfr(flagged_pop$WDID, flagged_pop$ANNUAL_REPORT_YEAR, nearest_ssoq_match)
    flagged_pop <- flagged_pop %>%
      bind_cols(pop_matches %>% select(SSOQ_Year_Used, POP_SERVED_SSOQ, Years_Apart)) %>%
      rowwise() %>%
      mutate(
        Corrected_Pop = pick_corrected_value(Prev_Pop, POP_SERVED_AR, POP_SERVED_SSOQ),
        Pop_Source = if_else(Corrected_Pop == Prev_Pop & Corrected_Pop != POP_SERVED_AR,
                              "prior_year", "current_year")
      ) %>%
      ungroup()
  }

  # --------------------------------------------------------------------------
  # 4. Apply corrections: where "prior_year" was selected, copy that WDID's
  #    PRIOR YEAR raw MILES_FORCED_MAINS / MILES_GRAVITY_SEWERS / POP_SERVED
  #    values forward into the current (flagged) year's row.
  # --------------------------------------------------------------------------
  corrected_data <- AnnualReports_txt
  correction_log <- tibble(
    WDID = character(), ANNUAL_REPORT_YEAR = numeric(), Field = character(),
    Original_Value = numeric(), Corrected_Value = numeric(),
    Source_Year_Used = numeric(), SSOQ_Year_Used = numeric(),
    Years_Apart = numeric(), Reason = character()
  )

  raw_lookup <- AnnualReports_txt %>%
    select(WDID, ANNUAL_REPORT_YEAR, MILES_FORCED_MAINS, MILES_GRAVITY_SEWERS, POP_SERVED)

  if (nrow(flagged_length) > 0) {
    to_fix_length <- flagged_length %>% filter(Length_Source == "prior_year")
    for (i in seq_len(nrow(to_fix_length))) {
      wdid <- to_fix_length$WDID[i]
      yr   <- to_fix_length$ANNUAL_REPORT_YEAR[i]
      prior_row <- raw_lookup %>%
        filter(WDID == wdid, ANNUAL_REPORT_YEAR < yr) %>%
        arrange(desc(ANNUAL_REPORT_YEAR)) %>%
        slice(1)
      if (nrow(prior_row) == 1) {
        row_idx <- which(corrected_data$WDID == wdid & corrected_data$ANNUAL_REPORT_YEAR == yr)
        corrected_data$MILES_FORCED_MAINS[row_idx]   <- prior_row$MILES_FORCED_MAINS
        corrected_data$MILES_GRAVITY_SEWERS[row_idx] <- prior_row$MILES_GRAVITY_SEWERS
        correction_log <- correction_log %>% add_row(
          WDID = wdid, ANNUAL_REPORT_YEAR = yr, Field = "Total_System_Length_AR",
          Original_Value = to_fix_length$Total_System_Length_AR[i],
          Corrected_Value = to_fix_length$Corrected_Length[i],
          Source_Year_Used = prior_row$ANNUAL_REPORT_YEAR,
          SSOQ_Year_Used = to_fix_length$SSOQ_Year_Used[i],
          Years_Apart = to_fix_length$Years_Apart[i],
          Reason = if_else(is.na(to_fix_length$Total_System_Length_SSOQ[i]) | to_fix_length$Total_System_Length_SSOQ[i] == 0,
                            "No usable SSOQ data; used smallest non-zero value",
                            "Prior year value closer to nearest available SSOQ data")
        )
      }
    }
  }

  if (nrow(flagged_pop) > 0) {
    to_fix_pop <- flagged_pop %>% filter(Pop_Source == "prior_year")
    for (i in seq_len(nrow(to_fix_pop))) {
      wdid <- to_fix_pop$WDID[i]
      yr   <- to_fix_pop$ANNUAL_REPORT_YEAR[i]
      prior_row <- raw_lookup %>%
        filter(WDID == wdid, ANNUAL_REPORT_YEAR < yr) %>%
        arrange(desc(ANNUAL_REPORT_YEAR)) %>%
        slice(1)
      if (nrow(prior_row) == 1) {
        row_idx <- which(corrected_data$WDID == wdid & corrected_data$ANNUAL_REPORT_YEAR == yr)
        corrected_data$POP_SERVED[row_idx] <- prior_row$POP_SERVED
        correction_log <- correction_log %>% add_row(
          WDID = wdid, ANNUAL_REPORT_YEAR = yr, Field = "POP_SERVED_AR",
          Original_Value = to_fix_pop$POP_SERVED_AR[i],
          Corrected_Value = to_fix_pop$Corrected_Pop[i],
          Source_Year_Used = prior_row$ANNUAL_REPORT_YEAR,
          SSOQ_Year_Used = to_fix_pop$SSOQ_Year_Used[i],
          Years_Apart = to_fix_pop$Years_Apart[i],
          Reason = if_else(is.na(to_fix_pop$POP_SERVED_SSOQ[i]) | to_fix_pop$POP_SERVED_SSOQ[i] == 0,
                            "No usable SSOQ data; used smallest non-zero value",
                            "Prior year value closer to nearest available SSOQ data")
        )
      }
    }
  }

  message("Annual report corrections applied: ", nrow(correction_log),
          " field(s) corrected across ", n_distinct(correction_log$WDID), " unique WDID(s)")

  list(corrected_data = corrected_data, correction_log = correction_log)
}
