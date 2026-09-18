# ============================================================================
# 05_enforcement.R
# Load and prepare enforcement order data, then build the combined
# Inadequacy List from spill performance and reporting criteria only.
#
# NOTE: Systems with active enforcement orders are no longer included in the
# Inadequacy List. They are identified separately as "Under Enforcement"
# systems.
# ============================================================================

message("Preparing enforcement order data...")

# ----------------------------------------------------------------------------
# 1. Load and filter enforcement orders
# ----------------------------------------------------------------------------
enforcement_orders_CIWQS <- enforcement_orders_CIWQS %>%
  filter(FACILITY.ID %in% Facilities_List$FACILITY.ID)

SSSGO_enforcement_orders <- enforcement_orders_CIWQS %>%
  filter(
    PROGRAM.CATEGORY == "SSO",
    STATUS == "Active",
    STATUS.1 == "Active",
    ENFORCEMENT.ACTION.TYPE %in% c(
      "Admin Civil Liability",
      "Cease and Desist Order",
      "Clean-up and Abatement Order",
      "Time Schedule Order",
      "Notice of Violation"
    ),
    as.Date(EFFECTIVE.DATE.1, format = "%m/%d/%Y") >= ANALYSIS_PERIOD_START,
    as.Date(EFFECTIVE.DATE.1, format = "%m/%d/%Y") <  ANALYSIS_PERIOD_END
  )

message("  Active enforcement orders found: ", nrow(SSSGO_enforcement_orders))

# ----------------------------------------------------------------------------
# 2. Add region and size info to enforcement orders
# ----------------------------------------------------------------------------

# Some enforcement rows have a blank WDID in this export even though they
# have a valid FACILITY.ID and clearly belong to a real, identifiable
# system (e.g. Barstow/Daggett Airport CS). Backfill WDID from
# Facilities_List via FACILITY.ID before excluding unmatched rows, so real
# enforcement history isn't silently dropped due to a gap in this specific
# export rather than a genuine identification problem.
facility_id_to_wdid <- Facilities_List %>%
  filter(!is.na(WDID), !is.na(FACILITY.ID)) %>%
  distinct(FACILITY.ID, WDID)

n_wdid_missing_before <- sum(is.na(SSSGO_enforcement_orders$WDID))

SSSGO_enforcement_orders <- SSSGO_enforcement_orders %>%
  left_join(facility_id_to_wdid, by = "FACILITY.ID", suffix = c("", "_backfill")) %>%
  mutate(WDID = coalesce(WDID, WDID_backfill)) %>%
  select(-WDID_backfill)

n_wdid_missing_after <- sum(is.na(SSSGO_enforcement_orders$WDID))
message("  WDID backfilled via FACILITY.ID for ",
        n_wdid_missing_before - n_wdid_missing_after,
        " enforcement row(s); ", n_wdid_missing_after,
        " row(s) remain unmatched (no FACILITY.ID found in Facilities_List) and will be excluded")

SSSGO_enforcement_orders_detailed <- SSSGO_enforcement_orders %>%
  filter(!is.na(WDID)) %>%
  mutate(REGION = substr(WDID, 1, 1)) %>%
  left_join(
    recent_attributes %>%
      select(WDID, Size_Category_Pop_Served, Size_Category_Length,
             POP_SERVED_COMBINED, TOTAL_SYS_LENGTH_COMBINED),
    by = "WDID"
  ) %>%
  select(
    REGION, WDID, FACILITY.NAME,
    Size_Category_Pop_Served, POP_SERVED_COMBINED,
    Size_Category_Length, TOTAL_SYS_LENGTH_COMBINED,
    ENFORCEMENT.ACTION.TYPE, EFFECTIVE.DATE.1,
    TOTAL.ASSESSMENT.AMOUNT, STATUS, STATUS.1,
    everything()
  )

enforcement_wdids <- SSSGO_enforcement_orders_detailed %>%
  distinct(WDID, FACILITY.NAME, REGION, Size_Category_Pop_Served,
           Size_Category_Length, POP_SERVED_COMBINED, TOTAL_SYS_LENGTH_COMBINED)

message("  Unique systems with active enforcement orders: ",
        n_distinct(enforcement_wdids$WDID))

# ----------------------------------------------------------------------------
# 3. Build combined Inadequacy List (spill performance + reporting only)
#    Enforcement orders are intentionally excluded here and identified
#    separately as "Under Enforcement" systems.
# ----------------------------------------------------------------------------
message("Building combined Inadequacy List (spill performance + reporting criteria)...")

combined_inadequate_sssgo_systems <- bind_rows(
  inadequate_reporting_criteria_combined %>%
    select(REGION, WDID, FACILITY.NAME) %>%
    mutate(Source = "Reporting Criteria"),
  low_perf_any_metric %>%
    select(REGION, WDID, FACILITY.NAME) %>%
    mutate(Source = "Low Performance - Spills")
) %>%
  group_by(REGION, WDID, FACILITY.NAME) %>%
  summarise(Source = paste(unique(Source), collapse = " & "), .groups = "drop")

combined_inadequate_sssgo_systems <- combined_inadequate_sssgo_systems %>%
  filter(WDID %in% (Facilities_List %>%
                      filter(CS.ASSESSMENT == "Yes") %>%
                      pull(WDID)))

message("Combined Inadequacy List complete: ",
        n_distinct(combined_inadequate_sssgo_systems$WDID), " systems",
        " (enforcement orders excluded — identified separately as Under Enforcement)")