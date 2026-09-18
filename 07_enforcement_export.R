# ============================================================================
# 07_under_enforcement.R
# Identify systems with active enforcement orders
# Produces a separate "Under Enforcement" list — these systems are NOT
# included in the Inadequacy List.
# ============================================================================

message("Building Under Enforcement list...")

# ----------------------------------------------------------------------------
# 1. Build comprehensive under-enforcement export table
# ----------------------------------------------------------------------------
enforcement_details_summary <- SSSGO_enforcement_orders %>%
  filter(!is.na(WDID)) %>%
  group_by(WDID) %>%
  summarise(
    Enforcement_Count       = n(),
    Enforcement_Types       = paste(unique(ENFORCEMENT.ACTION.TYPE), collapse = "; "),
    Latest_Enforcement_Date = max(as.Date(EFFECTIVE.DATE.1, format = "%m/%d/%Y"),
                                  na.rm = TRUE),
    Total_Assessment_Amount = sum(as.numeric(TOTAL.ASSESSMENT.AMOUNT), na.rm = TRUE),
    .groups = "drop"
  )

under_enforcement_systems <- enforcement_wdids %>%
  left_join(
    Facilities_List %>%
      select(WDID, FACILITY.ID, PROGRAM.CATEGORY, COMMUNITY.TYPE,
             FEDERAL.FACILITY, CS.ASSESSMENT),
    by = "WDID"
  ) %>%
  left_join(enforcement_details_summary, by = "WDID") %>%
  mutate(
    Also_On_Inadequacy_List = WDID %in% combined_inadequate_sssgo_systems$WDID,
    Inadequacy_Source = if_else(
      Also_On_Inadequacy_List,
      combined_inadequate_sssgo_systems$Source[
        match(WDID, combined_inadequate_sssgo_systems$WDID)],
      NA_character_
    )
  ) %>%
  filter(CS.ASSESSMENT == "Yes") %>%
  select(
    REGION, WDID, FACILITY.NAME, FACILITY.ID, PROGRAM.CATEGORY,
    COMMUNITY.TYPE, FEDERAL.FACILITY,
    Size_Category_Pop_Served, POP_SERVED_COMBINED,
    Size_Category_Length, TOTAL_SYS_LENGTH_COMBINED,
    Enforcement_Count, Enforcement_Types,
    Latest_Enforcement_Date, Total_Assessment_Amount,
    Also_On_Inadequacy_List, Inadequacy_Source
  ) %>%
  arrange(REGION, FACILITY.NAME)

message("  Under enforcement systems (CS Assessment = Yes): ",
        nrow(under_enforcement_systems))

# ----------------------------------------------------------------------------
# 2. Export
# ----------------------------------------------------------------------------
enforcement_filename <- paste0("Under_Enforcement_Systems_", Sys.Date(), ".xlsx")
enforcement_output_path <- file.path(OUTPUT_PATH, enforcement_filename)

write_xlsx(
  list(
    Under_Enforcement        = under_enforcement_systems,
    Enforcement_Orders_Detail = SSSGO_enforcement_orders_detailed
  ),
  enforcement_output_path
)

message("Under enforcement export complete: ",
        nrow(under_enforcement_systems),
        " systems saved to ", enforcement_output_path)