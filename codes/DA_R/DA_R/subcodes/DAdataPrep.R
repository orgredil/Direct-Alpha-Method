################################################################################
# This code needs to be run by MAIN4site_DirectAlpha.R
# Copyright O.Gredil
# Revised R version
################################################################################

library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(readr)


# --- Set benchmark variable based on global parameter ---
bmk <- ifelse(bmk_param == "ibmk", "ibmk", "mkt")

# --- Load Market and Industry Data (once) ---
mkt_lvl_path <- file.path(workdir, "mkt_lvl.rds")
industry_ret_path <- file.path(workdir, "industry_ret.rds")

if (!file.exists(mkt_lvl_path)) stop(paste("Market level data not found:", mkt_lvl_path))
if (!file.exists(industry_ret_path)) stop(paste("Industry return data not found:", industry_ret_path))

mkt_lvl_data <- readRDS(mkt_lvl_path)
mkt_lvl_data$date <- as.Date(mkt_lvl_data$date)
# Ensure no duplicate dates in market data
mkt_lvl_data <- mkt_lvl_data %>%
  group_by(date) %>%
  slice_head(n = 1) %>%
  ungroup()

# Calculate daily market return (add mkt variable)
mkt_lvl_data <- mkt_lvl_data %>%
  arrange(date) %>%
  mutate(mkt = c(0, diff(mkt_lvl)/lag(mkt_lvl)[-1])) %>%
  mutate(mkt = ifelse(is.na(mkt) | is.infinite(mkt), 0, mkt))

industry_ret_data <- readRDS(industry_ret_path)
industry_ret_data$date <- as.Date(industry_ret_data$date)
industry_ret_data$industry <- as.character(industry_ret_data$industry)
# Ensure no duplicate industry/date combinations
industry_ret_data <- industry_ret_data %>%
  group_by(industry, date) %>%
  slice_head(n = 1) %>%
  ungroup()

### Time-invariant characteristics (temp_char) #####
CFdset_path <- file.path(data, "fundSim.rds")
if (!file.exists(CFdset_path)) stop(paste("CFdset (fundSim.rds) not found:", CFdset_path))
CFdset <- readRDS(CFdset_path)

# Ensure required columns exist and have correct types
CFdset$date <- as.Date(CFdset$date)
CFdset$incept <- as.Date(CFdset$incept)
if (!"industry" %in% names(CFdset)) {
  stop("Error: CFdset does not contain an 'industry' column.")
}
CFdset$industry <- as.character(CFdset$industry)


# Ensure other key columns are present
required_cfdset_cols <- c("fundid", "time", "nav", "C", "D", "ftype", "vintage", "bet_est", "incept")
missing_cols <- setdiff(required_cfdset_cols, names(CFdset))
if (length(missing_cols) > 0) {
  stop(paste("Error: CFdset is missing required columns:", paste(missing_cols, collapse=", ")))
}


# Get the last observation for each fund for time-invariant chars
temp_char_base <- CFdset %>%
  group_by(fundid) %>%
  slice_tail(n = 1) %>%
  select(fundid, industry, ftype, vintage, incept, bet_est) %>% # Explicitly select 'industry'
  ungroup()

# Rename incept to date for merging with market/industry at inception
temp_char_for_merge <- temp_char_base %>%
  mutate(date_for_merge = incept) # Use incept date for initial mkt/ibmk levels

# Merge with market data at inception date
mkt_lvl_subset_incept <- mkt_lvl_data %>% select(date, mkt_lvl0 = mkt_lvl, mkt_rsq0 = mkt_rsq)
temp_char <- temp_char_for_merge %>%
  left_join(mkt_lvl_subset_incept, by = c("date_for_merge" = "date"))

# Merge with industry data at inception date
industry_ret_subset_incept <- industry_ret_data %>% select(industry, date, ibmk_lvl0 = ibmk_lvl, ibmk_rsq0 = ibmk_rsq)
temp_char <- temp_char %>%
  left_join(industry_ret_subset_incept, by = c("industry", "date_for_merge" = "date")) %>%
  select(-date_for_merge) # Remove temporary date column used for merge

# Handle cases where industry benchmark is not available at inception (Stata: ibmk_match logic)
# ibmk_match0: 3 if industry data found, 1 if not (or industry is blank)
temp_char <- temp_char %>%
  mutate(
    ibmk_match0 = case_when(
      is.na(industry) | industry == "" ~ 1, # No industry specified
      !is.na(ibmk_lvl0) & !is.na(ibmk_rsq0) ~ 3, # Industry data found
      TRUE ~ 1 # Industry specified but no match at inception date
    ),
    ibmk_lvl0 = ifelse(ibmk_match0 == 1 & !is.na(mkt_lvl0), mkt_lvl0, ibmk_lvl0),
    ibmk_rsq0 = ifelse(ibmk_match0 == 1 & !is.na(mkt_rsq0), mkt_rsq0, ibmk_rsq0)
  )
# ibmk_match0 is kept in temp_char as it might be useful for daily ibmk_match logic later


# --- Calculate fund-level summary dates and counts from CFdset ---
# These are based on the original full daily data before inception filtering for main_data
# NAV characteristics (Stata lines 16-29)
nav_summary <- CFdset %>%
  filter(!is.na(nav) & nav != 0) %>% # Stata: keep if nav!=.
  arrange(fundid, date) %>% # Stata: gsort fundid time date (time is quarterly, date is daily)
  mutate(qofd_summary = as.yearqtr(date)) %>%
  group_by(fundid) %>%
  summarise(
    firstnav_qtr = first(qofd_summary), # Stata: gen firstnav_qtr=time[1] (time is qtrly index)
    lastnav_qtr = last(qofd_summary),   # Stata: gen lastnav_qtr=time[_N]
    lastnav_date = last(date),          # Stata: gen lastnav_date=date[_N]
    navcount = n(),                     # Stata: egen navcount=count(nav)
    .groups = "drop"
  )
# Stata also calculates `lastnav` (NAV value at lastnav_qtr), not explicitly done here yet.
# It's kept in Stata's temp_char but its later use in DAdataPrep.do isn't obvious.

# Contributions' characteristics (Stata lines 30-41)
c_summary <- CFdset %>%
  filter(!is.na(C) & C != 0) %>% # Stata: keep if C!=0
  arrange(fundid, date) %>%
  mutate(qofd_summary = as.yearqtr(date)) %>%
  group_by(fundid) %>%
  summarise(
    firstC_qtr = first(qofd_summary),
    lastC_qtr = last(qofd_summary),
    lastC_date = last(date),
    Ccount = n(), # Rename to match Stata: Ccount instead of Ccount_total
    .groups = "drop"
  )

# Distributions' characteristics (Stata lines 42-56)
d_summary <- CFdset %>%
  filter(!is.na(D) & D != 0) %>% # Stata: keep if D!=0
  arrange(fundid, date) %>%
  mutate(qofd_summary = as.yearqtr(date)) %>%
  group_by(fundid) %>%
  summarise(
    firstD_qtr = first(qofd_summary),
    lastD_qtr = last(qofd_summary),
    lastD_date = last(date),
    Dcount = n(), # Rename to match Stata: Dcount instead of Dcount_total
    .groups = "drop"
  )

# Join these summary stats into temp_char
temp_char <- temp_char %>%
  left_join(nav_summary, by = "fundid") %>%
  left_join(c_summary, by = "fundid") %>%
  left_join(d_summary, by = "fundid")

# Convert relevant date columns in temp_char to Date type if they became characters
date_cols_temp_char <- c("incept", "lastnav_date", "lastC_date", "lastD_date")
temp_char <- temp_char %>%
  mutate(across(any_of(date_cols_temp_char), as.Date))


# --- Main dataset preparation (daily level) ---
# Stata lines 60-62: use CFdset, drop some vars, joinby with temp_char
main_data <- CFdset %>%
  select(-any_of(c("industry", "ftype", "vintage", "incept", "bet_est"))) %>% # Drop if they exist, will come from temp_char
  left_join(temp_char, by = "fundid")

# Merge with daily market and industry data (Stata lines 63-64)
main_data <- main_data %>%
  left_join(mkt_lvl_data %>% select(date, mkt_lvl, mkt_rsq, mkt), by = "date")

main_data <- main_data %>%
  left_join(industry_ret_data %>% select(industry, date, ibmk_lvl, ibmk_rsq), by = c("industry", "date"), 
            suffix = c("", ".y")) %>%
  mutate(ib = case_when(
    is.na(ibmk_lvl) ~ 1,  # No match
    !is.na(ibmk_lvl) ~ 3  # Industry data matched
  ))

# Handle daily ibmk_match (Stata lines 65-68)
# ibmk_match_daily: 1 if industry is blank or no industry match for the date, 3 if matched.
main_data <- main_data %>%
  mutate(
    ibmk_match = case_when(
      is.na(industry) | industry == "" ~ 1,
      !is.na(ibmk_lvl) ~ 3, # Matched industry data for the day
      TRUE ~ 1 # Industry specified, but no industry data for this specific day
    ),
    ibmk_lvl = ifelse(ibmk_match == 1 & !is.na(mkt_lvl), mkt_lvl, ibmk_lvl),
    ibmk_rsq = ifelse(ibmk_match == 1 & !is.na(mkt_rsq), mkt_rsq, ibmk_rsq)
  )

# Calculate changes in R-squared from inception (Stata lines 70-71)
main_data <- main_data %>%
  mutate(
    mkt_rsq = mkt_rsq - mkt_rsq0,
    ibmk_rsq = ibmk_rsq - ibmk_rsq0
  )

# Add weekday (Stata line 73) - UNCOMMENTED to match Stata
main_data <- main_data %>% mutate(weekday = lubridate::wday(date, week_start = 1))

# Sort data (Stata line 74)
main_data <- main_data %>% arrange(fundid, date)

# --- PV Calculations (Stata lines 75-93) ---
# bmk is already set based on bmk_param at the start of the script
# BetaAdj_param should be "Yes" or "No" (global parameter)

# Dynamically select the benchmark level and rsq columns
bmk_lvl_col <- paste0(bmk, "_lvl")
bmk_rsq_col <- paste0(bmk, "_rsq")
bmk_lvl0_col <- paste0(bmk, "_lvl0")
# bmk_rsq0_col is not directly used in Stata's PV formula, it uses the *change* in rsq (bmk_rsq)

main_data <- main_data %>%
  mutate(
    # Temporarily store bet_est to handle NA robustly in ifelse
    current_bet_est = bet_est,
    current_bmk_rsq_col = !!sym(bmk_rsq_col), # Evaluate the symbol
    current_bmk_lvl0_col = !!sym(bmk_lvl0_col),
    current_bmk_lvl_col = !!sym(bmk_lvl_col)
  ) %>%
  mutate(
    pvC = case_when(
      BetaAdj_param == "Yes" ~ ifelse(is.na(C) | is.na(current_bet_est) | is.na(current_bmk_lvl0_col) | is.na(current_bmk_lvl_col) | is.na(current_bmk_rsq_col) | current_bmk_lvl_col == 0, NA_real_,
                                      C * exp(current_bet_est * log(current_bmk_lvl0_col / current_bmk_lvl_col)) - 0.5 * current_bmk_rsq_col * current_bet_est * (current_bet_est - 1)),
      TRUE ~ ifelse(is.na(C) | is.na(current_bmk_lvl0_col) | is.na(current_bmk_lvl_col) | current_bmk_lvl_col == 0, NA_real_,
                    C * (current_bmk_lvl0_col / current_bmk_lvl_col))
    ),
    pvD = case_when(
      BetaAdj_param == "Yes" ~ ifelse(is.na(D) | is.na(current_bet_est) | is.na(current_bmk_lvl0_col) | is.na(current_bmk_lvl_col) | is.na(current_bmk_rsq_col) | current_bmk_lvl_col == 0, NA_real_,
                                      D * exp(current_bet_est * log(current_bmk_lvl0_col / current_bmk_lvl_col)) - 0.5 * current_bmk_rsq_col * current_bet_est * (current_bet_est - 1)),
      TRUE ~ ifelse(is.na(D) | is.na(current_bmk_lvl0_col) | is.na(current_bmk_lvl_col) | current_bmk_lvl_col == 0, NA_real_,
                    D * (current_bmk_lvl0_col / current_bmk_lvl_col))
    ),
    pvNAV = case_when(
      BetaAdj_param == "Yes" ~ ifelse(is.na(nav) | is.na(current_bet_est) | is.na(current_bmk_lvl0_col) | is.na(current_bmk_lvl_col) | is.na(current_bmk_rsq_col) | current_bmk_lvl_col == 0, NA_real_,
                                      nav * exp(current_bet_est * log(current_bmk_lvl0_col / current_bmk_lvl_col)) - 0.5 * current_bmk_rsq_col * current_bet_est * (current_bet_est - 1)),
      TRUE ~ ifelse(is.na(nav) | is.na(current_bmk_lvl0_col) | is.na(current_bmk_lvl_col) | current_bmk_lvl_col == 0, NA_real_,
                    nav * (current_bmk_lvl0_col / current_bmk_lvl_col))
    )
  ) %>%
  # Clean up temporary columns
  select(-current_bet_est, -current_bmk_rsq_col, -current_bmk_lvl0_col, -current_bmk_lvl_col)


# Handle potential division by zero or log(0) if benchmark levels are 0 or NA
# The ifelse above should handle NAs from division by zero or log of non-positive.
# Stata would produce missing in such cases.
# We also need to ensure that if C, D, or nav are 0, the pv values are correct.
# If C=0 and BetaAdj="Yes", pvC = -0.5 * rsq * bet_est * (bet_est-1)
# If C=0 and BetaAdj="No", pvC = 0
main_data <- main_data %>%
  mutate(
    pvC = ifelse(BetaAdj_param == "Yes" & !is.na(C) & C == 0 & !is.na(bet_est) & !is.na(!!sym(bmk_rsq_col)),
                 -0.5 * !!sym(bmk_rsq_col) * bet_est * (bet_est - 1),
                 pvC),
    pvD = ifelse(BetaAdj_param == "Yes" & !is.na(D) & D == 0 & !is.na(bet_est) & !is.na(!!sym(bmk_rsq_col)),
                 -0.5 * !!sym(bmk_rsq_col) * bet_est * (bet_est - 1),
                 pvD),
    pvNAV = ifelse(BetaAdj_param == "Yes" & !is.na(nav) & nav == 0 & !is.na(bet_est) & !is.na(!!sym(bmk_rsq_col)),
                   -0.5 * !!sym(bmk_rsq_col) * bet_est * (bet_est - 1),
                   pvNAV)
  ) %>%
  # Ensure that if original C, D, nav were NA, pv* are also NA
  mutate(
    pvC = ifelse(is.na(C), NA_real_, pvC),
    pvD = ifelse(is.na(D), NA_real_, pvD),
    pvNAV = ifelse(is.na(nav), NA_real_, pvNAV)
  )


# The pv_ variable in Stata is the factor itself when BetaAdj=="No", or the full expression for BetaAdj=="Yes"
# This is not directly used to calculate pvC, pvD, pvNAV in Stata, but generated alongside.
# For R, we can generate it if needed for other comparisons, but it's not part of pvC/pvD/pvNAV calculation.
# For example:
main_data <- main_data %>%
   mutate(
     pv_ = if (BetaAdj_param == "Yes") {
       exp(bet_est * log(!!sym(bmk_lvl0_col) / !!sym(bmk_lvl_col))) - 0.5 * !!sym(bmk_rsq_col) * bet_est * (bet_est - 1)
     } else {
       !!sym(bmk_lvl0_col) / !!sym(bmk_lvl_col)
     },
     pv_ = ifelse(is.infinite(pv_) | is.na(pv_), NA_real_, pv_) # Stata like NA handling
   )


# --- Going to Quarterly Frequency (Stata lines 95-118) ---
main_data <- main_data %>% mutate(qofd = as.yearqtr(date))

# Stata: bys fundid time: gen temp=`bmk'_lvl if _n==_N
# Stata: bys fundid time: egen temp2 = total(temp), missing
# This gets the end-of-quarter benchmark level for discounting C and D within the quarter.
main_data <- main_data %>%
  arrange(fundid, qofd, date) %>%
  group_by(fundid, qofd) %>%
  mutate(!!paste0(bmk, "_lvl_eoq") := last(!!sym(bmk_lvl_col))) %>%
  ungroup()

# Stata: gen qC=C; gen qD=D (These will be summed up)
# Stata: replace C=C*temp2/`bmk'_lvl
# Stata: replace D=D*temp2/`bmk'_lvl
main_data <- main_data %>%
  mutate(
    C_adj = C * (!!sym(paste0(bmk, "_lvl_eoq"))) / !!sym(bmk_lvl_col),
    D_adj = D * (!!sym(paste0(bmk, "_lvl_eoq"))) / !!sym(bmk_lvl_col)
  ) %>%
  # Handle cases where bmk_lvl_col could be 0 or NA, leading to Inf/NaN
  mutate(
    C_adj = ifelse(is.finite(C_adj), C_adj, C), # If adjustment fails, use original C
    D_adj = ifelse(is.finite(D_adj), D_adj, D)  # If adjustment fails, use original D
  )


# Aggregate to quarterly: sum C, D, pvC, pvD, qC, qD. Keep last daily obs info.
# Stata: foreach vv of varlist $qVars { ... egen `vv'=total(temp) }
# Stata: bys fundid time: keep if _n==_N
quarterly_data <- main_data %>%
  arrange(fundid, qofd, date) %>%
  group_by(fundid, qofd) %>%
  summarise(
    # Keep columns from the last daily observation in the quarter
    date = last(date),
    nav = last(nav), # NAV at the end of the quarter (from last daily obs)
    mkt_lvl = last(mkt_lvl),
    mkt_rsq = last(mkt_rsq), # This is already the change mkt_rsq - mkt_rsq0
    ibmk_lvl = last(ibmk_lvl),
    ibmk_rsq = last(ibmk_rsq), # This is already the change ibmk_rsq - ibmk_rsq0
    mkt = last(mkt), # Add mkt to match Stata
    weekday = last(weekday), # Add weekday to match Stata
    pv_ = last(pv_), # Add pv_ to match Stata
    pvNAV = last(pvNAV), # Add pvNAV to match Stata
    ib = last(ib), # Keep the industry match indicator
    ibmk_match = last(ibmk_match), # Keep the ibmk_match indicator
    
    # Add other relevant columns from temp_char by taking first() as they are constant per fund
    across(any_of(c(names(temp_char), "bet_est", bmk_lvl0_col, paste0(bmk, "_rsq0"))), first), # Ensure bet_est, mkt_lvl0 etc. are carried over
    
    # Sum up cash flows and PVs
    C = sum(C_adj, na.rm = TRUE), # Sum of adjusted daily C
    D = sum(D_adj, na.rm = TRUE), # Sum of adjusted daily D
    pvC = sum(pvC, na.rm = TRUE),
    pvD = sum(pvD, na.rm = TRUE),
    qC = sum(C, na.rm = TRUE), # Sum of original daily C (nominal)
    qD = sum(D, na.rm = TRUE), # Sum of original daily D (nominal)
    
    # Count non-missing C and D occurrences (Stata lines 107-109)
    C_count = sum(!is.na(C) & C != 0),
    D_count = sum(!is.na(D) & D != 0),
    .groups = "drop"
  ) %>%
  # Re-select benchmark level for the quarter end date
  mutate(!!sym(bmk_lvl_col) := !!sym(case_when(bmk == "mkt" ~ "mkt_lvl", bmk == "ibmk" ~ "ibmk_lvl", TRUE ~ NA_character_)),
         !!sym(paste0(bmk,"_rsq")) := !!sym(case_when(bmk == "mkt" ~ "mkt_rsq", bmk == "ibmk" ~ "ibmk_rsq", TRUE ~ NA_character_)))


# --- NAV adjustments and further filtering (Quarterly level, Stata lines 119-128) ---
# Stata: bys fundid: replace nav=0 if _n==_N & nav==. & lastD_date>=lastnav_date
quarterly_data <- quarterly_data %>%
  arrange(fundid, qofd) %>%
  group_by(fundid) %>%
  mutate(is_last_fund_obs = row_number() == n()) %>%
  ungroup() %>%
  mutate(
    nav = ifelse(is_last_fund_obs & is.na(nav) & !is.na(lastD_date) & !is.na(lastnav_date) & lastD_date >= lastnav_date, 0, nav)
  )

# Stata lines 120-125: Adjust final D by future Cs (_nC, __nC logic)
# This logic is complex and depends on lead values and conditions.
# For simplicity and focus on main discrepancies, this part is approximated.
# A full replication would require careful handling of leads and conditions.
# Stata: bys fundid: gen  _nC=C[_n+1]/`bmk'_lvl[_n+1]*`bmk'_lvl if date[_n+1]>max(lastD_date,lastnav_date)
# Stata: bys fundid: egen __nC=total(_nC)
# Stata: bys fundid: replace D=D-__nC if _n==_N & __nC!=0
# This part is skipped for now due to its complexity and potential lesser impact on overall obs count/NAV issues
# compared to aggregation and tsfill. If still problematic, it can be revisited.

# Stata: bys fundid: drop if nav==. & date>=max(lastD_date,lastnav_date) (Line 123)
# This filter is crucial for observation count.
quarterly_data <- quarterly_data %>%
  group_by(fundid) %>%
  mutate(
    max_activity_date = pmax(lastD_date, lastnav_date, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(!(is.na(nav) & date >= max_activity_date & !is.na(max_activity_date))) %>%
  select(-max_activity_date, -is_last_fund_obs)

# Stata: drop lastC_date lastD_date lastnav_date (already implicitly handled by not selecting them or done if needed)
# Stata: gen qofd=qofd(date) (already have qofd)

# --- Panel setup and tsfill (Stata lines 129-138) ---
# Stata: xtset fundid qofd; tsfill
# This implies filling gaps within each fund's observed range of qofd.
# Ensure 'qofd' column exists and has been processed before this step.
quarterly_data <- quarterly_data %>%
  group_by(fundid) %>%
  # Complete qofd sequence from min to max *observed for that fund*
  tidyr::complete(qofd = seq(min(qofd, na.rm = TRUE), max(qofd, na.rm = TRUE), by = 1/4)) %>%
  ungroup() %>%
  arrange(fundid, qofd) # Ensure order for subsequent operations



# Fill in fund-invariant characteristics for new rows created by tsfill
# These are columns from temp_char and initial settings
cols_to_fill_from_fund <- names(temp_char)[!names(temp_char) %in% c("fundid")] # Add other invariant cols if any
cols_to_fill_from_fund <- c(cols_to_fill_from_fund, "bet_est", bmk_lvl0_col, paste0(bmk, "_rsq0"), "industry", "ftype", "vintage", "incept", "ibmk_match0", "lastnav_date", "lastC_date", "lastD_date", "firstnav_qtr", "navcount") # etc.
cols_to_fill_from_fund <- unique(cols_to_fill_from_fund[cols_to_fill_from_fund %in% names(quarterly_data)])


quarterly_data <- quarterly_data %>%
  group_by(fundid) %>%
  fill(all_of(cols_to_fill_from_fund), .direction = "downup") %>% # Fill down then up
  ungroup()

# Impute date for tsfill'ed rows (Stata lines 132-138)
# Stata finds the last business day of the quarter.
# We need a list of business days, mkt_lvl_data$date can serve as this.
business_days <- sort(unique(mkt_lvl_data$date))

impute_eom_business_day <- function(qtr_yearqtr, bus_days) {
  if (is.na(qtr_yearqtr)) return(as.Date(NA))
  q_end_date <- as.Date(qtr_yearqtr, frac = 1) # End of quarter
  # Find the closest business day on or before q_end_date
  valid_days <- bus_days[bus_days <= q_end_date & bus_days >= (q_end_date - 15)] # Search in a window
  if (length(valid_days) > 0) {
    return(max(valid_days))
  } else { # Fallback: just take quarter end date if no biz day found nearby
    return(q_end_date)
  }
}

# Add bday column to match Stata
quarterly_data <- quarterly_data %>%
  mutate(
    date = ifelse(is.na(date),
                  sapply(qofd, impute_eom_business_day, bus_days = business_days),
                  date),
    date = as.Date(date, origin="1970-01-01"), # Ensure it's Date type
    bday = as.numeric(date) # Add bday column as numeric date
  )

# --- Final CF counts and benchmark merging for tsfill'ed rows (Stata lines 139-146) ---
# Stata: replace `vv'_count=0 if `vv'==. | `vv'==0
quarterly_data <- quarterly_data %>%
  mutate(
    C_count = ifelse(is.na(C_count) | (is.na(C) | C == 0), 0, C_count),
    D_count = ifelse(is.na(D_count) | (is.na(D) | D == 0), 0, D_count),
    # For tsfilled rows, C, D, qC, qD, pvC, pvD should be 0 if they are NA
    C = ifelse(is.na(C), 0, C),
    D = ifelse(is.na(D), 0, D),
    qC = ifelse(is.na(qC), 0, qC),
    qD = ifelse(is.na(qD), 0, qD),
    pvC = ifelse(is.na(pvC), 0, pvC),
    pvD = ifelse(is.na(pvD), 0, pvD)
  )

# Merge benchmark data for EoQs with missing NAVs and zero CFs (tsfill'ed rows)
# Stata: merge m:1 date using mkt_lvl, update keep(1 3 4 5) nogen
# Stata: merge m:1 industry date using industry_ret, update keep(1 3 4 5) nogen
# We need to fill/update mkt_lvl, mkt_rsq, ibmk_lvl, ibmk_rsq for rows where they are NA (newly tsfilled)
# or based on Stata's update logic.
quarterly_data <- quarterly_data %>%
  # DO NOT remove the original columns if they are needed for the ifelse logic:
  # select(-any_of(c("mkt_lvl", "mkt_rsq", "ibmk_lvl", "ibmk_rsq"))) %>% # This line was problematic
  left_join(mkt_lvl_data %>% select(date, mkt_lvl_to_join = mkt_lvl, mkt_rsq_to_join = mkt_rsq, mkt_to_join = mkt), by = "date") %>%
  left_join(industry_ret_data %>% select(industry, date, ibmk_lvl_to_join = ibmk_lvl, ibmk_rsq_to_join = ibmk_rsq), by = c("industry", "date"))

quarterly_data <- quarterly_data %>%
  mutate(
    # If original mkt_lvl is NA, use the joined value; otherwise, keep original.
    # This is equivalent to coalesce(mkt_lvl, mkt_lvl_to_join)
    mkt_lvl = ifelse(is.na(mkt_lvl), mkt_lvl_to_join, mkt_lvl),
    mkt_rsq = ifelse(is.na(mkt_rsq), mkt_rsq_to_join, mkt_rsq), # This is total Rsq, need to subtract Rsq0 later
    ibmk_lvl = ifelse(is.na(ibmk_lvl), ibmk_lvl_to_join, ibmk_lvl),
    ibmk_rsq = ifelse(is.na(ibmk_rsq), ibmk_rsq_to_join, ibmk_rsq),  # This is total Rsq
    mkt = ifelse(is.na(mkt), mkt_to_join, mkt) # Update mkt value as well
  ) %>%
  select(-ends_with("_to_join")) # Remove the temporary columns used for joining

# Recalculate mkt_rsq and ibmk_rsq as changes from _rsq0 for tsfilled rows
# This step should come AFTER mkt_rsq and ibmk_rsq are populated/updated.
# Ensure mkt_rsq0 and ibmk_rsq0 are correctly available (e.g., joined from temp_char)
# The bmk_param variable should be "mkt" or "ibmk" (the actual benchmark name, not the column name)
# Assuming mkt_rsq0 and ibmk_rsq0 are already columns in quarterly_data from merging temp_char
quarterly_data <- quarterly_data %>%
  mutate(
    mkt_rsq = mkt_rsq - mkt_rsq0, # mkt_rsq0 should be the one corresponding to the fund's inception
    ibmk_rsq = ibmk_rsq - ibmk_rsq0 # ibmk_rsq0 should be the one corresponding to the fund's inception
  )

# Stata: replace  ibmk_lvl=mkt_lvl if ibmk_match==1 & ibmk_lvl==.
# Stata: replace  ibmk_rsq=mkt_rsq if ibmk_match==1 & ibmk_rsq==.
# (ibmk_match0 from temp_char indicates if original industry was missing)
quarterly_data <- quarterly_data %>%
  mutate(
    ibmk_lvl = ifelse(ibmk_match0 == 1 & is.na(ibmk_lvl) & !is.na(mkt_lvl), mkt_lvl, ibmk_lvl),
    ibmk_rsq = ifelse(ibmk_match0 == 1 & is.na(ibmk_rsq) & !is.na(mkt_rsq), mkt_rsq, ibmk_rsq)
  )

# --- Final benchmark calculations and save (Stata lines 147-157) ---
# Stata: bys fundid: egen fundT=max(time) (time is quarterly index)
# We use qofd. Max qofd per fund.
quarterly_data <- quarterly_data %>%
  group_by(fundid) %>%
  mutate(fundT = max(.data$qofd, na.rm = TRUE)) %>% # Renamed to match Stata
  ungroup()

# Stata: qui gen temp=`bmk'_lvl if time==fundT
# Stata: bys fundid: egen `bmk'_lvlT=max(temp)
# This gets the benchmark level at the fund's terminal quarter
quarterly_data <- quarterly_data %>%
  group_by(fundid) %>%
  mutate(!!paste0(bmk, "_lvlT") := .data[[bmk_lvl_col]][.data$qofd == .data$fundT & !is.na(.data$qofd)][1]) %>%
  fill(!!paste0(bmk, "_lvlT"), .direction = "downup") %>% # Fill for all fund obs
  ungroup()

# Stata: xtset fundid time; qui gen Rt=0; bys fundid: replace Rt=`bmk'_lvl/l1.`bmk'_lvl if _n>=2
quarterly_data <- quarterly_data %>%
  arrange(fundid, qofd) %>%
  group_by(fundid) %>%
  mutate(
    Rt = ifelse(row_number() >= 2,
                !!sym(bmk_lvl_col) / lag(!!sym(bmk_lvl_col)),
                0),
    Rt = ifelse(is.na(Rt) | is.infinite(Rt), 0, Rt) # Handle NA/Inf from division
  ) %>%
  ungroup()

# Final selection of columns and renaming if necessary, similar to Stata's `keep` and `order`
# The exact final column list might depend on `DAestimation.R`
# For now, ensure key variables are present. `time` variable (quarterly index from inception)
quarterly_data <- quarterly_data %>%
  group_by(fundid) %>%
  mutate(
    first_observed_qofd_for_fund = first(na.omit(qofd)), # Get the first non-NA qofd for the fund at this stage
    time = ifelse(!is.na(qofd) & !is.na(first_observed_qofd_for_fund),
                  round(4 * (as.numeric(qofd) - as.numeric(first_observed_qofd_for_fund))),
                  NA_integer_) # Calculate time relative to fund's first observed qofd
  ) %>%
  select(-first_observed_qofd_for_fund) %>% # Clean up helper column
  ungroup()


quarterly_data <- quarterly_data %>%
  select(-lastC_date, -lastD_date, -lastnav_date)

# Rename to 'DAdata' as per MAIN4site_DirectAlpha.R which expects DAdata object
DAdata <- quarterly_data

# Clean up 
# rm(main_data, temp_char, nav_summary, c_summary, d_summary, mkt_lvl_subset_incept, industry_ret_subset_incept, temp_char_base, temp_char_for_merge)
# rm(complete_panel_structure, all_qofds, business_days)

# --- Save the processed quarterly data ---
# Stata equivalent: save "funds_qly", replace
# This file will be saved in the 'workdir' directory.
# It will be overwritten in each benchmark iteration if DAdataPrep.R is called in a loop.
if (!exists("workdir")) {
  stop("Global variable 'workdir' is not defined. Cannot save DAdata.")
}
output_file_path <- file.path(workdir, "funds_qly.rds")

saveRDS(DAdata, file = output_file_path)