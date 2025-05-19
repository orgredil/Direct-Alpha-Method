################################################################################
# This code needs to be run by MAIN4site_DirectAlpha.R
# Copyright O.Gredil
# Revised R version
################################################################################

# Assumes the following global variables are set by the calling script (e.g., MAIN4site_DirectAlpha.R):
# - workdir: path to scratch folder (for loading funds_qly.rds)
# - output_param: path to output folder (for saving fund_EOLest)
# - codes_param: path to subcodes folder (for sourcing irr_mata.R)
# - bmk_param: current benchmark being processed (e.g., "mkt" or "ibmk")
# - BetaAdj: "Yes" or "No"
# - DAplus: "Yes" or "No"

# Load libraries
library(dplyr)
library(zoo) # For na.approx

# Source the irr_mata function
irr_mata_path <- file.path(codes, "irr_mata.R")
if (!file.exists(irr_mata_path)) {
  stop(paste("DAestimation.R: irr_mata.R not found at", irr_mata_path))
}
source(irr_mata_path)

# --- Load Data and Initial Sort ---
DAdata_qly_path <- file.path(workdir, "funds_qly.rds")
if (!file.exists(DAdata_qly_path)) {
  stop(paste("DAestimation.R: funds_qly.rds not found at", DAdata_qly_path))
}
DAdata_qly <- readRDS(DAdata_qly_path)
DAdata_qly <- DAdata_qly %>% arrange(fundid, time) # 'time' is 0-indexed quarter



# --- Benchmark Setup ---
bmk_actual <- ifelse(bmk_param == "ibmk", "ibmk", "mkt")


# Define benchmark column names dynamically
bmk_lvl_col <- paste0(bmk, "_lvl")
bmk_lvl0_col <- paste0(bmk, "_lvl0")
bmk_lvlT_col <- paste0(bmk, "_lvlT") # Created in DAdataPrep.R
bmk_rsq_col <- paste0(bmk, "_rsq")   # Created in DAdataPrep.R as change from rsq0

# --- Time-series calculations (on the full quarterly panel) ---
DAdata_calc <- DAdata_qly %>%
  arrange(fundid, time) # Ensure correct order for cumulative sums and lags

# Calculate rc_0t (log(bmk_lvl / bmk_lvl0))
DAdata_calc <- DAdata_calc %>%
  mutate(
    rc_0t = log(!!sym(bmk_lvl_col) / !!sym(bmk_lvl0_col)),
    rc_0t = ifelse(is.finite(rc_0t), rc_0t, NA_real_)
  )

# Calculate __dfac (helper for cumC_fv, cumD_fv)
# __dfac = exp(log(bmk_lvlT/bmk_lvl0) - rc_0t) = (bmk_lvlT/bmk_lvl0) / (bmk_lvl/bmk_lvl0) = bmk_lvlT / bmk_lvl
DAdata_calc <- DAdata_calc %>%
  mutate(
    `__dfac` = !!sym(bmk_lvlT_col) / !!sym(bmk_lvl_col), # Note: Using backticks for __dfac due to double underscore
    `__dfac` = ifelse(is.finite(`__dfac`), `__dfac`, NA_real_)
  )

# Calculate C_fvT, D_fvT (helpers for cumC_fv, cumD_fv)
DAdata_calc <- DAdata_calc %>%
  mutate(
    C_fvT = C * `__dfac`,
    D_fvT = D * `__dfac`
  )

# Calculate cumC_fv, cumD_fv
# Stata: bys fundid: gen cumC_fv=sum(C*__dfac)/__dfac
DAdata_calc <- DAdata_calc %>%
  group_by(fundid) %>%
  mutate(
    sum_C_fvT_running = cumsum(tidyr::replace_na(C_fvT, 0)),
    sum_D_fvT_running = cumsum(tidyr::replace_na(D_fvT, 0))
  ) %>%
  mutate(
    cumC_fv = sum_C_fvT_running / `__dfac`,
    cumD_fv = sum_D_fvT_running / `__dfac`,
    cumC_fv = ifelse(is.finite(cumC_fv), cumC_fv, NA_real_),
    cumD_fv = ifelse(is.finite(cumD_fv), cumD_fv, NA_real_)
  ) %>%
  ungroup() # Keep __dfac for now if needed by other calcs, or select it out later

# PME calculation and imputation (Stata lines 7, 21-25)
DAdata_calc <- DAdata_calc %>%
  group_by(fundid) %>%
  mutate(
    sum_pvD_running = cumsum(tidyr::replace_na(pvD, 0)),
    sum_pvC_running = cumsum(tidyr::replace_na(pvC, 0)),
    # Initial PME based on pvD, pvNAV, pvC
    PME_initial = (sum_pvD_running + pvNAV) / sum_pvC_running, # pvNAV is from DAdataPrep
    PME_initial = ifelse(sum_pvC_running == 0, NA_real_, PME_initial)
  ) %>%
  ungroup() %>%
  mutate(
    # Alternative PME based on cumD_fv, nav, cumC_fv
    `_PME_calc` = (cumD_fv + nav) / cumC_fv, # nav is from DAdataPrep
    `_PME_calc` = ifelse(is.na(cumC_fv) | cumC_fv == 0, NA_real_, `_PME_calc`)
  ) %>%
  mutate(
    PME = ifelse(is.na(PME_initial), `_PME_calc`, PME_initial),
    PME = ifelse(is.na(PME) & time == 0, 0.98, PME) # Stata: time==0 is first quarter
  )

# Impute PME (Stata: ipolate PME bday)
DAdata_calc <- DAdata_calc %>%
  arrange(fundid, bday) %>% # bday is numeric date from DAdataPrep.R
  group_by(fundid) %>%
  mutate(PME_interpolated = zoo::na.approx(PME, bday, na.rm = FALSE, rule = 2)) %>% # rule=2 for extrapolation
  ungroup() %>%
  mutate(PME = ifelse(is.na(PME), PME_interpolated, PME))

# nav_ln (Stata line 26: cumC_fv-cumD_fv)
DAdata_calc <- DAdata_calc %>%
  mutate(nav_ln = cumC_fv - cumD_fv)

# nav_i (Stata line 28: cond(nav!=.,nav,(cumC_fv-C)*_PMEi+C*0.98 - cumD_fv))
# _PMEi is the imputed PME. So use the final PME column.
DAdata_calc <- DAdata_calc %>%
  mutate(
    nav_i = ifelse(!is.na(nav), nav, (cumC_fv - C) * PME + C * 0.98 - cumD_fv)
  )

# Update pvNAV (Stata lines 29-34)
DAdata_calc <- DAdata_calc %>%
  mutate(
    pvNAV_update_factor = if (BetaAdj_param == "Yes") {
      exp(-rc_0t * bet_est)
    } else {
      exp(-rc_0t)
    },
    pvNAV_update_factor = ifelse(is.finite(pvNAV_update_factor), pvNAV_update_factor, NA_real_),
    # Update pvNAV only if it was NA initially
    pvNAV = ifelse(is.na(DAdata_qly$pvNAV), nav_i * pvNAV_update_factor, DAdata_qly$pvNAV) # Use original pvNAV for condition
  )

# TVPI (Stata line 35: (sum(D)+nav_i)/sum(C))
DAdata_calc <- DAdata_calc %>%
  group_by(fundid) %>%
  mutate(
    sum_D_running = cumsum(tidyr::replace_na(D,0)),
    sum_C_running = cumsum(tidyr::replace_na(C,0)),
    TVPI = (sum_D_running + nav_i) / sum_C_running,
    TVPI = ifelse(sum_C_running == 0, NA_real_, TVPI)
  ) %>%
  ungroup()

# Filter time > 60 (Stata line 36: drop if time>60)
DAdata_filtered <- DAdata_calc %>% filter(time <= 60 | is.na(time))

# fundTe (Stata line 37: bys fundid: egen fundTe=max(time)) - on filtered data
DAdata_filtered <- DAdata_filtered %>%
  group_by(fundid) %>%
  mutate(fundTe = if(n() > 0 && any(!is.na(time))) max(time, na.rm = TRUE) else NA_integer_) %>%
  ungroup() %>%
  mutate(fundTe = ifelse(is.infinite(fundTe), NA_integer_, fundTe)) # Handle cases where all time might be NA after filter

# Update nav with nav_i if nav is missing (Stata line 38)
DAdata_filtered <- DAdata_filtered %>%
  mutate(nav = ifelse(is.na(nav), nav_i, nav))

# s_cd (for IRRcd calculation, Stata line 44)
DAdata_filtered <- DAdata_filtered %>%
  mutate(
    s_cd = ifelse(is.na(cumD_fv) | cumD_fv == 0, 1, (cumC_fv - nav_i) / cumD_fv),
    s_cd = ifelse(is.finite(s_cd), s_cd, NA_real_)
  )

# s_ca, NAVca, Dca (Stata lines 46-50)
DAdata_filtered <- DAdata_filtered %>%
  mutate(
    # Stata: cond(D+nav_i<D,1,abs(D)/(abs(D)+abs(nav_i))) -> cond(nav_i<0, ...)
    s_ca = ifelse(!is.na(nav_i) & nav_i < 0, 1,
                  ifelse( (abs(D) + abs(nav_i)) == 0 | is.na(abs(D) + abs(nav_i)), NA_real_,
                          abs(D) / (abs(D) + abs(nav_i)) ) )
  )

# Recursive calculation for NAVca and Dca
calculate_NAVca_Dca_for_fund <- function(df_fund) {
  if (nrow(df_fund) == 0) return(df_fund %>% mutate(NAVca = NA_real_, Dca = NA_real_)) # Add columns if empty
  df_fund <- df_fund %>% arrange(time)
  
  # Initialize NAVca with nav_i and Dca with D for all rows first
  df_fund$NAVca <- df_fund$nav_i
  df_fund$Dca <- df_fund$D
  
  if (nrow(df_fund) > 1) {
    for (i in 2:nrow(df_fund)) {
      # NAVca[_n-1] refers to the already calculated NAVca from the previous step
      prev_NAVca <- df_fund$NAVca[i-1] 
      Rt_curr <- df_fund$Rt[i] # Rt from DAdataPrep.R
      C_curr <- df_fund$C[i]
      s_ca_curr <- df_fund$s_ca[i]
      
      if (is.na(prev_NAVca) || is.na(Rt_curr) || is.na(C_curr) || is.na(s_ca_curr)) {
        df_fund$NAVca[i] <- NA_real_
        df_fund$Dca[i] <- NA_real_
      } else {
        prev_term <- prev_NAVca * Rt_curr + C_curr
        df_fund$NAVca[i] <- (1 - s_ca_curr) * prev_term
        df_fund$Dca[i] <- s_ca_curr * prev_term
      }
    }
  }
  return(df_fund)
}

DAdata_filtered <- DAdata_filtered %>%
  group_by(fundid) %>%
  group_modify(~ calculate_NAVca_Dca_for_fund(.x)) %>%
  ungroup()

# --- IRR Calculations ---
# This requires your irr_mata.R function.
# Source it: source("DA_R/subcodes/irr_mata.R") # Assuming it's in the right path
# The Stata script calculates IRRs within a loop for each fund.
# We'll use group_by and then apply the logic.

all_funds_irr_data <- DAdata_filtered %>%
  group_by(fundid) %>%
  do({
    fund_df <- .
    fund_df <- fund_df %>% arrange(time) # Ensure sorted by time for CF construction
    current_fundid <- unique(fund_df$fundid)
    # rmax_time is fundTe for this fund (max time after time<=60 filter)
    rmax_time <- unique(fund_df$fundTe) 
    if(length(rmax_time) == 0 || is.na(rmax_time)) rmax_time <- -1 # handle no valid rmax_time
    
    # Helper to prepare CF and call IRR, annualizes result
    calculate_irr_for_series <- function(cf_series_val, final_nav_val, rmax_time_val, time_vec_val, nav_series_for_final_nav) {
      if (length(cf_series_val) == 0) return(NA_real_)
      
      # Get the specific final NAV value at rmax_time from the provided nav_series
      final_nav_at_rmax <- NA_real_
      if (rmax_time_val != -1 && rmax_time_val %in% time_vec_val) {
        idx_rmax_for_nav <- match(rmax_time_val, time_vec_val)
        if(!is.na(idx_rmax_for_nav) && length(nav_series_for_final_nav) >= idx_rmax_for_nav) {
          final_nav_at_rmax <- nav_series_for_final_nav[idx_rmax_for_nav]
        }
      }
      
      idx_rmax_for_cf <- if(rmax_time_val != -1) match(rmax_time_val, time_vec_val) else NA_integer_
      
      if (!is.na(idx_rmax_for_cf) && !is.na(final_nav_at_rmax)) {
        cf_series_val[idx_rmax_for_cf] <- cf_series_val[idx_rmax_for_cf] + final_nav_at_rmax
      }
      
      cf_clean <- cf_series_val[!is.na(cf_series_val)]
      # Stata: replace CF=. if _cumCF==0 (sum of all CFs is 0)
      if (length(cf_clean) < 2 || (sum(cf_clean, na.rm=TRUE) == 0 && length(cf_clean) > 0) ) return(NA_real_)
      
      # irr_mata.R expects CFs sorted DESCENDING by time (CF_T, ..., CF_0)
      # If fund_df is sorted by time (0...T), then rev(cf_clean)
      res_irr_val <- irr_mata(rev(cf_clean)) 
      if(is.na(res_irr_val)) return(NA_real_)
      return(res_irr_val^4 - 1) # Annualize: (1+IRR_qtr)^4 - 1
    }
    
    # IRR (Stata uses 'nav' which is updated nav)
    cf_irr <- fund_df$D - fund_df$C
    irr_val <- calculate_irr_for_series(cf_irr, NA, rmax_time, fund_df$time, fund_df$nav)
    
    # IRRq (Stata uses 'nav')
    cf_irrq <- fund_df$qD - fund_df$qC # qD, qC from DAdataPrep.R
    irrq_val <- calculate_irr_for_series(cf_irrq, NA, rmax_time, fund_df$time, fund_df$nav)
    
    # IRRpv (DA_raw in Stata, uses nav_i)
    cf_irr_pv <- fund_df$D_fvT - fund_df$C_fvT
    irrpv_val <- calculate_irr_for_series(cf_irr_pv, NA, rmax_time, fund_df$time, fund_df$nav_i)
    
    # IRRpv2 (DA_badj in Stata, uses pvNAV)
    cf_irr_pv2 <- fund_df$pvD - fund_df$pvC
    irrpv2_val <- calculate_irr_for_series(cf_irr_pv2, NA, rmax_time, fund_df$time, fund_df$pvNAV)
    
    # IRRln (uses nav_ln)
    cf_irr_ln <- fund_df$D - fund_df$C
    irrln_val <- calculate_irr_for_series(cf_irr_ln, NA, rmax_time, fund_df$time, fund_df$nav_ln)
    
    # IRRcd (uses nav, and s_cd at rmax_time)
    s_cd_rmax <- NA_real_
    if(rmax_time != -1 && rmax_time %in% fund_df$time) {
      s_cd_rmax <- fund_df$s_cd[fund_df$time == rmax_time][1]
    }
    if(is.na(s_cd_rmax) && !is.null(fund_df$s_cd) && nrow(fund_df)>0) s_cd_rmax <- 1 # Default if NA, check Stata logic carefully
    cf_irr_cd <- fund_df$D * s_cd_rmax - fund_df$C
    irrcd_val <- calculate_irr_for_series(cf_irr_cd, NA, rmax_time, fund_df$time, fund_df$nav)
    
    # IRRst (uses NAVca)
    cf_irr_st <- fund_df$Dca - fund_df$C
    irrst_val <- calculate_irr_for_series(cf_irr_st, NA, rmax_time, fund_df$time, fund_df$NAVca)
    
    tibble::tibble(
      fundid = current_fundid,
      IRR = irr_val, IRRq = irrq_val, IRRpv = irrpv_val, IRRpv2 = irrpv2_val,
      IRRln = irrln_val, IRRcd = irrcd_val, IRRst = irrst_val
    )
  }) %>% ungroup()

# --- Summarize to one row per fund and add remaining calculations ---
# Stata: bys fundid: keep if _n==_N (line 179)
# This means taking the last observation's values for time-varying fields.
DAdata_summary <- DAdata_filtered %>%
  group_by(fundid) %>%
  arrange(time) %>%
  slice_tail(n = 1) %>% # Get the last observation for each fund
  ungroup()

# Join IRR results
DAdata_summary <- DAdata_summary %>%
  left_join(all_funds_irr_data, by = "fundid")

# Dnum, Cnum (Stata lines 172-175)
# These are counts of quarters with D_count > 0 or C_count > 0
# D_count and C_count are from DAdataPrep.R (quarterly counts of daily C/D)
num_summary <- DAdata_filtered %>% # Use DAdata_filtered (panel before slice_tail)
  group_by(fundid) %>%
  summarise(
    Dnum = sum(D_count > 0, na.rm = TRUE),
    Cnum = sum(C_count > 0, na.rm = TRUE),
    .groups = "drop"
  )
DAdata_summary <- DAdata_summary %>% left_join(num_summary, by = "fundid")

# vol_bmk (Stata lines 176-177)
vol_summary <- DAdata_filtered %>% # Use DAdata_filtered
  group_by(fundid) %>%
  summarise(
    vol_bmk = sqrt(mean(!!sym(bmk_rsq_col), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(vol_bmk = ifelse(is.nan(vol_bmk) | is.infinite(vol_bmk), NA_real_, vol_bmk))
DAdata_summary <- DAdata_summary %>% left_join(vol_summary, by = "fundid")

# durationEstFV, durationEst (Stata lines 168-169)
DAdata_summary <- DAdata_summary %>%
  mutate(
    durationEstFV = ifelse(PME > 0 & (1 + IRRpv) > 0 & !is.na(PME) & !is.na(IRRpv) & IRRpv != 0, log(PME) / log(1 + IRRpv), NA_real_),
    durationEst   = ifelse(TVPI > 0 & (1 + IRR) > 0 & !is.na(TVPI) & !is.na(IRR) & IRR != 0, log(TVPI) / log(1 + IRR), NA_real_)
  )

# anKSpme (Stata line 184)
DAdata_summary <- DAdata_summary %>%
  mutate(anKSpme = ifelse(PME > 0 & !is.na(PME), exp(log(PME)/5.0) - 1, NA_real_))

# DA_raw, DA_badj (Stata lines 185-186)
DAdata_summary <- DAdata_summary %>%
  mutate(DA_raw = IRRpv, DA_badj = IRRpv2)

# alf_LN, alf_CD, alf_ST (Stata lines 187-189)
DAdata_summary <- DAdata_summary %>%
  mutate(
    alf_LN = IRR - IRRln,
    alf_CD = IRR - IRRcd,
    alf_ST = IRR - IRRst
  )

# --- Final variable selection ---
# Stata drops: C_count, D_count, Rt, _cumCF, and intermediate IRRs (IRRln, IRRcd, IRRst, IRRq)
# Helper variables like __dfac, C_fvT, D_fvT, sum_C_fvT_running etc. should also be dropped.
# The list of variables in your R output `names(fund_EOLest_ibmk)` is the target structure,
# plus the newly added ones, and minus Ccount, Dcount.

final_vars_to_keep <- c(
  "fundid", "qofd", "date", "nav", "mkt_lvl", "mkt_rsq", "ibmk_lvl", "ibmk_rsq", "mkt", "weekday",
  "pv_", "pvNAV", "ib", "ibmk_match", "industry", "ftype", "vintage", "incept", "bet_est",
  "mkt_lvl0", "mkt_rsq0", "ibmk_lvl0", "ibmk_rsq0", "ibmk_match0",
  "firstnav_qtr", "lastnav_qtr", "navcount", # navcount is kept
  "firstC_qtr", "lastC_qtr", # Ccount dropped
  "firstD_qtr", "lastD_qtr", # Dcount dropped
  "C", "D", "pvC", "pvD", "qC", "qD", "bday", "fundT", "ibmk_lvlT", "time",
  "PME", "nav_ln", "nav_i", "TVPI", "IRR", "IRRpv", "IRRpv2",
  "durationEstFV", "durationEst", "Dnum", "Cnum", "vol_bmk", "anKSpme",
  "DA_raw", "DA_badj", "alf_LN", "alf_CD", "alf_ST",
  # Newly added and should be kept:
  "rc_0t", "cumC_fv", "cumD_fv", "fundTe", "NAVca", "Dca"
)
# Ensure all selected columns actually exist in DAdata_summary to avoid errors
existing_final_vars <- intersect(final_vars_to_keep, names(DAdata_summary))

fund_EOLest_final <- DAdata_summary %>%
  select(all_of(existing_final_vars))


# --- Save Results ---
output_filepath <- file.path(output, paste0("fund_EOLest_", bmk_actual, ".rds"))
saveRDS(fund_EOLest_final, file = output_filepath)
cat("DAestimation.R: Results saved to", output_filepath, "\n")

# Clean up large intermediate object if desired, though R's scoping usually handles this
# when the script/function finishes.
# rm(DAdata_qly)
# gc()


