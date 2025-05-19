# Load necessary libraries
library(data.table)
library(dplyr)
library(lubridate)
library(zoo)
# bizdays library is not strictly needed for the revised bday logic if mkt_lvl.rds is the sole source
# but might be used elsewhere or if a fallback is desired. For now, keeping it.
library(bizdays) 


# --- Script Parameters & Defaults ---
# These variables are expected to be set by the calling script (e.g., MAIN4site_DirectAlpha.R)
# However, for dummyFundRet, we set a default if not provided, to match Stata's FundDataSim.do behavior.

if (!exists("dummyFundRet")) {
  dummyFundRet <- "No" # Default value, as effectively set in FundDataSim.do
  cat("FundDataSim.R: 'dummyFundRet' was not defined by the calling script. Using default:", dummyFundRet, "\n")
}
# Other expected global variables from MAIN4site_DirectAlpha.R:
# FundsNumber, fVintage, lVintage, data (output path), CFdset, workdir (input path for rds files)

# Set seed for reproducibility (matches Stata's `set seed 2014`)
set.seed(2014)

# --- Load and Prepare Initial Data ---
# Expects industry_ret.rds in workdir, created by BenchmarkPrep.R
industry_ret_full <- readRDS(file.path(workdir, "industry_ret.rds"))
# Ensure date is Date type
industry_ret_full$date <- as.Date(industry_ret_full$date)


# Convert character global variables to numeric
FundsNumber_num <- as.numeric(FundsNumber)
fVintage_num <- as.numeric(fVintage)
lVintage_num <- as.numeric(lVintage)

# Prepare fund shell based on unique industries
fund_data <- industry_ret_full %>%
  select(industry) %>%
  distinct()

if (nrow(fund_data) == 0) {
  stop("No industries found in industry_ret.rds. Cannot simulate funds.")
}

fundsPerInd <- ceiling(FundsNumber_num / nrow(fund_data))

fund_data <- fund_data[rep(seq_len(nrow(fund_data)), each = fundsPerInd), ]

# Generate year and determine min/max years for vintage generation
temp_year_data <- industry_ret_full %>%
  mutate(year = year(date)) %>%
  filter(!is.na(year))

if (nrow(temp_year_data) == 0) {
  stop("Could not determine year range from industry_ret.rds dates.")
}

minyr_val <- min(temp_year_data$year, na.rm = TRUE) + 1
maxyr_val <- max(temp_year_data$year, na.rm = TRUE) - 6

# Ensure vintage range is valid
min_vintage_draw <- max(minyr_val, fVintage_num)
max_vintage_draw <- min(maxyr_val, lVintage_num)

if (min_vintage_draw > max_vintage_draw) {
  stop(paste("Invalid vintage range for simulation:", min_vintage_draw, "to", max_vintage_draw,
             ". Check fVintage, lVintage, and data date range."))
}

fund_data <- fund_data %>%
  mutate(
    vintage = sample(seq(min_vintage_draw, max_vintage_draw), n(), replace = TRUE),
    fundid = 1:n(),
    incept_month = sample(1:12, n(), replace = TRUE),
    incept_day = sample(1:28, n(), replace = TRUE) # Simplified day generation
  ) %>%
  mutate(incept = make_date(vintage, incept_month, incept_day)) %>%
  select(-incept_month, -incept_day)

# Join with full industry returns data
fund_data <- merge(fund_data, industry_ret_full, by = "industry", all.x = TRUE)

# Filter dates based on fund lifecycle
fund_data <- fund_data %>%
  filter(year(date) >= vintage & year(date) <= vintage + 13) %>%
  filter(date >= incept)

# --- Business Day Filtering using mkt_lvl.rds ---
# Load mkt_lvl.rds to get the definitive list of business/trading dates
# This file should be in the workdir, typically created by BenchmarkPrep.R
mkt_lvl_for_filtering <- readRDS(file.path(workdir, "mkt_lvl.rds"))

# Keep only the date column from mkt_lvl_for_filtering and ensure dates are unique
# This acts as our "bdays" calendar from Stata, derived from CRSP trading days.
valid_trading_dates <- mkt_lvl_for_filtering %>%
  select(date) %>%
  mutate(date = as.Date(date)) %>% # Ensure date is in Date format for joining
  distinct()

# Filter fund_data to keep only dates that are present in valid_trading_dates
# This is equivalent to Stata's `gen bday =bofd("bdays",date)` followed by `drop if bday==.`
fund_data <- fund_data %>%
  inner_join(valid_trading_dates, by = "date")

# Filter out funds with no data after business day filters
fund_data <- fund_data %>%
  group_by(fundid) %>%
  filter(n() > 0) %>%
  ungroup()

if (nrow(fund_data) == 0) {
  stop("No fund data remaining after business day filtering using mkt_lvl.rds.")
}

# Temporary qofd for filtering (Stata: gen qofd=qofd(date); su qofd; drop if qofd==r(max))
# This qofd is different from the one potentially dropped earlier in Stata if it were generated
fund_data <- fund_data %>%
  mutate(qofd_temp = as.yearqtr(date))

max_qofd_temp <- max(fund_data$qofd_temp, na.rm = TRUE)
fund_data <- fund_data %>%
  filter(qofd_temp != max_qofd_temp | is.na(qofd_temp)) %>% # Keep NAs if any
  select(-qofd_temp)


# Sort and fix incept date to be the first actual data date for the fund
fund_data <- fund_data %>%
  arrange(fundid, date) %>%
  group_by(fundid) %>%
  mutate(incept = first(date)) %>%
  ungroup()

# --- Cash and Value flow process by fund ---
fidVlty <- 0.1 # idiosyncratic returns' to industry vol-ty per year

fund_data <- fund_data %>%
  # bet2i is drawn per observation, as per Stata's gen bet2i=rnormal(1.1,.1)
  mutate(fidlogret = rnorm(n()) * fidVlty / sqrt(252) - 0.5 * (fidVlty^2) / 252,
         bet2i = rnorm(n(), mean = 1.1, sd = 0.1),
         ftype = paste0("N(1.1,", fidVlty^2, ") beta to ind"))

fund_data <- fund_data %>%
  arrange(fundid, date) %>%
  group_by(fundid) %>%
  mutate(
    age = as.numeric(date - first(date)) / 365.25,
    # fidCumLogRet uses the observation-specific bet2i
    fidCumLogRet = (cumsum(fidlogret) - first(fidlogret)) +
      bet2i * (log(ibmk_lvl) - log(first(ibmk_lvl))) +
      0.5 * bet2i * (bet2i - 1) * (ibmk_rsq - first(ibmk_rsq)),
    R0t = exp(fidCumLogRet)
  ) %>%
  ungroup()

if (dummyFundRet == "Yes") {
  fund_data <- fund_data %>%
    arrange(fundid, date) %>%
    group_by(fundid) %>%
    mutate(R0t = ibmk_lvl / first(ibmk_lvl)) %>%
    ungroup()
}

# Smoothed returns (barR0t)
smooth_return_fn <- function(x_r0t, alpha = 0.05) {
  y_barr0t <- numeric(length(x_r0t))
  if (length(x_r0t) > 0) {
    y_barr0t[1] <- x_r0t[1]
    if (length(x_r0t) > 1) {
      for (i in 2:length(x_r0t)) {
        y_barr0t[i] <- alpha * x_r0t[i] + (1 - alpha) * y_barr0t[i-1]
      }
    }
  }
  return(y_barr0t)
}

fund_data <- fund_data %>%
  arrange(fundid, date) %>%
  group_by(fundid) %>%
  mutate(barR0t = smooth_return_fn(R0t, alpha = (1 - 0.95))) %>%
  ungroup()

fund_data <- fund_data %>%
  group_by(fundid) %>%
  mutate(trueAlf = mean(fidlogret, na.rm = TRUE)) %>%
  ungroup()

# --- Interim calls's frequency ---
fund_data$C1s <- NA_integer_
idx_c1s_age <- fund_data$age <= 6 & !is.na(fund_data$age)
fund_data$C1s[idx_c1s_age] <- rbinom(sum(idx_c1s_age), 1, 0.0075)

fund_data <- fund_data %>%
  group_by(fundid) %>%
  mutate(C1s = ifelse(is.na(C1s), 0, C1s)) %>% # Fill NAs (e.g. age > 6) with 0
  arrange(date) %>% # arrange by date within fund for row_number()
  mutate(C1s = ifelse(row_number() == 1, 1, C1s)) %>% # First obs is a call
  ungroup()

# --- Interim distrib. freq. ---
fund_data$D1s <- NA_integer_
idx_d1s_age <- fund_data$age >= 3 & fund_data$age <= 12 & !is.na(fund_data$age)
fund_data$D1s[idx_d1s_age] <- rbinom(sum(idx_d1s_age), 1, 0.0075)

fund_data <- fund_data %>%
  group_by(fundid) %>%
  mutate(D1s = ifelse(is.na(D1s), 0, D1s)) %>% # Fill NAs with 0
  ungroup()

# --- Value-to-cumR mapping ---
# gen C2V=min(.99,rexponential(0.25)/age)*C1s
# Stata: rexponential(beta) -> R: rexp(n, rate = 1/beta) -> rate = 1/0.25 = 4
# Stata: x/0 is missing. min(k, missing) is k.
fund_data <- fund_data %>%
  mutate(
    rexp_val = rexp(n(), rate = 4),
    C2V_term = ifelse(age == 0, Inf, rexp_val / age), 
    C2V = pmin(0.99, C2V_term) * C1s
  ) %>%
  select(-rexp_val, -C2V_term)

# gen df =min(.99,normal(-2.5+.25*age+rnormal()))*D1s
# Stata: normal(x) is N(0,1) CDF -> R: pnorm(x)
fund_data <- fund_data %>%
  mutate(
    df_arg = -2.5 + 0.25 * age + rnorm(n()),
    df = pmin(0.99, pnorm(df_arg)) * D1s
  ) %>%
  select(-df_arg)

# Multiplier M calculation
fund_data <- fund_data %>%
  arrange(fundid, date) %>%
  group_by(fundid) %>%
  mutate(M2lagM = ifelse(row_number() == 1, NA_real_, 1 / (1 - df) - C2V)) %>%
  ungroup()

calculate_M_fn <- function(m2lagm_vector_for_fund) {
  m_vals <- numeric(length(m2lagm_vector_for_fund))
  if (length(m_vals) > 0) {
    m_vals[1] <- 1.0 # M is 1 for the first observation
    if (length(m_vals) > 1) {
      for (i in 2:length(m_vals)) {
        if (!is.na(m2lagm_vector_for_fund[i])) {
          m_vals[i] <- m_vals[i-1] * m2lagm_vector_for_fund[i]
        } else {
          # If M2lagM is NA (e.g. due to NA in df or C2V), M becomes NA from this point onwards
          m_vals[i:length(m_vals)] <- NA_real_ 
          break
        }
      }
    }
  }
  return(m_vals)
}

fund_data <- fund_data %>%
  arrange(fundid, date) %>%
  group_by(fundid) %>%
  mutate(M = calculate_M_fn(M2lagM)) %>%
  ungroup()

fund_data <- fund_data %>%
  mutate(V = R0t / M)

fund_data <- fund_data %>%
  arrange(fundid, date) %>%
  group_by(fundid) %>%
  mutate(C = ifelse(row_number() == 1, 1, C2V * V),
         fundSize = sum(C, na.rm = TRUE)) %>%
  ungroup()

# --- Terminal D and Value adjustments ---
# Stata: gen D=cond(df==0,0,df/(1-df)*V)
# R equivalent: D should be 0 if df is 0. If df is NA, D should be NA. Otherwise, calculate.
fund_data <- fund_data %>%
  mutate(
    D = case_when(
      is.na(df) ~ NA_real_,
      df == 0 ~ 0,
      TRUE ~ (df / (1 - df)) * V
    )
  )

# Early termination logic (temp variables)
fund_data <- fund_data %>%
  mutate(temp_cond = (V < 0.1 & age > 5 & !is.na(V) & !is.na(age)),
         temp = ifelse(is.na(temp_cond), 0, ifelse(temp_cond, 1, 0)))

fund_data <- fund_data %>%
  arrange(fundid, date) %>%
  group_by(fundid) %>%
  mutate(
    # temp1 = cumsum(temp), # Not directly used for filtering in R version, but kept for Stata logic reference
    temp2_lead = lead(temp, default = 0), 
    temp2 = ifelse(temp2_lead == 1, 1, 0),
    temp3 = cumsum(temp2)
  ) %>%
  ungroup()

# Drop if temp3 > 0 (Stata: drop if temp3>0)
# This means keep if temp3 <= 0 OR temp3 is NA
fund_data <- fund_data %>% filter(!(temp3 > 0 & !is.na(temp3)))

fund_data <- fund_data %>% select(-starts_with("temp"))


# Update D and V at terminal date for certain vintages
# Need to handle cases where fund_data might be empty after previous filters
if (nrow(fund_data) > 0) {
  max_vintage_val <- max(fund_data$vintage, na.rm = TRUE)
  
  fund_data <- fund_data %>%
    arrange(fundid, date) %>%
    group_by(fundid) %>%
    mutate(
      is_last_obs = row_number() == n(),
      cond_update_terminal = (first(vintage) < (max_vintage_val - 3) & is_last_obs)
    ) %>%
    mutate(
      D = ifelse(cond_update_terminal, D + V, D),
      V = ifelse(cond_update_terminal, 0, V)
    ) %>%
    select(-is_last_obs, -cond_update_terminal) %>%
    ungroup()
}


# --- NAV Calculation ---
if (nrow(fund_data) > 0) {
  fund_data <- fund_data %>%
    mutate(qofd = as.yearqtr(date))
  
  fund_data <- fund_data %>%
    arrange(fundid, qofd, date) %>%
    group_by(fundid, qofd) %>%
    mutate(nav_qtr_end = ifelse(row_number() == n(), barR0t / M, NA_real_)) %>%
    ungroup()
  
  # Consolidate nav: take the quarterly end NAVs
  fund_data$nav <- fund_data$nav_qtr_end
  
  # For the very last observation of a fund, adjust NAV based on V
  # Stata: bys fundid: replace nav=cond(V==0,0,barR0t/M) if _n==_N
  fund_data <- fund_data %>%
    arrange(fundid, date) %>%
    group_by(fundid) %>%
    mutate(
      nav = ifelse(row_number() == n(),
                   ifelse(!is.na(V) & V == 0, 0, barR0t / M), # if V is 0 (and not NA), nav is 0
                   nav) # Keep existing nav for non-last obs
    ) %>%
    ungroup() %>%
    select(-nav_qtr_end)
} else {
  # If fund_data is empty, create an empty nav column to prevent errors later
  fund_data$nav <- numeric(0)
  fund_data$qofd <- as.yearqtr(Sys.Date())[-1] # empty yearqtr
}


# --- NPV identity check (optional, for validation) ---
if (nrow(fund_data) > 0) {
  bmk_var_name <- "ibmk" # Benchmark variable prefix
  
  fund_data <- fund_data %>%
    arrange(fundid, date) %>%
    group_by(fundid) %>%
    mutate(
      !!paste0(bmk_var_name, "_lvl0") := first(!!sym(paste0(bmk_var_name, "_lvl"))),
      !!paste0(bmk_var_name, "_lvlT") := last(!!sym(paste0(bmk_var_name, "_lvl")))
    ) %>%
    ungroup()
  
  fund_data <- fund_data %>%
    mutate(
      rc_0t = log(!!sym(paste0(bmk_var_name, "_lvl")) / !!sym(paste0(bmk_var_name, "_lvl0"))),
      `__dfac` = exp(log(!!sym(paste0(bmk_var_name, "_lvlT")) / !!sym(paste0(bmk_var_name, "_lvl0"))) - rc_0t)
    )
  
  fund_data <- fund_data %>%
    arrange(fundid, date) %>%
    group_by(fundid) %>%
    mutate(
      C_fvT = C * `__dfac`,
      D_fvT = D * `__dfac`,
      cumC_fv = cumsum(ifelse(is.na(C), 0, C) * `__dfac`) / `__dfac`, # Handle potential NAs in C for cumsum
      cumD_fv = cumsum(ifelse(is.na(D), 0, D) * `__dfac`) / `__dfac`, # Handle potential NAs in D for cumsum
      `_PME` = (cumD_fv + ifelse(is.na(V), 0, V)) / cumC_fv, # Handle potential NA in V
      `_PME2` = (cumD_fv + ifelse(is.na(nav), 0, nav)) / cumC_fv # Handle potential NA in nav
    ) %>%
    ungroup()
  
  fund_data <- fund_data %>%
    arrange(fundid, date) %>%
    group_by(fundid) %>%
    mutate(RepErrE = ifelse(row_number() == n() & !is.na(V) & V != 0,
                            log(`_PME2` / `_PME`),
                            NA_real_)) %>%
    ungroup()
  
  fund_data$RepErr <- NA_real_
  idx_reperr_calc <- is.na(fund_data$RepErrE) & 
    !is.na(fund_data$`_PME2`) & !is.na(fund_data$`_PME`) & 
    fund_data$`_PME` != 0 & is.finite(fund_data$`_PME`) &
    fund_data$`_PME2` > 0 & fund_data$`_PME` > 0 # Avoid log of non-positive
  
  fund_data$RepErr[idx_reperr_calc] <- log(fund_data$`_PME2`[idx_reperr_calc] / fund_data$`_PME`[idx_reperr_calc])
}


# --- Final Filtering and Variable Selection ---
# keep if nav!=. | C>0 | D>0
if (nrow(fund_data) > 0) {
  fund_data <- fund_data %>%
    filter((!is.na(nav)) | (!is.na(C) & C > 0) | (!is.na(D) & D > 0))
}

# Recalculate qofd if it was dropped or for consistency, then calculate time
if (nrow(fund_data) > 0) {
  fund_data <- fund_data %>%
    mutate(qofd = as.yearqtr(date)) %>% 
    arrange(fundid, qofd, date) %>% 
    group_by(fundid) %>%
    mutate(time = round(4 * (as.numeric(qofd) - as.numeric(first(qofd))))) %>%
    ungroup()
} else {
  # Ensure 'time' column exists if fund_data is empty after filtering
  fund_data$time <- numeric(0)
  if (!"qofd" %in% names(fund_data)) fund_data$qofd <- as.yearqtr(Sys.Date())[-1]
}


# Rename bet2i to bet_est
if ("bet2i" %in% names(fund_data)) {
  fund_data <- fund_data %>% rename(bet_est = bet2i)
} else if (nrow(fund_data) > 0) { # if fund_data not empty but bet2i somehow missing
  fund_data$bet_est <- NA_real_
} else { # if fund_data is empty
  fund_data$bet_est <- numeric(0)
}


# Select and order final variables
final_vars <- c("fundid", "ftype", "industry", "vintage", "incept", "time", "date", "C", "D", "nav", "bet_est")

# Ensure all final_vars columns exist, adding them as NA if they don't (for empty df or missing cols)
for (col_name in final_vars) {
  if (!col_name %in% names(fund_data)) {
    # Determine type for empty column (this is a simplification)
    if (col_name %in% c("fundid", "vintage", "time")) {
      fund_data[[col_name]] <- integer(nrow(fund_data))
    } else if (col_name %in% c("C", "D", "nav", "bet_est")) {
      fund_data[[col_name]] <- numeric(nrow(fund_data))
    } else if (col_name %in% c("incept", "date")) {
      fund_data[[col_name]] <- as.Date(NA)
    } else { # ftype, industry
      fund_data[[col_name]] <- character(nrow(fund_data))
    }
  }
}

fund_data <- fund_data[, final_vars, drop = FALSE]

# Order columns as specified (already done by selecting in order)
# fund_data <- fund_data %>% select(all_of(final_vars)) # Redundant if selected as above


# --- Save Output ---
# CFdset and data (path) are from MAIN4site_DirectAlpha.R
output_filename <- file.path(data, paste0(CFdset, ".rds"))
saveRDS(fund_data, output_filename)

cat("FundDataSim.R: Simulated fund data saved to", output_filename, "\n")
if (nrow(fund_data) == 0) {
  cat("Warning: The final simulated fund dataset is empty.\n")
}

# Clean up (optional, as R manages memory)
# rm(industry_ret_full, fund_data, mkt_lvl_for_filtering, valid_trading_dates)
# gc()