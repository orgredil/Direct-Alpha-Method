################################################################################
# This code needs to be run by MAIN4site_DirectAlpha.R
# Copyright O.Gredil

# Load necessary libraries
library(data.table)
library(dplyr)
library(lubridate)
library(zoo)
library(bizdays)

# Load CRSP data
crsp_plus <- readRDS(file.path(data, "crsp_plus.rds"))

# Sort by date
crsp_plus <- crsp_plus[order(crsp_plus$date),]

# Generate market level and return squared variables
crsp_plus$mkt_lvl <- exp(cumsum(log(1 + crsp_plus$crsp)))
crsp_plus$mkt_rsq_2 <- crsp_plus$crsp^2
crsp_plus$qtr <- as.yearqtr(crsp_plus$date)
crsp_plus$mkt_rsq <- cumsum(crsp_plus$mkt_rsq_2)

# Rename crsp to mkt if needed
if("crsp" %in% names(crsp_plus)) {
  crsp_plus$mkt <- crsp_plus$crsp
}


# Keep only necessary variables
mkt_lvl <- crsp_plus[, c("mkt_lvl", "mkt_rsq", "date", "mkt")] # mkt_lvl$date is sorted

# Generate bday as a 0-indexed row number, assuming mkt_lvl is sorted by date.
# This replaces the previous bizdays-based calculation for mkt_lvl$bday.
mkt_lvl$bday <- 0:(nrow(mkt_lvl)-1)

# Save market level data
saveRDS(mkt_lvl, "mkt_lvl.rds")


# Load industry map
industry_map <- read.csv(file.path(data, "industry_map.csv"))
# Remove any existing internalshortcode columns
industry_map <- industry_map[, !grepl("internalshortcode\\d*$", names(industry_map))]

# Rename Industry to industry (lowercase)
if("Industry" %in% names(industry_map)) {
  names(industry_map)[names(industry_map) == "Industry"] <- "industry"
}

# Rename columns to match Stata code
if("FFIcode" %in% names(industry_map)) {
  names(industry_map)[names(industry_map) == "FFIcode"] <- "internalshortcode"
  if("FFIcode2" %in% names(industry_map)) {
    names(industry_map)[names(industry_map) == "FFIcode2"] <- "internalshortcode2"
  }
} else if("InternalShortCode" %in% names(industry_map)) {
  names(industry_map)[names(industry_map) == "InternalShortCode"] <- "internalshortcode"
  if("InternalShortCode2" %in% names(industry_map)) {
    names(industry_map)[names(industry_map) == "InternalShortCode2"] <- "internalshortcode2"
  }
}
saveRDS(industry_map, "industry_map.rds")

# Process industry returns
mkt_lvl <- readRDS("mkt_lvl.rds") # Reload to ensure consistency if any changes were made
fdate <- min(mkt_lvl$date)

# Load industry index data
ffi12_vwew_idx <- readRDS(file.path(data, "ffi12_vwew_idx.rds"))
ffi12_vwew_idx <- ffi12_vwew_idx[order(ffi12_vwew_idx$internalshortcode, ffi12_vwew_idx$date),]
ffi12_vwew_idx$internalshortcode2 <- ffi12_vwew_idx$internalshortcode

# Calculate log returns based on iScheme
if (iScheme == "EWFF") {
  ffi12_vwew_idx <- ffi12_vwew_idx %>%
    group_by(internalshortcode) %>%
    mutate(logtrr = log(IDXew) - log(lag(IDXew))) %>%
    ungroup()
} else {
  ffi12_vwew_idx <- ffi12_vwew_idx %>%
    group_by(internalshortcode) %>%
    mutate(logtrr = log(IDXvw) - log(lag(IDXvw))) %>%
    ungroup()
}

# Drop unnecessary columns and filter dates
ffi12_vwew_idx <- ffi12_vwew_idx[, !names(ffi12_vwew_idx) %in% c("IDXew", "IDXvw")]
ffi12_vwew_idx <- ffi12_vwew_idx[ffi12_vwew_idx$date > (fdate + 2),]

# Join with industry map - using joinby equivalent in R
industry_ret0 <- merge(ffi12_vwew_idx, industry_map, by = "internalshortcode", all.x = TRUE)
# Check for duplicates as in Stata
industry_ret0 <- industry_ret0[order(industry_ret0$industry, industry_ret0$date),]
saveRDS(industry_ret0, "industry_ret0.rds")

################################################################################

################################################################################
# Process industry characteristics
industry_ret0 <- readRDS("industry_ret0.rds")
industry_ret0 <- industry_ret0[order(industry_ret0$industry, industry_ret0$date),]

# Keep last observation for each industry
industry_char <- industry_ret0 %>%
  group_by(industry) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(-date, -logtrr)

# Create industry number
industry_char$indnum <- as.numeric(factor(industry_char$industry))
saveRDS(industry_char, "industry_char.rds")

# Use market returns for industry returns before their series begin
mkt_lvl <- readRDS("mkt_lvl.rds") # Reload
fdate <- min(mkt_lvl$date)

# Create stub data for early dates - replicating Stata's approach
industry_ret0 <- readRDS("industry_ret0.rds")
industry_ret0$indnum <- as.numeric(factor(industry_ret0$industry))

# Keep unique industry-date combinations
industry_stub <- industry_ret0 %>%
  select(indnum, date) %>%
  distinct() %>%
  group_by(indnum) %>%
  slice_head(n = 1) %>%
  mutate(date = date - 1) %>%
  ungroup()

# Filter out NA indnum from industry_stub to mimic Stata's xtset behavior before tsfill in stub creation
industry_stub <- industry_stub[!is.na(industry_stub$indnum), ]

# Expand and fill with earliest date - equivalent to Stata's expand 2
industry_stub <- rbind(
  industry_stub,
  industry_stub %>% mutate(date = fdate)
)

# Remove NA dates if any exist
if(sum(is.na(industry_stub$date)) > 0) {
  industry_stub <- industry_stub[!is.na(industry_stub$date), ]
}

# Ensure dates are properly formatted
industry_stub$date <- as.Date(industry_stub$date)

# Create complete time series - equivalent to Stata's tsfill
industry_stub <- industry_stub %>%
  arrange(indnum, date) %>%
  distinct(indnum, date)

# Complete the panel
min_date_stub <- min(industry_stub$date, na.rm = TRUE)
max_date_stub <- max(industry_stub$date, na.rm = TRUE)
all_dates_stub <- seq(from = min_date_stub, to = max_date_stub, by = "day")
all_indnums_stub <- unique(industry_stub$indnum) # Will not contain NA due to filter above

industry_stub_complete <- expand.grid(indnum = all_indnums_stub, date = all_dates_stub) %>%
  as.data.frame() %>%
  arrange(indnum, date)

# Join with industry characteristics - equivalent to Stata's joinby
industry_stub_complete <- merge(industry_stub_complete, industry_char, by = "indnum", all.x = TRUE)

# Merge with market returns - equivalent to Stata's merge m:1
industry_stub_complete <- merge(
  industry_stub_complete,
  mkt_lvl[, c("date", "mkt")],
  by = "date",
  all.x = TRUE
)

# Calculate log returns
industry_stub_complete$logtrr <- log(1 + industry_stub_complete$mkt)
industry_stub_complete <- industry_stub_complete[, !names(industry_stub_complete) %in% c("mkt")]
saveRDS(industry_stub_complete, "industry_retStub.rds")

# Combine industry returns with stub data
industry_ret0 <- readRDS("industry_ret0.rds")
industry_ret0 <- industry_ret0[order(industry_ret0$industry, industry_ret0$date),]

industry_stub_complete <- readRDS("industry_retStub.rds")
industry_combined <- rbind(
  industry_ret0,
  industry_stub_complete[, intersect(names(industry_stub_complete), names(industry_ret0))]
)

# Create industry number and calculate return metrics
industry_combined$indnum <- as.numeric(factor(industry_combined$industry))
industry_combined <- industry_combined %>%
  select(indnum, date, logtrr) %>%
  mutate(ret_rsq_2 = logtrr^2)

# Filter out rows where indnum or date is NA to mimic Stata's xtset behavior before the main tsfill
industry_combined <- industry_combined[!is.na(industry_combined$indnum) & !is.na(industry_combined$date), ]

# Ensure dates are properly formatted
industry_combined$date <- as.Date(industry_combined$date)

# Complete the panel again
min_date_combined <- min(industry_combined$date, na.rm = TRUE)
max_date_combined <- max(industry_combined$date, na.rm = TRUE)
all_dates_combined <- seq(from = min_date_combined, to = max_date_combined, by = "day")
all_indnums_combined <- unique(industry_combined$indnum) # Will not contain NA due to filter above

industry_complete <- expand.grid(indnum = all_indnums_combined, date = all_dates_combined) %>%
  as.data.frame() %>%
  arrange(indnum, date)

# Join with existing data
industry_complete <- merge(industry_complete, industry_combined, by = c("indnum", "date"), all.x = TRUE)
industry_complete <- merge(industry_complete, industry_char, by = "indnum", all.x = TRUE)

# Calculate industry benchmark metrics
industry_complete <- industry_complete %>%
  arrange(industry, date) %>%
  mutate(qtr = as.yearqtr(date)) %>%
  group_by(industry) %>%
  mutate(
    ibmk_rsq = cumsum(ifelse(is.na(ret_rsq_2), 0, ret_rsq_2)),
    ibmk_lvl = exp(cumsum(ifelse(is.na(logtrr), 0, logtrr)))
  ) %>%
  ungroup() %>%
  select(industry, date, ibmk_lvl, ibmk_rsq) %>%
  arrange(industry, date)

# Save final industry return data
saveRDS(industry_complete, "industry_ret.rds")

# Clean up temporary files
if (file.exists("industry_ret0.rds")) file.remove("industry_ret0.rds")
if (file.exists("industry_retStub.rds")) file.remove("industry_retStub.rds")
if (file.exists("industry_char.rds")) file.remove("industry_char.rds")