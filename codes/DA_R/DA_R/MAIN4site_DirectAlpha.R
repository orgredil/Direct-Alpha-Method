# R version 4.3.3 (2024-02-29 ucrt) -- "Angel Food Cake"
# Platform: x86_64-w64-mingw32/x64 (64-bit)


# Load necessary libraries
library(data.table)
library(lubridate)
library(dplyr)
library(zoo)
library(here)

# Set working directory and global variables
root <- here() 
output <- file.path(root, "output", "")
codes <- file.path(root, "subcodes", "")
data <- file.path(root, "data", "")
workdir <- file.path(root, "scratch", "")
setwd(workdir)

# Set global parameters
bmk <- "crsp"
iScheme <- "FF"
BetaAdj <- "Yes"
DAplus <- "Yes"
fundSim <- "Yes"

dummyFundRet <- "Yes" # if fund retuns equal industry returns 
dummyFundRet <- "No" # if fund returns feature idiosync compe and levered ind.ret


fVintage <- "1983"
lVintage <- "2014"

# Dataset name if not using simulated data
CFdset <- "yourFundDataName"

# Start logging
sink(file = file.path(output, "DAprepNest.log"), append = FALSE)
cat("Log started at", format(Sys.time()), "\n")

# Set options
options(width = 255)
# memory.limit(size = 3000) 

# Benchmarks' prep
if (iScheme == "FF" & bmk == "crsp") {
  cat("pre-supplied Fama-French industry and crsp returns are used\n")
  source(file.path(codes, "BenchmarkPrep.R"))
  
  # Copy files
  file.copy("industry_ret.rds", "industry_ret_FF.rds", overwrite = TRUE)
  file.copy("mkt_lvl.rds", "mkt_lvl_crsp.rds", overwrite = TRUE)
} else {
  cat("Need to provide industry_ret.rds and mkt_lvl.rds from elsewhere\n")
}

if (fundSim == "Yes") {
  FundsNumber <- "250"
  # The code below simulates cash flows for about FundsNumber of funds with varying risk across different industries
  # The true per period alpha of each fund is zero (but the sampling realization is not!)
  cat("simulated fund data are used\n")
  CFdset <- "fundSim"
  source(file.path(codes, "FundDataSim.R"))
} else {
  cat("Need to provide fund cash flows (and at least terminal NAV data)..\n")
  cat("...from elsewhere with variables as in data/fundSim.rds\n")
  cat("...and dates shifted to adjacent dates w/ the mkt return data (if necessary)\n")
}


# --- Uncomment codes below to use original fundSim.dta data as input for later estimation ----------------
#library(haven)
## 1. Build the path to the Stata file *inside* your data/ directory
#fundSim_dta <- file.path(data, "fundSim.dta")   
## 2. Read it
#CFdset2 <- read_dta(fundSim_dta)
## 3. Build the output file-name in the same folder
#out_path <- file.path(data, "fundSim.rds")      
## 4. Save as .rds (xz compression keeps size small; default gzip also fine)
#saveRDS(CFdset2, file = out_path)



# Process for different benchmarks
bmks <- c("mkt", "ibmk")

# Loop through benchmarks as in the Stata code
for (current_bmk in bmks) {
  # Set parameters for the sourced scripts to use
  bmk_param <- current_bmk
  BetaAdj_param <- BetaAdj
  
  cat("BEGIN preps for benchmark:", current_bmk, "\n")
  source(file.path(codes, "DAdataPrep.R"))
  cat("END preps for benchmark:", current_bmk, "\n")
  
  cat("BEGIN IRR computations for benchmark:", current_bmk, "\n")
  source(file.path(codes, "DAestimation.R"))
  cat("END IRR computations for benchmark:", current_bmk, "\n")
  
  # Fix: Use current_bmk for loading results
  fund_EOLest <- readRDS(file.path(output, paste0("fund_EOLest_", current_bmk, ".rds")))
  
  if (DAplus == "Yes") {
    var4stat <- c("DA_raw", "DA_badj", "anKSpme", "alf_LN", "alf_CD", "alf_ST")
  } else {
    var4stat <- c("DA_raw", "DA_badj")
  }
  
  # Fix: Use current_bmk instead of bmk for printing results
  cat("Results for benchmark:", current_bmk, "\n")
  
  # Summary statistics (equivalent to tabstat)
  summary_stats <- function(df, vars) {
    result <- data.frame()
    for (var in vars) {
      if (var %in% names(df)) {
        x <- df[[var]]
        stats <- c(
          N = sum(!is.na(x)),
          mean = mean(x, na.rm = TRUE),
          sd = sd(x, na.rm = TRUE),
          p1 = quantile(x, 0.01, na.rm = TRUE),
          p10 = quantile(x, 0.10, na.rm = TRUE),
          p25 = quantile(x, 0.25, na.rm = TRUE),
          p50 = quantile(x, 0.50, na.rm = TRUE),
          p75 = quantile(x, 0.75, na.rm = TRUE),
          p90 = quantile(x, 0.90, na.rm = TRUE),
          p99 = quantile(x, 0.99, na.rm = TRUE)
        )
        result <- rbind(result, stats)
      }
    }
    rownames(result) <- vars
    return(result)
  }
  
  # Fix: Add column labels to match Stata's tabstat output
  stats_result <- summary_stats(fund_EOLest, var4stat)
  cat("tabstat for benchmark:", current_bmk, "\n")
  print(stats_result)
  
  # Fix: Add header to match Stata's pwcorr output
  cat("\npwcorr for benchmark:", current_bmk, "\n")
  print(cor(fund_EOLest[, var4stat], use = "pairwise.complete.obs"))
}

# Close log
sink()