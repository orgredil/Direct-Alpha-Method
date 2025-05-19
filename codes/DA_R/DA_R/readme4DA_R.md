# Direct Alpha (DA) Performance Measurement in R

## Overview

This codebase implements the Direct Alpha (DA) methodology for measuring private equity fund performance. Direct Alpha is a performance metric that adjusts for market risk and allows for comparisons across funds with different risk exposures.

The code calculates several variants of performance measures:
- Direct Alpha raw (DA_raw)
- Direct Alpha with beta adjustment (DA_badj)
- Annualized KS-PME
- Various alpha measures (LN, CD, ST)

## File Structure

### Main Files
- `MAIN4site_DirectAlpha.R` - The main script that orchestrates the entire process
- `DA_R.Rproj` - R Project file for RStudio

### Subcode Files
- `BenchmarkPrep.R` - Prepares benchmark data (CRSP and Fama-French industry returns)
- `FundDataSim.R` - Simulates fund data or processes real fund data
- `DAdataPrep.R` - Prepares the data for DA estimation
- `DAestimation.R` - Performs the IRR calculations and Direct Alpha estimates
- `irr_mata.R` - IRR calculation function (ported from Stata's mata)

### Output Files
- `fund_EOLest_mkt.rds` - Results using market benchmark
- `fund_EOLest_ibmk.rds` - Results using industry benchmark
- `DAprepNest.log` - Log file with execution details and summary statistics

## Workflow

1. **Setup**: The main script sets working directories and parameters
2. **Benchmark Preparation**: Loads or generates market (CRSP) and industry (Fama-French) returns
3. **Fund Data**: Either loads real fund data or simulates fund data
4. **Analysis Loop**: For each benchmark type (market and industry):
   - Prepares data with `DAdataPrep.R`
   - Calculates IRRs with `DAestimation.R`
   - Generates summary statistics
5. **Output**: Saves results in RDS files and creates summary tables

## Key Parameters

- `bmk`: Benchmark type ("crsp" or custom)
- `iScheme`: Industry scheme ("FF" for Fama-French)
- `BetaAdj`: Whether to adjust for beta ("Yes" or "No")
- `DAplus`: Whether to calculate additional measures ("Yes" or "No")
- `fundSim`: Whether to use simulated data ("Yes" or "No")
- `dummyFundRet`: Controls fund return simulation method
- `fVintage` and `lVintage`: Vintage year range

## Running the Code

1. Open the project in RStudio by clicking on the `DA_R.Rproj` file
2. Open `MAIN4site_DirectAlpha.R`
3. Set working directories if needed
4. Run the script

## Required R Packages

- data.table
- lubridate
- dplyr
- zoo
- bizdays
- here
- haven (for Stata files, if used)

## Methodology

The Direct Alpha approach measures risk-adjusted performance by:
1. Converting fund cash flows to present values using benchmark returns
2. Calculating IRR on these market-adjusted cash flows (this is the "Direct Alpha")
3. Applying additional adjustments for beta when specified

## Results Interpretation

The output statistics include:
- `DA_raw`: Raw Direct Alpha measure
- `DA_badj`: Beta-adjusted Direct Alpha
- `anKSpme`: Annualized Kaplan-Schoar PME
- `alf_LN`: Log-normal alpha
- `alf_CD`: Continuous-discrete alpha
- `alf_ST`: Stochastic alpha

Positive values indicate outperformance relative to the benchmark.

## Sample Output

From the log file:
```
tabstat for benchmark: mkt 
        X253 X0.0216247748391101 X0.0748026165104867 X.0.131669042659029 X.0.0726003566924686 X.0.0269283117084319 X0.0142183345266889 X0.0736108133885152 X0.119970827094018 X0.211054819157028
DA_raw   253         0.021624775          0.07480262          -0.1316690          -0.07260036          -0.02692831        0.0142183345          0.07361081         0.11997083          0.2110548
```

The output shows funds in the sample have a mean DA_raw of 2.16%, indicating positive risk-adjusted performance relative to the market benchmark.

## References

This implementation is based on academic research on Direct Alpha methodology for private equity performance measurement.

## Credit

Copyright O. Gredil (referenced in the code files)
