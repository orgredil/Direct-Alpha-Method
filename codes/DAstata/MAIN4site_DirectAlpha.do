cap cd $root
global root "`c(pwd)'"
global output "${root}\output\\"
global codes "${root}\subcodes\\"
global data "${root}\data\\"
global workdir "${root}\scratch\\" 
cd $workdir
set scheme s1mono



global bmk "crsp"
global iScheme "FF" 
global BetaAdj "Yes"
global DAplus "Yes"
global fundSim "Yes"


global fVintage "1983"
global lVintage "2014"


*** the name of your stata dataset name if not the simulated data are used...  
global CFdset "yourFundDataName"  
**... must have the same structure and variables as codes/fundSim.dta produced by 
**... nust be put in the 'code'-folder


set graphics off
cap log close

log using "${output}DAprepNest.log",replace
clear
set linesize 255
set more off
set min_memory 3G
set matsize 1000
cap set niceness 2


** Benchmarks' prep
* if other than crsp/FF are usedmake sure that the file structure and variables ...  
* ... are the same as in code/industry_ret.dta and code/mkt_lvl.dta 
* ... that are prodoced with crsp&FF options 
* ... and contain the cumulative returns and return-squareds
if "${iScheme}"=="FF" & "${bmk}"=="crsp" {
disp "pre-supplied Fama-French industry and crsp returns are used"
do "${codes}BenchmarkPrep.do"
cap copy "industry_ret.dta" "industry_ret_FF.dta"
// cap rm "industry_ret.dta" 
cap copy "mkt_lvl.dta" "mkt_lvl_crsp.dta"
// cap rm "mkt_lvl.dta" 
}
else {
disp "Need to provide industry_ret.dta and mkt_lvl.dta from elsewhere"
}
if "${fundSim}"=="Yes" {
global FundsNumber "250"
// the code below simulates cash flows for about FundsNumber of funds with varying risk across differnt industries
// the true per period alpha of each fund is zero (but the sampling realization is not!)
disp "simulated fund data are used"
global CFdset "fundSim"
do "${codes}FundDataSim.do"
}
else {
disp "Need to provide fund cash flows (and at least terminal NAV data).. " 
disp "...from elsewere with variables as in data/fundSim.dta"
disp "...and dates shifted to adjacent dates w/ the mkt return data (if necessary)"
}
/////////////////////////////////////////////////////////////////////////////////

global bmks "ibmk"
global bmks "mkt ibmk"
foreach bmk in $bmks {
global bmk "`bmk'"
disp "BEGIN preps for benchmark: `bmk'"
do "${codes}DAdataPrep"
disp "END preps for benchmark: `bmk'"
disp "BEGIN IRR computations for benchmark: `bmk'"
do "${codes}DAestimation"
disp "END IRR computations for benchmark: `bmk'"

use "${output}fund_EOLest_`bmk'", clear
if "${DAplus}"=="Yes" {
global var4stat "DA_raw DA_badj anKSpme alf_LN alf_CD alf_ST"
}
else  {
global var4stat "DA_raw DA_badj"
}
disp "Results for benchmark: `bmk'"
tabstat $var4stat , st(N mean sd p1 p10 p25 p50 p75 p90 p99) 
pwcorr $var4stat  
}


cap log close


