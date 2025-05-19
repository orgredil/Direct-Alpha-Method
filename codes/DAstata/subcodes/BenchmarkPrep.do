////////////////////////////////////////////////////////////////////////////////
// this code need to be run by MAIN4site_DirectAlpha.do
/*Copyright O.Gredil*/
set more off
	use "${data}\crsp_plus", clear
gsort date
gen mkt_lvl=exp(sum(log(1+crsp)))
gen _mkt_rsq=crsp^2
gen qtr=qofd(date)
gen mkt_rsq=sum(_mkt_rsq)
cap rename crsp mkt
keep mkt_lvl mkt_rsq date mkt
bcal create bdays, from(date) replace
gen bday=bofd("bdays",date)
save mkt_lvl, replace

insheet using "${data}\industry_map.csv", comma clear names
drop internalshortcode*
rename (fficode fficode2)(internalshortcode internalshortcode2)
save industry_map, replace


use mkt_lvl, clear
qui su date
local fdate=r(min)
use "${data}\ffi12_vwew_idx", clear
gsort internalshortcode date
gen internalshortcode2=internalshortcode
	if "${iScheme}"=="EWFF" {
		bys internalshortcode: gen double logtrr=log(IDXew)-log(IDXew[_n-1])
	}
	else {
		bys internalshortcode: gen double logtrr=log(IDXvw)-log(IDXvw[_n-1])
	}
	drop IDXew IDXvw
drop if date<`fdate'+2

joinby internalshortcode using industry_map
duplicates rep industry date
save industry_ret0, replace
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
use industry_ret0, clear
gsort industry date
bys industry : keep if _n==_N
drop date logtrr 
egen indnum=group(industry)
save industry_char, replace

// use market returns for industry returns before their series begin
use mkt_lvl, clear
qui su date
local fdate=r(min)
use industry_ret0, clear
egen indnum=group(industry)
keep indnum date
duplicates drop indnum date, force
gsort indnum date
bys indnum: keep if _n==1
replace date=date-1
expand 2
bys indnum: replace date=`fdate' if _n==1
xtset indnum date
tsfill
duplicates drop indnum date, force
joinby indnum using industry_char
merge m:1 date using mkt_lvl, keepus(mkt) keep(matched) nogen
gen logtrr=log(1+mkt)
drop mkt
drop indnum
save industry_retStub, replace 

use industry_ret0, replace
gsort industry date
append using industry_retStub
egen indnum=group(industry)
keep indnum date logtrr
gen _ret_rsq=logtrr^2
xtset indnum date
tsfill
joinby indnum using industry_char
gsort industry date
gen qtr=qofd(date)
bys industry: gen ibmk_rsq=sum(_ret_rsq)
bys industry: gen ibmk_lvl=exp(sum(logtrr))
keep industry date ibmk_lvl ibmk_rsq
gsort industry date 
save industry_ret, replace

cap rm industry_ret0
cap rm industry_retStuby 
cap rm industry_char


