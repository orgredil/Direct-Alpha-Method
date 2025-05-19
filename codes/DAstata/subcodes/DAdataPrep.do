////////////////////////////////////////////////////////////////////////////////
// this code need to be run by MAIN4site_DirectAlpha.do
/*Copyright O.Gredil*/

/// time-invariant characteristics /////
use "${data}\${CFdset}", clear
bys fundid: keep if _n==_N
keep fundid industry* ftype vintage incept bet_est
rename incept date
merge m:1 date using mkt_lvl, keep(master matched) nogen keepus(mkt_lvl mkt_rsq)
merge m:1 industry date using industry_ret, keep(master matched) gen(ibmk_match) keepus(ibmk_lvl ibmk_rsq)
rename (date mkt_lvl mkt_rsq ibmk_lvl ibmk_rsq) (incept mkt_lvl0 mkt_rsq0 ibmk_lvl0 ibmk_rsq0)
replace ibmk_lvl0=mkt_lvl0 if ibmk_match==1
replace ibmk_rsq0=mkt_rsq0 if ibmk_match==1
save temp_char, replace
** NAV chars 
use "${data}\${CFdset}", clear
keep if nav!=.
gsort fundid time date
qui bysort fundid: egen navcount=count(nav)
qui bysort fundid: gen firstnav_qtr=time[1]
qui bysort fundid: gen lastnav_qtr=time[_N]
qui bysort fundid: egen lastnav=total(cond(time==lastnav_qtr,nav,.))
qui bysort fundid: gen lastnav_date=date[_N]
format lastnav_date %td
bys fundid: keep if _n==_N
keep fundid last* firstnav_qtr navcount
merge 1:1 fundid using temp_char, nogen keep(matched)
save temp_char, replace 
** Contributions' chars 
use "${data}\${CFdset}", clear
keep if C!=0
gsort fundid time date
qui bysort fundid: egen Ccount=count(C)
qui bysort fundid: gen firstC_qtr=time[1]
qui bysort fundid: gen lastC_qtr=time[_N]
qui bysort fundid: gen lastC_date=date[_N]
bys fundid: keep if _n==_N
keep fundid last* firstC_qtr Ccount
merge 1:1 fundid using temp_char, nogen keep(matched)
save temp_char, replace 
** Distributions' chars 
use "${data}\${CFdset}", clear
keep if D!=0
gsort fundid time date
qui bysort fundid: egen Dcount=count(D)
qui bysort fundid: gen firstD_qtr=time[1]
qui bysort fundid: gen lastD_qtr=time[_N]
qui bysort fundid: gen lastD_date=date[_N]
bys fundid: keep if _n==_N
keep fundid last* firstD_qtr Dcount
merge 1:1 fundid using temp_char, nogen keep(matched)
foreach vv of varlist *_date {
format `vv' %td 
}
save temp_char, replace 



use "${data}\${CFdset}", clear
drop industry* ftype vintage incept bet_est
joinby fundid using temp_char
merge m:1 date using mkt_lvl, keep(master matched) nogen
merge m:1 industry date using industry_ret, keep(master matched)  keepus(ibmk_lvl ibmk_rsq) gen(ib) 
replace ibmk_match=1 if industry==""
replace ibmk_lvl =mkt_lvl  if ibmk_match==1  
replace ibmk_lvl0=mkt_lvl0 if ibmk_match==1  
replace ibmk_rsq =mkt_rsq  if ibmk_match==1  

replace mkt_rsq=mkt_rsq-mkt_rsq0
replace ibmk_rsq=ibmk_rsq-ibmk_rsq0

gen weekday=dow(date)
gsort fundid date
if "${bmk}"=="ibmk" {
	local bmk "ibmk"
}
else {
	local bmk "mkt"
}
if "${BetaAdj}"=="Yes" {
gen pv_=exp(bet_est*log(`bmk'_lvl0/`bmk'_lvl))-0.5*`bmk'_rsq*bet_est*(bet_est-1)
gen pvD=D*exp(bet_est*log(`bmk'_lvl0/`bmk'_lvl))-0.5*`bmk'_rsq*bet_est*(bet_est-1)
gen pvC=C*exp(bet_est*log(`bmk'_lvl0/`bmk'_lvl))-0.5*`bmk'_rsq*bet_est*(bet_est-1)
gen pvNAV=nav*exp(bet_est*log(`bmk'_lvl0/`bmk'_lvl))-0.5*`bmk'_rsq*bet_est*(bet_est-1)

}
else {
gen pv_=`bmk'_lvl0/`bmk'_lvl
gen pvD=D*`bmk'_lvl0/`bmk'_lvl
gen pvC=C*`bmk'_lvl0/`bmk'_lvl
gen pvNAV=nav*`bmk'_lvl0/`bmk'_lvl
}
/////////////////////////////////////////////////////////////////////////////////////
*Going to the quarterly frequency w/ zeros for quarters with missing cash flows
global qVars "qD qC C D pvC pvD"
gsort fundid time date
cap drop temp*
bys fundid time: gen temp=`bmk'_lvl if _n==_N
bys fundid time: egen temp2 = total(temp), missing 
/* qC and qD will be nominal sums */
gen qC=C
gen qD=D
/*discount C and D at bmk return to be end-of-quarter values*/
replace C=C*temp2/`bmk'_lvl 
replace D=D*temp2/`bmk'_lvl 
foreach vv in C D {
qui bys fundid time: egen `vv'_count=count(`vv')
}
foreach vv of varlist $qVars {
disp "`vv'"
cap drop temp*
rename `vv' temp
qui bys fundid time: egen `vv'=total(temp)
cap drop temp*
}
bys fundid time: keep if _n==_N
bys fundid: replace nav=0 if _n==_N & nav==. & lastD_date>=lastnav_date
bys fundid: gen  _nC=C[_n+1]/`bmk'_lvl[_n+1]*`bmk'_lvl if date[_n+1]>max(lastD_date,lastnav_date)
bys fundid: egen __nC=total(_nC) 
order _*
bys fundid: drop if nav==. & date>=max(lastD_date,lastnav_date)
bys fundid: replace D=D-__nC if _n==_N & __nC!=0
drop _*
drop lastC_date lastD_date lastnav_date
gen qofd=qofd(date)

xtset fundid qofd
tsfill  //will make a difference if for some quartres NAVs are not availble
*pick the last business day in the quarter for padded quarters
forval dd=1/7 {
gen temp`dd' = bofd("bdays",dofq(qofd+1)-`dd')
}
egen temp_=rowfirst(temp1-temp7)
gen temp__=dofb(temp_,"bdays")
replace date=temp__ if date==.
cap drop temp*
foreach vv in C D {
replace `vv'_count=0 if `vv'==. | `vv'==0
}
*merge benchmark return data for EoQs with missing NAVs and zero CFs
merge m:1 date using mkt_lvl, update keep(1 3 4 5) nogen
merge m:1 industry date using industry_ret, update keep(1 3 4 5) nogen
replace  ibmk_lvl=mkt_lvl if ibmk_match==1 & ibmk_lvl==.
replace  ibmk_rsq=mkt_rsq if ibmk_match==1 & ibmk_rsq==.
bys fundid: egen fundT=max(time)
cap drop temp
qui gen temp=`bmk'_lvl if time==fundT
bys fundid: egen `bmk'_lvlT=max(temp) 
cap drop temp
order bday*
gsort fundid time
xtset fundid time
qui gen Rt=0 
bys fundid: replace  Rt=`bmk'_lvl/l1.`bmk'_lvl if _n>=2
save "funds_qly", replace 
