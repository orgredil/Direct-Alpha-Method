////////////////////////////////////////////////////////////////////////////////
// this code need to be run by MAIN4site_DirectAlpha.do
/*Copyright O.Gredil*/
set more off
use "funds_qly", clear // time increment should be quarterly in this file
gsort fundid time
bys fundid: gen PME=(sum(pvD)+pvNAV)/sum(pvC)
if "${bmk}"=="ibmk" {
	local bmk "ibmk"
}
else {
	local bmk "mkt"
}
gen rc_0t =log(`bmk'_lvl/`bmk'_lvl0)
gen __dfac=exp(log(`bmk'_lvlT/`bmk'_lvl0)-rc_0t)
bys fundid: gen C_fvT =C*__dfac
bys fundid: gen D_fvT =D*__dfac
bys fundid: gen cumC_fv=sum(C*__dfac)/__dfac
bys fundid: gen cumD_fv=sum(D*__dfac)/__dfac
bys fundid: gen _PME=(cumD_fv+nav)/cumC_fv
// scatter _PME PME //should be same if PV are not adjusted for beta 
replace PME =_PME if PME==.
bys fundid: replace PME = 0.98 if PME==. & time==0 
bys fundid: ipolate PME bday, gen(_PMEi)
replace PME =_PMEi if PME==.
bys fundid: gen  nav_ln = cumC_fv-cumD_fv
/* create interpolated NAVs to compute inputs for CapDyn and CamAs metrics in case some interim NAVs are missing*/ 
bys fundid: gen nav_i =cond(nav!=.,nav,(cumC_fv-C)*_PMEi+C*0.98 - cumD_fv)
if "${BetaAdj}"=="Yes" {
replace pvNAV = nav_i*exp(-rc_0t*bet_est) if pvNAV==.
}
else {
replace pvNAV = nav_i*exp(-rc_0t) if pvNAV==.
}
bys fundid: gen TVPI=(sum(D)+nav_i)/sum(C)
drop if time>60 // disregard any data beyond the 15th year 
bys fundid: egen fundTe=max(time)
replace nav=nav_i if nav==.

order fundid time *PME* `bmk'* cumD_* cumC_*  nav* pvNAV _* rc_0t date C D
drop _*

/*CD adjustment factor*/
gen s_cd = cond(cumD_fv==0,1,(cumC_fv-nav_i)/cumD_fv)
/*mPME adjustment factor*/
gen s_ca = cond(D+nav_i<D,1,abs(D)/(abs(D)+abs(nav_i)))
gen NAVca=nav_i
bys fundid: replace NAVca=(1-s_ca)*(NAVca[_n-1]*Rt+C) if _n>=2
gen Dca  =D
bys fundid: replace Dca=s_ca*(NAVca[_n-1]*Rt+C) if _n>=2

qui su fundid
local minfundid=r(min)
local maxfundid=r(max)
foreach vv in "" "q" "pv" "pv2" "ln" "cd" "st" {
qui gen IRR`vv'=.
}
/////////////////// end-of-life estimates  ////////////////////////////////
// fund-quarters need be sorted descending by time
gsort +fundid  -time
timer clear 1
timer on 1
qui tab fundid, matrow(fundIDs)
local rows=rowsof(fundIDs)
_dots 0, title(Estimation Loop running) reps(`rows')
forval i=1/`rows' {
local fid=fundIDs[`i',1]
// disp "`fid'"
	qui su time if fundid==`fid' 
	local rmax=r(max)
	qui su nav if fundid==`fid' & time==`rmax'
	local nav=r(mean)
	if `nav'!=. {
// 	/**/
	capture {
		//IRR w/ eoq-valued CFs
		cap drop CF
		cap drop _cumCF
		gen CF=D-C  if fundid==`fid'
		replace CF=CF+NAV if fundid==`fid' & time==`rmax'
		bys fundid: gen _cumCF=sum(CF) if fundid==`fid'
		replace CF=. if _cumCF==0
			mkmat CF if CF!=., matrix(cf)
		do "${codes}\irr_mata"
		scalar def root=properroot[1,1]
		replace IRR=(root)^4-1 if fundid==`fid' 
		mat drop properroot cf
		cap scalar drop root
		//IRR w/o adjustment for the inter-quarter timing	
		replace CF=qD-qC  if fundid==`fid'
		replace CF=CF+NAV if fundid==`fid' & time==`rmax'
		replace CF=. if _cumCF==0
			mkmat CF if CF!=., matrix(cf)
		do "${codes}\irr_mata"
		scalar def root=properroot[1,1]
		replace IRRq=(root)^4-1 if fundid==`fid' 
		mat drop properroot cf
		cap scalar drop root
		}
	capture {
    	//DA
		replace CF=D_fv-C_fv  if fundid==`fid'
		replace CF=CF+nav_i if fundid==`fid' & time==`rmax'
		replace CF=. if _cumCF==0
			mkmat CF if CF!=., matrix(cf)
		do "${codes}\irr_mata"
		scalar def root=properroot[1,1]
		replace IRRpv=(root)^4-1 if fundid==`fid' 
		mat drop properroot cf
		cap scalar drop root
    	//DA w/ beta estimates
		replace CF=pvD-pvC  if fundid==`fid'
		replace CF=CF+pvNAV if fundid==`fid' & time==`rmax'
		replace CF=. if _cumCF==0
			mkmat CF if CF!=., matrix(cf)
		do "${codes}\irr_mata"
		scalar def root=properroot[1,1]
		replace IRRpv2=(root)^4-1 if fundid==`fid' 
		mat drop properroot cf
		cap scalar drop root
		} 
		if "${DAplus}"=="Yes" {
	capture {
    	//Long-Nickels IRR
		replace CF=D-C if fundid==`fid'
		replace CF=CF+nav_ln if fundid==`fid' &time==`rmax'
			mkmat CF if CF!=., matrix(cf)
		do "${codes}\irr_mata"
		scalar def root=properroot[1,1]
		replace IRRln=(root)^4-1 if fundid==`fid' 
		mat drop properroot cf
		cap scalar drop root
		} 
	capture { 
		//Capital Dynamics IRR
		qui su s_cd if fundid==`fid' & time==`rmax'
		local s_cd=r(mean)
		replace CF=D*`s_cd'-C if fundid==`fid' 
		replace CF=CF+nav if fundid==`fid' & time==`rmax'
// 		replace CF=. if _cumCF==0
			mkmat CF if CF!=., matrix(cf)
		do "${codes}\irr_mata"
		scalar def root=properroot[1,1]
		replace IRRcd=(root)^4-1 if fundid==`fid' 
		mat drop properroot cf
		cap scalar drop root
		} 
	capture { 
		//Cambridge Associates IRR
		replace CF=Dca-C if fundid==`fid' 
		replace CF=CF+NAVca if fundid==`fid' & time==`rmax'
			mkmat CF if CF!=., matrix(cf)
		do "${codes}\irr_mata"
		scalar def root=properroot[1,1]
		replace IRRst=(root)^4-1 if fundid==`fid' 
		mat drop properroot cf
		cap scalar drop root
		}
		}
	**/
	}
_dots `i' 0
}
gsort fundid time
timer off 1
timer list 1
gen durationEstFV=log(PME)/log(1+IRRpv)
gen durationEst  =log(TVPI)/log(1+IRR)
gsort fundid time
 cap drop temp*
bys fundid: gen temp =(D_count>0)
bys fundid: gen temp1=(C_count>0)
bys fundid: gen Dnum=sum(temp)
bys fundid: gen Cnum=sum(temp1)
bys fundid: egen vol_bmk=mean(`bmk'_rsq)
         replace vol_bmk=sqrt(vol_bmk)
cap drop temp*
bys fundid: keep if _n==_N
drop CF *_count
/**/


gen anKSpme=exp(log(PME)/5.0)-1
gen DA_raw =IRRpv
gen DA_badj=IRRpv2
gen alf_LN=IRR-IRRln
gen alf_CD=IRR-IRRcd
gen alf_ST=IRR-IRRst
drop IRR?? IRR??? _cumCF Rt
save "${output}fund_EOLest_${bmk}", replace

