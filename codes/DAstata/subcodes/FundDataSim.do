////////////////////////////////////////////////////////////////////////////////
// this code need to be run by MAIN4site_DirectAlpha.do
/*Copyright O.Gredil*/
set seed 2014  // fix seeds so results are reproduceable
set more off
use industry_ret, clear
global dummyFundRet "Yes" // if fund retuns equal industry returns 
global dummyFundRet "No"  // if fund retuns feature idiosync compe and levered ind.ret 
qui gen year=year(date)
qui su year
local minyr=r(min)+1 
local maxyr=r(max)-6 
keep industry
duplicates drop
local fundsPerInd=ceil(${FundsNumber}/_N) 
disp "`fundsPerInd'"
expand `fundsPerInd'
gen vintage=runiformint(max(`minyr',${fVintage}),min(`maxyr',${lVintage}))
gen fundid=_n
gen incept=mdy(ceil(runiform()*12),ceil(runiform()*28),vintage)
format incept %td
joinby industry using industry_ret
drop if year(date)<vintage | year(date)>vintage+13
format incept %td
drop if date<incept
gen bday =bofd("bdays",date)
gen qofd=qofd(date)
qui su qofd
drop if qofd==r(max)
gen weekday=dow(date)
drop if bday==. 
drop bday* weekday qofd
gsort fundid date
bys fundid: replace incept=date[1] 


//Cash and Value flow process by fund
**since-inception returns and age in years
local fidVlty=0.1 // idiosyncratic returns' to industry vol-ty per year
gen fidlogret=rnormal()*`fidVlty'/sqrt(252) - 0.5*(`fidVlty'^2)/252
gen bet2i=rnormal(1.1,.1)
gen ftype="N(1.1,`fidVlty'^2) beta to ind" 
bys fundid: gen age=(date-date[1])/365.25
* see Korteweg and Sorensen (2010 RFS) or Korteweg and Nagel (2022 WP) for DGP setup
bys fundid: gen fidCumLogRet=sum(fidlogret)-fidlogret[1] ///
							+bet2i*(log(ibmk_lvl)-log(ibmk_lvl[1])) ///
							+0.5*bet2i*(bet2i-1)*(ibmk_rsq-ibmk_rsq[1])
bys fundid: gen R0t=exp(fidCumLogRet)
if "${dummyFundRet}"=="Yes" {
bys fundid: replace R0t=(ibmk_lvl/ibmk_lvl[1]) 
}
**smoothed returns at daily frequency
gen barR0t=R0t
bys fundid: replace barR0t=(1-.95)*R0t+.95*barR0t[_n-1] if _n>1
bys fundid: egen trueAlf=mean(fidlogret)
su trueAlf if barR0t==R0t
cap drop temp
save supertemp, replace 


use supertemp, clear


**interim calls's frequency calibrated to an average of 14 during first 6 years 
gen C1s=rbinomial(1,0.0075) if age<=6
bys fundid: replace C1s=0 if C1s==.
bys fundid: replace C1s=1 if _n==1
** interim distrib. freq. calibrated to an average 21 during 4th to 12th year 
bys fundid: gen D1s=rbinomial(1,0.0075) if inrange(age,3,12)==1
bys fundid: replace D1s=0 if D1s==.
// 	collapse (sum) C1s D1s, by(fundid)
// 	tabstat C1s D1s, st(mean p1 p25 p50 p75 p99)

**see Brown,Ghysels,Gredil (2023RFS) for details on Value-to-cumR mapping 
gen C2V=min(.99,rexponential(0.25)/age)*C1s //<1 since V includes C itself, 0.25x/age on average (if occur) 
gen df =min(.99,normal(-2.5+.25*age+rnormal()))*D1s //dist. as a fraction of Value
bys fundid: gen M2lagM=1/(1-df)-C2V if _n>1
gen M=1
bys fundid: replace M=M[_n-1]*M2lagM if _n>1

**assume the t=1 call and, thus, V are normalized to 1
gen V=R0t/M
bys fundid: gen C=cond(_n==1,1,C2V*V)
bys fundid: egen fundSize=total(C)

** terminal D end of the 13th year or when V 1st drops below 0.05*fundSize after 5th year
gen D=cond(df==0,0,df/(1-df)*V)
gen temp=cond(V<0.1 & age>5,1,0)
bys fundid: gen temp1=sum(temp)
bys fundid: gen temp2=1 if temp[_n+1]==1
replace temp2=0 if temp2==.
bys fundid: gen temp3=sum(temp2)
drop if temp3>0
drop temp*

qui su vintage
bys fundid: replace D=D+V if vintage<r(max)-3 & _n==_N
bys fundid: replace V=0 if vintage<r(max)-3 & _n==_N
gen qofd=qofd(date)
format qofd %tq
gsort fundid qofd date 
bys fundid qofd: gen nav=barR0t/M if _n==_N
//Assume that last reported nav is 0, whenever the true value is 
bys fundid: replace nav=cond(V==0,0,barR0t/M) if _n==_N

**NPV identity check:
* _PME should be (.9999,1.0001) if both bmk and fund returns are ibmk
* _PME2 & RepErr reflects return smoothing bias 
local bmk "ibmk"
bys fundid: gen `bmk'_lvl0=`bmk'_lvl[1] 
bys fundid: gen `bmk'_lvlT=`bmk'_lvl[_N] 
gen rc_0t =log(`bmk'_lvl/`bmk'_lvl0)
gen __dfac=exp(log(`bmk'_lvlT/`bmk'_lvl0)-rc_0t)
bys fundid: gen C_fvT =C*__dfac
bys fundid: gen D_fvT =D*__dfac
bys fundid: gen cumC_fv=sum(C*__dfac)/__dfac
bys fundid: gen cumD_fv=sum(D*__dfac)/__dfac
bys fundid: gen _PME=(cumD_fv+V)/cumC_fv
bys fundid: gen _PME2=(cumD_fv+nav)/cumC_fv
bys fundid: gen RepErrE=log(_PME2/_PME) if _n==_N & V!=0
gen RepErr=log(_PME2/_PME) if RepErrE==.
su _PME* RepErr*


keep if nav!=. | C>0 | D>0
bys fundid: gen time=qofd-qofd[1] 
rename bet2i bet_est
keep  fundid ftype industry vintage incept time date  C D nav bet_est
order fundid ftype industry vintage incept time date  C D nav bet_est
save "${data}\${CFdset}", replace

rm supertemp.dta
