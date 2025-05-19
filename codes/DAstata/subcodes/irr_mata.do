version 12.1
// Solves for (1+IRR) given a stata Nx1 matrix of equally spaced cash flows named 'cf'.
// cash flows in 'cf' need be sorted DESCENDING time-wise.
// Checks if returns were positive and takes smallest real root>=1 than
// ... largest real root<=1 if returns were negative.
// Creates Stata matrix named 'properroot' as the output.
//
// by O.Gredil

mata:
cf_=st_matrix("cf")
invested=-sum(select(cf_,Re(cf_):<0))
returned=sum(select(cf_,Re(cf_):>0))
T=rows(cf_)-1
//perturb the final distribution to require that the found root in monotone in it
uptick=invested*.001
cf_up=(uptick+cf_[1]\cf_[2..rows(cf_)]) 
cf_dn=(-uptick+cf_[1]\cf_[2..rows(cf_)]) 

cf_t=transposeonly(cf_)
cf_upt=transposeonly(cf_up)
cf_dnt=transposeonly(cf_dn)

roots=polyroots(cf_t)
rootsup=polyroots(cf_upt)
rootsdn=polyroots(cf_dnt)

rrsFlag=(Im(roots):==0):*(Re(roots):>0)
rruFlag=(Im(rootsup):==0):*(Re(rootsup):>0)
rrdFlag=(Im(rootsdn):==0):*(Re(rootsdn):>0)

rrFlag=rrsFlag:*rruFlag:*rrdFlag

if (sum(select(rrFlag,rrFlag:>=0))>0) {
realroots=select(roots,rrFlag)
realrootsdn=select(rootsdn,rrFlag)
realrootsup=select(rootsup,rrFlag)
below=Re(realroots):< Re(realrootsup)
above=Re(realroots):> Re(realrootsdn)
consroots=select(realroots,below:*above)
posroots=select(Re(consroots),Re(consroots):>=1)
negroots=select(Re(consroots),Re(consroots):<1 )
properroot= (returned>=invested ? min(posroots) : max(negroots))  
}
else if (sum(select(rrsFlag,rrsFlag:>=0))>0) {
// no guantree that the number of real roots is the same in this case
realroots=select(roots,rrsFlag)
posroots=min(select(Re(realroots),Re(realroots):>=1))
negroots=max(select(Re(realroots),Re(realroots):<1 ))

realrootsup=select(rootsup,rruFlag)
posrootsup=min(select(Re(realrootsup),Re(realrootsup):>=1))
if ((posroots>posrootsup):*(posrootsup:~=.))  {
	posroots=.
}
realrootsdn=select(rootsdn,rrdFlag)
negrootsdn=max(select(Re(realrootsdn),Re(realrootsdn):<1 ))
if ((negroots<negrootsdn):*(negrootsdn:~=.)) {
	negroots=.
}
	properroot= (returned>=invested ? min(posroots) : max(negroots))  
}
else {
properroot=.  
} 
st_matrix("properroot",properroot)
end
