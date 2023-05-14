cd "/Users/wqr/Desktop/113772-V1/AEJ"
use "main_fame.dta", clear
ssc install outreg2,replace 
 
summarize ln_avwage net_pcm ctreat1  treat1_NMW NMW avwage c_avwage99 avwage99_NMW grad2 unionmem ptwk female
	
*** TABLE 1: DIFF-IN-DIFF ***
cap estimates drop *

eststo: reg ln_avwage ctreat1 treat1_NMW NMW if pp==1, cluster(regno)
outreg2 using table1.xls, excel bdec(3) rdec(3) adjr2 tstat td(3) replace

eststo: reg net_pcm ctreat1 treat1_NMW NMW if pp==1, cluster(regno)
outreg2 using table1.xls, excel bdec(3) rdec(3) adjr2 tstat td(3) append

*** TABLE 2: WAGES AND PROFITABILITY BEFORE AND AFTER NMW, REGRESSION ESTIMATES
** Controls
cap estimates drop *
***Policy
**Discrete
*Main
eststo: xi: reg ln_avwage ctreat1  treat1_NMW NMW grad2 unionmem ptwk female i.sic2  i.year i.gorwk if pp==1,cluster(regno)
eststo: xi: reg net_pcm ctreat1  treat1_NMW NMW grad2 unionmem ptwk female i.sic2  i.year i.gorwk if pp==1,cluster(regno)
*No response
xi: mvreg ln_avwage net_pcm =  ctreat1  treat1_NMW NMW grad2 unionmem ptwk female i.sic2  i.year i.gorwk if pp==1
test [net_pcm]treat1_NMW + (([ln_avwage]treat1_NMW)*.27) = 0
**Continuous
*Main
eststo: xi: reg ln_avwage c_avwage99 avwage99_NMW NMW grad2 unionmem ptwk female i.sic2  i.year i.gorwk if pp==1,cluster(regno)
eststo: xi: reg net_pcm c_avwage99 avwage99_NMW NMW grad2 unionmem ptwk female i.sic2  i.year i.gorwk if pp==1,cluster(regno)
*No response
xi: mvreg ln_avwage net_pcm = c_avwage99 avwage99_NMW NMW grad2 unionmem ptwk female i.sic2  i.year i.gorwk if pp==1 
test [net_pcm]avwage99_NMW + (([ln_avwage]avwage99_NMW)*.27) = 0
esttab using table2.csv, replace stats(N, fmt(%9.0f %9.0g)) cells(b(star fmt(3)) se(par fmt(3))) starlevels( * 0.10 ** 0.05 *** 0.01) keep(treat1_NMW avwage99_NMW) nogap
                                   
**** FIGURE 2: CHANGE IN LN(AVWAGE) BY PERCENTILE IN THE FINANCIAL YEAR BEFORE
**** AND AFTER NMW INTRODUCTION
clear
use "main_fame.dta", clear
 
    *£3K lower bound
	local i=3
	pctile pcw95=ln_avwage if year==1995 & avwage>=`i', nq(100) genp(percent95)
	pctile pcw96=ln_avwage if year==1996 & avwage>=`i', nq(100) genp(percent96)
	pctile pcw97=ln_avwage if year==1997 & avwage>=`i', nq(100) genp(percent97)
	pctile pcw98=ln_avwage if year==1998 & avwage>=`i', nq(100) genp(percent98)
	pctile pcw99=ln_avwage if year==1999 & avwage>=`i', nq(100) genp(percent99)
	pctile pcw00=ln_avwage if year==2000 & avwage>=`i', nq(100) genp(percent00)
	pctile pcw01=ln_avwage if year==2001 & avwage>=`i', nq(100) genp(percent01)
	pctile pcw02=ln_avwage if year==2002 & avwage>=`i', nq(100) genp(percent02)

	sort percent95
	g diff96=pcw96-pcw95
	g diff97=pcw97-pcw96
	g diff98=pcw98-pcw97
	g diff99=pcw99-pcw98
	g diff00=pcw00-pcw99
	g diff01=pcw01-pcw00
	g diff02=pcw02-pcw01

	order pcw95 pcw96 pcw97 pcw98 pcw99 pcw00 pcw01 pcw02 percent95 percent96 percent97 percent98 percent99 percent00 percent01 percent02 diff96 diff97 diff98 diff99 diff00 diff01 diff02  
 
	lab var percent99 "Percentile"
	lab var diff99 "1998-1999"
	lab var diff00 "1999-2000"
 	lab var diff99 "1998-1999"
	lab var diff00 "1999-2000"
	lab var percent99 "Percentiles"
	*Figure 2 graph
	line diff99 diff00 percent99 if percent99<76, clpat(dash solid) ysc(r(-0.02 0.14)) xsc(r(0 70)) ylabel(0 0.05 0.10 0.15) xlabel(0 13 25 50 75) xline(13 50, lp(dash) lw(vvthin) lc(gs8)) text(0.14 14 "£12,000") text(0.10 53 "£20,000")

    gen treat = ln_avwage if avwage<=1.2
	gen control = ln_avwage if avwage > 1.2 & avwage < 2
	bysort year:egen treat_wage = mean(treat)
	bysort year:egen control_wage = mean(control)
	duplicates drop year,force
	scatter treat_wage year,c(1)||scatter control_wage year,c(1)
	