log using managed_carefix2.txt, text replace
/*
use mc91
append using mc92
append using mc93
append using mc94
append using mc95
append using mc96
append using mc97
append using mc98
append using mc99
append using mc00
append using mc01
append using mc02
append using mc03
append using mc04
append using mc05

replace undup_tot=st_tot if undup_tot==.
replace st_tot=undup_tot+pccm2 if pccm2!=.
replace st_tot=st_tot+php2 if php2!=.
replace state="XX" if state=="TOTAL"
sort year state
save mc91_05, replace

list state year undup_tot

clear
*/
use fmr90-05
sort state year
merge state year using mc91_05
tab _merge
sort _merge territory year state

tab year if _merge==2
save mc_fmr2, replace

/*Keep only "total computable" payshare*/
keep if payshare==3
sort year state
save mc_fmr2, replace

/*Adding Medicaid total enrollment data*/
sort year state
drop _merge
merge year state using mcdben
tab _merge
tab year if _merge==1
tab state if _merge==1 & year>1995
drop _merge

/*Checking added enrollment data against what is already in the fmr90-05 data set
	-looks like the national totals are different; I've double checked the ones
	 I added in, though*/
tab state if mdcdenroll!=mcdben & mdcdenroll!=. & mcdben!=.
tab year if mdcdenroll!=mcdben & mdcdenroll!=. & mcdben!=.

save mc_fmr2, replace

/*Need to get rid of territories & subtract Puerto Rico managed care ben. 
	from National totals*/

list state year st_tot undup_tot hmo mco php pihp if state=="PR"

replace st_tot=st_tot-53642 if state=="XX" & year==1994
replace undup_tot=undup_tot-53642 if state=="XX" & year==1994
replace st_tot=st_tot-53642 if state=="XX" & year==1995
replace undup_tot=undup_tot-53642 if state=="XX" & year==1995
replace st_tot=st_tot-801758 if state=="XX" & year==1996
replace undup_tot=undup_tot-801758 if state=="XX" & year==1996
replace st_tot=st_tot-702250 if state=="XX" & year==1997
replace undup_tot=undup_tot-702250 if state=="XX" & year==1997
replace st_tot=st_tot-813791 if state=="XX" & year==1998
replace undup_tot=undup_tot-813791 if state=="XX" & year==1998
replace st_tot=st_tot-764068 if state=="XX" & year==1999
replace undup_tot=undup_tot-764068 if state=="XX" & year==1999
replace st_tot=st_tot-828021 if state=="XX" & year==2000
replace undup_tot=undup_tot-828021 if state=="XX" & year==2000
replace st_tot=st_tot-898171 if state=="XX" & year==2001
replace undup_tot=undup_tot-898171 if state=="XX" & year==2001
replace st_tot=st_tot-1730570 if state=="XX" & year==2002
replace undup_tot=undup_tot-865285 if state=="XX" & year==2002
replace st_tot=st_tot-1714620 if state=="XX" & year==2003
replace undup_tot=undup_tot-857310 if state=="XX" & year==2003
replace st_tot=st_tot-1685654 if state=="XX" & year==2004
replace undup_tot=undup_tot-842827 if state=="XX" & year==2004
replace st_tot=st_tot-1697152 if state=="XX" & year==2005
replace undup_tot=undup_tot-865299 if state=="XX" & year==2005

replace hmo=hmo-53642 if state=="XX" & year==1994
replace hmo=hmo-53642 if state=="XX" & year==1995
replace hmo=hmo-801758 if state=="XX" & year==1996
replace hmo=hmo-702250 if state=="XX" & year==1997
replace php=php-865285 if state=="XX" & year==2002
replace mco=mco-813791 if state=="XX" & year==1998
replace mco=mco-764068 if state=="XX" & year==1999
replace mco=mco-828021 if state=="XX" & year==2000
replace mco=mco-898171 if state=="XX" & year==2001
replace mco=mco-865285 if state=="XX" & year==2002
replace mco=mco-857310 if state=="XX" & year==2003
replace mco=mco-842827 if state=="XX" & year==2004
replace mco=mco-848576 if state=="XX" & year==2005
replace pihp=pihp-857310 if state=="XX" & year==2003
replace pihp=pihp-842827 if state=="XX" & year==2004
replace pihp=pihp-848576 if state=="XX" & year==2005



drop if state=="AS"
drop if state=="GU"
drop if state=="MP"
drop if state=="PR"
drop if state=="VI"
drop if state=="XM"

/*Add/incorporate Dental enrollment data - see below for how to properly subtract from overall vars*/
sort state year
merge state year using dental_enroll
tab _merge  /*only 54 state-year obs with dental plans listed*/
drop _merge
replace mco=mco-mcodent if mcodent!=.      /*subtract from st_tot only (only =0 in years where undup_tot & st_tot are the same*/
replace hmo=hmo-hmodent if hmodent!=.      /*subtract from st_tot only starting in 1996; substract from both before*/
replace php=php-phpdent if phpdent!=.      /*subtract from st_tot only starting in 1996; substract from both before*/
replace php2=php2-php2dent if php2dent!=.  /*subtract from st_tot only*/
replace pahp=pahp-pahpdent if pahpdent!=.  /*subtract from st_tot only (only =0 in years where undup_tot & st_tot are the same*/


/*dropping dental plan enrollment from state totals*/
replace st_tot=st_tot-mcodent if mcodent!=.
replace st_tot=st_tot-hmodent if hmodent!=.
replace undup_tot=undup_tot-hmodent if hmodent!=. & year<1996
replace st_tot=st_tot-phpdent if phpdent!=.
replace undup_tot=undup_tot-phpdent if phpdent!=. & year<1996
replace st_tot=st_tot-php2dent if php2dent!=.
replace st_tot=st_tot-pahpdent if pahpdent!=.

save mc_fmr2, replace

/*managed care percentage variables*/

replace hmo=0 if hmo==.
replace php=0 if php==.
replace hio=0 if hio==.
replace pccm=0 if pccm==.
replace chc=0 if chc==.
replace st_tot=0 if st_tot==.
replace php2=0 if php2==.
replace pccm2=0 if pccm2==.
replace prog1115=0 if prog1115==.
replace other=0 if other==.
replace undup_tot=0 if undup_tot==.
replace mco=0 if mco==.
replace pihp=0 if pihp==.
replace pahp=0 if pahp==.
replace pace=0 if pace==.

gen mcpct=100*(undup_tot/mcdben)
sort year state
save mc_fmr2, replace

/*Adding SSI/BOE/etc variables for years we have them so far*/

merge year state using ssi
tab _merge  /*this is 50 states + DC + national total for 2005 when there isn't ssi data*/
rename _merge merge1
sort year state

merge year state using fmap
tab _merge  /*this is 50 states + DC + national total for 2005 and national total fo each year;
		no fmap data for these*/
rename _merge merge2
sort year state

merge year state using afdc
tab _merge
rename _merge merge3  /*this is 50 states + DC + national total for 2005 when there isn't afdc data*/
sort year state

merge year state using fymcdben
tab _merge
rename _merge merge4  /*this is 50 states + DC + national total for 2003-5 when there isn't
			fiscal year medicaid beneficiary enrollment data*/
sort year state
save mc_fmr2, replace

/* -already done, no need to repeat-
use boe
replace state="XX" if state=="Total"
sort year state
save boe, replace
*/
use mc_fmr2
merge year state using boe
tab _merge
tab year if _merge==1  /*data on this is only available for 1999-2003*/
drop _merge

save mc_fmr2, replace

/*Creating broad categories of expenditures*/

replace m_inpatientreg=0 if m_inpatientreg==.
replace m_inpatientdsh=0 if m_inpatientdsh==.
replace m_rx=0 if m_rx==.
replace s_rx=0 if s_rx==.
replace c_rx=0 if c_rx==.
replace s_inpatient=0 if s_inpatient==.
replace c_inpatientreg=0 if c_inpatientreg==.
replace c_inpatientdsh=0 if c_inpatientdsh==.
replace m_mentaldsh=0 if m_mentaldsh==.
replace m_mentalreg=0 if m_mentalreg==.
replace c_mentaldsh=0 if c_mentaldsh==.
replace c_mentalreg=0 if c_mentalreg==.
replace c_outpatientmental=0 if c_outpatientmental==.
replace s_mental=0 if s_mental==.
replace s_outpatientmental=0 if s_outpatientmental==.
replace m_mdcdmco=0 if m_mdcdmco==.
replace m_mdcdphp=0 if m_mdcdphp==.
replace m_prepaidamb=0 if m_prepaidamb==.
replace m_prepaidinp=0 if m_prepaidinp==.
replace m_primcasemgmt=0 if m_primcasemgmt==.
replace m_labrad=0 if m_labrad==.
replace c_labrad=0 if c_labrad==.
replace s_labrad=0 if s_labrad==.
replace m_dental=0 if m_dental==.
replace c_dental=0 if c_dental==.
replace s_dental=0 if s_dental==.
replace totmdcd=0 if totmdcd==.
replace totschip=0 if totschip==.

gen double inpat=m_inpatientreg+m_inpatientdsh+s_inpatient+c_inpatientreg+c_inpatientdsh
gen double mental=m_mentaldsh+m_mentalreg+c_mentaldsh+c_mentalreg+c_outpatientmental+s_mental+s_outpatientmental
gen double mcexp=m_mdcdmco+m_mdcdphp+m_prepaidamb+m_prepaidinp+m_primcasemgmt
gen double labrad=m_labrad+c_labrad+s_labrad
gen double allspend=totmdcd+totschip
gen double allrx=m_rx+s_rx+c_rx
gen double allspendnod=allspend-m_dental-s_dental-c_dental

save mc_fmr2, replace

/*Creating estimates of June 30 Medicaid Managed Care enrollment using the 1996 June 30/FY ratio*/

gen mcd96rat=mcdben/fymcdben if year==1996
egen mcd96ratio=min(mcd96rat), by (state)
replace mcdben=fymcdben*mcd96ratio if year<1996

/*Incorporate these estimates to create mcpct for pre-1996*/
replace mcpct=100*(undup_tot/mcdben) if year<1996
sort statename year
save mc_fmr2, replace

/*Adding Urban Institute Data*/
use uimmc
gen fips=real(fips97)
sort fips year
merge fips year using "census county population\countypop"
drop if year>=2002
tab _merge
/*the county 30113 is Yellowstone Park, which doesn't likely have a human population*/
gen hmompop=hmom*pop
gen hmovpop=hmov*pop
gen pccmmpop=pccmm*pop
gen pccmvpop=pccmv*pop
gen nommcpop=nommc*pop
gen hmo=1 if hmom==1
replace hmo=1 if hmov==1
replace hmo=0 if hmo!=1
gen hmopop=hmo*pop
gen pccmmonlypop= pccmm_only*pop
gen onlyvolpop= onlyvol*pop
gen mandhmopop=  mandhmo*pop
gen mixedmandpop=  mixedmand*pop
isid fips year
collapse (sum) pop pop017 pop014 pop019 hmompop hmovpop pccmmpop pccmvpop nommcpop pccmmonlypop onlyvolpop mandhmopop mixedmandpop, by( stname97 year)
isid stname97 year

/*to match UI/Medicaid data*/
replace stname97="District of Columbia" if stname97=="DC"
rename stname97 statename
sort statename year
merge statename year using mc_fmr2
tab _merge
tab _merge if year>2001  /*UI data only goes up to 2001*/
tab _merge if year<2002
tab statename if _merge==2 & year<2002  /*national-level of UI data has to be constructed*/
drop _merge

/*get national totals together for UI*population data*/
egen nathmompop=sum(hmompop), by(year)
egen nathmovpop=sum(hmovpop), by(year)
egen natpccmmpop=sum(pccmmpop), by(year)
egen natpccmvpop=sum(pccmvpop), by(year)
egen natnommcpop=sum(nommcpop), by(year)
egen nathmopop=sum(hmopop), by(year)
egen natpccmmonlypop=sum(pccmmonlypop), by(year)
egen natonlyvolpop=sum(onlyvolpop), by(year)
egen natmandhmopop=sum(mandhmopop), by(year)
egen natmixedmandpop=sum(mixedmandpop), by(year)
egen natpop=sum(pop), by(year)

replace hmompop=nathmompop if state=="XX"
replace hmovpop=nathmovpop if state=="XX"
replace pccmmpop=natpccmmpop if state=="XX"
replace pccmvpop=natpccmvpop if state=="XX"
replace nommcpop=natnommcpop if state=="XX"
replace hmopop=nathmopop if state=="XX"
replace pccmmonlypop=natpccmmonlypop if state=="XX"
replace onlyvolpop=natonlyvolpop if state=="XX"
replace mandhmopop=natmandhmopop if state=="XX"
replace mixedmandpop=natmixedmandpop if state=="XX"
replace pop=natpop if state=="XX"

drop nat*

/*get percentages of population in different programs*/
gen pcthmom=hmompop/pop
gen pcthmov=hmovpop/pop
gen pctpccmm=pccmmpop/pop
gen pctpccmv=pccmvpop/pop
gen pctnommc=nommcpop/pop
gen pcthmo=hmopop/pop
gen pctpccmmonly=pccmmonlypop/pop
gen pctonlyvol=onlyvolpop/pop
gen pctmandhmo=mandhmopop/pop
gen pctmixedmand=mixedmandpop/pop

gen pctmand=pctpccmmonly+pctmandhmo+pctmixedmand   /*percent county pop under mandatory managed care programs*/


/*some descriptive statistics*/

table year, contents(mean pcthmom mean pcthmov mean pctpccmm mean pctpccmv), [aweight=pop] if state!="XX"
table year, contents(sd pcthmom sd pcthmov sd pctpccmm sd pctpccmv), [aweight=pop] if state!="XX"
table year, contents(mean pctnommc mean pctpccmmonly mean pctonlyvol mean pctmandhmo mean pctmixedmand), [aweight=pop] if state!="XX"
table year, contents(sd pctnommc sd pctpccmmonly sd pctonlyvol sd pctmandhmo sd pctmixedmand), [aweight=pop] if state!="XX"

list year  totmdcd totschip inpat mental mcexp labrad allspend allrx if state=="XX"

/*creating per capita numbers*/
gen pcapallspend=allspend/mcdben
gen pcapallspendnod=allspendnod/mcdben
gen pcapinpat=inpat/mcdben
gen pcapmental=mental/mcdben
gen pcapmcexp=mcexp/mcdben
gen pcapmcexp2=mcexp/undup_tot
gen pcapallrx=allrx/mcdben
save mc_fmr2, replace

table year, contents (mean pcapallspend mean pcapinpat mean pcapmental mean pcapmcexp mean pcapallrx), [aweight=mcdben] if state!="XX"
table year, contents (sd pcapallspend sd pcapinpat sd pcapmental sd pcapmcexp sd pcapallrx), [aweight=mcdben] if state!="XX"
table year, contents (mean pcapmcexp2 sd pcapmcexp2), [aweight=undup_tot] if state!="XX"

table year, contents (mean mcpct sd mcpct), [aweight=mcdben] if state!="XX"

/*AFDC percentages (of total population & medicaid population, assuming
  all on AFDC are on Medicaid*/
gen pctpopafdc=(afdcadult+afdckid)/pop
gen pctmcafdc=(afdcadult+afdckid)/mcdben
gen pctmcafdckid=afdckid/mcdben
save mc_fmr2, replace

table year, contents (mean pctpopafdc sd pctpopafdc), [aweight=pop] if state!="XX"
table year, contents (mean pctmcafdc sd pctmcafdc mean pctmcafdckid sd pctmcafdckid), [aweight=mcdben] if state!="XX"

/*SSI percentages, assuming all on SSI are on Medicaid*/
gen pctpopssi=ssi/pop
gen pctmcssi=ssi/mcdben
save mc_fmr2, replace

table year, contents (mean pctpopssi sd pctpopssi), [aweight=pop] if state!="XX"
table year, contents (mean pctmcssi sd pctmcssi), [aweight=mcdben] if state!="XX"

/*percentages of kids 0-17; include 0-14 & 0-19 because no 0-17 option in 2000+ years*/
gen pctage017=pop017/pop
gen pctage014=pop014/pop
gen pctage019=pop019/pop
save mc_fmr2, replace

/*Distribution of managed care enrollment among program type*/
gen pcthmo=(hmo+mco)/undup_tot
gen pctphp=(php+pihp+pahp)/undup_tot
gen pctpccm=pccm/undup_tot
gen pcthio=hio/undup_tot

table year, contents (mean pcthmo mean pctphp mean pctpccm mean pcthio), [aweight=undup_tot] if state!="XX"
table year, contents (sd pcthmo sd pctphp sd pctpccm sd pcthio), [aweight=undup_tot] if state!="XX"

/*get rid of national observations!!!*/
drop if state=="XX"

/*keep only years 1991-2001 for now*/
keep if year>=1991 & year<=2001

/*descriptive stats*/
sum mcpct pctmand pctpopssi pctpopafdc pctage014 pctage019 if year>=1991 & year<=2001
sum mcpct pctmand pctpopssi pctpopafdc pctage014 pctage019 [aweight=mcdben] if year>=1991 & year<=2001

sum pctage017 if year>=1991 & year<=1999
sum pctage017 if year>=1991 & year<=1999 [aweight=mcdben]


/*a few regressions to start*/

/*look at first stage*/
replace mcpct=mcpct/100
reg mcpct pctmand
xi: reg mcpct pctmand i.state
xi: reg mcpct pctmand i.year
xi: reg mcpct pctmand i.state i.year
reg mcpct pctmand, cluster(state)
*xi: reg mcpct pctmand i.state, cluster(state)
*xi: reg mcpct pctmand i.year, cluster(state)
xi: reg mcpct pctmand i.state i.year, cluster(state)
reg mcpct pctmand [aweight=mcdben]
*xi: reg mcpct pctmand i.state [aweight=mcdben]
*xi: reg mcpct pctmand i.year [aweight=mcdben]
xi: reg mcpct pctmand i.state i.year [aweight=mcdben]
reg mcpct pctmand [aweight=mcdben], cluster(state)
*xi: reg mcpct pctmand i.state [aweight=mcdben], cluster(state)
*xi: reg mcpct pctmand i.year [aweight=mcdben], cluster(state)
xi: reg mcpct pctmand i.state i.year [aweight=mcdben], cluster(state)
reg mcpct pctmand [aweight=pop]
*xi: reg mcpct pctmand i.state [aweight=pop]
*xi: reg mcpct pctmand i.year [aweight=pop]
xi: reg mcpct pctmand i.state i.year [aweight=pop]
/*reg mcpct pctmand [aweight=undup_tot]
*xi: reg mcpct pctmand i.state [aweight=undup_tot]
*xi: reg mcpct pctmand i.year [aweight=undup_tot]
xi: reg mcpct pctmand i.state i.year [aweight=undup_tot]
*/
/*don't need these for now
reg pcapallspend mcpct pctpopssi pctpopafdc 
reg pcapallspendnod mcpct pctpopssi pctpopafdc 
ivreg pcapallspend (mcpct=pctmand) pctpopssi pctpopafdc 
ivreg pcapallspendnod (mcpct=pctmand) pctpopssi pctpopafdc 
xi: ivreg pcapallspend (mcpct=pctmand) pctpopssi pctpopafdc i.state i.year
xi: ivreg pcapallspend (mcpct=pctmand) pctpopssi pctpopafdc i.state i.year, robust
xi: ivreg pcapallspend (mcpct=pctmand) pctpopssi pctpopafdc i.state i.year, robust, [aweight=mcdben]
xi: ivreg pcapallspendnod (mcpct=pctmand) pctpopssi pctpopafdc i.state i.year
xi: ivreg pcapallspendnod (mcpct=pctmand) pctpopssi pctpopafdc i.state i.year, robust
xi: ivreg pcapallspendnod (mcpct=pctmand) pctpopssi pctpopafdc i.state i.year, robust, [aweight=mcdben]

gen lnpcapallspend=ln(pcapallspend)
gen lnpcapallspendnod=ln(pcapallspendnod)
reg lnpcapallspend mcpct pctpopssi pctpopafdc 
reg lnpcapallspendnod mcpct pctpopssi pctpopafdc 
ivreg lnpcapallspend (mcpct=pctmand) pctpopssi pctpopafdc 
ivreg lnpcapallspendnod (mcpct=pctmand) pctpopssi pctpopafdc 
xi: ivreg lnpcapallspend (mcpct=pctmand) pctpopssi pctpopafdc i.state i.year
xi: ivreg lnpcapallspend (mcpct=pctmand) pctpopssi pctpopafdc i.state i.year, robust
xi: ivreg lnpcapallspend (mcpct=pctmand) pctpopssi pctpopafdc i.state i.year, robust, [aweight=mcdben]
xi: ivreg lnpcapallspendnod (mcpct=pctmand) pctpopssi pctpopafdc i.state i.year
xi: ivreg lnpcapallspendnod (mcpct=pctmand) pctpopssi pctpopafdc i.state i.year, robust
xi: ivreg lnpcapallspendnod (mcpct=pctmand) pctpopssi pctpopafdc i.state i.year, robust, [aweight=mcdben]


/*some tables - least/most per capita spenders by year/category*/

sort year pcapallspend
list year state pcapallspend in 105/109
list year state pcapallspend in 151/156
list year state pcapallspend in 261/264
list year state pcapallspend in 308/312
list year state pcapallspend in 521/525
list year state pcapallspend in 568/572
list year state pcapallspend in 781/785
list year state pcapallspend in 828/832

sort year pcapinpat 
list year state pcapinpat in 105/109
list year state pcapinpat in 151/156
list year state pcapinpat in 261/264
list year state pcapinpat in 308/312
list year state pcapinpat in 521/525
list year state pcapinpat in 568/572
list year state pcapinpat in 781/785
list year state pcapinpat in 828/832

sort year pcapmental 
list year state pcapmental in 105/109
list year state pcapmental in 151/156
list year state pcapmental in 261/264
list year state pcapmental in 308/312
list year state pcapmental in 521/525
list year state pcapmental in 568/572
list year state pcapmental in 781/785
list year state pcapmental in 828/832

sort year pcapallrx 
list year state pcapallrx in 105/109
list year state pcapallrx in 151/156
list year state pcapallrx in 261/264
list year state pcapallrx in 308/312
list year state pcapallrx in 521/525
list year state pcapallrx in 568/572
list year state pcapallrx in 781/785
list year state pcapallrx in 828/832

sort year pcapmcexp2 
list year state pcapmcexp2 in 105/109
list year state pcapmcexp2 in 151/156
list year state pcapmcexp2 in 261/264
list year state pcapmcexp2 in 308/312
list year state pcapmcexp2 in 521/525
list year state pcapmcexp2 in 566/572
list year state pcapmcexp2 in 781/785
list year state pcapmcexp2 in 826/832

sort year pctpopafdc 
list year state pctpopafdc in 105/109
list year state pctpopafdc in 151/156
list year state pctpopafdc in 261/264
list year state pctpopafdc in 308/312
list year state pctpopafdc in 521/525
list year state pctpopafdc in 568/572
list year state pctpopafdc in 781/785
list year state pctpopafdc in 828/832

sort year pctpopssi 
list year state pctpopssi in 105/109
list year state pctpopssi in 151/156
list year state pctpopssi in 261/264
list year state pctpopssi in 308/312
list year state pctpopssi in 521/525
list year state pctpopssi in 568/572
list year state pctpopssi in 781/785
list year state pctpopssi in 828/832
*/

clear
log close

***creating expenditure variables & do labels, etc
use mc_fmr2
gen admin=at_totnetexp
gen mcaid_inpat=m_inpatientreg+m_inpatientdsh
gen mcaid_mc=m_coinsurance+m_mdcdmco+m_prepaidamb+m_prepaidinp+m_mdcdcoin+m_allcareeld+m_targetcasemgmt+m_primcasemgmt+m_mdcdphp
gen mcaid_rx=m_rx+m_drugrebnatl+m_drugrebstate
gen mcaid_mental=m_mentalreg+m_mentaldsh
gen mcaid_mcare=m_mdcrparta+m_mdcrpartb
gen mcaid_outpat=m_physserv+m_outpatienthosp+m_otherprac+m_clinicserv
gen mcaid_other=m_nursfacserv+m_interpublic+m_interprivate+m_labrad+m_homehealthserv+m_steril+m_abortions+m_epsdt+m_rural+m_pov120134+m_pov135175+m_personalserv+m_mdcdgroup+m_mdcdother+m_homecommun+m_homecommuneld+m_community+m_hospice+m_emergency+m_fedhealthcenter+m_otherserv+mc_collections
gen mcaid_dental=m_dental
gen ms_inpat=c_inpatientreg+c_inpatientdsh
gen ms_prem=c_premupto150grosspd+c_premupto150csoff+c_premover150grosspd+c_premover150csoff
gen ms_rx=c_rx+c_drugrebnatl+c_drugrebstate
gen ms_mental=c_mentalreg+c_mentaldsh+c_outpatientmental
gen ms_outpat=c_physsurg+c_outpatienthosp+c_visionserv+c_otherprac+c_clinicserv+c_therapyserv
gen ms_dental=c_dental
gen ms_mcare=c_mdcrpay
gen ms_other=c_labrad+c_mepequip+c_famplan+c_abortions+c_screenserv+c_homehealth+c_homecommun+c_hospice+c_medtrans+c_otherserv+cc_collections+c_nurscareservices
gen ms_casemgmt=c_casemgmt
gen schip_prem=s_premupto150grosspd+s_premupto150csoff+s_premover150grosspd+s_premover150csoff
gen schip_mental=s_mental+s_outpatientmental
gen schip_outpat=s_physsurg+s_outpatienthosp+s_visionserv+s_otherprac+s_clinicserv+s_therapyserv
gen schip_rx=s_rx
gen schip_dental=s_dental
gen schip_other=s_labrad+s_mepequip+s_famplan+s_abortions+s_screenserv+s_homehealth+s_reserved+s_homecommun+s_hospice+s_medtrans+s_otherserv+s_outreach+s_admin+sc_collections+s_nurscareservices
gen schip_casemgmt=s_casemgmt
gen schip_inpat=s_inpatient

keep mt_totnetexp st_totnetexp ct_totnetexp admin mcaid_* ms_* schip_* year pop pop017 pop014 pop019 hmompop hmovpop pccmmpop pccmvpop hmopop nommcpop pccmmonlypop onlyvolpop mandhmopop mixedmandpop state  hmo php hio pccm chc st_tot php2 pccm2 prog1115 other undup_tot mco pihp pahp pace mcdben hmodent mcodent pahpdent phpdent php2dent mcpct ssi  afdcadult afdckid  fymcdben mcd96ratio pcthmom pcthmov pctpccmm pctpccmv pcthmo pctnommc pctpccmmonly pctonlyvol pctmandhmo pctmixedmand pctmand pctpopafdc pctpopssi pctage017 pctage014 pctage019

label var pop017 "State Population Aged 0-17"
label var pop014 "State Population Aged 0-14"
label var pop019 "State Population Aged 0-19"
label var pctage017 "Percent of State Population Aged 0-17"
label var pctage014 "Percent of State Population Aged 0-14"
label var pctage019 "Percent of State Population Aged 0-19"
label var mt_totnetexp "Total Medicaid Only Expenses"
label var st_totnetexp "Total SCHIP Expenses"
label var ct_totnetexp "Total Medicaid/SCHIP Expenses"
label var admin "Administrative Expenses"
label var mcaid_inpat "Medicaid Inpatient Expenses"
label var mcaid_mc "Medicaid Managed Care Expenses"
label var mcaid_rx "Medicaid Prescription Expenses"
label var mcaid_mental "Medicaid Mental Health Expenses"
label var mcaid_mcare "Medicaid Medicare Expenses"
label var mcaid_outpat "Medicaid Outpatient Expenses"
label var mcaid_other "Medicaid Other Expenses - incl. collections"
label var mcaid_dental "Medicaid Dental Expenses"
label var ms_inpat "Medicaid/SCHIP Inpatient Expenses"
label var ms_prem "Medicaid/SCHIP Premium Expenses"
label var ms_dental "Medicaid/SCHIP Dental Expenses"
label var ms_mcare "Medicaid/SCHIP Medicare Expenses"
label var ms_other "Medicaid/SCHIP Other Expenses"
label var ms_casemgmt "Medicaid/SCHIP Case Management Expenses"
label var ms_mental "Medicaid/SCHIP Mental Health Expenses"
label var ms_outpat "Medicaid/SCHIP Outpatient Expenses"
label var ms_rx "Medicaid/SCHIP Drug Expenses"
label var schip_inpat "SCHIP Inpatient Expenses"
label var schip_prem "SCHIP Premium Expenses"
label var schip_dental "SCHIP Dental Expenses"
label var schip_other "SCHIP Other Expenses"
label var schip_casemgmt "SCHIP Case Management Expenses"
label var schip_mental "SCHIP Mental Health Expenses"
label var schip_outpat "SCHIP Outpatient Expenses"
label var schip_rx "SCHIP Drug Expenses"

d

assert hmo==0 if mco!=0
assert mco==0 if hmo!=0
replace hmo=mco if year>=1998
count if mco!=0
drop mco

save stg1mc_exp, replace
