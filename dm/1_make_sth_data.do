capture log close
log using "~/documents/crg/wash-benefits/bangladesh/src/sth/dm/1_make_sth_data.log", replace

*********************************************
* WASH Benefits Bangladesh
* STH Kato-Katz analysis

* Make analysis dataset

* by Jade
*********************************************
clear all
set more off


*--------------------------------------------
* Read in KK data 
*--------------------------------------------
use "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/2-STH-kato-katz/4-WASHB-P-sckk.dta", clear

* drop spillover children
drop if personid=="S1"
* drop adults
drop if personid=="A1"

* one child had a missing for one slide and 0 for second slide
* reassigning the missing to zero
replace originalAL="0" if dataid=="30607" & personid=="O1"

*--------------------------------------------
* Take average of KK egg counts, create binary variables
*--------------------------------------------
destring original*, replace

* convert long to wide for duplicate slides
keep dataid labdate personid slide originalAL originalTT originalHW counter defday
ren originalAL al
ren originalTT tt
ren originalHW hw

destring slide, replace

drop if slide==.

reshape wide al hw tt counter, i(dataid personid) j(slide)

* counter 
replace counter1="HKC" if counter1=="HKC "

gen counter = .
replace counter= 1 if (counter1 =="HKC" & counter2=="RK") | (counter1=="RK" & counter2=="HKC") 
replace counter= 2 if (counter1 =="HKC" & counter2=="MHR") | (counter1=="MHR" & counter2=="HKC") 
replace counter= 3 if (counter1 =="HKC" & counter2=="SNJ") | (counter1=="SNJ" & counter2=="HKC") 
replace counter= 4 if (counter1 =="MHR" & counter2=="RK") | (counter1=="RK" & counter2=="MHR") 
replace counter= 5 if (counter1 =="MHR" & counter2=="SNJ") | (counter1=="SNJ" & counter2=="MHR") 
replace counter= 6 if (counter1 =="RK" & counter2=="SNJ") | (counter1=="SNJ" & counter2=="RK") 
replace counter= 6 if (counter1 =="RK" & counter2=="RK") | (counter1=="RK" & counter2=="RK") 

* eggs per gram of stool (EPG) =  sum of the two fecal egg counts
* from duplicate Kato-Katz thick smears times 12

gen alepg = (al1 + al2)*12
gen hwepg = (hw1 + hw2)*12
gen ttepg = (tt1 + tt2)*12

gen logalepg = log(alepg)
gen loghwepg = log(hwepg)
gen logttepg = log(ttepg)

replace logalepg= log(alepg+1) if alepg==0
replace loghwepg= log(hwepg+1) if hwepg==0
replace logttepg= log(ttepg+1) if ttepg==0

gen al = (al1>0 | al2>0)
gen tt = (tt1>0 | tt2>0)
gen hw = (hw1>0 | hw2>0)

* create variable for any sth infection
gen sth=.
replace sth=1 if al==1 | hw==1 | tt==1
replace sth=0 if al==0 & hw==0 & tt==0

gen alint=0
replace alint=1 if alepg>=1 & alepg<5000
replace alint=2 if alepg>=5000 & alepg<50000
replace alint=3 if alepg>=50000
replace alint=. if alepg==.

gen ttint=0
replace ttint=1 if ttepg>=1 & ttepg<1000
replace ttint=2 if ttepg>=1000 & ttepg<10000
replace ttint=3 if ttepg>=10000
replace ttint=. if ttepg==.

gen hwint=0
replace hwint=1 if hwepg>=1 & hwepg<2000
replace hwint=2 if hwepg>=2000 & hwepg<4000
replace hwint=3 if hwepg>=4000
replace hwint=. if hwepg==.

label define intl 0 "No infection" 1 "Low intensity" 2 "Moderate intensity" 3 "High intensity"
label values alint ttint hwint intl

gen almh=0
replace almh=1 if alint==2 | alint==3
replace almh=. if alint==. 

gen ttmh=0
replace ttmh=1 if ttint==2 | ttint==3
replace ttmh=. if ttint==. 

gen hwmh=0
replace hwmh=1 if hwint==2 | hwint==3
replace hwmh=. if hwint==. 

gen sthmh=0
replace sthmh=1 if alint==2 | alint==3 | hwint==2 | hwint==3 | ttint==2 | ttint==3
replace sthmh=. if alint==. & ttint==. & hwint==.

label variable alepg "Ascaris eggs per gram"
label variable ttepg "Trichuris eggs per gram"
label variable hwepg "Hookworm eggs per gram"

label variable al "Any Ascaris eggs"
label variable tt "Any Trichuris eggs"
label variable hw "Any hookworm eggs"

label variable alint "Ascaris infection intensity"
label variable ttint "Any infection intensity"
label variable hwint "Any infection intensity"

label variable almh "Moderate/heavy Ascaris infection"
label variable ttmh "Moderate/heavy Trichuris infection"
label variable hwmh "Moderate/heavy hookworm infection"
label variable sthmh "Any moderate/heavy infection"

drop al1 al2 tt1 tt2 hw1 hw2

tempfile sth
save `sth'

* ---------------------------------------------
* Child age and sex at endline and birth order

* If data available from primary analysis dataset, 
* use that. If not, use PSTH dataset. 
* ---------------------------------------------
use "~/Dropbox/WASHB-Bangladesh-Data/1-primary-outcome-datasets/washb-bangladesh-diar.dta", clear
keep dataid childid clusterid sex dob 
ren childid personid
duplicates drop 
tempfile primdob
save `primdob'

* merge in birth order for T1 from anthro dataset
preserve
use "~/Dropbox/WASHB-Bangladesh-Data/1-primary-outcome-datasets/washb-bangladesh-anthro.dta", clear
keep dataid childid clusterid birthord
ren childid personid
duplicates drop
tempfile primbirthord
save `primbirthord'
restore

merge 1:1 dataid personid using `primbirthord'
drop _m

preserve
use "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/2-STH-kato-katz/WASHB-PSTH-Day1survey_STHCohort.dta", clear
ren col1 persondesc
ren col3 sex_psth
ren col4 dob
ren col5 birthorder_psth
ren col6 dobsource_psth
ren col7_1 pcohort
ren col7_2 sthcohort
ren col8 personid
ren col9 previousCid

replace personid = previousCid if personid=="C1"

* clean dob
gen day=substr(dob,1,2) if strlen(dob)==10
replace day=substr(dob,1,1) if strlen(dob)==9
gen month = substr(dob,4,2) if strlen(dob)==10
replace month = substr(dob,3,2) if strlen(dob)==9
gen year = substr(dob,7,4) if strlen(dob)==10
replace year = substr(dob,6,4) if strlen(dob)==9

replace month="1" if month=="/1"
replace month="2" if month=="/2"
replace month="3" if month=="/3"
replace month="4" if month=="/4"
replace month="5" if month=="/5"
replace month="6" if month=="/6"
replace month="7" if month=="/7"
replace month="8" if month=="/8"
replace month="9" if month=="/9"

destring month day year, replace

gen dobnew=mdy(month,day,year)
format dobnew %d
drop day month year dob
ren dobnew dob_psth

/* clean entry date
gen day=substr(EntryDate,9,2) 
gen month = substr(EntryDate,6,2) 
gen year = substr(EntryDate,1,4) 

destring month day year, replace

gen date=mdy(month,day,year)
format date %d
drop EntryDate*/

* clean sex variable
label define sexl 1 "Male" 0 "Female"
label values sex sexl 
codebook sex
list dataid if sex==.

drop if personid=="A1"

keep dataid personid sex dob_psth birthorder_psth previousCid
order dataid personid
tempfile agesex
save `agesex'
restore

merge 1:1 dataid personid using `agesex'
drop _m

* drop other "C" children
gen keep = (personid=="T1" | personid=="T2" | personid=="O1")
replace keep = 1 if previousCid!=""
keep if keep==1
replace personid="C1" if personid=="C1" | personid=="C2" | personid=="C3" | personid=="C4" 

tempfile agesex2
save `agesex2'

* merge primary analysis dob with sth dataset to identify
* who is missing dob and needs dob to be merged from psth
use `sth', clear
merge 1:1 dataid personid using `agesex2'

* drop if no KK data
drop if _m==2
drop if labdate==. & alepg==. & hwepg==. & ttepg==. 
drop _m

* use psth dob if primary dob not available for T1 T2
replace dob = dob_psth if dob==. & (personid=="T1" | personid=="T2" | personid=="C1")
replace dob = dob_psth if  personid=="O1" 
drop dob_psth

* use psth sex if primary sex not available for T1 T2
replace sex = sex_psth if sex==. & (personid=="T1" | personid=="T2" | personid=="C1")
replace sex = sex_psth if personid=="O1" 
drop sex_psth 

* use psth birth order if primary dob not available for T1 T2
replace birthord = birthorder_psth if birthord==. & (personid=="T1" | personid=="T2" | personid=="C1")
replace birthord = birthorder_psth if  personid=="O1" 
drop birthorder_psth


* create age at each time point
gen double aged=(labdate-dob)
gen double agem=(labdate-dob)/30.4167
gen double agey=(labdate-dob)/365.25

* fill in missing clusterid
replace clusterid = substr(dataid,1,3) if clusterid==""

tempfile sth
save `sth'

*--------------------------------------------
* Impute households lost to FU that had live births
* after baseline
*--------------------------------------------
use "~/Dropbox/WASHB-Bangladesh-Data/1-primary-outcome-datasets/washb-bangladesh-track-compound.dta", clear
gen lb = 1
replace lb = 0 if miss1r==1
keep dataid lb

* impute T1 and T2 from baseline list of twins
preserve 
import excel using "~/Dropbox/WASHB-PSTH/Sampling Frame/baseline survey info/Twin Child list.xlsx", clear firstrow
gen newdataid=string(dataid,"%05.0f")
drop dataid
ren newdataid dataid
keep dataid childid
ren childid personid
tempfile t1t2
save `t1t2'
restore

merge 1:m dataid using `t1t2'
drop _m

* impute T1 for all others
replace personid="T1" if personid==""

* impute C1 from baseline list
preserve
use "~/Dropbox/WASHB-PSTH/Sampling Frame/IT_c1info.dta", clear
gen hasc1=2
keep dataid hasc1
tempfile c1
save `c1'
restore

merge m:1 dataid using `c1'

expand hasc1 if personid=="T1", gen(newrow)
sort dataid
replace personid="C1" if newrow==1 & hasc1==2 

drop newrow hasc1 _m

* drop the two ids that are in C1 but not in the main study tracking file
drop if personid==""

tempfile lb
save `lb'

*--------------------------------------------
* Impute individuals lost to FU at PSTH round
*--------------------------------------------
use "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/2-STH-kato-katz/5-WASHB-P-sckk-fieldtrack.dta", clear
keep dataid personid stoolever hhstatus
* drop duplicated slides
duplicates drop

* for the rows with hhstatus!=E, there is one row per possible personid
* need to delete the irrelevant ones
gen keeppersonid=0
replace keeppersonid=1 if hhstatus=="E"
replace keeppersonid=1 if personid=="T1"
replace keeppersonid=1 if personid=="O1"

* merge in which dataid has c1 from baseline list
merge m:1 dataid using `c1'

replace keeppersonid=1 if hasc1==2 & personid=="C1" 
order dataid personid keepp
drop hasc1 _m

* merge in which dataid has t1t2 from baseline list
count
merge m:1 dataid personid using `t1t2'
count

replace keeppersonid=1 if personid=="T2" & _m==3 & hhstatus!="E"
replace keeppersonid=1 if personid=="T2" & _m==2

drop _m

* drop extra personid rows
keep if keeppersonid==1

* only keep rows for O1 with HHstatus=="E"
drop if hhstatus!="E"  & personid=="O1"

duplicates drop
tempfile fustool
save `fustool'

use `lb', clear
merge 1:1 dataid personid using `fustool'

* drop the compounds that were not enrolled at baseline
* and are coded as L in the PSTH file
drop if hhstatus =="L" & _m < 3

gen fu_reason = 1
replace fu_reason = 2 if hhstatus!="E"  & hhstatus!=""
replace fu_reason = 3 if stoolever==0
replace fu_reason = 9 if hhstatus==""

label define fu_reasonl 1 "In STH sample" 2 "Loss to FU" 3 "No stool" 4 "No KK" 9 "Excluded from PSTH"
label values fu_reason fu_reasonl

drop if lb == 0 

drop _m
*drop lb hhstatus keepp stoolever

tempfile allfu
save `allfu'

use `sth', clear
merge 1:1 dataid personid using `allfu'

replace fu_reason = 4 if labdate==. & fu_reason==1

* drop the kids that we accidentally didn't enroll at endline
drop if fu_reason==9

* generate overall loss to fu variable
gen hasoutcome = (fu_reason==1)

drop _m

drop clusterid
gen clusterid=substr(dataid, 1,3)

order clusterid 

* Impute O1 - first add one for each compound with a T1
egen hast1=count(personid) if personid=="T1" & fu==2, by(dataid)
egen o1=count(personid) if personid=="O1" & fu==1, by(dataid)
egen haso1=max(o1), by(dataid)

replace hast1=2 if hast1==1

expand hast1, gen(flag)
sort dataid personid
replace personid="O1" if flag==1 & hast1==2
drop if personid=="O1" & haso1==1 & flag==1
replace fu = 2 if personid=="O1" & flag==1

* Impute O1 - second, only keep O1s for the proportion of compounds
* not lost to FU at endline that had O1s 
preserve
use "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/2-STH-kato-katz/5-WASHB-P-sckk-fieldtrack.dta", clear
drop if slide=="2"
keep if enroll!=""
tab personid
* proportion =  3,148 /  4,102   =0.77
restore

/* keep the imputed O1s for random subset of 1088 *0.77=835 children
set seed 12345
gen rand = runiform()

sort rand
egen randseq=seq() if personid=="O1" & fu==2

gen keepo1=0
replace keepo1=1 if fu!=2
replace keepo1=1 if randseq<=835
replace keepo1=. if personid!="O1"
drop if keepo1==0 
drop keepo1 rand randseq*/

* import the O1s we need to impute from Ayse's list 
preserve
insheet using "~/Box Sync/WASHB Parasites/Analysis datasets/Ayse/2-washb-bangladesh-imputeo1.csv", clear
gen imputeo1=1
gen newdataid=string(dataid,"%05.0f")
drop dataid
ren newdataid dataid
tempfile imputeo1
save `imputeo1'
restore

merge m:1 dataid using `imputeo1'

drop if personid=="O1" & imputeo1!=1 & fu==2
drop _m

* create indicator for index child
gen index = 0
replace index = 1 if personid=="T1" | personid=="T2" 

drop lb keeppersonid stoolever hhstatus hast1 o1 haso1 flag 


*--------------------------------------------
* Merge with baseline covariates
*--------------------------------------------

preserve
use "~/Dropbox/WASHB-Bangladesh-Data/1-primary-outcome-datasets/washb-bangladesh-enrol.dta", clear

label define roof 0 "No improved roof" 1 "Has improved roof"
label define walls 0 "No improved wall" 1 "Has improved wall" 
label define floor 0 "No improved floor" 1 "Has improved floor" 
label define elec 0 "No electricity" 1 "Has electricity" 
label define asset_wardrobe 0 "No wardrobe" 1 "Has wardrobe" 
label define asset_table 0 "No table" 1 "Has table" 
label define asset_chair 0 "No chair" 1 "Has chair" 
label define asset_khat 0 "No khat" 1 "Has khat" 
label define asset_chouki 0 "No chouki" 1 "Has chouki" 
label define asset_tv 0 "No TV" 1 "Has TV" 
label define asset_refrig 0 "No refrigerator" 1 "Has refrigerator" 
label define asset_bike 0 "No bicycle" 1 "Has bicycle" 
label define asset_moto 0 "No motorcycle" 1 "Has motorcycle" 
label define asset_sewmach 0 "No sewing machine" 1 "Has sewing machine" 
label define asset_mobile 0 "No mobile phone" 1 "Has mobile phone" 
label define asset_clock 0 "No clock" 1 "Has clock" 
label define asset_radio 0 "No radio" 1 "Has radio" 
label define asset_phone 0 "No land phone" 1 "Has land phone" 

label value roof roof 
label value walls walls
label value floor floor
label value elec elec
label value asset_wardrobe asset_wardrobe
label value asset_table asset_table
label value asset_chair asset_chair
label value asset_khat asset_khat
label value asset_chouki asset_chouki
label value asset_tv asset_tv
label value asset_refrig asset_refrig
label value asset_bike asset_bike
label value asset_moto asset_moto
label value asset_sewmach asset_sewmach
label value asset_mobile asset_mobile
label value asset_clock asset_clock
label value asset_radio asset_radio
label value asset_phone asset_phone

* create wealth index
pca roof floor walls elec asset_wardrobe asset_table asset_chair asset_khat asset_chouki asset_radio asset_tv asset_refrig asset_bike asset_moto asset_sewmach asset_mobile asset_phone
predict score1
xtile wealth = score1, nq(2)
drop score1
tempfile base
save `base'
restore

merge m:1 dataid using `base'

gen month=month(labdate)

* drop if no KK data
drop if _m==2
drop _m

* ---------------------------------------------
* Create effect modifier variables 
* ---------------------------------------------
preserve
use "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/2-STH-kato-katz/WASHB-PSTH-Day1survey_HHCensus.dta", clear
* Number of individuals living in the compound (<10 vs. ≥10)
gen ncompover10 = (col7 >=10) 
* Number of children aged 5-14 years living in the compound (0 vs. ≥1)
gen n5to14 = (col3>=1 & col3<.)

keep dataid ncompover10 n5to14
tempfile censvar
save `censvar'
restore

merge m:1 dataid using `censvar'
drop _m

* Percentage of mud floors in household and latrine area (0 vs. ≥1)
preserve
use "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/2-STH-kato-katz/WASHB-PSTH-Day1survey-main.dta", clear

replace qF2=. if qF2==888
replace qF5=. if qF5==888

gen dirtfloor_hh = (qF2>1)
replace dirtfloor_hh =. if qF2==.

gen dirtfloor_lat = (qF5>1)
replace dirtfloor_lat =. if qF5==.

keep dataid dirtfloor*
tempfile dirtfloor
save `dirtfloor'
restore

merge m:1 dataid using `dirtfloor'
drop _m

* Caregiver-reported deworming in last six months
preserve
use "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/2-STH-kato-katz/WASHB-PSTH-Day1survey_DiarrheaDeworming.dta", clear
gen dw=0
replace dw=1 if c301==1
replace dw=. if c301==999 | c301==.

* Caregiver-reported geophagia by child in last week
ren c305_4 geophagia
recode geophagia (999=.)

ren childno personid

keep dataid personid dw geophagia
tempfile dw
save `dw'
restore

merge m:1 dataid personid using `dw'
drop if _m==2
drop _m

* Whether the child is observed to be wearing shoes
preserve
use "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/2-STH-kato-katz/5-WASHB-P-sckk-fieldtrack.dta", clear
keep if slide=="1"
recode shoes (2=0) (99=.)

* Samples from same-day vs. previous-day defecation
recode defday (2=0)

keep dataid personid shoes defday
tempfile shoedef
save `shoedef'
restore

merge m:1 dataid personid using `shoedef'
drop _m

* school-age child or not
gen sac=0
replace sac=1 if agey>=5 & agey<=12
replace sac=. if agey==.

* Compound has latrine with water seal that flushes into pit or septic tank 
preserve
use "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/1-Main-survey/1_Baseline/1. WASHB_Baseline_main_survey.dta", clear
gen lat=0
replace lat=1 if q809_9a==1 & q809_9b<=3
replace lat=. if q809_9a==888
replace lat=. if q809_9a==. & q809_9b==.
replace lat=0 if q809_9==0
replace lat=0 if q808==4

* Compound has dedicated tool to clean up feces and reports using it to dispose of child feces
gen scoop=0
replace scoop=1 if q919==1 & q921_2
replace scoop=0 if q919==2 
replace scoop=. if q921_2==. & q919==1
replace scoop=. if q919==.

* No open defecation reported by any compound member
gen noopendef=0
#delimit;
replace noopendef=1 if (q801_a==3 |q801_a==888) & (q801_b==3|q801_b==888) & 
	(q801_c==3|q801_c==888) & (q801_d==3|q801_d==888) & (q801_e==3|q801_e==888);
#delimit cr
replace noopendef=. if q801_a==999 | q801_b==999 | q801_c==999 | q801_d==999 | q801_e==999

keep lat scoop noopendef dataid 
tempfile latvars
save `latvars'
restore

merge m:1 dataid using `latvars'
drop if _m==2
drop _m

* ---------------------------------------------
* Creating categorical covariates for analyses with sparse data
* ---------------------------------------------
xtile momagebin = momage if hasoutcome==1, nquantiles(2)
xtile momheightbin = momheight if hasoutcome==1, nquantiles(2)
recode momagebin momheightbin (2=1) (1=0)

gen hfiacatbin = .
replace hfiacatbin = 0 if (hfiacat==1) 
replace hfiacatbin = 1 if (hfiacat==2 | hfiacat==3 | hfiacat==4) 

gen Ncompbin = .
replace Ncompbin= 0 if Ncomp<=10 
replace Ncompbin= 1 if Ncomp>=11 & Ncomp<.

gen Nlt18bin = . 
replace Nlt18bin = 0 if Nlt18<=1 
replace Nlt18bin = 1 if Nlt18>=2 & Nlt18<.

gen watminbin=.
replace watminbin = 0 if watmin==0 
replace watminbin = 1 if watmin>=1 & watmin< .  

gen wet=.
replace wet = 0 if month<6 | (month>10 & month<.)
replace wet = 1 if month>=6 & month<=10

*--------------------------------------------
* Merge in treatment assignment
*--------------------------------------------
merge m:1 clusterid using "/Volumes/0-Treatment-assignments/washb-bangladesh-tr.dta"

drop _m

order dataid personid block clusterid tr sex dob age* 

save "~/Dropbox/WASHB Parasites/Analysis datasets/Jade/sth.dta", replace
save "~/Box Sync/WASHB Parasites/Analysis datasets/Jade/sth.dta", replace
outsheet using "~/Dropbox/WASHB Parasites/Analysis datasets/Jade/sth.csv", replace comma
outsheet using "~/Box Sync/WASHB Parasites/Analysis datasets/Jade/sth.csv", replace comma

log close
