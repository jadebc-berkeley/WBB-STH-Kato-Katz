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
*cd "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/2-STH-kato-katz/"


*--------------------------------------------
* Read in KK data 
*--------------------------------------------
use "/Users/jadederong/Dropbox/WASHB Parasites/Temp Data/2-WASHB-P-kk-temp.dta"

* drop spillover children
drop if personid=="S1"

* one child had a missing for one slide and 0 for second slide
* reassigning the missing to zero
replace originalAL="0" if dataid=="30607" & personid=="O1"

*--------------------------------------------
* Take average of KK egg counts, create binary variables
*--------------------------------------------
destring original*, replace

* convert long to wide for duplicate slides
keep dataid labdate personid slide originalAL originalTT originalHW
ren originalAL al
ren originalTT tt
ren originalHW hw

destring slide, replace

reshape wide al hw tt, i(dataid personid) j(slide)

* eggs per gram of stool (EPG) =  sum of the two fecal egg counts
* from duplicate Kato-Katz thick smears times 12

gen alepg = (al1 + al2)*12
gen hwepg = (hw1 + hw2)*12
gen ttepg = (tt1 + tt2)*12

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
* Child age and sex at endline
* ---------------------------------------------
use "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/2-STH-kato-katz/WASHB-PSTH-Day1survey_STHCohort.dta", clear
ren col1 persondesc
ren col3 sex
ren col4 dob
ren col5 birthorder
ren col6 dobsource
ren col7_1 pcohort
ren col7_2 sthcohort
ren col8 personid
ren col9 previousid

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
ren dobnew dob

* clean entry date
gen day=substr(EntryDate,9,2) 
gen month = substr(EntryDate,6,2) 
gen year = substr(EntryDate,1,4) 

destring month day year, replace

gen date=mdy(month,day,year)
format date %d
drop EntryDate

* clean sex variable
label define sexl 1 "Male" 0 "Female"
label values sex sexl 
codebook sex
list dataid if sex==.

* create age at each time point
gen double aged=(date-dob)
gen double agem=(date-dob)/30.4167
gen double agey=(date-dob)/365.25

* dropping age vars that came with dataset - not sure how calculated
drop agemonth age

keep dataid personid sex dob date age*  
order dataid personid
tempfile agesex
save `agesex'

use `sth', clear
merge 1:1 dataid personid using `agesex'

* drop if no KK data
drop if _m==2
drop _m

*--------------------------------------------
* Merge with baseline covariates
*--------------------------------------------
merge m:1 dataid using "~/Dropbox/WASHB-Bangladesh-Data/1-primary-outcome-datasets/washb-bangladesh-enrol.dta"

* drop if no KK data
drop if _m==2
drop _m

*--------------------------------------------
* Merge in treatment assignment
*--------------------------------------------
merge m:1 clusterid using "~/Dropbox/WASHB-Bangladesh-Data/1-primary-outcome-datasets/washb-bangladesh-blind-tr.dta"
* drop if no KK data

drop if _m==2
drop _m

order dataid personid block clusterid hhid tr sex dob age* 

save "~/Dropbox/WASHB Parasites/Analysis datasets/Jade/sth.dta", replace
outsheet using "~/Dropbox/WASHB Parasites/Analysis datasets/Jade/sth.csv", replace comma

log close
