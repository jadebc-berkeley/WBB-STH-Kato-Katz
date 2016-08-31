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

*--------------------------------------------
* Take average of KK egg counts, create binary variables
*--------------------------------------------
destring original*, replace

collapse (mean) originalAL originalTT originalHW, by(dataid labdate personid)

ren originalAL alepg
ren originalTT ttepg
ren originalHW hwepg

gen al = (alepg>0)
gen tt = (ttepg>0)
gen hw = (hwepg>0)

gen alint=0
replace alint=1 if alepg>=1 & alepg<5000
replace alint=2 if alepg>=5000 & alepg<50000
replace alint=3 if alepg>=50000
replace alint=. if alepg==.

gen ttint=0
replace ttint=1 if ttepg>=1 & ttepg<5000
replace ttint=2 if ttepg>=5000 & ttepg<50000
replace ttint=3 if ttepg>=50000
replace ttint=. if ttepg==.

gen hwint=0
replace hwint=1 if hwepg>=1 & hwepg<5000
replace hwint=2 if hwepg>=5000 & hwepg<50000
replace hwint=3 if hwepg>=50000
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

label define mhl 0 "No/low intensity infection" 1 "Moderate/heavy infection"
label values almh ttmh hwmh mhl

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

*--------------------------------------------
* Merge with baseline covariates
*--------------------------------------------
* CHANGE THIS directory later
merge m:1 dataid using "~/Dropbox/WASHB Parasites/Temp Data/primary/washb-bangladesh-hbgdki-enrol.dta"

* drop if no KK data
drop if _m==2
drop _m

*--------------------------------------------
* Merge in treatment assignment
*--------------------------------------------
merge m:1 dataid using "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/1-Main-survey/1_Baseline/0. WASHB_Blinded_tr_assignment.dta"
* drop if no KK data
drop if _m==2
drop _m

* create variable for any sth infection
gen sth=.
replace sth=1 if al==1 | hw==1 | tt==1
replace sth=0 if al==0 & hw==0 & tt==0

order dataid personid tr

save "~/Dropbox/WASH Benefits/Bangladesh/STH/Data/sth.dta", replace
outsheet using "~/Dropbox/WASH Benefits/Bangladesh/STH/Data/sth.csv", replace comma

log close
