capture log close
log using "~/documents/crg/wash-benefits/bangladesh/src/sth/dm/1_make_consort.log", replace

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
* Follow-up at year 2 
* (compounds lost, moved, absent, withdrew, no LB, child death)
*--------------------------------------------
use "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/2-STH-kato-katz/5-WASHB-P-sckk-fieldtrack.dta", clear

keep dataid hhstatus 

duplicates drop 

* merge in tr assignment
merge m:1 dataid using "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/1-Main-survey/1_Baseline/0. WASHB_Blinded_tr_assignment.dta"
drop if _m==1
drop _m

* get info on hh's with "L" enroll code
* from main trial source
preserve
use "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/1-Main-survey/3_Endline/01. WASHB_Midline_Endline_data_count_cleaned.dta", clear

* dropped out at midline or endline
gen dropout=0
replace dropout=1 if status_midline==2 | status_endline==2 | status_endline==.
replace dropout=0 if status_endline==1

* manually cleaning while we wait for Kishor
replace status_midline=1 if dataid=="02604" 
replace status_midline=1 if dataid=="03704"
replace status_endline=0 if dataid=="02604" 
replace status_endline=0 if dataid=="03704"

replace reason_midline="" if dataid=="02604"
replace reason_midline="" if dataid=="03704"
replace reason_endline="ABSENT" if dataid=="02604"
replace reason_endline="ABSENT" if dataid=="03704"

* consolidate reasons for drop out and midline and endline
gen str reason = ""
replace reason = reason_midline if dropout==1 & (reason_endline=="" | status_endline==.)
replace reason = reason_endline if dropout==1 & (reason_endline!="" & status_endline!=.)

* loss to FU --------------

* no live birth
gen nolb=0
#delimit;
replace nolb=1 if reason=="ABORTION" |
	reason=="FALSE PREGNANCY" | reason=="MISCARRIAGE" |
	reason=="STILL BIRTH";
#delimit cr
replace nolb=. if reason==""
replace nolb=0 if dropout==0

* withdrew
gen withdrew=0
replace withdrew=1 if reason=="REFUSE" 
replace withdrew=. if reason==""
replace withdrew=0 if dropout==0

* moved
gen moved=0
replace moved=1 if reason=="MIGRATION OUT"
replace moved=. if reason==""
replace moved=0 if dropout==0

* moved
gen absent=0
replace absent=1 if reason=="ABSENT"
replace absent=. if reason==""
replace absent=0 if dropout==0

* deaths
gen cdeath=0 
replace cdeath=1 if reason=="CHILD DEATH"
replace cdeath=. if reason==""
replace cdeath=0 if dropout==0

keep dataid nolb withdrew moved absent cdeath

tempfile svydropout
save `svydropout'

restore

merge 1:1 dataid using `svydropout'
drop _m

gen sthmoved=0
replace sthmoved=1 if hhstatus=="M" | (moved==1 & hhstatus=="L")

gen sthabsent=0
replace sthabsent=1 if hhstatus=="A" | (absent==1 & hhstatus=="L")

gen sthnolb=0
replace sthnolb=1 if nolb==1

gen sthwd=0
replace sthwd=1 if hhstatus=="R" | (hhstatus=="L" & withdrew==1)

gen sthcd=0
replace sthcd=1 if hhstatus=="D" | cdeath==1

drop nolb withdrew moved absent cdeath

outsheet using "~/Dropbox/WASHB Parasites/Analysis datasets/Jade/endline_withdraw.csv", replace comma


*--------------------------------------------
* Enrolled T1, O1, C1
*--------------------------------------------
use "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/2-STH-kato-katz/5-WASHB-P-sckk-fieldtrack.dta", clear

keep dataid personid hhstatus enroll*

duplicates drop 

* merge in tr assignment
merge m:1 dataid using "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/1-Main-survey/1_Baseline/0. WASHB_Blinded_tr_assignment.dta"
drop if _m==1
drop _m

outsheet using "~/Dropbox/WASHB Parasites/Analysis datasets/Jade/endline_enroll_sth.csv", replace comma


*--------------------------------------------
* Has sample T1, O1, C1
*--------------------------------------------
use "~/Dropbox/WASHB Parasites/Temp Data/2-WASHB-P-kk-temp.dta", clear

* drop spillover children
drop if personid=="S1"

destring original*, replace

drop if originalAL==. & originalTT==. & originalHW==.
collapse (mean) originalAL originalTT originalHW, by(dataid labdate personid)

* merge in tr assignment
merge m:1 dataid using "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/1-Main-survey/1_Baseline/0. WASHB_Blinded_tr_assignment.dta"
drop if _m==2
drop _m


outsheet using "~/Dropbox/WASHB Parasites/Analysis datasets/Jade/endline_kk.csv", replace comma

log close
