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
use "/Users/jadederong/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/1-Main-survey/3_Endline/01. WASHB_Midline_Endline_data_count_cleaned.dta", clear

* merge in tr assignment
merge 1:1 dataid using "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/1-Main-survey/1_Baseline/0. WASHB_Blinded_tr_assignment.dta"
drop _m

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

* primary analysis hhs ------------


keep dataid tr nolb withdrew moved cdeath dropout reason absent

outsheet using "~/Dropbox/WASH Benefits/Bangladesh/STH/Data/endline_withdraw.csv", replace comma


*--------------------------------------------
* Enrolled T1, O1, C1
*--------------------------------------------
import excel using "~/Dropbox/WASHB-PSTH/Field tracking/1_Day 1 data/Untouched/WASHB-PSTH-Day1-Field-tracking.xlsx", sheet("Day1") clear
drop if _n==1 | _n==2 | _n==3

ren A dataid
ren B clusterid
ren C motherid
ren D arm
ren E EEtarget
ren F status
ren G EEenrolled
ren H FRAname
ren I day1_v1_day
ren J day1_v1_month
ren K day1_v1_year
ren L day1_v2_day
ren M day1_v2_month
ren N day1_v2_year
ren O day1_v3_day
ren P day1_v3_month
ren Q day1_v3_year
ren R need_water
ren S need_handrinse
ren T need_food
ren U need_toyball
ren V need_flytape
ren W need_soil
ren X done_water
ren Y done_waterblank
ren Z done_childhandrinse
ren AA done_handblank
ren AB done_foodcollected
ren AC done_foodduplicate
ren AD done_toyball
ren AE done_flytape
ren AF done_soil
ren AG listT1
ren AH listT2
ren AI listC1
ren AJ enrollT1
ren AK enrollT2
ren AL enrollC1
ren AM enrollO1
ren AN enrollA1
ren AO nameT1
ren AP sexT1
ren AQ nameT2
ren AR sexT2
ren AS nameC1
ren AT sexC1
ren AU nameO1
ren AV sexO1
ren AW nameA1
ren AX sexA1
ren AY dw_compound
ren AZ dw_preg
ren BA dw_1_2y
ren BB dw_under12m

* reformat long string variables
foreach var of varlist dataid EEenrolled enroll* list* status need_* sex* done_* dw_* {
	format %8s `var'
}

keep dataid status enroll*

* merge in tr assignment
merge 1:1 dataid using "~/Dropbox/WASHB-Bangladesh-Data/0-Untouched-data/1-Main-survey/1_Baseline/0. WASHB_Blinded_tr_assignment.dta"
drop if _m==1
drop _m

* convert from rows for compounds to rows for individuals
ren enrollT1 enroll1
ren enrollT2 enroll2
ren enrollC1 enroll3
ren enrollO1 enroll4
ren enrollA1 enroll5 

reshape long enroll, i(dataid tr status) j(person)
format %8s enroll

gen str personid="T1" if person==1
replace personid="T2" if person==2
replace personid="C1" if person==3
replace personid="O1" if person==4
replace personid="A1" if person==5

drop person

outsheet using "~/Dropbox/WASH Benefits/Bangladesh/STH/Data/endline_enroll_sth.csv", replace comma


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


outsheet using "~/Dropbox/WASH Benefits/Bangladesh/STH/Data/endline_kk.csv", replace comma

log close
