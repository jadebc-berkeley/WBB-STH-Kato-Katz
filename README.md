# WBB-STH-Kato-Katz
### WASH Benefits Bangladesh STH Kato-Katz analysis

This is a repository for the WASH Benefits Bangladesh soil-transmitted helminths Kato-Katz analysis trial [NCT01590095](https://clinicaltrials.gov/ct2/show/NCT01590095). This analysis was independently replicated by Jade Benjamin-Chung (jadebc@berkeley.edu) and Ayse Ercumen (aercume@ncsu.edu). 

### Associated protocols and datasets

The pre-specified analysis protocol and all data required to run the analyses will be made available in concert with the publication of the article through the Open Science Framework: [https://osf.io/v2c8p/](https://osf.io/v2c8p/). Once data is downloaded from OSF, the data directory for the user must be changed in `0-config.R`.

The scripts in the dm directory process raw data and create the analysis datasets that are shared publicly through OSF. The raw, unprocessed, data are not publicly available at this time, but will be within approximately 2 years time (e.g., by 2019) to allow for the further development of meta-data documentation and an access platform. We will strive to update this page to link to those files when they are available.

### WASH Benefits Package

This analysis used an R package developed for the WASH Benefits study intention-to-treat analyses called [washb](https://github.com/ben-arnold/washb). 

For all analysis scripts, you will need to change directory statements within them to point them to the files on your local directory. Similar directory statement changes will be needed wherever output files are saved down (e.g., raw estimates, figures).

### Directory structure

**`analysis`** : analysis scripts

* **`1-unadjusted`**: unadjusted analyses for STH prevalence and intensity
* **`2-binary-adjusted`**: adjusted analyses for STH prevalence 
* **`3-epg-adjusted`**: adjusted analyses for STH intensity
* **`4-binary-effectmod`**: effect modification analyses for STH prevalence 
* **`5-epg-effectmod`**: effect modification analyses for STH intensity

**`dm`** : data management scripts

**`figures`** : scripts to make figures

**`tables`** : scripts to make tables

