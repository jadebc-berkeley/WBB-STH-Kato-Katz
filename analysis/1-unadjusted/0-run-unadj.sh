
#!/bin/bash

cd
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_prev.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_unadj.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_mh_pr_unadj.RData"

cd "Documents/CRG/wash-benefits/bangladesh/src/sth/analysis/1-unadjusted/"

rm -f 1a-prev.Rout
rm -f 1b-PR-prev-unadj.Rout
rm -f 1c-PR-prev-mh-unadj.Rout
rm -f 1d-PR-epg-unadj.Rout

R CMD BATCH 1a-prev.R 
R CMD BATCH 1b-PR-prev-unadj.R 
R CMD BATCH 1c-PR-prev-mh-unadj.R
R CMD BATCH 1d-PR-epg-unadj.R