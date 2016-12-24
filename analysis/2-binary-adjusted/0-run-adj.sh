
#!/bin/bash

cd
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj.RData"
rm -f "~/Dropbox/WASHB Parasites/Results/Jade/sth_pr_mh_adj.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_ipcw.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_mh_ipcw.RData"

cd "Documents/CRG/wash-benefits/bangladesh/src/sth/analysis/2-binary-adjusted/"

rm -f 2a-PR-prev-adj.Rout
rm -f 2b-PR-prev-mh-adj.Rout 
rm -f 3a-PR-prev-ipcw.Rout
rm -f 3b-PR-prev-mh-ipcw.Rout

R CMD BATCH 2a-PR-prev-adj.R 
R CMD BATCH 2b-PR-prev-mh-adj.R 
R CMD BATCH 3a-PR-prev-ipcw.R
R CMD BATCH 3b-PR-prev-mh-ipcw.R 