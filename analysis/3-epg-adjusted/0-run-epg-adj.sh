
#!/bin/bash

cd
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_ipcw.RData"

cd "Documents/CRG/wash-benefits/bangladesh/src/sth/analysis/2-binary-adjusted/"

rm -f 2c-PR-epg-adj.Rout
rm -f 3c-PR-epg-ipcw.Rout 

R CMD BATCH 2c-PR-epg-adj.R 
R CMD BATCH 3c-PR-epg-ipcw.R 
