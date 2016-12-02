
#!/bin/bash

cd
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj.RData"
rm -f "~/Dropbox/WASHB Parasites/Results/Jade/sth_pr_mh_adj.RData"

cd "Documents/CRG/wash-benefits/bangladesh/src/sth/analysis"

rm -f 2a-PR-prev-adj.Rout
rm -f 2a-PR-prev-mh-adj.Rout 

R CMD BATCH 2a-PR-prev-adj.R 
R CMD BATCH 2a-PR-prev-mh-adj.R 
