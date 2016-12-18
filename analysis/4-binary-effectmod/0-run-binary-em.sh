
#!/bin/bash

cd
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_mean_em.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_prev_em.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_index.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_psac.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_Ncomp.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_Nchild.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_defday.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_latrine.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_scoop.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_noopendef.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_wealth.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_dirtfloor_hh.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_dirtfloor_lat.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_dw.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_geophagia.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_adj_shoes.RData"

cd "Documents/CRG/wash-benefits/bangladesh/src/sth/analysis/4-binary-effectmod/"

rm -f 4-0-mean-index.Rout
rm -f 4-0-prev-index.Rout 
rm -f 4a-PR-prev-adj-index.Rout
rm -f 4b-PR-prev-adj-psac.Rout
rm -f 4c-PR-prev-adj-Ncomp.Rout 
rm -f 4d-PR-prev-adj-Nchild.Rout
rm -f 4e-PR-prev-adj-deftoday.Rout
rm -f 4f-PR-prev-adj-latrine.Rout
rm -f 4g-PR-prev-adj-scoop.Rout
rm -f 4h-PR-prev-adj-noopendef.Rout
rm -f 4i-PR-prev-adj-wealth.Rout
rm -f 4j-PR-prev-adj-hmud.Rout
rm -f 4k-PR-prev-adj-lmud.Rout
rm -f 4l-PR-prev-adj-dw.Rout
rm -f 4m-PR-prev-adj-geophagia.Rout
rm -f 4n-PR-prev-adj-shoes.Rout

R CMD BATCH 4-0-mean-index.R
R CMD BATCH 4-0-prev-index.R 
R CMD BATCH 4a-PR-prev-adj-index.R
R CMD BATCH 4b-PR-prev-adj-psac.R
R CMD BATCH 4c-PR-prev-adj-Ncomp.R 
R CMD BATCH 4d-PR-prev-adj-Nchild.R
R CMD BATCH 4e-PR-prev-adj-deftoday.R
R CMD BATCH 4f-PR-prev-adj-latrine.R
R CMD BATCH 4g-PR-prev-adj-scoop.R
R CMD BATCH 4h-PR-prev-adj-noopendef.R
R CMD BATCH 4i-PR-prev-adj-wealth.R
R CMD BATCH 4j-PR-prev-adj-hmud.R
R CMD BATCH 4k-PR-prev-adj-lmud.R
R CMD BATCH 4l-PR-prev-adj-dw.R
R CMD BATCH 4m-PR-prev-adj-geophagia.R
R CMD BATCH 4n-PR-prev-adj-shoes.R