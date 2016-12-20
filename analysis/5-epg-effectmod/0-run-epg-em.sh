
#!/bin/bash

cd
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_index.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_psac.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_Ncomp.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_Nchild.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_defday.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_latrine.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_scoop.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_noopendef.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_wealth.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_dirtfloor_hh.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_dirtfloor_lat.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_dw.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_geophagia.RData"
rm -f "~/Box Sync/WASHB Parasites/Results/Jade/sth_pr_epg_adj_shoes.RData"

cd "Documents/CRG/wash-benefits/bangladesh/src/sth/analysis/5-epg-effectmod/"

rm -f 5a-PR-prev-adj-index.Rout
rm -f 5b-PR-prev-adj-psac.Rout
rm -f 5c-PR-prev-adj-Ncomp.Rout 
rm -f 5d-PR-prev-adj-Nchild.Rout
rm -f 5e-PR-prev-adj-deftoday.Rout
rm -f 5f-PR-prev-adj-latrine.Rout
rm -f 5g-PR-prev-adj-scoop.Rout
rm -f 5h-PR-prev-adj-noopendef.Rout
rm -f 5i-PR-prev-adj-wealth.Rout
rm -f 5j-PR-prev-adj-hmud.Rout
rm -f 5k-PR-prev-adj-lmud.Rout
rm -f 5l-PR-prev-adj-dw.Rout
rm -f 5m-PR-prev-adj-geophagia.Rout
rm -f 5n-PR-prev-adj-shoes.Rout

R CMD BATCH 5a-PR-epg-adj-index.R
R CMD BATCH 5b-PR-epg-adj-psac.R
R CMD BATCH 5c-PR-epg-adj-Ncomp.R 
R CMD BATCH 5d-PR-epg-adj-Nchild.R
R CMD BATCH 5e-PR-epg-adj-deftoday.R
R CMD BATCH 5f-PR-epg-adj-latrine.R
R CMD BATCH 5g-PR-epg-adj-scoop.R
R CMD BATCH 5h-PR-epg-adj-noopendef.R
R CMD BATCH 5i-PR-epg-adj-wealth.R
R CMD BATCH 5j-PR-epg-adj-hmud.R
R CMD BATCH 5k-PR-epg-adj-lmud.R
R CMD BATCH 5l-PR-epg-adj-dw.R
R CMD BATCH 5m-PR-epg-adj-geophagia.R
R CMD BATCH 5n-PR-epg-adj-shoes.R