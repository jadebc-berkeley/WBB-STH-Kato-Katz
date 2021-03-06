
#!/bin/bash

rm -f 4-0-mean-index.Rout
rm -f 4-0-prev-index.Rout 
rm -f 4au-PR-prev-unadj-index.Rout
rm -f 4bu-PR-prev-unadj-psac.Rout
rm -f 4cu-PR-prev-unadj-Ncomp.Rout 
rm -f 4du-PR-prev-unadj-Nchild.Rout
rm -f 4eu-PR-prev-unadj-deftoday.Rout
rm -f 4fu-PR-prev-unadj-latrine.Rout
rm -f 4gu-PR-prev-unadj-scoop.Rout
rm -f 4hu-PR-prev-unadj-noopendef.Rout
rm -f 4iu-PR-prev-unadj-wealth.Rout
rm -f 4ju-PR-prev-unadj-hmud.Rout
rm -f 4ku-PR-prev-unadj-lmud.Rout
rm -f 4lu-PR-prev-unadj-dw.Rout
rm -f 4mu-PR-prev-unadj-geophagia.Rout
rm -f 4nu-PR-prev-unadj-shoes.Rout

R CMD BATCH 4-0-mean-index.R
R CMD BATCH 4-0-prev-index.R 
R CMD BATCH 4au-PR-prev-unadj-index.R
R CMD BATCH 4bu-PR-prev-unadj-psac.R
R CMD BATCH 4cu-PR-prev-unadj-Ncomp.R 
R CMD BATCH 4du-PR-prev-unadj-Nchild.R
R CMD BATCH 4eu-PR-prev-unadj-deftoday.R
R CMD BATCH 4fu-PR-prev-unadj-latrine.R
R CMD BATCH 4gu-PR-prev-unadj-scoop.R
R CMD BATCH 4hu-PR-prev-unadj-noopendef.R
R CMD BATCH 4iu-PR-prev-unadj-wealth.R
R CMD BATCH 4ju-PR-prev-unadj-hmud.R
R CMD BATCH 4ku-PR-prev-unadj-lmud.R
R CMD BATCH 4lu-PR-prev-unadj-dw.R
R CMD BATCH 4mu-PR-prev-unadj-geophagia.R
R CMD BATCH 4nu-PR-prev-unadj-shoes.R