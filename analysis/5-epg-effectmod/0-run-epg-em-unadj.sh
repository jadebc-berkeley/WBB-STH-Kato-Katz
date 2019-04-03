
#!/bin/bash

rm -f 5-0-mean-index.Rout
rm -f 5au-PR-prev-unadj-index.Rout
rm -f 5bu-PR-prev-unadj-psac.Rout
rm -f 5cu-PR-prev-unadj-Ncomp.Rout 
rm -f 5du-PR-prev-unadj-Nchild.Rout
rm -f 5eu-PR-prev-unadj-deftoday.Rout
rm -f 5fu-PR-prev-unadj-latrine.Rout
rm -f 5gu-PR-prev-unadj-scoop.Rout
rm -f 5hu-PR-prev-unadj-noopendef.Rout
rm -f 5iu-PR-prev-unadj-wealth.Rout
rm -f 5ju-PR-prev-unadj-hmud.Rout
rm -f 5ku-PR-prev-unadj-lmud.Rout
rm -f 5lu-PR-prev-unadj-dw.Rout
rm -f 5mu-PR-prev-unadj-geophagia.Rout
rm -f 5nu-PR-prev-unadj-shoes.Rout

R CMD BATCH 5-0-mean-index.R
R CMD BATCH 5au-PR-epg-unadj-index.R
R CMD BATCH 5bu-PR-epg-unadj-psac.R
R CMD BATCH 5cu-PR-epg-unadj-Ncomp.R 
R CMD BATCH 5du-PR-epg-unadj-Nchild.R
R CMD BATCH 5eu-PR-epg-unadj-deftoday.R
R CMD BATCH 5fu-PR-epg-unadj-latrine.R
R CMD BATCH 5gu-PR-epg-unadj-scoop.R
R CMD BATCH 5hu-PR-epg-unadj-noopendef.R
R CMD BATCH 5iu-PR-epg-unadj-wealth.R
R CMD BATCH 5ju-PR-epg-unadj-hmud.R
R CMD BATCH 5ku-PR-epg-unadj-lmud.R
R CMD BATCH 5lu-PR-epg-unadj-dw.R
R CMD BATCH 5mu-PR-epg-unadj-geophagia.R
R CMD BATCH 5nu-PR-epg-unadj-shoes.R