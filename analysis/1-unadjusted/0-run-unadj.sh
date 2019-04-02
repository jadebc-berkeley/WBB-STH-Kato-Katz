
#!/bin/bash

rm -f 1a-prev.Rout
rm -f 1b-PR-prev-unadj.Rout
rm -f 1c-PR-prev-mh-unadj.Rout
rm -f 1d-PR-epg-unadj.Rout
rm -f 1e-PR-prev-unadj-tmle.Rout
rm -f 1f-PR-prev-mh-unadj-tmle.Rout

R CMD BATCH 1a-prev.R 
R CMD BATCH 1b-PR-prev-unadj.R 
R CMD BATCH 1c-PR-prev-mh-unadj.R
R CMD BATCH 1d-PR-epg-unadj.R
R CMD BATCH 1e-PR-prev-unadj-tmle.R
R CMD BATCH 1f-PR-prev-mh-unadj-tmle.R
