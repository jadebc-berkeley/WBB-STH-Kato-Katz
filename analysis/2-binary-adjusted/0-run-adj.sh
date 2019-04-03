
#!/bin/bash

rm -f 2a-PR-prev-adj.Rout
rm -f 2b-PR-prev-mh-adj.Rout 
rm -f 2c-PR-prev-ipcw.Rout
rm -f 2d-PR-prev-mh-ipcw.Rout

R CMD BATCH 2a-PR-prev-adj.R 
R CMD BATCH 2b-PR-prev-mh-adj.R 
R CMD BATCH 2c-PR-prev-ipcw.R
R CMD BATCH 2d-PR-prev-mh-ipcw.R 