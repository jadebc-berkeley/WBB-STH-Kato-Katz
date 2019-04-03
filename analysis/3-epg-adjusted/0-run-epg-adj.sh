
#!/bin/bash

rm -f 3a-PR-epg-adj.Rout
rm -f 3b-PR-epg-ipcw.Rout 

R CMD BATCH 3a-PR-epg-adj.R 
R CMD BATCH 3b-PR-epg-ipcw.R 
