##############################################
# WASH Benefits Bangladesh STH Kato-Katz Study
# Primary outcome analysis  

# Configure data paths and load required packages

# by Jade Benjamin-Chung
# jadebc@berkeley.edu
##############################################
# load required packages
library(washb)
library(here)
library(devtools)

# define data paths
sth_data_path = "~/Dropbox/WASHB Parasites/Analysis datasets/Jade/sth.csv"

# generated data paths
save_data_path = "~/Box Sync/WASHB Parasites/Results/Jade/"

# load base functions
source("~/documents/crg/wash-benefits/bangladesh/src/sth/analysis/0-base-programs.R")
