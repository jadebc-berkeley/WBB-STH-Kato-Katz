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
# sth_data_path = "~/Dropbox/WASHB Parasites/Analysis datasets/Jade/sth.csv"
sth_data_path = "~/Dropbox/WASHB-Bangladesh-Data/2-sth-kk-outcome-datasets/Public/washb-bangladesh-sth-public.csv"

# generated data paths
save_data_path = "~/Box Sync/WASHB Parasites/Results/Jade/"
save_data_path = "~/Box Sync/WASHB Parasites/Results/Public-test/"

# load base functions
source(paste0(here::here(),"/analysis/0-base-programs.R"))
source(paste0(here::here(),"/table-scripts/0-base-table-functions.R"))

# define figure and table paths
figure_path = paste0(here::here(), "/figures/")
table_path = paste0(here::here(), "/tables/")