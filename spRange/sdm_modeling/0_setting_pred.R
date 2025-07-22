### =========================================================================
### Set directories and source functions
### =========================================================================
library(raster)
input_dir <- 'raw_treemap_tree_family_combined/'

# Directories
output_dir <- 'output/Output/'
templ_dir = enstr_path = temp_dir = paste0('Temp/Temp',sample(1:9999999999,1),'/')
#templ_dir = enstr_path = temp_dir = paste0('Temp',sample(1:9999999999,1),'/')
if(!dir.exists(enstr_path)){
  dir.create(enstr_path, recursive = T)
}

# Source function files
cmon.files=list.files("code//sdm_functions/",full.names = T)
sapply(cmon.files,source)
source("code/range_functions/range_fun_pb.R")

# Source R libraries
lib_vect <- c("raster",'sf', "tools","dismo","cluster","class",
              "gam","gbm","randomForest","ROCR","parallel","mclust",
              "geometry","FNN","dplyr","magrittr","sf")
#"ClusterR","rgeos","rgdal","maptools",
sapply(lib_vect,require,character.only=TRUE)

### =========================================================================
### Prep current-time environmental layers
### =========================================================================

load('data/Map_World_Polygon/rst_poll.rdata') # load rstlist
load('data/CHELSA/rstlist.rdata') # load rstlist
load('data/CHELSA/rstlist_f.rdata') # load rstlist
rst.env <- raster('data/CHELSA/10km_climate_cell_land.tif')

pred_sdm <- c('NPP',"Prec_seasonality","Temp_seasonality",
              "Prec_sum","Temp_mean", 'LGM_MAT_anomaly')

year.sn.combn <- read.csv('data/year_sn/year_sn_combn.csv')

### =========================================================================
### Further definitions
### =========================================================================

# Geographic projection
proj <- 4326 

### =========================================================================
### Species data
### =========================================================================

cat("Setting done...\n")