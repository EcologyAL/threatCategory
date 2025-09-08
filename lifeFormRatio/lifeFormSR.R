setwd("~")
library(raster);library(dplyr);library(parallel);library(stringr)

# read the base map in raster form in 1km
rstland <- raster('data/CHELSA/10km_climate_cell_land.tif')
rstland.p <- rasterToPoints(rstland)

# load species list and species growth forms
spls <- read.csv('data/splist/spls_outputs.csv')
spls.lf <- read.csv('data/lifeForm/species_lifefrom_NOgenusFill.csv')
spls <- left_join(spls,spls.lf)
spls$lifeForm <- spls$lifeForm %>% str_replace_all('  ',' ')

lf.list <- unique(spls$lifeForm)

flag <- !is.na(spls$path_range)

# =========================================================
# multithreading setup
# initialize data 
# =========================================================

cl <- makeCluster(2)
clusterEvalQ(cl,{
  setwd("~")
  library(raster);library(dplyr);library(parallel);library(stringr)
  
  rstland <- raster('~/data/CHELSA/10km_climate_cell_land.tif') # base map
  rstland.p <- rasterToPoints(rstland)
  
  spls <- read.csv('~/splist/spls_outputs.csv') # species list
  spls.lf <- read.csv('~/data/lifeForm/species_lifefrom_NOgenusFill.csv') # species life form
  spls <- left_join(spls,spls.lf)
  spls$lifeForm <- spls$lifeForm %>% str_replace_all('  ',' ')
  
  lf.list <- unique(spls$lifeForm) # extract all life forms
NULL})

# =========================================================
# multithread main body is embedded in loops for life forms
# Each worker thread corresponds to one species
# multithread is to extract species range 
# =========================================================

for (lf in lf.list) {
  print(lf)
  flag.num <- which(flag&(spls$lifeForm%in%lf))
  
  # Multithreading main body
  resall <- parLapply(cl,flag.num,function(iii){
    path <- spls$path_range[iii]
    path.out <- path %>% str_replace('data/F_SPP_Output_csv/','output/HSD/') %>% 
      str_remove('mod6/') %>% str_remove('mod10/')
    
    sp.range <- read.csv(path.out)
    return(sp.range)
  })
  resall <- do.call('rbind',resall)
  resall <- as.data.frame(resall)
  
  # calculate species richness in a life form
  dat <- resall %>% group_by(cell) %>% 
    summarise_if(is.logical, sum) 
  dat <- as.data.frame(dat)
  print(length(flag.num))
  write.csv(dat,paste0('~/data/lifeForm/cellInfo/',lf,'.csv'),row.names = F)
}


stopCluster(cl)

