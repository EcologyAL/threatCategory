setwd("E:/LuoA/PRIME/")
library(raster);library(dplyr);library(parallel);library(stringr)

rstland <- raster('E:/LuoA/PRIME/data/CHELSA/10km_climate_cell_land.tif')
rstland.p <- rasterToPoints(rstland)

spls <- read.csv('E:/LuoA/PRIME/splist/spls_outputs.csv')
spls.lf <- read.csv('F:/luoao/RLI/data/lifeForm/species_lifefrom_NOgenusFill.csv')
spls <- left_join(spls,spls.lf)
spls$lifeForm <- spls$lifeForm %>% str_replace_all('  ',' ')

lf.list <- unique(spls$lifeForm[flag])
# lf.list <- c("evergreen trees","drought_deciduous trees","cold_deciduous trees","needleleaf trees",
# "evergreen shrubs","drought_deciduous shrubs","cold_deciduous shrubs",
# "c3 grasses","c4 grasses","climbers","geophytes","succulents","therophytes","forbs")%>% str_replace_all('  ',' ')

# lf.list <- c("evergreen broadleaf trees","drought_deciduous broadleaf trees","cold_deciduous broadleaf trees","needleleaf trees",
# "evergreen broadleaf shrubs","drought_deciduous broadleaf shrubs","cold_deciduous broadleaf shrubs",
# "c3 grasses","c4 grasses","climbers","geophytes","succulents","therophytes","forbs")%>% str_replace_all('  ',' ')

#flag.num <- which(!is.na(spls$path_range))
flag <- !is.na(spls$path_range)

cl <- makeCluster(100)
clusterEvalQ(cl,{
  setwd("E:/LuoA/PRIME/")
  library(raster);library(dplyr);library(parallel);library(stringr)
  
  rstland <- raster('E:/LuoA/PRIME/data/CHELSA/10km_climate_cell_land.tif')
  rstland.p <- rasterToPoints(rstland)
  
  spls <- read.csv('E:/LuoA/PRIME/splist/spls_outputs.csv')
  spls.lf <- read.csv('F:/luoao/RLI/data/lifeForm/species_lifefrom_NOgenusFill.csv')
  spls <- left_join(spls,spls.lf)
  spls$lifeForm <- spls$lifeForm %>% str_replace_all('  ',' ')
  
  lf.list <- unique(spls$lifeForm)
  # lf.list <- c("evergreen trees","drought_deciduous trees","cold_deciduous trees","needleleaf trees",
  #              "evergreen shrubs","drought_deciduous shrubs","cold_deciduous shrubs",
  #              "c3 grasses","c4 grasses","climbers","geophytes","succulents","therophytes","forbs")%>% str_replace_all('  ',' ')
  NULL})

for (lf in lf.list) {
  print(lf)
  flag.num <- which(flag&(spls$lifeForm%in%lf))
  
  
  resall <- parLapply(cl,flag.num,function(iii){
    path <- spls$path_range[iii]
    path.out <- path %>% str_replace('data/F_SPP_Output_csv/','output/HSD/') %>% str_remove('mod6/') %>% str_remove('mod10/')
    
    sp.range <- read.csv(path.out)
    return(sp.range)
  })
  resall <- do.call('rbind',resall)
  resall <- as.data.frame(resall)
  
  dat <- resall %>% group_by(cell) %>% 
    #mutate(sn_1981.2010=sum(sn_1981.2010)) %>% 
    summarise_if(is.logical, sum) 
  dat <- as.data.frame(dat)
  print(length(flag.num))
  write.csv(dat,paste0('F:/luoao/RLI/data/lifeForm/cellInfo/',lf,'.csv'),row.names = F)
}


stopCluster(cl)

