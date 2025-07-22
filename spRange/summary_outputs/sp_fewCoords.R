setwd("E:/LuoA/PRIME/")
library(raster);library(dplyr);library(alphahull);library(parallel);library(stringr)


# set parallel ------------------------------------------------------------


cl <- makeCluster(96)
clusterEvalQ(cl,{
  setwd("E:/LuoA/PRIME/")
  library(raster);library(dplyr);library(alphahull);library(parallel);library(stringr)
  spls <- read.csv('splist/spls_outputs.csv')
  
  rstland <- raster('data/CHELSA/10km_climate_cell_land.tif')
  rstland[] <- 1:length(rstland[])
  
  rst_1d <- raster()
  rst_1d[] <- 1:length(rst_1d[])

  return(NULL)
})

# species to run ----------------------------------------------------------

setwd("E:/LuoA/PRIME/")
spls <- read.csv('splist/spls_outputs.csv')

#flag <- file.exists(spls$path_range)
#flag.num <- which((spls$num_coords>0)&(!flag))
flag.num <- which((spls$num_coords>0))


# read csv ----------------------------------------------------------------


i <- flag.num[1]
sp.ds <- parLapply(cl,flag.num,function(i){
  sp.ds <- read.csv(spls$path[i])
  sp.ds$speciesKey <- spls$speciesKey[i]
  
  sp.ds$cell <- extract(rstland,sp.ds[,1:2])
  sp.ds$cell_1d <- extract(rst_1d,sp.ds[,1:2])
  return(sp.ds)
})

# extract cell number -----------------------------------------------------

sp.ds <- do.call('rbind',sp.ds)
sp.ds <- data.frame(speciesKey=sp.ds$speciesKey,cell=sp.ds$cell,cell_1d=sp.ds$cell_1d) %>% na.omit() %>% distinct()
write.csv(sp.ds,'output/summary/cellInfo_10km/spcell_10km_1d_all.csv',row.names=T)
stopCluster(cl)


# 
# # read csv ----------------------------------------------------------------
# 
# 
# i <- flag.num[1]
# sp.ds <- parLapply(cl,flag.num,function(i){
#   sp.ds <- read.csv(spls$path[i])
#   sp.ds$speciesKey <- spls$speciesKey[i]
#    
#   #sp.ds <- do.call('rbind',sp.ds)
#   
#   if(length(unique(sp.ds$cell))<=10){
#     sp.ds$cell <- extract(rstland,sp.ds[,1:2])
#     sp.ds$cell_1d <- extract(rst_1d,sp.ds[,1:2])
#     return(sp.ds)
#   }else{
#     return(data.frame(decimalLongitude = NA,
#                       decimalLatitude = NA,
#                       speciesKey=sp.ds$speciesKey,
#                       cell=NA,cell_1d=NA)) #%>% na.omit() %>% distinct())
#   }
#   
# })          
# 
# 
# num_coords_c <- parLapply(cl,flag.num,function(i){
#   sp.ds <- read.csv(spls$path[i])
#   sp.ds$speciesKey <- spls$speciesKey[i]
#   
#   
#   sp.ds$cell <- extract(rstland,sp.ds[,1:2])
#   tmp <- unique(sp.ds$cell)
#   sum(!is.na(tmp))
# })
# spls$num_coords_c <- 0
# spls$num_coords_c[flag.num] <- unlist(num_coords_c)
# 
# write.csv(spls,'splist/spls_outputs.csv',row.names = F)
# 
# path <- 'F:/luoao/RLI/data/spinfo/spls_mark_all.csv'
# tmp <- read.csv(path)
# tmp <- left_join(tmp,spls[,c('speciesKey','num_coords_c')])
# write.csv(tmp,path,row.names = F)
# 
# # extract cell number -----------------------------------------------------
# 
# sp.ds <- do.call('rbind',sp.ds.list)
# sp.ds <- data.frame(speciesKey=sp.ds$speciesKey,cell=sp.ds$cell,cell_1d=sp.ds$cell_1d) %>% na.omit() %>% distinct()
# write.csv(sp.ds,'output/summary/cellInfo_10km/spcell_10km_1d_0_10_cell.csv')
# stopCluster(cl)
