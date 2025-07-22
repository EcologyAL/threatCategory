setwd("～")
library(raster);library(dplyr);library(alphahull);library(parallel);library(sf)

#### country info
library(rnaturalearth)
cty_shp <- ne_countries()
land.rst <- raster('data/CHELSA/10km_climate_cell_land.tif')
xy <- coordinates(land.rst) %>% as.data.frame()
xy$cell <- 1:nrow(xy)
query_point <- SpatialPointsDataFrame(xy[,1:2], xy, proj4string=CRS("+proj=longlat +datum=WGS84"))
over.pts <- over(query_point, cty_shp)
over.pts.s <- over.pts[,c('sovereignt','sov_a3','admin','income_grp','continent')]
over.pts.s$cell <- 1:nrow(xy)
over.pts.s <- over.pts.s[!is.na(over.pts.s$sov_a3),]

#colnames(sp.ds.1d)

#### mark output path
spls <- read.csv('splist/spls_mark.csv')
# flag.read <- which((spls$num_coords_c>3)&#(spls$num_coords_c<20)&
#                      (file.exists(paste0('data/F_SPP_Output/',spls$family,'/',spls$speciesKey,'/',spls$speciesKey,'.csv'))))
# flag.read <- which((spls$num_coords_c>3)&#(spls$num_coords_c<20)&
#                      file.exists(paste0('data/F_SPP_Output/',spls$family,'/',spls$speciesKey,'/',spls$speciesKey,'.csv'))&
#                      (!file.exists(paste0('output/spInfoCsv/',spls$speciesKey,'.csv'))))

cl <- makeCluster(100)
clusterEvalQ(cl,{setwd("E:/LuoA/PRIME/");library(dplyr)})
clusterEvalQ(cl,{spls <- read.csv('splist/spls_mark.csv');NULL})
clusterExport(cl,"over.pts.s")

iii <- flag.read[1]
resall <- parLapply(cl,flag.read,function(iii){
  fa <- spls$family[iii]
  id <- spls$speciesKey[iii]
  
  path_output <- paste0('data/F_SPP_Output/',fa,'/',id,'/',id,'.csv')
  spds <- read.csv(path_output)
  spds <- spds %>% mutate(speciesKey=id) 
  
  spds <- left_join(spds,over.pts.s)
  colnames(spds) <- stringr::str_replace(colnames(spds),'X','sn_') 
  sp.info <- spds %>% group_by(sov_a3) %>% mutate(sov_n=sum(sn_1981.2010>0)) %>% 
    ungroup() %>% dplyr::select(speciesKey,sovereignt,sov_a3,admin,sov_n,income_grp,continent) %>% distinct()
  
  write.csv(sp.info,paste0('output/spInfoCsv/',id,'.csv'),row.names = F)
  return(sp.info)
})

flag.read <- which((file.exists(paste0('output/spInfoCsv/',spls$speciesKey,'.csv'))))
resall <- parLapply(cl,flag.read,function(iii){
  fa <- spls$family[iii]
  id <- spls$speciesKey[iii]
  
  read.csv(paste0('output/spInfoCsv/',id,'.csv'))
})
resall <- do.call('rbind',resall)
resall <- resall[,-1]
write.csv(resall,'F:/luoao/RLI/data/spinfo/sp_cty.csv',row.names = F)

################
################
resall <- parLapply(cl,flag.read,function(iii){
  fa <- spls$family[iii]
  id <- spls$speciesKey[iii]
  
  path_output <- paste0('data/F_SPP_Output/',fa,'/',id,'/',id,'.csv')
  spds <- read.csv(path_output)
  spds <- spds %>% mutate(speciesKey=id) 
  spds <- left_join(spds,over.pts.s)
  sp.info <- spds %>% group_by(sov_a3) %>% mutate(sov_n=sum(sn_1981.2010>0)) %>% 
    ungroup() %>% dplyr::select(speciesKey,sovereignt,sov_a3,admin,sov_n,income_grp,continent) %>% distinct()
  
  return(sp.info)
  # sp.range <- c(
  #   sn_1981.2010=sum(spds$sn_1981.2010>0),
  #   sn_2011.2040_mean_ssp370=sum(spds$sn_2011.2040_mean_ssp370>0),
  #   sn_2041.2070_mean_ssp370=sum(spds$sn_2041.2070_mean_ssp370>0),
  #   sn_2071.2100_mean_ssp370=sum(spds$sn_2071.2100_mean_ssp370>0)
  # )
  # sp.info
  
  # for(i in 2:4) sp.range[i] <- round((sp.range[1]-sp.range[i])/sp.range[1]*100,1)
  # for(i in 2:4) sp.range[i] <- ifelse(sp.range[i]<0,0,sp.range[i]) 
  
  # spds_1d <- spds
  # for (i in 1:10) {spds_1d[,i] <- spds_1d[,i]>0}
  # spds_1d <- spds_1d %>% dplyr::select(-cell) %>% distinct()
  
  # return(list(sp.range,spds_1d))
})
sp.range.ls <- lapply(resall, function(x)x[[1]])
sp.ds.1d.ls <- lapply(resall, function(x)x[[2]])

sp.range <- do.call('rbind',sp.range.ls)
sp.ds.1d.flag <- do.call('rbind',lapply(sp.ds.1d.ls, dim))
flag <- which(sp.ds.1d.flag[,2]%in%14)
for(i in flag){
  tmp <- sp.ds.1d.ls[[i]]
  colnames(tmp) <- colnames(tmp) %>% stringr::str_replace('X','sn_')
  sp.ds.1d.ls[[i]] <- tmp[,colnames(tmp) %in% colnames(sp.ds.1d.ls[[1]])]
}

sp.ds.1d <- do.call('rbind',sp.ds.1d.ls)

save(sp.range,file='output/spcell_1d/sp_range_decline.rdata')
save(sp.ds.1d,file='output/spcell_1d/sp_ds_1d.rdata')

#### output data at 1 degree resolution
resall_1d <- resall
for (i in 1:10) {resall_1d[,i] <- resall_1d[,i]>0}

resall_1d <- resall_1d %>% dplyr::select(-cell) %>% distinct()
save(resall_1d,file='output/spcell_1degree/spcell_1degree_3_20.rdata')


# setwd("E:/LuoA/PRIME/")
# library(raster);library(dplyr);library(alphahull);library(parallel);library(sf)
# 
# spls <- read.csv('splist/spls_mark.csv')
# flag.read <- which((spls$num_coords_c>3)&#(spls$num_coords_c<20)&
#                      (file.exists(paste0('data/F_SPP_Output/',spls$family,'/',spls$speciesKey,'/',spls$speciesKey,'.csv'))))
# 
# cl <- makeCluster(100)
# #clusterExport(cl,"spls")
# clusterEvalQ(cl,{setwd("E:/LuoA/PRIME/");library(dplyr)})
# #clusterEvalQ(cl,{library(dplyr)})
# clusterEvalQ(cl,{spls <- read.csv('splist/spls_mark.csv');NULL})
# 
# resall <- parLapply(cl,flag.read,function(iii){
#   fa <- spls$family[iii]
#   id <- spls$speciesKey[iii]
# 
#   path_output <- paste0('data/F_SPP_Output/',fa,'/',id,'/',id,'.csv')
#   spds <- read.csv(path_output)
#   spds <- spds %>% mutate(speciesKey=id)
#   if(ncol(spds)!=13){
#     colnames(spds) <- colnames(spds) %>% stringr::str_replace('X','sn_')
#     spds <- spds[,!colnames(spds)%in%c('x','y')]
#   }
#   return(spds)
# })
# resall <- do.call('rbind',resall)
# 
# sp.ds.1d.flag <- do.call('rbind',lapply(sp.ds.1d.ls, dim))
# flag <- which(sp.ds.1d.flag[,2]%in%14)
# for(i in flag){
#   tmp <- sp.ds.1d.ls[[i]]
#   colnames(tmp) <- colnames(tmp) %>% stringr::str_replace('X','sn_')
#   sp.ds.1d.ls[[i]] <- tmp[,colnames(tmp) %in% colnames(sp.ds.1d.ls[[1]])]
# }
# 
# resall <- do.call('rbind',resall)
# sp <- unique(resall$speciesKey)
# save(resall,'output/spcell_10km/spcell_10km_3_inf.rdata')
# 
# load('output/spcell_10km/spcell_10km_3_20.rdata')
# 
# #### output data at 1 degree resolution
# resall_1d <- resall
# for (i in 1:10) {resall_1d[,i] <- resall_1d[,i]>0}
# 
# resall_1d <- resall_1d %>% dplyr::select(-cell) %>% distinct()
# save(resall_1d,file='output/spcell_1degree/spcell_1degree_3_20.rdata')
# 
# #### summary species imformation
# sp.range <- resall %>% group_by(speciesKey) %>% summarise(
#   sn_1981.2010=sum(sn_1981.2010>0),
#   sn_2011.2040_mean_ssp370=sum(sn_2011.2040_mean_ssp370>0),
#   sn_2041.2070_mean_ssp370=sum(sn_2041.2070_mean_ssp370>0),
#   sn_2071.2100_mean_ssp370=sum(sn_2071.2100_mean_ssp370>0),
#   .groups = 'drop'
# ) %>% as.data.frame()
# for(i in 3:5) sp.range[,i] <- round((sp.range[,2]-sp.range[,i])/sp.range[,2]*100,3)
# for(i in 3:5) sp.range[,i] <- ifelse(sp.range[,i]<0,0,sp.range[,i])
# write.csv(sp.range,'output/spcell_10km/sp_range_declineRate.csv',row.names=F)
# # hist(sp.range[,4])
# # sum(sp.range[,5]==100,na.rm=T)
# # sum(sp.range[,4]==100,na.rm=T)
# # sum(sp.range[,3]==100,na.rm=T)
# #
# # m <- lm(sp.range[,5]~sp.range[,2])
# 
# #### extract species country
# library(rnaturalearth)
# cty_shp <- ne_countries()
# land.rst <- raster('data/CHELSA/10km_climate_cell_land.tif')
# xy <- coordinates(land.rst) %>% as.data.frame()
# xy$cell <- 1:nrow(xy)
# query_point <- SpatialPointsDataFrame(xy[,1:2], xy, proj4string=CRS("+proj=longlat +datum=WGS84"))
# over.pts <- over(query_point, cty_shp)
# over.pts.s <- over.pts[,c('sovereignt','sov_a3','admin','income_grp','continent')]
# over.pts.s$cell <- 1:nrow(xy)
# 
# sp.cell <- read.csv('output/spcell_10km/spcell_10km_1d_0_10.csv')
# load('output/spcell_10km/')
# colnames(sp.ds.1d)
