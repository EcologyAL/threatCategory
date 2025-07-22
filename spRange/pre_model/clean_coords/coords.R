####======================================
#### new form: coords, GBIF&EA
####======================================
setwd('E:/LuoA/SP_mapping/')
library(raster);library(rgdal);library(dplyr);library(stringr)
spls <- read.csv('splist/spls_fasp_GBIF_EA.csv')
library(parallel)
cl <- makeCluster(90)
clusterExport(cl,"spls")
clusterEvalQ(cl,{
  setwd('E:/LuoA/SP_mapping/')
  library(raster);library(rgdal);library(dplyr)
})

parLapply(cl,1:nrow(spls),function(i){
  fa <- spls$family[i]
  if(fa=='') return(NULL)
  id <- spls$species_id[i]
  sp <- spls$species[i]
  ge <- stringr::str_split(sp,' ')[[1]][1]
  
  xy <- data.frame(decimalLongitude=NULL,decimalLatitude=NULL)
  if(file.exists( paste0('F_SPP_Data_Archive//',fa,'/',id,'/',id,'_gbif_mark.csv') )){
    xy_add <- read.csv(paste0('F_SPP_Data_Archive//',fa,'/',id,'/',id,'_gbif_mark.csv'))
    xy_add <- xy_add[(!xy_add$kew_clean)&xy_add$gp_keep,]
    xy <- rbind(xy,xy_add[,c('decimalLongitude','decimalLatitude')])
  }
  
  if(file.exists( paste0('F_SPP_Data_Archive/',fa,'/',id,'/',id,'_ea_grids_mark.csv') )){
    xy_add <- read.csv(paste0('F_SPP_Data_Archive/',fa,'/',id,'/',id,'_ea_grids_mark.csv'))
    xy_add <- xy_add[!xy_add$kew_clean,]
    xy <- rbind(xy,xy_add[,c('decimalLongitude','decimalLatitude')])
  }
  
  #thin occurence data 
  
  if(nrow(xy)>0) {
    xy[,1] <- round(as.numeric(xy[,1]),3)
    xy[,2] <- round(as.numeric(xy[,2]),3)
    xy <- distinct(xy)
    write.csv(xy,paste0('F_SPP_Data_Archive/',fa,'/',id,'/',id,'_coords','.csv'),row.names=F)
  }
  return(NULL)
})
stopCluster(cl)

####======================================
#### GBIF ONLY
####======================================
setwd('E:/LuoA/SP_mapping/')
library(raster);library(rgdal);library(dplyr);library(stringr)
spls <- read.csv('splist/spls_fasp_GBIF_EA.csv')
library(parallel)
cl <- makeCluster(90)
clusterExport(cl,"spls")
clusterEvalQ(cl,{
  setwd('E:/LuoA/SP_mapping/')
  library(raster);library(rgdal);library(dplyr)
})

parLapply(cl,1:nrow(spls),function(i){
  fa <- spls$family[i]
  if(fa=='') return(NULL)
  id <- spls$species_id[i]
  sp <- spls$species[i]
  ge <- stringr::str_split(sp,' ')[[1]][1]
  
  xy <- data.frame(decimalLongitude=NULL,decimalLatitude=NULL)
  path <- paste0('F_SPP_Data_Archive//',fa,'/',id,'/',id,'_coords.csv')
  if(file.exists(path)){
    xy_add <- read.csv(path)
    xy <- rbind(xy,xy_add[,c('decimalLongitude','decimalLatitude')])
  }
  
  path <- paste0('F_SPP_Data_Archive/',fa,'/',id,'/',id,'_gbif_only1.csv')
  if(file.exists(path)){
    xy_add <- read.csv(path)
    xy <- rbind(xy,xy_add[,c('decimalLongitude','decimalLatitude')])
  }
  xy <- distinct(xy)
  if(nrow(xy)>0) write.csv(xy,paste0('F_SPP_Data_Archive/',fa,'/',id,'/',id,'_coords','.csv'),row.names=F)
  return(NULL)
})
stopCluster(cl)

####======================================
#### spls
####======================================
spls <- read.csv('splist/spls_fasp_GBIF_EA.csv')
#spls$coords <- NA

# for (i in 1:nrow(spls)) {
#   if(i%%1000==0)print(i)
# 
#   fa <- spls$family[i]
#   if(fa=='') next
#   id <- spls$species_id[i]
#   sp <- spls$species[i]
#   path <- paste0('F_SPP_Data_Archive/',fa,'/',id,'/',id,'_coords','.csv')
#   if(file.exists(path)){
#     xy <- read.csv(path)
#     spls$coords[i] <- nrow(xy)
#   }
# }

library(parallel)
cl <- makeCluster(90)
clusterExport(cl,"spls")
clusterEvalQ(cl,{
  setwd('E:/LuoA/SP_mapping/')
  library(dplyr)
})

n_coords<-parLapply(cl,1:nrow(spls),function(i){

  fa <- spls$family[i]
  if(fa=='') return(NULL)
  id <- spls$species_id[i]
  sp <- spls$species[i]
  path <- paste0('F_SPP_Data_Archive/',fa,'/',id,'/',id,'_coords','.csv')
  #spls_tmp <- spls[i,]
  if(file.exists(path)){
    xy <- read.csv(path)
    spls_tmp <- data.frame(species=sp,n_coords=nrow(xy)) 
    return(spls_tmp)
  }else{
    return(data.frame(species=sp,n_coords=0))
  }
})
n_coords <- do.call('rbind',n_coords)

spls <- dplyr::select(spls,species,species_id,family)
spls <- left_join(spls,n_coords) 
spls$n_coords[is.na(spls$n_coords)] <- 0
#spls <- dplyr::select(spls,species,family,species_id,n_coords)
write.csv(spls,'splist/spls_summary.csv',row.names = F)
