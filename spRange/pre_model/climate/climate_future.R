library(raster);library(dplyr)
load('data/CHELSA/rstlist.rdata')
rstlist.c <- rstlist
#year.sn <- expand.grid(c('2041-2070','2071-2100'),c("ssp126","ssp370","ssp585"))
year.sn <- expand.grid(c('2011-2040','2041-2070','2071-2100'),c("ssp126","ssp370","ssp585"))
year.sn <- paste0(year.sn$Var1,'_mean_',year.sn$Var2)

fli.f=c('data/CHELSA/10km/CHELSA_npp_',
      'data/CHELSA/10km/CHELSA_bio15_',
      'data/CHELSA/10km/CHELSA_bio4_',
      'data/CHELSA/10km/CHELSA_bio12_',
      'data/CHELSA/10km/CHELSA_bio1_',
      'data/CHELSA/10km/CHELSA_bio1_anomaly_')

fli.l <- lapply(year.sn, function(x){
  paste0(fli.f,x,'.tif')
})

rstlist.f <- lapply(fli.l, function(fli.tmp){
  print(fli.tmp)
  rstlist=lapply(fli.tmp,function(x){
    rst=raster(x)
    return(rst[])
  })
  
  rstlist <- do.call('cbind',rstlist)
  pred_sdm <- c('NPP',"Prec_seasonality","Temp_seasonality",
                "Prec_sum","Temp_mean", 'LGM_MAT_anomaly')
  colnames(rstlist) <- pred_sdm
  
  rst <- raster('data/CHELSA/10km_CHELSA_npp_1981-2010_V.2.1.tif')
  rst[] <- 1:length(rst[])
  xy <- raster::coordinates(rst) %>% as.data.frame()
  
  gprst <- raster('data/Map_World_Polygon/world_rst_bf.tif')
  flag <- raster::extract(gprst,xy)
  
  rst[is.na(flag)] <- NA
  #writeRaster(rst,'data/CHELSA/10km_climate_cell_land.tif')
  
  rstlist <- cbind(rstlist,xy,data.frame(cell=rst[]))
  colnames(rstlist)
  
  rstlist<-rstlist[!is.na(rstlist$cell),]
  rstlist<-rstlist[!is.na(rstlist$LGM_MAT_anomaly),]
  
  rst.poll <- raster();rst.poll[] <- 1:64800
  rstlist$cell_poll <- cellFromXY(rst.poll,rstlist[,c('x','y')])
  return(rstlist)
})
#rst.poll <- rasterToPolygons(rst.poll) %>% st_as_sf(crs=proj)

#write.csv(rstlist,'data/CHELSA/rstlist.csv',row.names = F)
names(rstlist.f) <- year.sn
save(rstlist.f,file='data/CHELSA/rstlist_f.rdata')
#save(rst.poll,file='data/Map_World_Polygon/rst_poll.rdata')
