library(raster);library(dplyr)
fli=c('data/CHELSA/10km_CHELSA_npp_1981-2010_V.2.1.tif',
      'data/CHELSA/10km_CHELSA_bio15_1981-2010_V.2.1.tif',
      'data/CHELSA/10km_CHELSA_bio4_1981-2010_V.2.1.tif',
      'data/CHELSA/10km_CHELSA_bio12_1981-2010_V.2.1.tif',
      'data/CHELSA/10km_CHELSA_bio1_1981-2010_V.2.1.tif',
      'data/CHELSA/10km_PMIP_CCSM4_BIO_01_anomaly.tif')

#fli <- stringr::str_replace(fli,"CHELSA_","10km_CHELSA_") 

rstlist=lapply(fli,function(x){
  rst=raster(x)
  return(rst[])
})
rstlist <- do.call('cbind',rstlist)
pred_sdm <- c('NPP',"Prec_seasonality","Temp_seasonality",
              "Prec_sum","Temp_mean", 'LGM_MAT_anomaly')
colnames(rstlist) <- pred_sdm

rst <- raster(fli[[1]])
rst[] <- 1:length(rst[])
xy <- raster::coordinates(rst) %>% as.data.frame()

gprst <- raster('data/Map_World_Polygon/world_rst_bf.tif')
flag <- raster::extract(gprst,xy)

rst[is.na(flag)] <- NA
writeRaster(rst,'data/CHELSA/10km_climate_cell_land.tif')

rstlist <- cbind(rstlist,xy,data.frame(cell=rst[]))
colnames(rstlist)

rstlist<-rstlist[!is.na(rstlist$cell),]
rstlist<-rstlist[!is.na(rstlist$LGM_MAT_anomaly),]

rst.poll <- raster();rst.poll[] <- 1:64800
rstlist$cell_poll <- cellFromXY(rst.poll,rstlist[,c('x','y')])
rst.poll <- rasterToPolygons(rst.poll) %>% st_as_sf(crs=proj)

write.csv(rstlist,'data/CHELSA/rstlist.csv',row.names = F)
save(rstlist,file='data/CHELSA/rstlist.rdata')
save(rst.poll,file='data/Map_World_Polygon/rst_poll.rdata')
