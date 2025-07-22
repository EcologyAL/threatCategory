setwd('D:/work/RLI/')
sapply(c('foreign','sf','dplyr','ggplot2','raster','RColorBrewer','stringr','patchwork'),require,character.only=T)#'rasterVis'

mapdat <- read.dbf('data/map/data_1d/data_1d_land.dbf')
abf <- raster('data/zonation/rankmap.tif')
mapdat$ABF <- abf[mapdat$cell_poll]

#mapdat <- mapdat %>% na.omit()
p <- sum(mapdat$PAs30)/nrow(mapdat)
pae.p.n <- round((0.30-p)*nrow(mapdat))
rli.p.n <- round((0.30)*nrow(mapdat))
#mapdat <- read.csv('data/outputs/cell_1d.csv')

mapdat <- mapdat[order(mapdat$RLI_ensemb),]
mapdat.noPA <- mapdat[!mapdat$PAs30,]
pae.RLI.thr <- mapdat.noPA$RLI_ensemb[pae.p.n]
rli.RLI.thr <- mapdat.noPA$RLI_ensemb[rli.p.n]

# NP_P 0
# NP_UP 1
# PAEP_P 2
# PAEP_UP 3
# RLIP_P 4
# RLIP_UP 5

mapdat$Priority <- as.numeric(mapdat[['PAs30']])
mapdat$Priority[(mapdat[['PAs30']])&(mapdat[["RLI_ensemb"]]<=rli.RLI.thr)] <- 2
mapdat$Priority[(!mapdat[['PAs30']])&(mapdat[["RLI_ensemb"]]<=rli.RLI.thr)] <- 3
#mapdat$Priority[(mapdat[['PAs30']])&(mapdat[["RLI_ensemb"]]<=pae.RLI.thr)] <- 4
mapdat$Priority[(!mapdat[['PAs30']])&(mapdat[["RLI_ensemb"]]<=pae.RLI.thr)] <- 4

rst <- raster()
rst[mapdat$cell_poll] <- mapdat$Priority
plot(rst)
#
mapdat.noPA <- mapdat.noPA[order(mapdat.noPA$ABF,decreasing = T),]
pae.abf.thr <- mapdat.noPA$ABF[pae.p.n]

mapdat$Priority2 <- as.numeric(mapdat[['PAs30']])
mapdat$Priority2[(mapdat[['PAs30']])&
                   (mapdat[["RLI_ensemb"]]<=pae.RLI.thr)&
                   (mapdat[["abf"]]<pae.abf.thr)] <- 2
mapdat$Priority2[(mapdat[['PAs30']])&
                   (mapdat[["RLI_ensemb"]]>pae.RLI.thr)&
                   (mapdat[["abf"]]>=pae.abf.thr)] <- 3

mapdat$Priority2[(mapdat[['PAs30']])&
                   (mapdat[["RLI_ensemb"]]<=pae.RLI.thr)&
                   (mapdat[["abf"]]>=pae.abf.thr)] <- 4

#shp <- read.dbf('data/outputs/map1d/data_1d_land.dbf')
write.dbf(mapdat,'data/map/data_1d/data_1d_land.dbf')
write.dbf(mapdat,'data/outputs/map1d/data_1d_land.dbf')

