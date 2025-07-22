library(ggplot2);library(patchwork);library(stringr);library(sf);library(dplyr)

setwd("E:/luoa/")
source('data/map/Robin/MapRobin.R')
world <- read_sf('data/map/rst_shape/raw/raster_boarder.shp')
world.map <- read.csv('Chapter3/data/mapinfo/worldMap.csv')
world.map <- left_join(world,world.map)
world.map[is.na(world.map$SR),-(1:2)] <- 0
world.map.proj <- st_transform(world.map, crs = "+proj=robin")
world.map <- world.map.proj

ft <- read.csv('data/future/cellInfo_future.csv')
vars <- c("X2070_ssp126","X2070_ssp126_limit","X2070_ssp126_current",
          "X2070_ssp370","X2070_ssp370_limit","X2070_ssp370_current",
          "X2070_ssp585","X2070_ssp585_limit","X2070_ssp585_current",
          "X2100_ssp126","X2100_ssp126_limit","X2100_ssp126_current",
          "X2100_ssp370","X2100_ssp370_limit","X2100_ssp370_current",
          "X2100_ssp585","X2100_ssp585_limit","X2100_ssp585_current") %>% 
  str_remove('X') %>% paste0('_unlimit') %>% str_replace('_limit_unlimit','_limit') %>% str_replace('_current_unlimit','_nondispersal')

colnames(ft)[-(1:3)] <- colnames(ft)[-(1:3)] %>% str_remove('X') %>% paste0('_unlimit') %>% str_replace('_limit_unlimit','_limit') %>% str_replace('_current_unlimit','_nondispersal')
ft[,vars] <- (ft[,vars]-ft[,'current'])/(ft[,'current'])

world.map <- left_join(world.map.proj,ft)

world.map

# gain & lost -------------------------------------------------------------

library(parallel)

cl <- makeCluster(24)

clusterEvalQ(cl,{
  library(ggplot2);library(patchwork);library(stringr);library(sf);library(dplyr)
  setwd("E:/luoa/")
  
  rm(spc)
  load('data/spc/future/2010.rdata')
  spc.c <- spc
  
  rm(spc)
  load('data/spc/future/2100_ssp370.rdata')
  spc.f <- spc
  
  cells <- spc.c$cells %>% unique()
})

rm(spc)
load('data/spc/future/2010.rdata')
spc.c <- spc

rm(spc)
load('data/spc/future/2100_ssp370.rdata')
spc.f <- spc

cells <- spc.c$cells %>% unique()
cell.info <- parLapply(cl,cells,function(cell){
  print(which(cells%in%cell))
  sp.c <- spc.c$speciesKey[spc.c$cells%in%cell] %>% unique()
  sp.f <- spc.f$speciesKey[spc.f$cells%in%cell] %>% unique()
  cell.info <- data.frame(cell=cell,SR_c=length(sp.c),SR_f=length(sp.f),
                                          gain=length(sp.f[!sp.f%in%sp.c]),loss=length(sp.c[!sp.c%in%sp.f]))
  return(cell.info)
})
cell.info <- do.call('rbind',cell.info)

rm(spc)
load('data/spc/future/2010.rdata')
spc.c <- spc

rm(spc)
load('data/spc/future/2100_ssp370.rdata')
spc.f <- spc

cell.info <- data.frame()
cells <- spc.c$cells %>% unique()
for(cell in cells) {
  print(which(cells%in%cell))
  sp.c <- spc.c$speciesKey[spc.c$cells%in%cell] %>% unique()
  sp.f <- spc.f$speciesKey[spc.f$cells%in%cell] %>% unique()
  cell.info <- rbind(cell.info,data.frame(cell=cell,SR_c=length(sp.c),SR_f=length(sp.f),
             gain=length(sp.f[!sp.f%in%sp.c]),loss=length(sp.c[!sp.c%in%sp.f])))
}



#bk <- cell.info
colnames(cell.info)[3] <- 'X2100_ssp370_limit_pureInc'
dat<-read.csv('data/spc/future/cellInfo_future.csv')
dat <- left_join(dat,cell.info[,c(1:3)])
dat$gain <- (dat$X2100_ssp370_limit_pureInc - dat$current)/dat$current
dat$loss <- (dat$current - (dat$X2100_ssp370_limit_pureInc - dat$X2100_ssp370_limit))/dat$current
colnames(dat)
write.csv(dat[,-1],'data/spc/future/cellInfo_future.csv',row.names = F)


# pd & fd -----------------------------------------------------------------

setwd('E:/luoa/')
sapply(list('dplyr','ape','raster','sf','magrittr','RColorBrewer'), require,character.only=TRUE)
source('Chapter3/code/func/func.R')

load('data/spc/spc.rdata');spc %<>% na.omit()
splist <- spc[,c(1,3)] %>% distinct()

load('data/spc/future/2010.rdata');spc %<>% na.omit()
colnames(spc)[1]<-'cell'
spc<-left_join(spc,splist)

world <- read_sf('data/map/rst_shape/raw/raster_boarder.shp')

tre.pd <- read.tree('data/tre/treS3.tre')
tre.fd <- read.tree('data/funcTre/tree_290792_H_no_WD.tree')
# 
# world.map <- al.sr(spc,world)
# 
# cl.num<-25
# tre <- tre.pd
# source('Chapter7/code/main/al.pd.R')# output pd
# 
# world.map <- left_join(world.map,pd)
# colnames(world.map)[4] <- 'PD'
# 
# tre <- tre.fd
# source('Chapter3/code/main/al.pd.R')# output pd
# fd <- pd
# colnames(fd)[2] <- 'FD'
# world.map <- left_join(world.map,fd)
# 
#world.map.tmp <- world.map

load('data/spc/future/2100_ssp370.rdata');spc %<>% na.omit()
colnames(spc)[1]<-'cell'
spc<-left_join(spc,splist)

world <- read_sf('data/map/rst_shape/raw/raster_boarder.shp')
tre.pd <- read.tree('data/tre/treS3.tre')
tre.fd <- read.tree('data/funcTre/tree_290792_H_no_WD.tree')

world.map <- al.sr(spc,world)
#cl.num<-30
tre <- tre.pd
source('Chapter3/code/main/al.pd.R')# output pd

world.map <- left_join(world.map,pd)
colnames(world.map)[4] <- 'PD'

tre <- tre.fd
source('Chapter3/code/main/al.pd.R')# output pd
fd <- pd
colnames(fd)[2] <- 'FD'
world.map <- left_join(world.map,fd)

colnames(world.map)[3:5]<-c('SR2100','PD2100','FD2100')
world.map <- world.map.tmp %>% left_join(st_drop_geometry(world.map))

#
world.map <- left_join(world,cell.info)
save(world.map,file='Chapter7/data/2100.rdata')
