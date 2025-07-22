setwd("E:/LuoA/PRIME/")
library(raster);library(dplyr);library(alphahull);library(parallel);library(stringr)


# set parallel ------------------------------------------------------------


cl <- makeCluster(98)
clusterEvalQ(cl,{
  setwd("E:/LuoA/PRIME/")
  library(raster);library(dplyr);library(alphahull);library(parallel);library(stringr)
  spls <- read.csv('splist/spls_outputs.csv')
  return(NULL)
})

clusterEvalQ(cl,{
  elev.rst <- raster('data/elevation/wc2.1_10km_elev.tif')
  ecosys.tb <- read.csv('data/ecoregion/bioreg_ll.csv')
  
  xy.cell <- raster('data/Map_World_Polygon/world_rst_bf.tif')
  xy.cell[] <- 1:length(xy.cell[])
  xy.cell <- rasterToPoints(xy.cell)
  colnames(xy.cell)[3] <- 'cell'
  
  rst.cell.poll <- raster('data/CHELSA/10km_climate_cell_land.tif')
  rst.cell.poll <- cellFromXY(raster(),coordinates(rst.cell.poll))
  
  NULL
})


# species to run ----------------------------------------------------------

setwd("E:/LuoA/PRIME/")
spls <- read.csv('splist/spls_outputs.csv')

flag <-  file.exists(spls$path_range %>%  str_replace('data/F_SPP_Output_csv/','output/HSD/') %>% str_remove('mod6/') %>% str_remove('mod10/'))
#flag.num <- which(flag&(!spls$speciesKey%in%spds.1d$speciesKey))
flag.num <- which(flag)

i <- flag.num[1]

# get sp range size in each scenario and dispersal ability -----------------

spds.1d.list <- parLapply(cl,flag.num,function(i){
  id <- spls$speciesKey[i]
  path <- spls$path[i]
  path.out <- spls$path_range[i] %>% str_replace('data/F_SPP_Output_csv/','output/HSD/') %>% str_remove('mod6/') %>% str_remove('mod10/')
  
  spds <- read.csv(path.out)
  colnames(spds)[2] <- 'cell'
  spds$cell_poll <- rst.cell.poll[spds$cell]
  spds <- spds %>% dplyr::select(-cell) %>% distinct()
  spds <- spds[rowSums(spds[,2:29])>0,]
  spds
})
#spds.1d.list <- c(spds.1d.list.raw,spds.1d.list)
spds.1d <- do.call('rbind',spds.1d.list)
length(unique(spds.1d$speciesKey))
#write.csv(spds.1d,'output/summary/cellInfo_1d/sp_cellpoll_all_sn_noClip.csv',row.names = F)
write.csv(spds.1d,'output/summary/cellInfo_1d/sp_cellpoll_all_sn_noClip.csv',row.names = F)

stopCluster(cl)

spds.1d.all <- read.csv('output/summary/cellInfo_10km/spcell_10km_1d_all_cell.csv')
spds.1d.all <- spds.1d.all[!spds.1d.all$cell%in%4991971,] 
spds.1d.all <- spds.1d.all[!spds.1d.all$speciesKey%in%spds.1d$speciesKey,] %>% 
  rename(cell_poll=cell_1d) %>% 
  dplyr::select(speciesKey,cell_poll) %>% 
  distinct()
spds.1d.all <- rbind(spds.1d.all,spds.1d[,c('speciesKey','cell_poll')]) %>% distinct()
write.csv(spds.1d.all,'output/summary/cellInfo_1d/sp_cellpoll_all.csv',row.names = F)
write.csv(spds.1d.all,'F:/luoao/RLI/data/spcell_1d/sp_cellpoll_all.csv',row.names = F)
