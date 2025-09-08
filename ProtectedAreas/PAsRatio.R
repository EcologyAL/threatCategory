setwd("E:/LuoA/PRIME/")
library(raster);library(dplyr);library(alphahull);library(parallel);library(stringr)


# set parallel ------------------------------------------------------------


cl <- makeCluster(100)
clusterEvalQ(cl,{
  setwd("E:/LuoA/PRIME/")
  library(raster);library(dplyr);library(alphahull);library(parallel);library(stringr)
  spls <- read.csv('splist/spls_outputs.csv')
  PAs <- read.csv('F:/luoao/RLI/data/PAs/PA_cell_10km.csv')
  return(NULL)
})


# species to run ----------------------------------------------------------


spls <- read.csv('splist/spls_outputs.csv')
flag.num <- which(!is.na(spls$path_range))

# get sp range size in each scenario and dispersal ability -----------------

splist <- parLapply(cl,flag.num,function(i){
  path <- spls$path_range[i]
  path.out <- path %>% str_replace('data/F_SPP_Output_csv/','output/HSD/') %>% str_remove('mod6/') %>% str_remove('mod10/')
  
  sp.range <- read.csv(path.out)
  
  # summary sp range
  sp.info <- sp.range[,-c(1,2)] %>% colSums()
  #sp.info <- c(speciesKey=spls$speciesKey[i],sp.info)
  sp.info.PAs <- sp.range[sp.range$cell%in%PAs$cell,-c(1,2)] %>% colSums()
  #sp.info.PAs <- c(speciesKey=spls$speciesKey[i],sp.info.PAs)
  sp.info <- sp.info.PAs/sp.info %>% as.matrix() %>% t() %>% as.data.frame()
  sp.info <- data.frame(speciesKey=spls$speciesKey[i],sp.info)
  
  return(sp.info)
})
sp.info <- do.call('rbind',splist)
rm(splist)
write.csv(sp.info,'output/summary/spInfo/spInfo_PA_ratio.csv',row.names=F)
#sp.info <- sp.info[sp.info$sn_1981.2010,]

splist <- parLapply(cl,flag.num,function(i){
  path <- spls$path_range[i]
  path.out <- path %>% str_replace('data/F_SPP_Output_csv/','output/HSD/') %>% str_remove('mod6/') %>% str_remove('mod10/')
  
  sp.range <- read.csv(path.out)
  
  # summary sp range
  sp.info <- sp.range[,-c(1,2)] %>% colSums()
  #sp.info <- c(speciesKey=spls$speciesKey[i],sp.info)
  sp.info.PAs <- sp.range[sp.range$cell%in%PAs$cell,-c(1,2)] %>% colSums()
  #sp.info.PAs <- c(speciesKey=spls$speciesKey[i],sp.info.PAs)
  sp.info <- sp.info.PAs/sp.info %>% as.matrix() %>% t() %>% as.data.frame()
  sp.info <- data.frame(speciesKey=spls$speciesKey[i],sp.info)
  
  return(sp.info)
})
sp.info <- do.call('rbind',splist)
rm(splist)

spds.10km <- read.csv('output/summary/cellInfo_10km/spcell_10km_1d_all_cell.csv')
spds.10km <- spds.10km[!spds.10km$speciesKey%in%sp.info$speciesKey,] %>% dplyr::select(speciesKey,cell) %>% distinct()
spds.10km$PAs <- spds.10km$cell%in%PAs$cell
spds.10km <- spds.10km %>% group_by(speciesKey) %>% 
  mutate(PAsRatio = sum(PAs)/n()) %>% ungroup() %>% 
  dplyr::select(speciesKey,PAsRatio) %>% distinct()
sp.pas <- sp.info %>% rename(PAsRatio=sn_1981.2010) %>% 
  dplyr::select(speciesKey,PAsRatio) %>% rbind(spds.10km)
write.csv(sp.pas,'F:/luoao/RLI/data/spinfo/sp_PAs_ratio.csv',row.names = F)
