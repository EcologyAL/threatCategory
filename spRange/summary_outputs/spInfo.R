setwd("E:/LuoA/PRIME/")
library(raster);library(dplyr);library(alphahull);library(parallel);library(stringr)


# set parallel ------------------------------------------------------------


cl <- makeCluster(100)
clusterEvalQ(cl,{
  setwd("E:/LuoA/PRIME/")
  library(raster);library(dplyr);library(alphahull);library(parallel);library(stringr)
  spls <- read.csv('splist/spls_outputs.csv')
  #rst.land <- raster('data/CHELSA/10km_climate_cell_land.tif')
  #rook0 <- sqrt(matrix((1:37 - 19)^2, ncol=37,nrow=37,byrow=T) + matrix((1:37 - 19)^2, ncol=37, nrow=37, byrow=F)) 
  
  #rook6 <- rook0 <= 6 + sqrt(0.5)-0.5;rook6[!rook6] <- NA;rook6[19,19] <- FALSE;rook6 <- rook6[(19-6):(19+6),(19-6):(19+6)]
  #rook12 <- rook0 <= 12 +  sqrt(0.5)-0.5;rook12[!rook12] <- NA;rook12[19,19] <- FALSE;rook12 <- rook12[(19-12):(19+12),(19-12):(19+12)]
  #rook18 <- rook0 <= 18 + sqrt(0.5)-0.5;rook18[!rook18] <- NA;rook18[19,19] <- FALSE;rook18 <- rook18[(19-18):(19+18),(19-18):(19+18)]
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
  sp.info <- c(speciesKey=spls$speciesKey[i],sp.info)
  return(sp.info)
})
sp.info <- do.call('rbind',splist)
rm(splist)
write.csv(sp.info,'output/summary/spInfo/spInfo.csv',row.names=F)

cluster()

# list file path ----------------------------------------------------------


# setwd("E:/LuoA/PRIME/")
# spls <- read.csv('splist/spls_mark.csv')
# spls$path_range <- NA
# tmp <- paste0(getwd(),'/data/F_SPP_Output_csv/mod6/',spls$speciesKey,'.csv')
# flag <- tmp%in%list.files(paste0(getwd(),'/data/F_SPP_Output_csv/mod6/'),full.names = T) 
# spls$path_range[flag] <- tmp[flag]
# 
# tmp <- paste0(getwd(),'/data/F_SPP_Output_csv/mod10/',spls$speciesKey,'.csv')
# flag <- tmp%in%list.files(paste0(getwd(),'/data/F_SPP_Output_csv/mod10/'),full.names = T) 
# spls$path_range[flag] <- tmp[flag]
# 
# write.csv(spls,'splist/spls_outputs.csv',row.names = F)

sn_ds <- rbind(paste0(colnames(spds)[2:10],'_DS_unlimit.csv'), 
      paste0(colnames(spds)[2:10],'_DS_limit.csv'), 
      paste0(colnames(spds)[2:10],'_DS_no.csv'))