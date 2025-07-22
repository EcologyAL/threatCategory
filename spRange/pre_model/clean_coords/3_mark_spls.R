####======================================
#### create files
####======================================
setwd('E:/LuoA/PRIME/')
spls <- read.csv('splist/spls.csv',encoding='UTF-8')
# path1 <- paste0(getwd(),'/data/F_SPP_Coord/',spls$family)
# path2 <- paste0(getwd(),'/data/F_SPP_Coord/',spls$family,'/',spls$speciesKey) 
# lapply(unique(path1), function(x){if(!dir.exists(x)) dir.create(x)})
# lapply(unique(path2), function(x){if(!dir.exists(x)) dir.create(x)})
library(dplyr)
library(parallel)
cl <- makeCluster(100)
clusterEvalQ(cl,{
  setwd('E:/LuoA/PRIME/')
  library(dplyr)
  spls <- read.csv('splist/spls.csv',encoding='UTF-8')
  NULL
})
test <- parLapply(cl,1:nrow(spls), function(i){
  #test <- parLapply(cl,1:30, function(i){
  print(i)
  spds <- read.csv(spls$path_out[i])
  spds <- spds[!is.na(spds$decimalLatitude),]
  if(nrow(spds)<1) return(NULL)
  spds1 <- spds %>% filter(!is_alien) %>% filter(!is_outlier) %>% 
    filter(gbif_mark)
  spds2 <- spds %>% filter(!is_alien) %>% filter(!is_outlier) %>% 
    filter(gbif_mark) %>% filter(region_mark)
  tmp <- data.frame(species=spls$species[i],family=spls$family[i],
                    num_ori=nrow(spds),num_1=nrow(spds1),num_2=nrow(spds2))
  return(tmp)
})
test.s <- do.call('rbind',test)

stopCluster(cl)

spls <- left_join(spls,test.s)
spls <- spls[!is.na(spls$num_ori),]

sum(spls$num_ori>0)
sum(spls$num_1>0)
sum(spls$num_2>0)

###############################################################################
###############################################################################
setwd('E:/LuoA/PRIME/')
spls <- read.csv('splist/spls.csv',encoding='UTF-8')
spls$path <- paste0(getwd(),'/data/F_SPP_Coord/',spls$family,'/',spls$speciesKey,'/',spls$speciesKey,'_coords.csv') 
spls$num <- 0
spls$num_coords <- 0

library(dplyr);library(parallel)
cl <- makeCluster(95)
clusterEvalQ(cl,{
  setwd('E:/LuoA/PRIME/')
  library(dplyr)
  spls <- read.csv('splist/spls.csv',encoding='UTF-8')
  spls$path <- paste0(getwd(),'/data/F_SPP_Coord/',spls$family,'/',spls$speciesKey,'/',spls$speciesKey,'_coords.csv') 
  spls$num <- 0
  spls$num_coords <- 0
  NULL
})

flag <- file.exists(spls$path_archive)
flag <- which(flag)

spinfo <- parLapply(cl,flag, function(i){
  #test <- parLapply(cl,1:30, function(i){
  #print(i)
  
  spds <- read.csv(spls$path_archive[i])
  spds <- spds[!is.na(spds$decimalLatitude),]
  spls$num[i] <- nrow(spds)
  
  if(file.exists(spls$path[i])){
    spds <- read.csv(spls$path[i])
    spds <- spds[!is.na(spds$decimalLatitude),]
    spls$num_coords[i] <- nrow(spds)
  }else{
    spls$num_coords[i] <- 0
  }
  
  return(spls[i,])
})
spinfo.s <- do.call('rbind',spinfo)

stopCluster(cl)

# sum((spinfo.s$num>0)&(spinfo.s$num_coords==0))
# sum((spinfo.s$num>0))
# sum((spinfo.s$num_coords>0))
write.csv(spinfo.s,'splist/spls_mark.csv',row.names = F)
