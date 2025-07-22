library(sf);library(dplyr)
setwd('E:/LuoA/PRIME/')
spls <- read.csv('splist/spls.csv',encoding='UTF-8')
spls$path <- paste0(getwd(),'/data/F_SPP_Coord/',spls$family,'/',spls$speciesKey,'/',spls$speciesKey,'_coords.csv') 
flag.num <- 1:nrow(spls)

cl <- makeCluster(90)
clusterEvalQ(cl,{
  setwd('E:/LuoA/PRIME/')
  library(dplyr)
  spls <- read.csv('splist/spls.csv',encoding='UTF-8')
  spls$path <- paste0(getwd(),'/data/F_SPP_Coord/',spls$family,'/',spls$speciesKey,'/',spls$speciesKey,'_coords.csv') 
  NULL
})


#test <- parLapply(cl,1:nrow(spls), function(i){
test <- parLapply(cl,flag.num, function(i){
  print(i)
  if(file.exists(spls$path[i])) unlink(spls$path[i])
  if(!file.exists(spls$path_out[i])) return(data.frame(sp=spls$species[i],x=NA,y=NA))
  
  spds <- read.csv(spls$path_out[i])
  spds <- spds[!is.na(spds$decimalLatitude),]
  spds <- spds[abs(spds$decimalLatitude-spds$decimalLongitude)>0.0001,]
  
  if(nrow(spds)<1) return(data.frame(sp=spls$species[i],x=NA,y=NA))
  
  # spds2 <- spds %>% filter(!is_alien) %>% filter(!is_outlier) %>% 
  #   filter(gbif_mark) %>% filter(region_mark)
  # spds2 <- spds2[,c('decimalLongitude','decimalLatitude')]
  
  
  spds2 <- spds %>% filter(!is_alien) %>% filter(!is_outlier) %>% filter(gbif_mark)
  spds2 <- spds2 %>% filter(!institutionCode%in%c("PKU_CHN",'PKU_EA'))
  
  flag <- spds2$region_wcvp_mark&spds2$region_pku_mark
  if(sum(flag)>20){
    spds2 <- spds2 %>% filter(flag|institutionCode%in%c("PKU"))
  } else{
    spds2 <- spds2 %>% filter(region_mark|institutionCode%in%c("PKU"))
  }
  
  spds2 <- spds2[,c('decimalLongitude','decimalLatitude')]
  if(nrow(spds2)<1) return(data.frame(sp=spls$species[i],x=NA,y=NA))
  
  write.csv(spds2,spls$path[i],row.names = F)
  tmp <- data.frame(sp=spls$species[i],x=spds2[,'decimalLongitude'],y=spds2[,'decimalLatitude'])
  return(tmp)
})

stopCluster(cl)

#### keep species in restricted area

library(sf);library(dplyr)
setwd('E:/LuoA/PRIME/')
spls <- read.csv('splist/spls_mark.csv',encoding='UTF-8')
flag <- lapply(test, function(x)all(is.na(x[,2]))) %>% unlist();table(flag)
flag.num <- flag.num[flag]
#flag <- which((spls$num>0)&(spls$num<=5)&(spls$num_coord==0))
# flag <- read.csv('splist/tmp_ea_spls.csv',encoding='UTF-8')
# flag <- spls$speciesKey%in%flag$speciesKey
# flag.num <- which((spls$num>0)&(spls$num_coord==0)&flag)

cl <- makeCluster(90)
clusterEvalQ(cl,{
  setwd('E:/LuoA/PRIME/')
  library(dplyr);library(raster)
  spls <- read.csv('splist/spls_mark.csv',encoding='UTF-8')
  #rst <- raster()
  #spls <- read.csv('splist/spls.csv',encoding='UTF-8')
  #spls$path <- paste0(getwd(),'/data/F_SPP_Coord/',spls$family,'/',spls$speciesKey,'/',spls$speciesKey,'_coords.csv') 
  NULL
})

test <- parLapply(cl,flag.num, function(i){
  print(i)
  if(file.exists(spls$path[i])) unlink(spls$path[i])
  if(!file.exists(spls$path_out[i])) return(data.frame(sp=spls$species[i],x=NA,y=NA))
  
  spds <- read.csv(spls$path_out[i])
  spds <- spds[!is.na(spds$decimalLatitude),]
  spds <- spds[abs(spds$decimalLatitude-spds$decimalLongitude)>0.0001,]
  
  if(nrow(spds)<1) return(data.frame(sp=spls$species[i],x=NA,y=NA))
  
  spds2 <- spds %>% filter(!is_alien) %>% filter(!is_outlier) %>% filter(gbif_mark)
  
  
  if(nrow(spds2)<1) return(data.frame(sp=spls$species[i],x=NA,y=NA))
  
  if((diff(range(spds2$decimalLatitude))<3)&(diff(range(spds2$decimalLongitude))<3)){
    spds2 <- spds2[,c('decimalLongitude','decimalLatitude')]
    write.csv(spds2,spls$path[i],row.names = F)
  }
  
  tmp <- data.frame(sp=spls$species[i],x=spds2[,'decimalLongitude'],y=spds2[,'decimalLatitude'])
  return(tmp)
})


stopCluster(cl)

#### keep species in very restricted area

# library(sf);library(dplyr)
# setwd('E:/LuoA/PRIME/')
# spls <- read.csv('splist/spls_mark.csv',encoding='UTF-8')
# #flag <- which((spls$num>0)&(spls$num<=5)&(spls$num_coord==0))
# flag <- which((spls$num>0)&(spls$num_coord==0))
# 
# cl <- makeCluster(90)
# clusterEvalQ(cl,{
#   setwd('E:/LuoA/PRIME/')
#   library(dplyr);library(raster)
#   spls <- read.csv('splist/spls_mark.csv',encoding='UTF-8')
#   #rst <- raster()
#   #spls <- read.csv('splist/spls.csv',encoding='UTF-8')
#   #spls$path <- paste0(getwd(),'/data/F_SPP_Coord/',spls$family,'/',spls$speciesKey,'/',spls$speciesKey,'_coords.csv') 
#   NULL
# })
# 
# test <- parLapply(cl,flag, function(i){
#   print(i)
#   if(file.exists(spls$path[i])) unlink(spls$path[i])
#   if(!file.exists(spls$path_out[i])) return(data.frame(sp=spls$species[i],x=NA,y=NA))
#   
#   spds <- read.csv(spls$path_out[i])
#   spds <- spds[!is.na(spds$decimalLatitude),]
#   spds <- spds[abs(spds$decimalLatitude-spds$decimalLongitude)>0.0001,]
#   
#   if(nrow(spds)<1) return(data.frame(sp=spls$species[i],x=NA,y=NA))
#   
#   spds2 <- spds  %>% filter(!is_outlier)#%>% filter(!is_alien) %>% filter(!is_outlier) %>% filter(gbif_mark)
#   
#   
#   if(nrow(spds2)<1) return(data.frame(sp=spls$species[i],x=NA,y=NA))
#   
#   if((diff(range(spds2$decimalLatitude))<1)&(diff(range(spds2$decimalLongitude))<1)){
#     spds2 <- spds2[,c('decimalLongitude','decimalLatitude')]
#     write.csv(spds2,spls$path[i],row.names = F)
#   }
#   
#   tmp <- data.frame(sp=spls$species[i],x=spds2[,'decimalLongitude'],y=spds2[,'decimalLatitude'])
#   return(tmp)
# })
# 
# stopCluster(cl)
