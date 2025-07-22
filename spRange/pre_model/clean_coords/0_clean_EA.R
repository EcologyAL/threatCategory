setwd('E:/LuoA/')
library(dplyr)
#spds.ea <- read.csv('PRIME/data/EA_spds/points_clean_20231225.csv')
spds.ea <- read.csv('PRIME/data/EA_spds/points_clean_20240830.csv')
spMatch <- read.csv('GBIF/spls/sp_match_GBIF.csv',encoding='UTF-8')

#spMatch.ea <- spMatch[spMatch$species%in%spds.ea$species,]
#spMatch.ea <- spMatch[(spMatch$species%in%spds.ea$species)&(!spMatch$verbatim_name%in%spds.ea$species),]
spds.ea.id <- spds.ea %>% left_join(spMatch[,c('species','speciesKey')]%>%distinct())
spds.ea.id <- spds.ea.id %>% left_join(spMatch[,c('verbatim_name','speciesKey')]%>%distinct()%>%
                                         rename(species=verbatim_name,speciesKey2=speciesKey))
spds.ea.id$speciesKey[is.na(spds.ea.id$speciesKey)] <- spds.ea.id$speciesKey2[is.na(spds.ea.id$speciesKey)]
spds.ea.id <- spds.ea.id %>% filter(!is.na(speciesKey))
spds.ea.id <- spds.ea.id %>% filter(!is.na(speciesKey)) %>% distinct

spMatch$path_archive <- paste0(getwd(),'/PRIME/data/F_SPP_Data_Archive/',spMatch$family,'/',spMatch$speciesKey,'/',spMatch$speciesKey,'.csv')
flag <- lapply(spMatch$path_archive, file.exists) %>% unlist
spMatch <- spMatch[flag,]
spMatch.ea <- spMatch %>% dplyr::select(speciesKey,path_archive) %>% filter(speciesKey %in% spds.ea.id$speciesKey) %>% 
  filter(!is.na(speciesKey)) %>% distinct

# test <- lapply(1:nrow(spMatch.ea),function(i){
#   #if(i%%1000==0) print(i)
#   print(i)
#   path_archive <- spMatch$path_archive[i]
#   tmp <- spds.ea.id[spds.ea.id$speciesKey%in%spMatch.ea$speciesKey[i],]
#   tmp <- data.frame(gbifID=NA,decimalLatitude=tmp$decimalLatitude,decimalLongitude=tmp$decimalLatitude,
#                     coordinateUncertaintyInMeters=NA,year=2023,
#                     speciesKey=tmp$speciesKey,institutionCode=tmp$source) %>% filter(!is.na(decimalLatitude))
#   
#   dat <- read.csv(path_archive,encoding = 'UTF-8')
#   dat <- dat[!dat$institutionCode%in%c("PKU_EA","PKU_CHN","PKU"),]
#   dat <- rbind(dat,tmp) %>% distinct() %>% filter(!is.na(decimalLatitude))
#   write.csv(dat,path_archive,row.names = F,fileEncoding = 'UTF-8')
#   return(nrow(tmp))
# })

# parallel
library(parallel)
cl <- makeCluster(80)

clusterEvalQ(cl,{
  setwd('E:/LuoA/')
  library(dplyr)
  spds.ea <- read.csv('PRIME/data/EA_spds/points_clean_20240830.csv')
  spMatch <- read.csv('GBIF/spls/sp_match_GBIF.csv',encoding='UTF-8')

  spds.ea.id <- spds.ea %>% left_join(spMatch[,c('species','speciesKey')]%>%distinct())
  spds.ea.id <- spds.ea.id %>% left_join(spMatch[,c('verbatim_name','speciesKey')]%>%distinct()%>%
                                           rename(species=verbatim_name,speciesKey2=speciesKey))
  spds.ea.id$speciesKey[is.na(spds.ea.id$speciesKey)] <- spds.ea.id$speciesKey2[is.na(spds.ea.id$speciesKey)]
  #spds.ea.id <- spds.ea.id %>% filter(!is.na(speciesKey))
  spds.ea.id <- spds.ea.id %>% filter(!is.na(speciesKey)) %>% distinct
  
  spMatch$path_archive <- paste0(getwd(),'/PRIME/data/F_SPP_Data_Archive/',spMatch$family,'/',spMatch$speciesKey,'/',spMatch$speciesKey,'.csv')
  flag <- lapply(spMatch$path_archive, file.exists) %>% unlist
  spMatch <- spMatch[flag,]
  spMatch.ea <- spMatch %>% dplyr::select(speciesKey,path_archive) %>% filter(speciesKey %in% spds.ea.id$speciesKey) %>% 
    filter(!is.na(speciesKey)) %>% distinct
  return(NULL)
})

i <- 1
test <- parLapply(cl,1:nrow(spMatch.ea),function(i){
#test <- parLapply(cl,1:42837,function(i){
  #if(i%%1000==0) print(i)
  print(i)
  path_archive <- spMatch.ea$path_archive[i]
  tmp <- spds.ea.id[spds.ea.id$speciesKey%in%spMatch.ea$speciesKey[i],]
  tmp <- data.frame(gbifID=NA,decimalLatitude=tmp$decimalLatitude,decimalLongitude=tmp$decimalLongitude,
                    coordinateUncertaintyInMeters=NA,year=2023,
                    speciesKey=tmp$speciesKey,institutionCode=tmp$source) %>% filter(!is.na(decimalLatitude))
  
  dat <- read.csv(path_archive,encoding = 'UTF-8')
  dat <- dat[!dat$institutionCode%in%c("PKU_EA","PKU_CHN","PKU"),]
  dat <- rbind(dat,tmp) %>% distinct() %>% filter(!is.na(decimalLatitude))
  write.csv(dat,path_archive,row.names = F,fileEncoding = 'UTF-8')
  return(nrow(tmp))
})

# library(stringr)
# test <- spMatch.ea
# for (test in 1:nrow(test)) {
#   
# }