setwd('E:/LuoA/')
library(dplyr)
#####################################################
###### create archive, this is new version. extract from GBIF archive
#####################################################
spls <- read.csv('GBIF/spls/spls.csv',encoding = 'UTF-8')
#load('GBIF/spls_gbif_row.rdata')
spls <- spls %>% #filter(taxonRank%in%"SPECIES") %>%
  mutate(speciesKey = as.numeric(speciesKey))%>% filter(!is.na(speciesKey)) #filter(!family%in%"") %>% filter(!is.na(speciesKey))
flag <- spls$family%in%'';spls$family[flag] <- 'NA'
#spls <- spls[flag,]

fas <- unique(spls$family)
for (fa in fas) {
  if(!dir.exists(paste0('PRIME/data/F_SPP_Data_Archive/',fa))){
    dir.create(paste0('PRIME/data/F_SPP_Data_Archive/',fa))
  }
}

for (i in 1:nrow(spls)) {
  if(i%%10000==1) print(i)
  dir.create(paste0('PRIME/data/F_SPP_Data_Archive/',spls$family[i],'/',spls$speciesKey[i]))
}

nafl <- data.frame(gbifID=NA,decimalLatitude=NA,decimalLongitude=NA,coordinateUncertaintyInMeters=NA,
                   year=NA,speciesKey=NA,institutionCode=NA )
for (i in 1:nrow(spls)) {
  if(i%%10000==1) print(i)
  #unlink(paste0('PRIME/data/F_SPP_Data_Archive/',spls$family[i],'/',spls$speciesKey[i],'/',spls$speciesKey[i],'.csv'))
  write.csv(nafl,paste0('PRIME/data/F_SPP_Data_Archive/',spls$family[i],'/',spls$speciesKey[i],'/',spls$speciesKey[i],'.csv'),row.names = F)
}

#### write data, from GBIF to archive
spMatch <- read.csv('GBIF/spls/sp_match_GBIF.csv')
spMatch$path_gbif <- paste0(getwd(),'/GBIF/F_SPP_Data_Archive/',spMatch$family,'/',spMatch$speciesKey_0,'/',spMatch$speciesKey_0,'.csv')
flag <- lapply(spMatch$path_gbif, file.exists) %>% unlist
spMatch <- spMatch[flag,]
spMatch$path_archive <- paste0(getwd(),'/PRIME/data/F_SPP_Data_Archive/',spMatch$family,'/',spMatch$speciesKey,'/',spMatch$speciesKey,'.csv')
flag <- lapply(spMatch$path_archive, file.exists) %>% unlist
spMatch <- spMatch[flag,]

test <- lapply(1:nrow(spMatch),function(i){
  if(i%%1000==0) print(i)
  print(i)
  Sys.time() %>% print()
  path_archive <- spMatch$path_archive[i]
  path_gbif <- spMatch$path_gbif[i]
  tmp <- read.csv(path_gbif,encoding = 'UTF-8')
  if(!all(is.na(tmp$decimalLatitude))){
    dat <- read.csv(path_archive)#,encoding = 'UTF-8')
    dat <- rbind(dat,tmp) %>% distinct() %>% filter(!is.na(decimalLatitude))
    write.csv(dat,path_archive,row.names = F)#,fileEncoding = 'UTF-8')
    return(nrow(tmp))
  }else{
    return(0)
  }
})

library(parallel)
cl <- makeCluster(64)
clusterEvalQ(cl,{
  library(dplyr)
  setwd('E:/LuoA/')
  return(0)
})
clusterExport(cl,'spMatch')

test <- parLapply(cl,1:nrow(spMatch),function(i){
  tryCatch({
    path_archive <- spMatch$path_archive[i]
    path_gbif <- spMatch$path_gbif[i]
    tmp <- read.csv(path_gbif)#,encoding = 'UTF-8')
    if(!all(is.na(tmp$decimalLatitude))){
      dat <- read.csv(path_archive)#,encoding = 'UTF-8')
      dat <- rbind(dat,tmp) %>% distinct() %>% filter(!is.na(decimalLatitude))
      write.csv(dat,path_archive,row.names = F)#,fileEncoding = 'UTF-8')
      return(nrow(tmp))
    }else{
      return(0)
    }
  },error=function(e){
    print(e)
    return(i)
  })
})
stopCluster(cl)
{
  tmp <- read.csv(paste0('GBIF/GBIF_LIST/',fl),encoding = 'UTF-8') %>% filter(!occurrenceStatus%in%'ABSENT') %>%
    dplyr::select(-X,-occurrenceStatus,-rightsHolder,-scientificName)
  tmp <- tmp[!is.na(tmp$decimalLatitude),]
  tmp <- tmp[!tmp$species%in%'',]
  tmp$species <- stringr::str_replace_all(tmp$species,'???','x')
  tmp$speciesKey <- as.numeric(tmp$speciesKey)
  tmp <- tmp[tmp$speciesKey%in%spls$speciesKey,]
  
  sps <- data.frame(speciesKey=unique(tmp$speciesKey))
  sps <- left_join(sps,spls)
  fas <- unique(sps$family)
  rcd <- data.frame(NULL)
  
  for(fa in fas){
    tmp_fa <- tmp[tmp$speciesKey%in%sps$speciesKey[sps$family%in%fa],]
    print(Sys.time())
    print(paste0(fl,": ",fa))
    sps_fa <- sps[sps$family%in%fa,]
    
    tryCatch({
      for (i in 1:nrow(sps_fa)) {
        id <- sps_fa$speciesKey[i]
        sp <- sps_fa$species[i]
        #fa <- sps_fa$family[i]
        if(!file.exists(paste0('PRIME/F_SPP_Data_Archive/',fa,'/',id,'/',id,'.csv'))) next
        spds <- read.csv( paste0('PRIME/F_SPP_Data_Archive/',fa,'/',id,'/',id,'.csv') )
        spds <- rbind(spds,tmp_fa[tmp_fa$species%in%sp,colnames(spds)])%>%distinct()%>%filter(!is.na(decimalLatitude))
        write.csv(spds,paste0('PRIME/F_SPP_Data_Archive/',fa,'/',id,'/',id,'.csv'),row.names=F)
        if(i==24) dada
      }
      rcd <- rbind(rcd,data.frame(fl=fl,fa=fa,status=TRUE,i=i,is=sum(nrow(sps_fa))))
    }, error = function(e) {
      print(e)
      #rcd <- rbind(rcd,data.frame(fl=fl,fa=fa,status=FALSE,i=i,is=sum(nrow(sps_fa))))
    })
  }
  return(data.frame(fl=fl,fa=fas,status=fas%in%rcd$fa))
  # for(fa in fas){
  #   tmp_fa <- tmp[tmp$speciesKey%in%sps$speciesKey[sps$family%in%fa],]
  #   print(Sys.time())
  #   print(paste0(fl,": ",fa))
  #   sps_fa <- sps[sps$family%in%fa,]
  #   for (i in 1:nrow(sps_fa)) {
  #     id <- sps_fa$speciesKey[i]
  #     sp <- sps_fa$species[i]
  #     #fa <- sps_fa$family[i]
  #     if(!file.exists(paste0('PRIME/F_SPP_Data_Archive/',fa,'/',id,'/',id,'.csv'))) next
  #     spds <- read.csv( paste0('PRIME/F_SPP_Data_Archive/',fa,'/',id,'/',id,'.csv') )
  #     spds <- rbind(spds,tmp_fa[tmp_fa$species%in%sp,colnames(spds)])%>%distinct()%>%filter(!is.na(decimalLatitude))
  #     write.csv(spds,paste0('PRIME/F_SPP_Data_Archive/',fa,'/',id,'/',id,'.csv'),row.names=F)
  #   }
  # }
  # return(0)
})
stopCluster(cl)
# for(fl in fls){
#   tmp <- read.csv(paste0('GBIF/GBIF_LIST/',fl),encoding = 'UTF-8') %>% filter(!occurrenceStatus%in%'ABSENT') %>%
#     dplyr::select(-X,-occurrenceStatus,-rightsHolder,-scientificName)
#   tmp <- tmp[!is.na(tmp$decimalLatitude),]
#   tmp <- tmp[!tmp$species%in%'',]
#   tmp$species <- stringr::str_replace_all(tmp$species,'???','x')
#   tmp <- tmp[tmp$speciesKey%in%spls$speciesKey,]
#   
#   sps <- data.frame(speciesKey=unique(tmp$speciesKey))
#   sps <- left_join(sps,spls)
#   fas <- unique(sps$family)
# 
#   for(fa in fas){
#     tmp_fa <- tmp[tmp$speciesKey%in%sps$speciesKey[sps$family%in%fa],]
#     print(Sys.time())
#     print(paste0(fl,": ",fa))
#     sps_fa <- sps[sps$family%in%fa,]
#     
#     for (i in 1:nrow(sps_fa)) {
#       id <- sps_fa$speciesKey[i]
#       sp <- sps_fa$species[i]
#       #fa <- sps_fa$family[i]
#       if(!file.exists(paste0('PRIME/F_SPP_Data_Archive/',fa,'/',id,'/',id,'.csv'))) next
#       spds <- read.csv( paste0('PRIME/F_SPP_Data_Archive/',fa,'/',id,'/',id,'.csv') )
#       spds <- rbind(spds,tmp_fa[tmp_fa$species%in%sp,colnames(spds)])%>%distinct()%>%filter(!is.na(decimalLatitude))
#       write.csv(spds,paste0('PRIME/F_SPP_Data_Archive/',fa,'/',id,'/',id,'.csv'),row.names=F)
#     }
#     
#   }
# }
for(fl in fls[18:35]){
  tmp <- read.csv(paste0('GBIF/GBIF_LIST/',fl),encoding = 'UTF-8') %>% filter(!occurrenceStatus%in%'ABSENT') %>%
    dplyr::select(-X,-occurrenceStatus,-rightsHolder,-scientificName)
  tmp <- tmp[!is.na(tmp$decimalLatitude),]
  tmp <- tmp[!tmp$species%in%'',]
  tmp$species <- stringr::str_replace_all(tmp$species,'???','x')
  if(class(tmp$speciesKey)=='character') tmp$speciesKey <- as.numeric(tmp$speciesKey)
  if(class(tmp$speciesKey)=='factor') tmp$speciesKey <- as.numeric(as.character(tmp$speciesKey))
  tmp <- tmp[tmp$speciesKey%in%spls$speciesKey,]
  
  sps <- data.frame(speciesKey=unique(tmp$speciesKey))
  sps <- left_join(sps,spls)
  fas <- unique(sps$family)
  
  for(fa in fas){
    tmp_fa <- tmp[tmp$speciesKey%in%sps$speciesKey[sps$family%in%fa],]
    print(Sys.time())
    print(paste0(fl,": ",fa))
    sps_fa <- sps[sps$family%in%fa,]
    for (i in 1:nrow(sps_fa)) {
      id <- sps_fa$speciesKey[i]
      sp <- sps_fa$species[i]
      #fa <- sps_fa$family[i]
      if(!file.exists(paste0('PRIME/F_SPP_Data_Archive/',fa,'/',id,'/',id,'.csv'))) next
      spds <- read.csv( paste0('PRIME/F_SPP_Data_Archive/',fa,'/',id,'/',id,'.csv') )
      spds <- rbind(spds,tmp_fa[tmp_fa$species%in%sp,colnames(spds)])%>%distinct()%>%filter(!is.na(decimalLatitude))
      write.csv(spds,paste0('PRIME/F_SPP_Data_Archive/',fa,'/',id,'/',id,'.csv'),row.names=F)
    }
  }
}
