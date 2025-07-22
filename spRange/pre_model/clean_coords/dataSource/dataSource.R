setwd('E:/LuoA/PRIME/')
spls <- read.csv('splist/spls.csv',encoding='UTF-8')
spls <- spls[spls$num_clean>0,]

cl <- makeCluster(80)
clusterEvalQ(cl,{
  setwd('E:/LuoA/PRIME/')
  library(dplyr)
  spls <- read.csv('splist/spls.csv',encoding='UTF-8')
  NULL
})
flag <- 1:nrow(spls)
flag <- which(spls$speciesKey%in%test)
data.source <- parLapply(cl,flag, function(i){
  print(i)
  spds <- read.csv(spls$path_out[i])
  spds <- spds[!is.na(spds$decimalLatitude),]
  spds <- spds[abs(spds$decimalLatitude-spds$decimalLongitude)>0.001,]
  
  if(nrow(spds)<1) return(NULL)
  # spds1 <- spds %>% filter(!is_alien) %>% filter(!is_outlier) %>% 
  #   filter(gbif_mark)
  spds1 <- spds %>% mutate(flag = (!is_alien)&(!is_outlier)&(gbif_mark)&(region_mark)) 
  
  if(nrow(spds1)<1) return(NULL)
  
  #tmp <- data.frame(sp=spls$species[i],cell=cellFromXY(rst,spds1[,c('decimalLongitude','decimalLatitude')])) %>% distinct()
  tmp <- data.frame(institutionCode=spds1$institutionCode,flag=spds1$flag) #%>% distinct()
  
  if(ncol(tmp)<=0) return(NULL)
  return(tmp)
})

stopCluster(cl)
data.source <- do.call('rbind',data.source)
data.source.s <- data.source %>% group_by(institutionCode) %>% mutate(is.n=sum(flag),n=n()) %>% 
  ungroup() %>% dplyr::select(-flag) %>% distinct() 
data.source.s$p <- (data.source.s$n-data.source.s$is.n)/data.source.s$n
write.csv(data.source.s,'data/dataSource/dataSource.csv',row.names = F)
data.source.s[(data.source.s$p==1),] 
