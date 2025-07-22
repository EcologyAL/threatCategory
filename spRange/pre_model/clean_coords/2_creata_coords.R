setwd('E:/LuoA/PRIME/')
spls <- read.csv('splist/spls_mark.csv',encoding='UTF-8')

library(parallel)
cl <- makeCluster(90)
clusterEvalQ(cl,{
  setwd('E:/LuoA/PRIME/')
  library(dplyr)
  spls <- read.csv('splist/spls_mark.csv',encoding='UTF-8')
  NULL
})
flag <- which(((spls$num>3)&(spls$num_coords==0)))

test <- parLapply(cl,flag, function(i){
  #test <- parLapply(cl,1:30, function(i){
  print(i)
  spds <- read.csv(spls$path_out[i])
  spds <- spds[!is.na(spds$decimalLatitude),]
  
  if(nrow(spds)<1) return(data.frame(species=spls$species[i],family=spls$family[i],
                                            num_ori=0,num_clean=0))
  spds2 <- spds %>% filter(!is_alien) %>% filter(!is_outlier) %>% 
    filter(gbif_mark) %>% filter(!institutionCode%in%c("PKU_CHN",'PKU_EA')) #%>% filter(region_mark)
  
  tmp <- data.frame(species=spls$species[i],family=spls$family[i],
                    num_ori=nrow(spds),num_clean=nrow(spds2))
  return(tmp)
})
test.s <- do.call('rbind',test)

spds <- parLapply(cl,flag, function(i){
  #test <- parLapply(cl,1:30, function(i){
  print(i)
  spds <- read.csv(spls$path_out[i])
  spds <- spds[!is.na(spds$decimalLatitude),]
  if(nrow(spds)<1) return(data.frame(sp=spls$species[i],x=NA,y=NA))
  
  spds2 <- spds %>% filter(!is_alien) %>% filter(!is_outlier) %>% 
    filter(gbif_mark) %>% filter(!institutionCode%in%c("PKU_CHN",'PKU_EA'))#%>% filter(region_mark)
  
  if(nrow(spds2)<1) return(data.frame(sp=spls$species[i],x=NA,y=NA))
  tmp <- data.frame(sp=spls$species[i],x=spds2[,c('decimalLongitude')],y=spds2[,c('decimalLatitude')])
  
  return(tmp)
})
spds <- do.call('rbind',spds)

rst <- raster()
spds$cell <- cellFromXY(rst,spds[,c('x','y')])
spds <- spds[,c('cell','sp')]  %>% distinct() %>% group_by(cell) %>% mutate(sr=n()) %>% 
  ungroup() %>% dplyr::select(-sp) %>% distinct()
spds <- na.omit(spds)
rst[spds$cell] <- spds$sr
plot(rst)
################################################################
################################################################
################################################################
################################################################
####======================================
#### make files in coords
####======================================
setwd('E:/LuoA/PRIME/')
spls <- read.csv('splist/spls.csv',encoding='UTF-8')
spls$path <- paste0('E:/LuoA/PRIME/data/F_SPP_Coord/',spls$family,'/',spls$speciesKey,'/',spls$speciesKey,'_coords.csv')
flag <- file.exists(spls$path)
flag <- which(flag)

library(dplyr);library(parallel)
cl <- makeCluster(90)
clusterEvalQ(cl,{
  setwd('E:/LuoA/PRIME/')
  library(dplyr)
  spls <- read.csv('splist/spls.csv',encoding='UTF-8')
  spls$path <- paste0('E:/LuoA/PRIME/data/F_SPP_Coord/',spls$family,'/',spls$speciesKey,'/',spls$speciesKey,'_coords.csv')
  NULL
})
test <- parLapply(cl,flag, function(i){
  #test <- parLapply(cl,1:30, function(i){
  print(i)
  spds <- read.csv(spls$path[i])
  spds <- spds[!is.na(spds$decimalLatitude),]
  if(nrow(spds)<1) return(NULL)
  tmp <- data.frame(speciesKey=spls$speciesKey[i],family=spls$family[i],
                    num=sum(!is.na(spds$decimalLongitude)))
  return(tmp)
})
test.s <- do.call('rbind',test)
spls <- left_join(spls,test.s)
colnames(spls)
spls <- spls %>% dplyr::select(-path_archive) %>% dplyr::select(-path_out)
write.csv(spls,'splist/spls_coords.csv',row.names = F)
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
cl <- makeCluster(90)
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
  spds2 <- spds %>% filter(!is_alien) %>% filter(!is_outlier) %>% 
    filter(gbif_mark) %>% filter(region_mark)
  tmp <- data.frame(species=spls$species[i],family=spls$family[i],
             num_ori=nrow(spds),num_gift=sum(spds$region_mark),num_clean=nrow(spds2))
  return(tmp)
})
test.s <- do.call('rbind',test)

spls <- left_join(spls,test.s)
spls <- spls[!is.na(spls$num_ori),]

spls.test <- spls[spls$num_gift==0,]

clusterEvalQ(cl,{
  library(raster)
  rst <- raster()
  NULL
})

clusterExport(cl,"spls.test")

sr.l <- parLapply(cl,flag, function(i){
#sr.l <- parLapply(cl,which(!spls$num_clean%in%0), function(i){
#sr.l <- parLapply(cl,1:90, function(i){
  print(i)
  spds <- read.csv(spls$path_out[i])
  spds <- spds[!is.na(spds$decimalLatitude),]
  spds <- spds[abs(spds$decimalLatitude-spds$decimalLongitude)>0.001,]

  if(nrow(spds)<1) return(NULL)
  spds1 <- spds %>% filter(!is_alien) %>% filter(!is_outlier) %>% 
    filter(gbif_mark)

  if(nrow(spds1)<1) return(NULL)
  
  tmp <- data.frame(sp=spls$species[i],cell=cellFromXY(rst,spds1[,c('decimalLongitude','decimalLatitude')])) %>% distinct()
  if(ncol(tmp)<=0) return(NULL)
  return(tmp)
})
#stopCluster(cl)

sr <- do.call('rbind',sr.l)
sr <- sr[,2] %>% table() %>% as.data.frame()
colnames(sr) <- c('v1','v2')
sr$v1 <- sr$v1 %>% as.character() %>% as.numeric()

rst <- raster()
rst[sr[,1]] <- sr[,2]
plot(rst) 

rst2 <- raster()
rst2[sr[,1]] <- sr[,2]
rst2[rst2>10000] <- NA
plot(rst2) 

#############

#############
flag <- which(! spls$num_clean %in% 0 )
sr.l2 <- parLapply(cl,flag, function(i){
  #sr.l <- parLapply(cl,1:90, function(i){
  print(i)
  spds <- read.csv(spls$path_out[i])
  spds <- spds[!is.na(spds$decimalLatitude),]
  spds <- spds[abs(spds$decimalLatitude-spds$decimalLongitude)>0.001,]
  
  if(nrow(spds)<1) return(data.frame(sp=spls$species[i],x=NA,y=NA))
  spds <- spds %>% filter(!is_alien) %>% filter(!is_outlier) %>% 
    filter(gbif_mark)
  spds <- spds %>% filter(!institutionCode%in%c("PKU_CHN",'PKU_EA'))
  
  if(nrow(spds)<1) return(data.frame(sp=spls$species[i],x=NA,y=NA))
  tmp <- data.frame(sp=spls$species[i],x=spds[,c('decimalLongitude')],y=spds[,c('decimalLatitude')])
  return(tmp)
})

sr.all <- do.call('rbind',sr.l2)

rst2 <- raster()

rst3 <- raster('data/CHELSA/10km_climate_cell_land.tif')

#sr.all.flag <- sr.all[sr.all[,2]%in%flag,] %>% distinct()
sr.all.flag <- sr.all
sr.all.flag <- data.frame(sp = sr.all.flag$sp,
                          cell = cellFromXY(rst2,sr.all.flag[,c('x','y')]),
                          cell2 = cellFromXY(rst3,sr.all.flag[,c('x','y')])) %>% distinct()
sr2 <- sr.all.flag %>% dplyr::select(sp,cell) %>% distinct() %>% 
  group_by(cell) %>% mutate(sr2=length(unique(sp))) %>% ungroup() %>% 
  dplyr::select(cell,sr2) %>% distinct() %>% na.omit()
sr3 <- sr.all.flag %>% dplyr::select(sp,cell2) %>% distinct() %>%  
  group_by(cell2) %>% mutate(sr3=length(unique(sp))) %>% ungroup() %>% 
  dplyr::select(cell2,sr3) %>% distinct() %>% na.omit()

rst <- rst3;rst[] <- NA
rst[sr3$cell2] <- sr3$sr3
rst.sr3 <- rst
plot(rst.sr3)

rst <- rst2;rst[] <- NA
rst[sr2$cell] <- sr2$sr2
rst.sr2 <- rst
plot(rst.sr2)

test <- sr2[(sr2$p>0.99)&(sr2$sr2>100),]
test <- sr2[(sr2$p>0.75)&(sr2$sr2>300),]
plot(rst);points(xyFromCell(rst3,test$cell2))
stopCluster(cl)
