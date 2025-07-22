library(ggplot2);library(stringr);library(sf);library(dplyr)#;library(patchwork)
setwd('F:/luoao/RLI/')
sp.timespan <- read.csv('data/spinfo/spls_mark_all.csv')
#sp.timespan$timespan%in%'' <- '2041-2070';sp.range$timespan[is.na(sp.range$timespan)] <- '2041-2070'
sp.range <- read.csv('data/spinfo/rangeChange/spInfo_half.csv')
sp.range[,-(1:2)] <- (sp.range[,2]-sp.range[,-(1:2)])/sp.range[,2]*100
#sp.range <- sp.range %>% as.data.frame()
sp.range <- left_join(sp.range,sp.timespan)

for (i in 3:29) {
  flag4 <- which(sp.range[,i]>=100)
  flag1 <- which(sp.range[,i]<=0)
  sp.range[flag4,i] <- 99.9
  sp.range[flag1,i] <- -0.1
  sp.range[,i] <- cut(sp.range[,i],c(-1,5,30,50,80,100)) %>% as.numeric()
  sp.range[,i] <- factor(sp.range[,i],levels=1:5,labels=c('LC','NT','VU','EN','CR')) %>% as.character()
}

sp.a3c <- sp.range
for (ds in c('DS_unlimit','DS_limit','DS_no')) {
  for (sn in c('ssp126','ssp370','ssp585')) {
    sp.a3c$tmp <- NA
    for(year in c('2011-2040','2041-2070','2071-2100')){
      flag <- which(sp.a3c$timespan %in% year)
      sp.a3c[flag,'tmp'] <- sp.a3c[flag,paste0('sn_',str_replace(year,'-','.'),'_mean_',sn,'_',ds)]
    }
    colnames(sp.a3c)[colnames(sp.a3c)%in%'tmp'] <- paste0(sn,'_',ds)
    print(paste0(sn,'_',ds))
  }
}

for (ds in c('DS_unlimit','DS_limit','DS_no')) {
  for (sn in c('ssp126','ssp370','ssp585')) {
    sp.a3c$tmp <- NA
    sp.a3c[,'tmp'] <- sp.a3c[,paste0('sn_','2071.2100','_mean_',sn,'_',ds)]
    colnames(sp.a3c)[colnames(sp.a3c)%in%'tmp'] <- paste0(sn,'_',ds,'_2100')
  }
}

cols <- c()
for (ds in c('DS_unlimit','DS_limit','DS_no')) {
  for (sn in c('ssp126','ssp370','ssp585')) {
    cols <- c(cols,paste0(sn,'_',ds))
  }
}
for (ds in c('DS_unlimit','DS_limit','DS_no')) {
  for (sn in c('ssp126','ssp370','ssp585')) {
    cols <- c(cols,paste0(sn,'_',ds,'_2100'))
  }
}

sp.a3c <- sp.a3c[,c('speciesKey',cols)]
write.csv(sp.a3c,'data/IUCNcategory/A3c/A3c_noEX.csv',row.names = F)

# sp.a3c <- read.csv('data/IUCNcategory/A3c/A3c_noEX.csv')
# EX <- read.csv('data/IUCNcategory/A3c/A3c_ex.csv')
# cols <- colnames(sp.a3c)[-1]
# for (col in cols) {
#   str_remove(col,c('sn_','_mean_'))
#   
#   EX$speciesKey[EX[,]]
#   flag <- sp.a3c$speciesKey%in%
#   sp.a3c[,col]
# }

# merge extinction species list -------------------------------------------


sp.a3c <- read.csv('data/IUCNcategory/A3c/A3c_noEX.csv')
sp.ex <- read.csv('data/IUCNcategory/A3c/A3c_ex.csv')
sp.a3c <- left_join(sp.a3c,sp.timespan[,c('speciesKey','timespan')])

for (ds in c('DS_unlimit','DS_limit','DS_no')) {
  for (sn in c('ssp126','ssp370','ssp585')) {
    for(year in c('2011-2040','2041-2070','2071-2100')){
      col <- paste0('sn_',str_replace(year,'-','.'),'_mean_',sn,'_',ds)
      
      flag <- which( (sp.a3c$timespan %in% year) & 
                       (!is.na(sp.a3c[,paste0(sn,'_',ds)])) &
                       (sp.a3c$speciesKey%in%sp.ex$speciesKey[sp.ex[,col]]) )
      sp.a3c[flag,paste0(sn,'_',ds)] <- 'EX'
    }
    print(paste0(sn,'_',ds))
  }
}

for (ds in c('DS_unlimit','DS_limit','DS_no')) {
  for (sn in c('ssp126','ssp370','ssp585')) {
    col <- paste0('sn_',str_replace('2071.2100','-','.'),'_mean_',sn,'_',ds)
    
    flag <- which( (sp.a3c$timespan %in% year) & 
                     (!is.na(sp.a3c[,paste0(sn,'_',ds,'_2100')])) &
                     (sp.a3c$speciesKey%in%sp.ex$speciesKey[sp.ex[,col]]) )
    sp.a3c[flag,paste0(sn,'_',ds,'_2100')] <- 'EX'
  }
}

write.csv(sp.a3c,'data/IUCNcategory/A3c/A3c.csv',row.names = F)

# load('data/spcell_1d/sp_ds_1d_ls.rdata')
# sp.ds.1d.flag <- do.call('rbind',lapply(sp.ds.1d.ls, dim))
# flag <- which(sp.ds.1d.flag[,2]%in%14)
# for(i in flag){
#   tmp <- sp.ds.1d.ls[[i]]
#   colnames(tmp) <- colnames(tmp) %>% stringr::str_replace('X','sn_')
#   sp.ds.1d.ls[[i]] <- tmp[,colnames(tmp) %in% colnames(sp.ds.1d.ls[[1]])]
# }
# sp.ds.1d <- do.call('rbind',sp.ds.1d.ls)
# 
# sr <- sp.ds.1d %>% filter(sn_1981.2010>0) %>% dplyr::select(cell_poll,speciesKey) %>% 
#   distinct() %>% group_by(cell_poll) %>% summarise(sr=n()) %>% as.data.frame()
# 
# RLI.tmp <- sp.ds.1d %>% filter(sn_1981.2010>0) %>% dplyr::select(cell_poll,speciesKey) %>% 
#   left_join(sp.range[,c('speciesKey','IUCNcategory')]) %>% distinct() 
# RLI.tmp$wt <- 1-(RLI.tmp$IUCNcategory-1)/4
# RLI <- RLI.tmp %>% 
#   group_by(cell_poll) %>% mutate(sr=n(),RLI=mean(wt,na.rm=T)) %>% 
#   ungroup() %>% dplyr::select(-speciesKey,-IUCNcategory) %>% distinct()
# 
# library(raster)
# rstland <- raster('E:/LuoA/PRIME/data/Map_land/landboundary.tif')
# 
# rst <- raster()  
# rst[RLI$cell_poll] <- RLI$sr
# rst[is.na(rstland[])] <- NA
# rst.sr <- rst
# plot(rst.sr)
# 
# rst <- raster()  
# rst[RLI$cell_poll] <- RLI$RLI
# rst[is.na(rstland)] <- NA
# rst.rli <- rst
# plot(rst.rli)
