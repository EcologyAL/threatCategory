library(ggplot2);library(stringr);library(sf);library(dplyr)#;library(patchwork)
setwd('F:/luoao/RLI/')
sp.timespan <- read.csv('data/lifeForm/spls_mark_all.csv')
load('data/spcell_1d/sp_range_decline.rdata')
sp.range <- sp.range %>% as.data.frame()
sp.range <- left_join(sp.range,sp.timespan)
sp.range$timespan[is.na(sp.range$timespan)] <- '2041-2100'

sp.range$rangechange <- NA
colnames(sp.range)[3:5]

flag <- sp.range$timespan%in%'2011-2040';sp.range$rangechange[flag] <- sp.range[flag,2+1]
flag <- sp.range$timespan%in%'2041-2070';sp.range$rangechange[flag] <- sp.range[flag,2+2]
flag <- sp.range$timespan%in%'2071-2100';sp.range$rangechange[flag] <- sp.range[flag,2+3]

sp.range$IUCNcategory <- cut(sp.range$rangechange,c(0,30,50,80,100)) %>% as.numeric()
sp.range$IUCNcategory[sp.range$rangechange>=100] <- 5
sp.range$IUCNcategory[sp.range$rangechange<=0] <- 1
#sp.range$IUCNcategory <- factor(sp.range$IUCNcategory,levels=1:5,labels=c('LC','VU','EN','CR','EX'))

tb <- table(sp.range$IUCNcategory[sp.range$num_coords_c>5])
tb/sum(tb)

load('data/spcell_1d/sp_ds_1d_ls.rdata')
sp.ds.1d.flag <- do.call('rbind',lapply(sp.ds.1d.ls, dim))
flag <- which(sp.ds.1d.flag[,2]%in%14)
for(i in flag){
  tmp <- sp.ds.1d.ls[[i]]
  colnames(tmp) <- colnames(tmp) %>% stringr::str_replace('X','sn_')
  sp.ds.1d.ls[[i]] <- tmp[,colnames(tmp) %in% colnames(sp.ds.1d.ls[[1]])]
}
sp.ds.1d <- do.call('rbind',sp.ds.1d.ls)

sr <- sp.ds.1d %>% filter(sn_1981.2010>0) %>% dplyr::select(cell_poll,speciesKey) %>% 
  distinct() %>% group_by(cell_poll) %>% summarise(sr=n()) %>% as.data.frame()

RLI.tmp <- sp.ds.1d %>% filter(sn_1981.2010>0) %>% dplyr::select(cell_poll,speciesKey) %>% 
  left_join(sp.range[,c('speciesKey','IUCNcategory')]) %>% distinct() 
RLI.tmp$wt <- 1-(RLI.tmp$IUCNcategory-1)/4
RLI <- RLI.tmp %>% 
  group_by(cell_poll) %>% mutate(sr=n(),RLI=mean(wt,na.rm=T)) %>% 
  ungroup() %>% dplyr::select(-speciesKey,-IUCNcategory) %>% distinct()

library(raster)
rstland <- raster('E:/LuoA/PRIME/data/Map_land/landboundary.tif')

rst <- raster()  
rst[RLI$cell_poll] <- RLI$sr
rst[is.na(rstland[])] <- NA
rst.sr <- rst
plot(rst.sr)

rst <- raster()  
rst[RLI$cell_poll] <- RLI$RLI
rst[is.na(rstland)] <- NA
rst.rli <- rst
plot(rst.rli)
