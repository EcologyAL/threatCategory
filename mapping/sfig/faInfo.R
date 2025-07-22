setwd('D:/work/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','RColorBrewer','stringr','patchwork'),require,character.only=T)#'rasterVis'

spinfo <- read.csv('data/outputs/spInfo/sp_assess_simple.csv') %>% distinct()
spls <- read.csv('data/outputs/spInfo/spls.csv')
spinfo <- left_join(spinfo,spls[,c('speciesKey','family')])
spinfo <- spinfo[!is.na(spinfo$family),]
tmp <- spinfo;tmp$family <- 'All'
spinfo <- rbind(spinfo,tmp)

fainfo <- spinfo %>% dplyr::select(family,intCategory) %>% 
  group_by(family)%>% 
  mutate(
    N = n(),
    N_assessed = sum(intCategory%in%c('LC','NT','VU','EN','CR','EX')),
    TH = sum(intCategory%in%c('VU','EN','CR','EX')),
    LC = sum(intCategory%in%c('LC')),
    NT = sum(intCategory%in%c('NT')),
    VU = sum(intCategory%in%c('VU')),
    EN = sum(intCategory%in%c('EN')),
    CR = sum(intCategory%in%c('CR')),
    EX = sum(intCategory%in%c('EX'))) %>% 
  ungroup() %>%  
  dplyr::select(-intCategory) %>% distinct()
fainfo

fainfo <- fainfo[order(fainfo$N,decreasing = T),]
fainfo$RLI <- (fainfo$LC+fainfo$NT*0.8+fainfo$VU*0.6+
                 fainfo$EN*0.4+fainfo$CR*0.2+fainfo$EX*0)/fainfo$N_assessed
tmp <- round(fainfo[,4:10]/fainfo[[2]] * 100,3)
colnames(tmp) <- paste0(colnames(tmp),'(%)')
fainfo <- cbind(fainfo,tmp)

fainfo <- fainfo[order(fainfo$N,decreasing = T),]

write.csv(fainfo,'outputs/table/faInfo.csv',row.names = F)