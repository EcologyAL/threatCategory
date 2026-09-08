
# RLI: climCategory -------------------------------------------------------

red.list <- read.csv(paths["spAssAll"])
red.list$category <- red.list[['ssp370_limit']]

sp.ds.1d.raw <- sp.ds.1d <- read.csv(paths['spds1d'])

sp.ds.1d <- left_join(sp.ds.1d.raw,red.list[,c('speciesKey','category')])
sp.ds.1d <- sp.ds.1d[!is.na(sp.ds.1d$category),]
sp.ds.1d$category <- factor(sp.ds.1d$category,levels=c('LC','NT','VU','EN','CR','EX')) %>% as.numeric()
sp.ds.1d$wt <- 1-(sp.ds.1d$category-1)/max(sp.ds.1d$category-1,na.rm=T)
rli <-  sp.ds.1d %>% group_by(cell_poll) %>% 
  mutate(RLI=mean(wt,na.rm=T),
         SR=n()) %>% 
  ungroup() %>%
  dplyr::select(cell_poll,RLI,SR) %>% distinct() %>% as.data.frame()
colnames(rli)[1] <- 'cell_poll'
cellInfo <- rli

#write.csv(cellInfo,paths['cellInfo'],row.names = F)

# delta RLI

red.list <- read.csv(paths["spAssSim"])
sp.ds.1d.raw <- sp.ds.1d <- read.csv(paths['spds1d'])

sp.ds.1d <- left_join(sp.ds.1d.raw,red.list[,c('speciesKey',
                                               'redlistCategory','intCategory',
                                               'wt_redlistC','wt_intC')])
sp.ds.1d <- sp.ds.1d[!is.na(sp.ds.1d$wt_redlistC),]

if(max(sp.ds.1d$wt_intC,sp.ds.1d$wt_intC,na.rm=T)!=1) print(warning('wt MAX IS NOT 1!'))

rli <-  sp.ds.1d %>% group_by(cell_poll) %>% 
  mutate(RLI_new=mean(wt_intC,na.rm=T),
         RLI_old=mean(wt_redlistC,na.rm=T)) %>%
  ungroup() %>% 
  dplyr::select(cell_poll,RLI_new,RLI_old) %>% distinct() 
rli$deltaRLI <- rli$RLI_new - rli$RLI_old

cellInfo <- left_join(cellInfo,rli)
#write.csv(cellInfo,paths['cellInfo'],row.names = F)

# threat proportion

# RLI: climCategory -------------------------------------------------------

red.list <- read.csv(paths["spAssAll"])
red.list$category <- red.list[['ssp370_limit']]

sp.ds.1d.raw <- sp.ds.1d <- read.csv(paths['spds1d'])
sp.ds.1d <- left_join(sp.ds.1d.raw,red.list[,c('speciesKey','category')])
sp.ds.1d <- sp.ds.1d[!is.na(sp.ds.1d$category),]


thr <-  sp.ds.1d %>% group_by(cell_poll) %>% 
  reframe(SR=n(),
         SR_LC=sum(category%in%'LC'),
         SR_NT=sum(category%in%'NT'),
         SR_VU=sum(category%in%'VU'),
         SR_EN=sum(category%in%'EN'),
         SR_CREX=sum(category%in%c('CR','EX')),
         SR_THR=sum(category%in%c('VU','EN','CR','EX'))) %>% 
  as.data.frame() %>%
  distinct() %>%
  mutate(p_threat=SR_THR/SR*100,
         p_VU=SR_VU/SR*100,
         p_EN=SR_EN/SR*100,
         p_CREX=SR_CREX/SR*100) %>% 
  dplyr::select(-SR) %>% distinct()

cellInfo <- left_join(cellInfo,thr)
write.csv(cellInfo,paths['cellInfo'],row.names = F)
