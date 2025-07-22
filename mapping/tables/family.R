setwd('D:/work/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','rasterVis','RColorBrewer','stringr','ggbeeswarm'),require,character.only=T)

# read assessment -----------------------------------------------------

red.list <- read.csv('data/outputs/spInfo/sp_assess_simple.csv')
red.list$intCategory <- factor(red.list$intCategory,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()
red.list$intCategory_com <- factor(red.list$intCategory_com,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()
red.list$redlistCategory <- factor(red.list$redlistCategory,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()


# joint family list  ------------------------
spls <- read.csv('data/outputs/spInfo/spls.csv')
spls <- spls[spls$class%in%c('Magnoliopsida','Liliopsida'),]
sp_pa <- read.csv('data/outputs/spInfo/1dRnage/sp_PAs')

dat <- left_join(spls[,c('speciesKey','family')],red.list[,c('speciesKey',
                                    'intCategory','intCategory_com','redlistCategory',
                                    'wt_intC','wt_intC_com','wt_redlistC')])
dat <- dat %>% left_join(sp_pa[,c('speciesKey','Protected')])

dat$category <- dat$intCategory %>% as.numeric()
dat$wt <- dat$wt_intC

tmp <- dat
tmp$family <- 'All S.'
dat <- rbind(dat,tmp)

pdat <- dat %>% group_by(family) %>% 
  mutate(SR=sum(category>1,na.rm=T),
         TSR=sum(category>3,na.rm=T),
         PTSR=sum(category>3 & Protected,na.rm=T),
         RLI=mean(wt,na.rm=T),
         RLI_com=mean(wt_intC_com,na.rm=T),
         RLI_IUCN=mean(wt_redlistC,na.rm=T)) %>% 
  ungroup()  %>% 
  mutate(
    T.S.P = round(TSR/SR * 100,1),
    P.T.S.P  = round(PTSR/TSR * 100,1),
    deltaRLI = round(RLI_com - RLI_IUCN,3)
  )%>% 
  dplyr::select(family,
                SR, TSR, P.T.S.P, T.S.P,
                RLI,RLI_com,RLI_IUCN,deltaRLI) %>% 
  distinct() %>% 
  na.omit()
colnames(pdat) <- c('Family',
                    'S.', 'T.S.', 'Protected T.S.(%)','T.S.(%)',
                    'RLI','RLI (Com.S.)','RLI (IUCN)','Delta RLI')
pdat <- pdat[order(pdat$S.,decreasing = T),]


pdat$RLI <- pdat$RLI %>% round(3)
pdat$`RLI (Com.S.)` <- pdat$`RLI (Com.S.)` %>% round(3)
pdat$`RLI (IUCN)` <- pdat$`RLI (IUCN)` %>% round(3)
pdat$`Delta RLI` <- pdat$`Delta RLI` %>% round(3)
write.csv(pdat,'outputs/table/family/family.csv',row.names = F,na = '')
