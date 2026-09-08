# RLI: climCategory -------------------------------------------------------

red.list <- read.csv(paths["spAssAll"])
red.list$category <- red.list[['ssp370_limit']]

sp.ds.1d.raw <- sp.ds.1d <- read.csv(paths['spds1d'])

sp.ds.1d <- left_join(sp.ds.1d.raw,red.list[,c('speciesKey','category')])
sp.ds.1d <- sp.ds.1d[!is.na(sp.ds.1d$category),]
sp.ds.1d$category <- factor(sp.ds.1d$category,levels=c('LC','NT','VU','EN','CR','EX')) %>% as.numeric()
sp.ds.1d$wt <- 1-(sp.ds.1d$category-1)/5
rli <-  sp.ds.1d %>% group_by(cell_poll) %>% 
  mutate(RLI=mean(wt,na.rm=T),
         SR=n()) %>% 
  ungroup() %>%
  dplyr::select(cell_poll,RLI,SR) %>% distinct() %>% as.data.frame()
colnames(rli)[1] <- 'cell_poll'
cellInfo <- rli

#write.csv(cellInfo,paths['cellInfo'],row.names = F)

# RLI: climCategory; criterion -------------------------------------------------------

red.list <- read.csv(paths["spAssAll"])

red.list$A <- red.list[['A_ssp370_limit']]
red.list$B <- red.list[['B_ssp370_limit']]
spls <- read.csv(paths["spMarkAll"])
red.list$B[red.list$speciesKey%in%spls$speciesKey[spls$num_coords_c>10]] <- NA
#red.list$B[red.list$B%in%c('NT','LC',NA)] <- NA

sp.ds.1d <- left_join(sp.ds.1d.raw,red.list[,c('speciesKey','A','B')])
#sp.ds.1d <- sp.ds.1d[!is.na(sp.ds.1d$A),]

sp.ds.1d$A <- factor(sp.ds.1d$A,levels=c('LC','NT','VU','EN','CR','EX')) %>% as.numeric()
sp.ds.1d$B <- factor(sp.ds.1d$B,levels=c('LC','NT','VU','EN','CR','EX')) %>% as.numeric()

sp.ds.1d$wt_A <- 1-(sp.ds.1d$A-1)/max(sp.ds.1d$A-1,na.rm=T)
sp.ds.1d$wt_B <- 1-(sp.ds.1d$B-1)/max(sp.ds.1d$B-1,na.rm=T)

rli <-  sp.ds.1d %>% group_by(cell_poll) %>% 
  mutate(RLI_A=mean(wt_A,na.rm=T),
         RLI_B=mean(wt_B,na.rm=T),
         flag=sum(!is.na(wt_B))) %>% 
  ungroup() 
rli$RLI_B[rli$flag<10] <- NA
rli <-  dplyr::select(rli,cell_poll,RLI_A,RLI_B) %>% distinct() %>% as.data.frame()
colnames(rli)[1] <- 'cell_poll'
cellInfo <- left_join(cellInfo,rli)

#write.csv(cellInfo,paths['cellInfo'],row.names = F)


# delta RLI, int category ---------------------------------------------------------------

red.list <- read.csv(paths["spAssSim"])
sp.ds.1d.raw <- sp.ds.1d <- read.csv(paths['spds1d'])

sp.ds.1d <- left_join(sp.ds.1d.raw,red.list[,c('speciesKey',
                                               'redlistCategory','intCategory',
                                               'wt_redlistC','wt_intC')])
#sp.ds.1d <- sp.ds.1d[!is.na(sp.ds.1d$wt_redlistC),]

if(max(sp.ds.1d$wt_intC,sp.ds.1d$wt_intC,na.rm=T)!=1) print(warning('wt MAX IS NOT 1!'))

rli <-  sp.ds.1d %>% group_by(cell_poll) %>% 
  mutate(RLI_int=mean(wt_intC,na.rm=T),
         RLI_old=mean(wt_redlistC,na.rm=T)) %>%
  ungroup() %>% 
  dplyr::select(cell_poll,RLI_int,RLI_old) %>% distinct() 
rli$deltaRLI <- rli$RLI_int - rli$RLI_old
cellInfo <- left_join(cellInfo,rli)

sp.ds.1d <- left_join(sp.ds.1d.raw,red.list[,c('speciesKey',
                                               'redlistCategory','intCategory',
                                               'wt_redlistC','wt_intC')])
sp.ds.1d <- sp.ds.1d[!is.na(sp.ds.1d$wt_redlistC),]
#sp.rl <- red.list$speciesKey[!is.na(red.list$redlistCategory)]
rli <-  sp.ds.1d %>% group_by(cell_poll) %>% 
  mutate(RLI_int_shareSp=mean(wt_intC,na.rm=T),
         RLI_old=mean(wt_redlistC,na.rm=T)) %>%
  ungroup() %>% 
  dplyr::select(cell_poll,RLI_int_shareSp,RLI_old) %>% distinct() 
rli$deltaRLI_int_shareSp <- rli$RLI_int_shareSp - rli$RLI_old
cellInfo <- left_join(cellInfo,rli[,c('cell_poll','RLI_int_shareSp','deltaRLI_int_shareSp')])

#write.csv(cellInfo,paths['cellInfo'],row.names = F)


# delta fcc -----------------------------------------------------

sp.ds.1d <- left_join(sp.ds.1d.raw,red.list[,c('speciesKey',
                                               'redlistCategory','climCategory',
                                               'wt_redlistC','wt_climC')])
#sp.ds.1d <- sp.ds.1d[!is.na(sp.ds.1d$wt_redlistC),]
if(max(sp.ds.1d$wt_intC,sp.ds.1d$wt_climC,na.rm=T)!=1) print(warning('wt MAX IS NOT 1!'))

rli <-  sp.ds.1d %>% group_by(cell_poll) %>% 
  mutate(RLI_clim=mean(wt_climC,na.rm=T),
         RLI_old=mean(wt_redlistC,na.rm=T),SR=n()) %>% 
  ungroup() %>% 
  dplyr::select(cell_poll,RLI_clim,RLI_old,SR) %>% distinct() 
rli$deltaRLI_climC <- rli$RLI_clim - rli$RLI_old
cellInfo <- left_join(cellInfo,rli[,c('cell_poll','deltaRLI_climC','RLI_clim')])
  
# SR FCC -------------------------------------------------------

red.list <- read.csv(paths["spAssAll"])
red.list$category <- red.list[['ssp370_limit']]

#sp.ds.1d.raw <- sp.ds.1d <- read.csv(paths['spds1d'])
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
#write.csv(cellInfo,paths['cellInfo'],row.names = F)

# PAs -------------------------------------------------------

PAs <- foreign::read.dbf(paths['PAs.dbf'])
PAs <- left_join(PAs[,c('cell_poll','PAs30')],cellInfo)
shp <- read_sf(paths['baseMap'])
PAs <- left_join(PAs,sf::st_drop_geometry(shp))
PAs$RLI_int[!PAs$land2%in%'T'] <- 1;PAs$RLI_int[PAs$SR<100] <- 1

n30 <- ceiling(0.3*64800*0.29)
PAs$PAs30 <- PAs$PAs30%in%c('T','TRUE','1')
PAs$RLI30 <- FALSE
PAs$RLI30[order(PAs$RLI_int)[1:n30]] <- TRUE

ex.num <- ceiling(n30 - sum(PAs$PAs30))
flag <- order(PAs$RLI_int)
flag <- flag[!flag%in%which(PAs$PAs30)][1:ex.num]
PAs$PAE30 <- FALSE
PAs$PAE30[flag] <- TRUE

PAs$Priority2 <- 'NP'; table(PAs$Priority2)
PAs$Priority2[PAs$RLI30] <- 'RLI30'; table(PAs$Priority2)
PAs$Priority2[PAs$PAE30] <- 'PAE30'; table(PAs$Priority2)

cellInfo <- left_join(cellInfo,PAs[,c('cell_poll','PAs30','Priority2')])
write.csv(cellInfo,paths['cellInfo'],row.names = F)
shp <- left_join(shp,PAs,by='cell_poll')
write_sf(shp,paths['PAs.mapping'])
