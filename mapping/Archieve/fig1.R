setwd('F:/luoao/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','RColorBrewer','stringr'),require,character.only=T)#'rasterVis'

red.list <- read.csv('data/IUCNcategory/sp_assess_all.csv')# %>% na.omit()
red.list <- red.list[!is.na(red.list$ssp370_limit),]
pdat <- red.list[,c('ssp370_limit','A_ssp370_limit','B_ssp370_limit','D')]
pdat$B_ssp370_limit[is.na(pdat$B_ssp370_limit)] <- 'LC'
pdat$D[is.na(pdat$D)] <- 'LC'

pdat$IUCN <- pdat$ssp370_limit
flag <- which(pdat$ssp370_limit == pdat$A_ssp370_limit)
pdat$IUCN[flag] <- paste0(pdat$IUCN[flag],'_A')
flag <- which(pdat$ssp370_limit == pdat$B_ssp370_limit)
pdat$IUCN[flag] <- paste0(pdat$IUCN[flag],'_B')
flag <- which(pdat$ssp370_limit == pdat$D)
pdat$IUCN[flag] <- paste0(pdat$IUCN[flag],'_D')

pdat$IUCN <- pdat$IUCN %>% str_replace('A_B_D','M') %>% 
  str_replace('A_B','M') %>% 
  str_replace('A_D','M') %>% 
  str_replace('B_D','M')
  
# flag <- which(pdat$ssp370_limit%in%'LC'&(!pdat$ssp370_limit%in%'LC_A'))
# pdat$IUCN[flag] <- 'LC_A_B_C'
table(pdat$IUCN)
sum(is.na(pdat$IUCN))
pdat$type <- 'ssp370_limit'

#
pdata <- pdat
pdata$IUCN_c <- factor(pdata$ssp370_limit,levels=c('LC','NT','VU','EN','CR','EX'))
pdata <- pdata %>% group_by(IUCN_c) %>% mutate(IUCN_num=n()) %>% dplyr::select(IUCN_num,IUCN_c) %>% distinct()
pdata <- pdata[order(as.numeric(pdata$IUCN_c)),]

pdata$IUCN_num_p <- pdata$IUCN_num/sum(pdata$IUCN_num)
pdata$IUCN_label <- pdata$IUCN_c
levels(pdata$IUCN_label) <- levels(pdata$IUCN_label) %>% 
  str_remove('LC_') %>% str_remove('NT_') %>% str_remove('VU_') %>% 
  str_remove('EN_') %>% str_remove('CR_') %>% str_remove('EX_')

pdata$IUCN_num_cum <- max(cumsum(pdata$IUCN_num)) - (cumsum(pdata$IUCN_num) - pdata$IUCN_num/2)
pdata$p_label <- paste0(round(pdata$IUCN_num_p*100,1),'%')

pdata1 <- pdata
mycol <- c("#70BE50","#CCE226","#FFF204","#F89D57","#ED1C2E","BLACK")
ggplot() + 
  geom_col(data=pdata,aes(x=1,y=IUCN_num,fill=IUCN_c,col=IUCN_c),
           width=0.8,
           col='transparent')+
  scale_fill_manual(values = mycol,guide=NULL) +
  geom_text(data=pdata,aes(x=1.5,y=IUCN_num_cum,label=IUCN_label)) +
  geom_text(data=pdata,aes(x=1.25,y=IUCN_num_cum,label=p_label),cex=3) +
  geom_text(data=pdata,aes(x=0.75,y=IUCN_num_cum,label=''),cex=3) +
  coord_flip()+
  theme_void()

#

pdata <- pdat
pdata$IUCN_c <- factor(pdata$ssp370_limit,levels=c('LC','NT','VU','EN','CR','EX'))
pdata <- pdata %>% group_by(IUCN) %>% mutate(IUCN_num=n()) %>% dplyr::select(IUCN,IUCN_num,IUCN_c) %>% distinct()
pdata$IUCN <- factor(pdata$IUCN,levels=c('EX_A',
                                         'CR_A','CR_B','CR_M',
                                         'EN_A','EN_B','EN_M',
                                         'VU_A','VU_B','VU_M',
                                         'NT_A','NT_B','NT_D','NT_M',
                                         'LC_M') %>% rev())
# pdata$IUCN <- factor(pdata$IUCN,levels=c('LC_M',
#                                          'NT_A','NT_B','NT_D','NT_M',
#                                          'VU_A','VU_B','VU_M',
#                                          'EN_A','EN_B','EN_M',
#                                          'CR_A','CR_B','CR_M',
#                                          'EX_A'))
pdata <- pdata[order(as.numeric(pdata$IUCN)),]

pdata$IUCN_num_p <- pdata$IUCN_num/sum(pdata$IUCN_num)
pdata$IUCN_label <- pdata$IUCN
levels(pdata$IUCN_label) <- levels(pdata$IUCN_label) %>% 
  str_remove('LC_') %>% str_remove('NT_') %>% str_remove('VU_') %>% 
  str_remove('EN_') %>% str_remove('CR_') %>% str_remove('EX_')

pdata$IUCN_num_cum <- max(cumsum(pdata$IUCN_num))-(cumsum(pdata$IUCN_num) - pdata$IUCN_num/2) 
pdata$p_label <- paste0(round(pdata$IUCN_num_p*100,1),'%')
#pdata$p_label[pdata$IUCN_num_p<0.002] <- ''

mycol <- c("#70BE50","#CCE226","#FFF204","#F89D57","#ED1C2E","BLACK")
mycol2 <- c()
for (i in 1:6) {
  original_color <- mycol[i]
  palette <- colorRampPalette(c(original_color, "white"))
  mycol2 <- c(mycol2,palette(6)[1:sum(as.numeric(pdata$IUCN_c)%in%i)] %>% rev())
}
pdata <- pdata[order(as.numeric(pdata$IUCN)),]

pdata2 <- pdata
p2 <- ggplot() +  
  geom_col(data=pdata2,aes(x=1,y=IUCN_num,fill=IUCN,col=IUCN),
           width=0.8,
           col='white')+
  scale_fill_manual(values = mycol2,guide=NULL) +
  geom_text(data=pdata2,aes(x=0.5,y=IUCN_num_cum,label=IUCN_label)) +
  geom_text(data=pdata2,aes(x=0.25,y=IUCN_num_cum,label=p_label),cex=3) +
  coord_flip()+
  theme_void()

#
library(patchwork)
windows(height = 2,width = 7)

p1 <- ggplot() + 
  geom_col(data=pdata1,aes(x=0.5,y=IUCN_num,fill=IUCN_c,col=IUCN_c),
           width=0.5,
           col='transparent')+
  scale_fill_manual(values = mycol,guide=NULL) +
  geom_text(data=pdata1,aes(x=1.25,y=IUCN_num_cum,label=IUCN_label)) +
  geom_text(data=pdata1,aes(x=1,y=IUCN_num_cum,label=p_label),cex=3) +
  xlim(c(0.2,1.5))+
  coord_flip()+
  theme_void()
p2 <- ggplot() +  
  geom_col(data=pdata2,aes(x=1,y=IUCN_num,fill=IUCN,col=IUCN),
           width=0.25,
           col='white')+
  scale_fill_manual(values = mycol2,guide=NULL) +
  geom_text(data=pdata2,aes(x=0.6,y=IUCN_num_cum,label=IUCN_label)) +
  geom_text(data=pdata2,aes(x=0.3,y=IUCN_num_cum,label=p_label),cex=3) +
  xlim(c(0,1.2))+
  coord_flip()+
  theme_void()
p1+p2+plot_layout(ncol=1)

