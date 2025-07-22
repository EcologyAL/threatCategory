setwd('D:/work/RLI/')
options(ggplot2.text.family = "Arial")
sapply(c('sf','dplyr','ggplot2','raster','RColorBrewer','stringr'),require,character.only=T)#'rasterVis'
sn <- 'ssp370_limit'

red.list <- read.csv('data/outputs/spInfo/sp_assess_all.csv')# %>% na.omit()
sp <- read.csv('data/outputs/spInfo/spls.csv')
sp <- sp$speciesKey[sp$class%in%c('Magnoliopsida','Liliopsida')]
red.list <- red.list[red.list$speciesKey%in%sp,]

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

pdat$IUCN[is.na(pdat$ssp370_limit)] <- 'DD'
pdat$ssp370_limit[is.na(pdat$ssp370_limit)] <- 'DD'

table(pdat$IUCN)
sum(is.na(pdat$IUCN))
pdat$type <- 'ssp370_limit'

#

pdata <- pdat
pdata$IUCN_c <- factor(pdata$ssp370_limit,levels=c('DD','LC','NT','VU','EN','CR','EX'))
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

#
pdata <- pdat
pdata$IUCN_c <- factor(pdata$ssp370_limit,levels=c('DD','LC','NT','VU','EN','CR','EX'))
pdata <- pdata %>% group_by(IUCN) %>% mutate(IUCN_num=n()) %>% dplyr::select(IUCN,IUCN_num,IUCN_c) %>% distinct()
#pdata <- pdata[pdata$IUCN_num>180,]
pdata$IUCN <- factor(pdata$IUCN,levels=c('EX_A',
                                         'CR_A','CR_B','CR_M',
                                         'EN_A','EN_B','EN_M',
                                         'VU_A','VU_B','VU_M',
                                         'NT_A','NT_B','NT_D','NT_M',
                                         'LC_M',
                                         'DD') %>% rev())
#
pdata <- pdata[order(as.numeric(pdata$IUCN)),]

pdata$IUCN_num_p <- pdata$IUCN_num/sum(pdata$IUCN_num)
pdata$IUCN_label <- pdata$IUCN
levels(pdata$IUCN_label) <- levels(pdata$IUCN_label) %>% 
  str_remove('LC_') %>% str_remove('NT_') %>% str_remove('VU_') %>% 
  str_remove('EN_') %>% str_remove('CR_') %>% str_remove('EX_')

pdata$IUCN_num_cum <- max(cumsum(pdata$IUCN_num))-(cumsum(pdata$IUCN_num) - pdata$IUCN_num/2) 
pdata$p_label <- paste0(round(pdata$IUCN_num_p*100,1),'%')
#pdata$p_label[pdata$IUCN_num_p<0.002] <- ''
pdata <- pdata[pdata$IUCN_num_p > 0.0005,]

mycol <- c("#D1D1C6","#70BE50","#CCE226","#FFF204","#F89D57","#ED1C2E","BLACK")
mycol2 <- c()
for (i in 1:7) {
  original_color <- mycol[i]
  palette <- colorRampPalette(c(original_color, "white"))
  mycol2 <- c(mycol2,palette(6)[1:sum(as.numeric(pdata$IUCN_c)%in%i)] %>% rev())
}
pdata2 <- pdata
pdata2$IUCN_num_cum.raw <- pdata2$IUCN_num_cum

sum(pdata2$IUCN_num_p[pdata2$IUCN_label%in%c('A') & pdata2$IUCN_c%in%c('VU','EN','CR','EX')] )
sum(pdata2$IUCN_num_p[pdata2$IUCN_label%in%c('B') & pdata2$IUCN_c%in%c('VU','EN','CR','EX')] )
sum(pdata2$IUCN_num_p[pdata2$IUCN_label%in%c('M') & pdata2$IUCN_c%in%c('VU','EN','CR','EX')] )

#
library(patchwork)
pdata1$p_label <- str_remove(pdata1$p_label,'%')
p1 <- ggplot() + 
  geom_col(data=pdata1,aes(x=0.5,y=IUCN_num,fill=IUCN_c,col=IUCN_c),
           width=0.5,
           col='transparent')+
  scale_x_continuous(lim = c(0.24,1.5), expand = c(0,0))+
  scale_y_continuous(expand = c(0.01,0.01))+
  scale_fill_manual(values = mycol,guide=NULL) +
  geom_text(data=pdata1,aes(x=1.25,y=IUCN_num_cum+500,label=IUCN_label)) +
  geom_text(data=pdata1,aes(x=0.9,y=IUCN_num_cum,label=p_label),cex=3) +
  geom_text(aes(x=1,y=nrow(pdat)-5000,label='(%)'),cex=3) +
  #xlim(c(0.2,1.5))+
  coord_flip()+
  theme_void()
p1

#pdata2 <- pdata2[!pdata2$p_label%in%'0%',]
pdata2$p_label <- str_remove(pdata2$p_label,'%')
pdata2$IUCN_num_cum[11] <- pdata2$IUCN_num_cum.raw[11] - 1000
pdata2$IUCN_num_cum[10] <- pdata2$IUCN_num_cum.raw[10] - 500
pdata2$IUCN_num_cum[9] <- pdata2$IUCN_num_cum.raw[9] - 750
pdata2$IUCN_num_cum[8] <- pdata2$IUCN_num_cum.raw[8] - 3200
pdata2$IUCN_num_cum[7] <- pdata2$IUCN_num_cum.raw[7] + 3200

p2 <- ggplot() +  
  geom_col(data=pdata2,aes(x=1,y=IUCN_num,fill=IUCN,col=IUCN),
           width=0.15,
           col='white')+
  scale_x_continuous(lim = c(0.45,1.15), expand = c(0,0))+
  scale_y_continuous(expand = c(0.01,0.01))+
  scale_fill_manual(values = mycol2,guide=NULL) +
  geom_text(data=pdata2,aes(x=0.78,y=IUCN_num_cum,label=IUCN_label),cex=2.4) +
  geom_text(data=pdata2,aes(x=0.62,y=IUCN_num_cum,label=p_label),cex=2) +
  geom_text(aes(x=0.63,y=nrow(pdat)-5000,label='(%)'),cex=2) +
  #xlim(c(0,1.2))+
  coord_flip()+
  theme_void()
segs <- c(0,cumsum(rev(pdata2$IUCN_num)))
for (seg in segs) {
  p2 <- p2 + annotate('segment',y=seg,yend=seg,x=0.85,xend=0.925,lwd=0.5)
}
#windows(height = 2,width = 7)
#p1+p2+plot_layout(ncol=1)


# red list index map ------------------------------------------------------

#setwd('F:/luoao/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','RColorBrewer'),require,character.only=T)

#### load data
shp <- read_sf('data/map/data_1d/data_1d_land.shp')
rli <- read.csv('data/outputs/cell_1d.csv')
rli$RLI <- rli$RLI_climC

# red.list <- read.csv('data/outputs/spInfo/sp_assess_simple.csv')
# sp.ds.1d.raw <- sp.ds.1d <- read.csv('data/spcell_1d/sp_cellpoll_all.csv')
# 
# red.list$IUCN <- red.list[['climCategory']]
# sp.ds.1d <- left_join(sp.ds.1d.raw,red.list[,c('speciesKey','IUCN')])
# sp.ds.1d <- sp.ds.1d[!is.na(sp.ds.1d$IUCN),]
# sp.ds.1d$IUCN <- factor(sp.ds.1d$IUCN,levels=c('LC','NT','VU','EN','CR','EX')) %>% as.numeric()
# sp.ds.1d$wt <- 1-(sp.ds.1d$IUCN-1)/max(sp.ds.1d$IUCN-1)
# rli <-  sp.ds.1d %>% group_by(cell_poll) %>% mutate(RLI=mean(wt),SR=n()) %>% ungroup() %>% 
#   dplyr::select(cell_poll,RLI,SR) %>% distinct() 
#colnames(rli)[1] <- 'cell_poll'

shp <- shp[,c('cell_poll')]
shp <- left_join(shp,rli)
shp$RLI[shp$SR<10] <- NA

########

mypalette <- colorRampPalette(c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred"))
mycol <- rev(mypalette(100))
mycol <- mycol[c(seq(1,50,2),51:100)]
# mypalette <- colorRampPalette(c("#FFF204","#F89D57","#ED1C2E","darkred"))
# mycol <- rev(mypalette(100))

p3 <- ggplot(shp) +    
  geom_sf(aes(fill = RLI,colour=RLI)) + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='RLI',
                       na.value = 'gray95') +
  scale_colour_gradientn(colors = mycol,na.value = 'gray95') +
  scale_x_continuous(limits = c(-170,170)) +
  theme_bw() + theme(panel.grid=element_blank(),
                     legend.position = c(0.05,0.35),
                     #legend.direction = 'horizontal',
                     legend.key.height = unit(0.25, "in"),
                     legend.key.width = unit(0.1, "in"))

p3

windows(height = 4.8,width = 7)
p <- p1+p2+p3+plot_layout(ncol=1,heights = c(0.7,0.6,3.5))
p
ggsave('outputs/fig1/figure1.jpg',width = 7,height = 4.8)
ggsave('outputs/fig1/figure1.eps',width = 7,height = 4.8)
