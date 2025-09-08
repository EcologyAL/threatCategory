setwd('F:/luoao/RLI/')
#sapply(c('sf','dplyr','ggplot2','raster','rasterVis','RColorBrewer'),require,character.only=T)
sapply(c('sf','dplyr','ggplot2','raster','RColorBrewer'),require,character.only=T)
#### load data
shp <- read_sf('data/map/data_1d/data_1d_land.shp')
red.list <- read.csv('data/IUCNcategory/sp_assess.csv')
sp.ds.1d <- read.csv('data/spcell_1d/sp_cellpoll_all.csv')

#load('data/sp_ds_1d.rdata')
#sp.ds.1d <- sp.ds.1d[sp.ds.1d$sn_1981.2010,c('cell_poll','speciesKey')] %>% distinct()
sn <- 'ssp370_limit'

red.list$IUCN <- red.list[[sn]]
sp.ds.1d <- left_join(sp.ds.1d.raw,red.list[,c('speciesKey','IUCN')])
sp.ds.1d <- sp.ds.1d[!is.na(sp.ds.1d$IUCN),]
sp.ds.1d$IUCN <- factor(sp.ds.1d$IUCN,levels=c('LC','VU','EN','CR','EX')) %>% as.numeric()
sp.ds.1d$wt <- 1-(sp.ds.1d$IUCN-1)/max(sp.ds.1d$IUCN-1)

#sp.ds.1d <- sp.ds.1d[!sp.ds.1d$speciesKey%in%test$speciesKey,]
rli <-  sp.ds.1d %>% group_by(cell_poll) %>% mutate(RLI=mean(wt),SR=n()) %>% ungroup() %>% 
  dplyr::select(cell_poll,RLI,SR) %>% distinct() 

colnames(rli)[1] <- 'layer'
shp <- left_join(shp,rli)
shp$RLI[shp$SR<10] <- NA
########

mypalette <- colorRampPalette(c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred"))
mycol <- rev(mypalette(100))
#mycol <- mycol[1:50,seq(51,100,2)]
mycol <- mycol[c(seq(1,50,2),51:100)]

p <- ggplot(shp) +    
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
p
ggsave('results/figure/figs/figure3.jpg',width = 7,height = 3)

p <- ggplot(shp) +    
  geom_sf(aes(fill = SR,colour=SR)) + # ,lwd =0  
  scale_fill_gradientn(colors = rev(mycol),
                       name='SR',
                       na.value = 'gray95') +
  scale_colour_gradientn(colors = rev(mycol),na.value = 'gray95') +
  scale_x_continuous(limits = c(-170,170)) +
  theme_bw() + theme(panel.grid=element_blank(),
                     legend.position = c(0.55,0.1),
                     legend.direction = 'horizontal',
                     legend.key.height = unit(0.08, "in"),
                     legend.key.width = unit(0.42, "in"))
p
ggsave('results/figure/sfigs/speciesRichness.jpg',width = 7,height = 3)


# threaten species ratio --------------------------------------------------


setwd('F:/luoao/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','RColorBrewer'),require,character.only=T)
#### load data
shp <- read_sf('data/map/data_1d/data_1d_land.shp')
red.list <- read.csv('data/IUCNcategory/sp_assess.csv')
sp.ds.1d.raw <- sp.ds.1d <- read.csv('data/spcell_1d/sp_cellpoll_all.csv')

unique(colnames(red.list))[-1]
for (variable in vector) {
  
}

sn <- 'ssp370_limit'

red.list$IUCN <- red.list[[sn]]
sp.ds.1d <- left_join(sp.ds.1d.raw,red.list[,c('speciesKey','IUCN')])
sp.ds.1d <- sp.ds.1d[!is.na(sp.ds.1d$IUCN),]
#sp.ds.1d$IUCN <- factor(sp.ds.1d$IUCN,levels=c('LC','VU','EN','CR','EX')) %>% as.numeric()
#sp.ds.1d$wt <- 1-(sp.ds.1d$IUCN-1)/max(sp.ds.1d$IUCN-1)

#sp.ds.1d <- sp.ds.1d[!sp.ds.1d$speciesKey%in%test$speciesKey,]
c.ratio <- sp.ds.1d %>% group_by(cell_poll) %>% mutate(SR=n()) %>% 
  ungroup() %>% dplyr::select(cell_poll,SR) %>% distinct() 
for(i in list('VU','EN',c('CR','EX'))){
  tmp <-  sp.ds.1d %>% group_by(cell_poll) %>% mutate(Category=sum(IUCN%in%i),SR=n()) %>% 
    ungroup() %>% dplyr::select(cell_poll,Category,SR) %>% distinct() 
  tmp[[i[1]]] <- tmp$Category/tmp$SR
  tmp <- tmp[,c('cell_poll',i[1])] %>% distinct()
  c.ratio <- left_join(c.ratio,tmp)
}
colnames(c.ratio)[1] <- 'layer'
shp <- shp[,'layer']
shp <- left_join(shp,c.ratio)
shp <- shp[!is.na(shp$SR),]
shp$VU[shp$VU>0.9] <- NA
shp$VU[(shp$VU>0.4)&(shp$SR>100)] <- NA

p.s <- list()
for(i in list('VU','EN',c('CR','EX'))){
  shp$value <- shp[[i[1]]]
  shp$value[shp$SR<10] <- NA
  
  #mypalette <- colorRampPalette(c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred"))
  #mycol <- rev(mypalette(100))
  #mycol <- mycol[1:50,seq(51,100,2)]
  #mycol <- mycol[seq(1,50,4),51:100]
  mycol <- hcl.colors(100, palette = "viridis", alpha = NULL, rev = FALSE, fixup = TRUE)
  mycol <- mycol[c(seq(1,50,3),51:100)]
  
  p <- ggplot(shp) +    
    geom_sf(aes(fill = value,colour=value)) +
    scale_fill_gradientn(colors = mycol,
                         name=i[1],
                         na.value = 'gray95') +
    scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide=NULL) +
    #scale_x_continuous(limits = c(-170,170)) +
    theme_bw() + 
    theme(panel.grid=element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'bottom',
          legend.direction = 'horizontal',
          legend.title.position = 'top',
          legend.key.height = unit(0.08, "in"),
          legend.key.width = unit(0.4, "in"))
  p
  p.s <- c(p.s,list(p))
}
p.s[[1]]+p.s[[2]]+p.s[[3]]+plot_layout(nrow=1,widths = 7/3*c(1,1,1),height = 2)
ggsave('results/figure/figs/figure3b.jpg',width = 7,height = 2,units='in',dpi=300)
########

mypalette <- colorRampPalette(c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred"))
mycol <- rev(mypalette(100))
#mycol <- mycol[1:50,seq(51,100,2)]
mycol <- mycol[seq(1,50,4),51:100]

p <- ggplot(shp) +    
  geom_sf(aes(fill = RLI,colour=RLI)) + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='RLI',
                       na.value = 'gray95') +
  scale_colour_gradientn(colors = mycol,na.value = 'gray95') +
  scale_x_continuous(limits = c(-170,170)) +
  theme_bw() + theme(panel.grid=element_blank(),
                     legend.position = c(0.55,0.1),
                     legend.direction = 'horizontal',
                     legend.key.height = unit(0.08, "in"),
                     legend.key.width = unit(0.42, "in"))
p
ggsave('results/figure/figs/figure3.jpg',width = 7,height = 3)

p <- ggplot(shp) +    
  geom_sf(aes(fill = SR,colour=SR)) + # ,lwd =0  
  scale_fill_gradientn(colors = rev(mycol),
                       name='SR',
                       na.value = 'gray95') +
  scale_colour_gradientn(colors = rev(mycol),na.value = 'gray95') +
  scale_x_continuous(limits = c(-170,170)) +
  theme_bw() + theme(panel.grid=element_blank(),
                     legend.position = c(0.55,0.1),
                     legend.direction = 'horizontal',
                     legend.key.height = unit(0.08, "in"),
                     legend.key.width = unit(0.42, "in"))
p
ggsave('results/figure/sfigs/speciesRichness.jpg',width = 7,height = 3)