setwd('F:/luoao/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','RColorBrewer'),require,character.only=T)
#### load data
shp <- read_sf('data/map/data_1d/data_1d_land.shp')
tmp <- st_coordinates(shp) %>% as.data.frame() %>% group_by(L3) %>% mutate(Y=max(Y)) %>% ungroup() %>% 
  dplyr::select(L3,Y) %>% distinct()
shp$lat <- tmp$Y

red.list <- read.csv('data/IUCNcategory/sp_assess.csv')
red.list.iucn  <- read.csv('data/IUCNcategory/sp_assess_IUCN_DONE.csv')
red.list <- left_join(red.list.iucn,red.list) %>% na.omit()
sn <- 'ssp370_limit'

red.list$IUCN <- factor(red.list[[sn]],levels=c('LC','NT','VU','EN','CR','EX')) %>% as.numeric()
red.list$IUCN_old <- factor(red.list$redlistCategory,levels=c('LC','NT','VU','EN','CR','EX')) %>% as.numeric()
red.list$IUCN <- apply(cbind(red.list$IUCN,red.list$IUCN_old),1,max)

sp.ds.1d.raw <- read.csv('data/spcell_1d/sp_cellpoll_all.csv')


sp.ds.1d <- left_join(sp.ds.1d.raw,red.list[,c('speciesKey','IUCN','IUCN_old')])
sp.ds.1d <- sp.ds.1d[!is.na(sp.ds.1d$IUCN),]
max_n <- max(c(sp.ds.1d$IUCN-1,sp.ds.1d$IUCN_new-1))
sp.ds.1d$wt_new <- 1-(sp.ds.1d$IUCN-1)/max_n
sp.ds.1d$wt_old <- 1-(sp.ds.1d$IUCN_old-1)/max_n

rli <-  sp.ds.1d %>% group_by(cell_poll) %>% mutate(RLI_new=mean(wt_new),RLI_old=mean(wt_old),SR=n()) %>% ungroup() %>% 
  dplyr::select(cell_poll,RLI_new,RLI_old,SR) %>% distinct() 
rli$deltaRLI <- rli$RLI_new - rli$RLI_old

colnames(rli)[1] <- 'layer'
shp <- shp[,c('layer','lat')]
shp <- left_join(shp,rli)
shp$deltaRLI[(shp$SR<10)&(shp$lat>55)] <- NA
########

#mypalette <- colorRampPalette(c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred"))
#mycol <- rev(mypalette(100))
#mycol <- mycol[1:50,seq(51,100,2)]
mycol <- heat.colors(100)# %>% rev()
mycol <- mycol[c(seq(1,50,2),51:100)]

p <- ggplot(shp) +    
  geom_sf(aes(fill = deltaRLI,colour=deltaRLI)) + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='ΔRLI',
                       na.value = 'gray95') +
  scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide=FALSE) +
  scale_x_continuous(limits = c(-170,170)) +
  theme_bw() + theme(panel.grid=element_blank(),
                     legend.position = c(0.05,0.35),
                     #legend.direction = 'horizontal',
                     legend.key.height = unit(0.25, "in"),
                     legend.key.width = unit(0.1, "in"))
p
ggsave('results/figure/figs/figure2b.jpg',width = 7,height = 3)

####

sp.ds.1d <- left_join(sp.ds.1d.raw,red.list[,c('speciesKey','IUCN','IUCN_old')])
th.sp <-  sp.ds.1d %>% group_by(cell_poll) %>% mutate(t_sr=sum(!is.na(IUCN)),sr=n()) %>% ungroup() %>% 
  dplyr::select(cell_poll,t_sr,sr) %>% distinct() 
#th.sp$u_t_p <- 1 - th.sp$t_sr/th.sp$sr
th.sp$gap <- th.sp$sr - th.sp$t_sr
th.sp$gap_p <- th.sp$gap/th.sp$sr

colnames(th.sp)[1] <- 'layer'
shp <- shp[,c('layer','lat')]
shp <- left_join(shp,th.sp)
shp$gap[(shp$t_sr<10)&(shp$lat>55)] <- NA
shp$gap_p[(shp$t_sr<10)&(shp$lat>55)] <- NA
########
mycol <- hcl.colors(100, palette = "viridis", alpha = NULL, rev = FALSE, fixup = TRUE) %>% rev()
mycol <- mycol[c(1:50,seq(51,100,2))]

p <- ggplot(shp) +    
  geom_sf(aes(fill = gap,colour=gap)) + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='Assessment gap (%)',
                       na.value = 'gray95',
                       transform	= 'sqrt') +
  scale_colour_gradientn(colors = mycol,na.value = 'gray95',
                         guide=FALSE,
                         transform	= 'sqrt') +
  scale_x_continuous(limits = c(-170,170)) +
  theme_bw() + theme(panel.grid=element_blank(),
                     legend.position = c(0.12,0.35),
                     #legend.direction = 'horizontal',
                     legend.key.height = unit(0.25, "in"),
                     legend.key.width = unit(0.1, "in"))
p
ggsave('results/figure/Sfigs/gap_n.jpg',width = 7,height = 3)


mycol <- hcl.colors(100, palette = "viridis", alpha = NULL, rev = FALSE, fixup = TRUE) %>% rev()
mycol <- mycol[c(1:50,seq(51,100,6))]

p <- ggplot(shp) +    
  geom_sf(aes(fill = gap_p,colour=gap_p)) + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='   (%)',
                       #fill='transparent'
                       na.value = 'gray95',
                       transform	= 'sqrt') +
  scale_colour_gradientn(colors = mycol,na.value = 'gray95',
                         guide=FALSE,
                         transform	= 'sqrt') +
  scale_x_continuous(limits = c(-170,170)) +
  labs(title='Assessment gap') +
  theme_bw() + theme(panel.grid=element_blank(),
                     legend.position = c(0.08,0.35),
                     #legend.direction = 'horizontal',
                     legend.key.height = unit(0.25, "in"),
                     legend.key.width = unit(0.1, "in"))
p
ggsave('results/figure/figs/figure2c.jpg',width = 7,height = 3)

