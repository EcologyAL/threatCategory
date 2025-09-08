# map ------------------------------------------------------
setwd('D:/work/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','RColorBrewer','patchwork'),require,character.only=T)


# load sp distribution data and join data --------------------------------------------------

red.list <- read.csv('data/outputs/sp_assess_simple.csv')

sp.ds.1d.raw <- sp.ds.1d <- read.csv('data/spcell_1d/sp_cellpoll_all.csv')

sp.ds.1d <- left_join(sp.ds.1d.raw,red.list[,c('speciesKey','category','wt_new')])
sp.ds.1d <- sp.ds.1d[!is.na(sp.ds.1d$wt_new),]
#sp.ds.1d$category <- factor(sp.ds.1d$IUCN,levels=c('LC','NT','VU','EN','CR','EX')) %>% as.numeric()


# category ----------------------------------------------------------------

sp.ds.1d$threat <- sp.ds.1d$category %in% c('VU','EN','CR','EX')

sp.ds.1d$VU <- sp.ds.1d$category %in% c('VU')
sp.ds.1d$EN <- sp.ds.1d$category %in% c('EN')
sp.ds.1d$CREX <- sp.ds.1d$category %in% c('CR','EX')


# aggregate map data -----------------------------------------------------


dat <- sp.ds.1d %>% group_by(cell_poll) %>% 
  mutate(SR=length(unique(speciesKey)),
         threat=sum(threat),
         VU=sum(VU),
         EN=sum(EN),
         CREX=sum(CREX)) %>% 
  dplyr::select(cell_poll,
                SR,
                threat,
                VU,EN,CREX) %>% 
  distinct() %>% 
  mutate(p_threat=threat/SR*100,
         p_VU=VU/SR*100,
         p_EN=EN/SR*100,
         p_CREX=CREX/SR*100) %>% 
  rename(layer=cell_poll)

# mapping -----------------------------------------------------------------

shp <- read_sf('data/map/data_1d/data_1d_land.shp')

dat[dat$SR<10,-1] <- NA
shp <- shp[,'layer']
shp <- left_join(shp,dat)

#mypalette <- colorRampPalette(c("#1e469b","#2681b6","#35b9c5","#96d2b0","#F9F8CA")) 
#mypalette <- colorRampPalette(c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred"))
#mycol <- (mypalette(100))

mypalette <- colorRampPalette(c("#F2F1E6","#F3D78A","#F8984F","#C74647","#982C2C"))
mycol <- (mypalette(100))

p1 <- ggplot(shp) +    
  geom_sf(aes(fill = threat,colour=threat)) + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='',
                       na.value = 'gray95') +
  scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide=FALSE) +
  scale_x_continuous(limits = c(-170,170)) +
  scale_y_continuous(limits = c(-108,80)) +
  theme_bw() + 
  theme(panel.grid=element_blank(),
        legend.position = c(0.5,0.125),
        legend.title = element_text(size=6),
        legend.text =  element_text(size=6),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = 'horizontal',
        legend.key.height = unit(0.05, "in"),
        legend.key.width = unit(0.3, "in")) +
  labs(title='(a) Threatened species richness')
p1

mycol <- hcl.colors(100, palette = "viridis")%>% rev()

p2 <- ggplot(shp) +    
  geom_sf(aes(fill = p_threat,colour=p_threat)) + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='(%)',
                       na.value = 'gray95') +
  scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide=FALSE) +
  scale_x_continuous(limits = c(-170,170)) +
  scale_y_continuous(limits = c(-108,80)) +
  theme_bw() + 
  theme(panel.grid=element_blank(),
        legend.position = c(0.5,0.125),
        legend.title = element_text(size=6),
        legend.text =  element_text(size=6),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = 'horizontal',
        legend.key.height = unit(0.05, "in"),
        legend.key.width = unit(0.3, "in")) +
  labs(title='(b) Threatened species proportion')
p2

p3 <- ggplot(shp) +    
  geom_sf(aes(fill = p_VU,colour=p_VU)) + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='(%)',
                       na.value = 'gray95') +
  scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide=FALSE) +
  scale_x_continuous(limits = c(-170,170)) +
  scale_y_continuous(limits = c(-125,80)) +
  theme_bw() + 
  theme(panel.grid=element_blank(),
        legend.position = c(0.5,0.15),
        legend.title = element_text(size=5),
        legend.text =  element_text(size=5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = 'horizontal',
        legend.key.height = unit(0.025, "in"),
        legend.key.width = unit(0.2, "in")) +
  labs(title='(c) VU')
p3

p4 <- ggplot(shp) +    
  geom_sf(aes(fill = p_EN,colour=p_EN)) + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='(%)',
                       na.value = 'gray95') +
  scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide=FALSE) +
  scale_x_continuous(limits = c(-170,170)) +
  scale_y_continuous(limits = c(-125,80)) +
  theme_bw() + 
  theme(panel.grid=element_blank(),
        legend.position = c(0.5,0.15),
        legend.title = element_text(size=5),
        legend.text =  element_text(size=5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = 'horizontal',
        legend.key.height = unit(0.025, "in"),
        legend.key.width = unit(0.2, "in")) +
  labs(title='(d) EN')
p4

p5 <- ggplot(shp) +    
  geom_sf(aes(fill = p_CREX,colour=p_CREX)) + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='(%)',
                       na.value = 'gray95') +
  scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide=FALSE) +
  scale_x_continuous(limits = c(-170,170)) +
  scale_y_continuous(limits = c(-125,80)) +
  theme_bw() + 
  theme(panel.grid=element_blank(),
        legend.position = c(0.5,0.15),
        legend.title = element_text(size=5),
        legend.text =  element_text(size=5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = 'horizontal',
        legend.key.height = unit(0.025, "in"),
        legend.key.width = unit(0.2, "in")) +
  labs(title='(e) CR and EX')
p5

#windows(height = 4.8,width = 7)
p1row <- p1+p2+plot_layout(nrow = 1,widths = c(3,3),heights = 2.4)
ggsave('results/figure/figs/figure3a.jpg',width = 6,height = 2.4)
ggsave('results/figure/figs/figure3a.eps',width = 6,height = 2.4)

p2row <- p3+p4+p5+plot_layout(nrow=1,widths = c(2,2,2),heights = 1.8)
ggsave('results/figure/figs/figure3b.jpg',width = 6,height = 1.8)
ggsave('results/figure/figs/figure3b.eps',width = 6,height = 1.8)
