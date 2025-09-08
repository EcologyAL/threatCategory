setwd('D:/work/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','RColorBrewer','stringr','circlize'),require,character.only=T)#'rasterVis'


# read new assessment -----------------------------------------------------


red.list <- read.csv('data/outputs/spInfo/sp_assess_simple.csv')# %>% na.omit()
sp <- read.csv('data/outputs/spls.csv')
sp <- sp$speciesKey[sp$class%in%c('Magnoliopsida','Liliopsida')]
red.list <- red.list[red.list$speciesKey%in%sp,]
#sn <- 'ssp370_limit'

#red.list$category <- red.list[,sn]
red.list$category <- red.list$intCategory
red.list$category[is.na(red.list$category)] <- 'DD'

# read iucn assessment ----------------------------------------------------


# red.list.iucn <- read.csv('data/IUCNcategory/redlist_species_data/simple_summary_match_ag.csv') %>% 
#   na.omit() %>% 
#   left_join(red.list[,c('speciesKey','category')])
# rm(red.list)

# int assessment -----------------------------------------------------

# red.list <- red.list.iucn
# red.list$category <- factor(red.list$category,levels=c('DD','LC','NT','VU','EN','CR','EX'))
red.list$intCategory <- factor(red.list$category,levels=c('DD','LC','NT','VU','EN','CR','EX'))
red.list$redlistCategory <- factor(red.list$redlistCategory,levels=c('DD','LC','NT','VU','EN','CR','EX'))

#red.list$intCategory <- red.list$category
# red.list <- mutate(red.list,
#                    intCategory=ifelse(as.numeric(redlistCategory)>as.numeric(category),
#                                            redlistCategory,
#                                            category))
# red.list$intCategory <- factor(red.list$intCategory,levels = rev(1:7),labels = rev(c('DD','LC','NT','VU','EN','CR','EX')))

red.list.cp <- red.list[,c('intCategory','redlistCategory')] %>% na.omit() %>% 
  group_by(intCategory,redlistCategory) %>% mutate(n=n()) %>% distinct()
red.list.cp <- red.list.cp[order(red.list.cp$intCategory,decreasing = T),]
red.list.cp <- red.list.cp[order(red.list.cp$redlistCategory),]
levels(red.list.cp$redlistCategory) <- c('DD ','LC ','NT ','VU ','EN ','CR ',' EX ')

# map circle --------------------------------------------------------------


mycol <- c("#D1D1C6","#70BE50","#CCE226","#FFF204","#F89D57","#ED1C2E","BLACK");mycol <- c(rev(mycol),mycol)

{
  cairo_ps('outputs/fig2/fig2a_Ensem.ps',width=3.9,height=3.9)
  #jpeg('outputs/fig2a_noEnsem.jpg',width=3.6,height=3.6,units = 'in',res=300)
  circos.par(start.degree = 0)
  chordDiagram(red.list.cp,big.gap = 15,
               annotationTrack = c('name','grid'),
               grid.col =  mycol)
  
  circos.track(
    track.index = 1, 
    panel.fun = function(x, y) {
      xlim = get.cell.meta.data("xlim")
      ylim = get.cell.meta.data("ylim")
      sector.name = get.cell.meta.data("sector.index")
      xplot = get.cell.meta.data("xplot")
      by = ifelse(abs(xplot[2] - xplot[1]) > 30, 0.2, 0.5)
      for (p in seq(by, 1, by = by)) {
        circos.text(
          p * (xlim[2] - xlim[1]) + xlim[1],
          mean(ylim)-0.5,
          paste0(p * 100, "%"),
          cex = 0.35,
          adj = c(0.5, 0),
          niceFacing = TRUE
        )
      }
    }, bg.border = NA)
  circos.clear()
  dev.off()
}

# load shp and sp.ds.1d ---------------------------------------------------
shp <- read_sf('data/map/data_1d/data_1d_land.shp')
tmp <- st_coordinates(shp) %>% as.data.frame() %>% group_by(L3) %>% mutate(Y=max(Y)) %>% ungroup() %>% 
  dplyr::select(L3,Y) %>% distinct()
shp$lat <- tmp$Y

sp.ds.1d.raw <- read.csv('data/spcell_1d/sp_cellpoll_all.csv')

sp.ds.1d <- left_join(sp.ds.1d.raw,red.list[,c('speciesKey','redlistCategory','intCategory')])
sp.ds.1d <- sp.ds.1d[!is.na(sp.ds.1d$intCategory),]

sp.ds.1d <- sp.ds.1d %>%  
  mutate(redlistCategory=as.numeric(redlistCategory)-2,
         intCategory=as.numeric(intCategory)-2) %>% 
  mutate(wt_old = 1 - redlistCategory/5,
         wt_new = 1 - intCategory/5) %>% 
  mutate(wt_old = ifelse(wt_old>1,NA,wt_old),
         wt_new = ifelse(wt_new>1,NA,wt_new)) 

if(max(sp.ds.1d$redlistCategory,sp.ds.1d$intCategory)!=5) print(warning('MAX IS NOT 5!'))

rli <-  sp.ds.1d %>% group_by(cell_poll) %>% 
  mutate(RLI_new=mean(wt_new,na.rm=T),
         RLI_old=mean(wt_old,na.rm=T),SR=n()) %>% 
  ungroup() %>% 
  dplyr::select(cell_poll,RLI_new,RLI_old,SR) %>% distinct() 
rli$deltaRLI <- rli$RLI_new - rli$RLI_old

colnames(rli)[1] <- 'layer'
shp <- shp[,c('layer','lat')]
shp <- left_join(shp,rli)
shp$deltaRLI[(shp$SR<10)&(shp$lat>55)] <- NA
shp$deltaRLI[which(shp$deltaRLI>0)] <- 0.022

# mapping ---------------------------------------------------

mycol <- heat.colors(100)# %>% rev()
mycol <- mycol[c(seq(1,50,2),51:100)]
#shp$deltaRLI[which(shp$deltaRLI>0)]
p <- ggplot(shp) +    
  geom_sf(aes(fill = deltaRLI,colour=deltaRLI)) + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='??RLI',
                       na.value = 'gray95') +
  scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide=FALSE) +
  scale_x_continuous(limits = c(-170,170)) +
  theme_bw() + theme(panel.grid=element_blank(),
                     legend.position = c(0.1,0.35),
                     axis.text.x = element_blank(),
                     axis.text.y = element_blank(),
                     axis.ticks = element_blank(),
                     #legend.direction = 'horizontal',
                     legend.key.height = unit(0.1, "in"),
                     legend.key.width = unit(0.05, "in")) + 
  labs(title='(c) RLI anomaly')
p
ggsave('results/figure/figs/figure2c_em.eps',width = 3.5,height = 2)
ggsave('results/figure/figs/figure2c_em.jpg',width = 3.5,height = 2,dpi=300)

mypalette <- colorRampPalette(c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred"))
mycol <- rev(mypalette(100))
mycol <- mycol[c(1:10,seq(11,100,5))]
shp$RLI_old_2 <- shp$RLI_old
#shp$RLI_old_2[shp$RLI_old_2<0.4] <- NA

p <- ggplot(shp) +    
  geom_sf(aes(fill = RLI_old_2,colour=RLI_old_2)) + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       #trans='exp',
                       name='RLI',
                       na.value = 'gray95') +
  scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide=FALSE) +
  scale_x_continuous(limits = c(-170,170)) +
  theme_bw() + theme(panel.grid=element_blank(),
                     legend.position = c(0.1,0.35),
                     axis.text.x = element_blank(),
                     axis.text.y = element_blank(),
                     axis.ticks = element_blank(),
                     #legend.direction = 'horizontal',
                     legend.key.height = unit(0.1, "in"),
                     legend.key.width = unit(0.05, "in")) +
  labs(title='(b) IUCN assessment')
p
ggsave('results/figure/figs/figure2b_em.eps',width = 3.5,height = 2)
ggsave('results/figure/figs/figure2b_em.jpg',width = 3.5,height = 2,dpi=300)

shp.dat <- sf::st_drop_geometry(shp)
ggplot(data=shp.dat)+
  geom_point(aes(x=RLI_new,y=RLI_old))

ggplot(data=shp.dat)+
  geom_point(aes(x=RLI_new,y=RLI_old),cex=0.05,alpha=0.1)+
  scale_x_continuous(trans = 'exp',lim=c(0.35,1.01))+
  scale_y_continuous(trans = 'exp',lim=c(0.35,1.01))+
  geom_abline(intercept = 0, slope = 1)+
  theme_classic()
# scale_x_continuous(trans = 'exp',lim=c(0.6,1.01))+
# scale_y_continuous(trans = 'exp',lim=c(0.6,1.01))


m <- lm(data=shp.dat,RLI_new ~ RLI_old)
summary(m)

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

