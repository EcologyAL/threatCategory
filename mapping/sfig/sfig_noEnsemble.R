setwd('D:/work/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','RColorBrewer','stringr','circlize'),require,character.only=T)#'rasterVis'


# read new assessment -----------------------------------------------------

red.list <- read.csv('data/outputs/spInfo/sp_assess_simple.csv')# %>% na.omit()
sp <- read.csv('data/outputs/spInfo/spls.csv')
sp <- sp$speciesKey[sp$class%in%c('Magnoliopsida','Liliopsida')]
red.list <- red.list[red.list$speciesKey%in%sp,]

red.list$category <- red.list$intCategory
red.list$category[is.na(red.list$category)] <- 'DD'

# int assessment -----------------------------------------------------

red.list$intCategory <- factor(red.list$climCategory,levels=c('DD','LC','NT','VU','EN','CR','EX'))
red.list$redlistCategory <- factor(red.list$redlistCategory,levels=c('DD','LC','NT','VU','EN','CR','EX'))

red.list.cp <- red.list[,c('intCategory','redlistCategory')] %>% na.omit() %>% 
  group_by(intCategory,redlistCategory) %>% mutate(n=n()) %>% distinct()
red.list.cp <- red.list.cp[order(red.list.cp$intCategory,decreasing = T),]
red.list.cp <- red.list.cp[order(red.list.cp$redlistCategory),]
levels(red.list.cp$redlistCategory) <- c('DD ','LC ','NT ','VU ','EN ','CR ',' EX ')

# map circle --------------------------------------------------------------


mycol <- c("#D1D1C6","#70BE50","#CCE226","#FFF204","#F89D57","#ED1C2E","BLACK");mycol <- c(rev(mycol),mycol)

{
  cairo_ps('outputs/sfig/fig2a_notIntCircle.ps',width=2,height=2,pointsize=6)
  #jpeg('outputs/fig2a_noEnsem.jpg',width=3.6,height=3.6,units = 'in',res=300)
  circos.par(start.degree = 0)
  chordDiagram(red.list.cp,big.gap = 15,
               annotationTrack = c('name','grid'),
               annotationTrackHeight = mm_h(c(2, 2)),
               grid.col =  mycol)
  
  circos.track(
    track.index = 1, 
    panel.fun = function(x, y) {
      xlim = get.cell.meta.data("xlim")
      ylim = get.cell.meta.data("ylim")
      sector.name = get.cell.meta.data("sector.index")
      xplot = get.cell.meta.data("xplot")
      #by = ifelse(abs(xplot[2] - xplot[1]) > 30, 0.25, 0.5)
      by = if(abs(xplot[2] - xplot[1]) > 30){
        0.25
      }else if(abs(xplot[2] - xplot[1]) > 5){
        0.5
      } else{
        1
      }
      for (p in seq(by, 1, by = by)[-length(seq(by, 1, by = by))]) {
        circos.text(
          p * (xlim[2] - xlim[1]) + xlim[1],
          mean(ylim)-0.5,
          paste0(p * 100, "%"),
          cex = 0.5,
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
# tmp <- st_coordinates(shp) %>% as.data.frame() %>% group_by(L3) %>% mutate(Y=max(Y)) %>% ungroup() %>% 
#   dplyr::select(L3,Y) %>% distinct()
# shp$lat <- tmp$Y

sp.ds.1d.raw <- read.csv('data/spcell_1d/sp_cellpoll_all.csv')

sp.ds.1d <- left_join(sp.ds.1d.raw,red.list[,c('speciesKey',
                                               'redlistCategory','climCategory',
                                               'wt_redlistC','wt_climC')])
sp.ds.1d <- sp.ds.1d[!is.na(sp.ds.1d$wt_redlistC),]

if(max(sp.ds.1d$wt_intC,sp.ds.1d$wt_climC,na.rm=T)!=1) print(warning('wt MAX IS NOT 1!'))

rli <-  sp.ds.1d %>% group_by(cell_poll) %>% 
  mutate(RLI_new=mean(wt_climC,na.rm=T),
         RLI_old=mean(wt_redlistC,na.rm=T),SR=n()) %>% 
  ungroup() %>% 
  dplyr::select(cell_poll,RLI_new,RLI_old,SR) %>% distinct() 
rli$deltaRLI <- rli$RLI_new - rli$RLI_old

#colnames(rli)[1] <- 'layer'
shp <- shp[,c('cell_poll','land')]
shp <- left_join(shp,rli)
shp$deltaRLI[!shp$land] <- NA
#shp$deltaRLI[which(shp$SR<10)] <- NA
#shp$deltaRLI[(shp$SR<10)&(shp$lat>55)] <- NA
#shp$deltaRLI[which(shp$deltaRLI>0)] <- 0.022

# mapping ---------------------------------------------------

mycol <- heat.colors(100)# %>% rev()
mycol <- mycol[c(seq(1,50,2),51:100)]
#shp$deltaRLI[which(shp$deltaRLI>0)]
p <- ggplot(shp) +    
  geom_sf(aes(fill = deltaRLI,colour=deltaRLI)) + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='',
                       na.value = 'gray95') +
  scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide='none') +
  scale_x_continuous(limits = c(-170,170)) +
  theme_bw() + theme(panel.grid=element_blank(),
                     legend.position = c(0.08,0.35),
                     axis.text.x = element_blank(),
                     axis.text.y = element_blank(),
                     axis.ticks = element_blank(),
                     #legend.direction = 'horizontal',
                     legend.key.height = unit(0.2, "in"),
                     legend.key.width = unit(0.15, "in")) #+ 
#labs(title='(c) RLI anomaly')
p
ggsave('outputs/sfig/notIntCompare/figure2c_em.pdf',width = 6,height = 3)
ggsave('outputs/sfig/notIntCompare/figure2c_em.jpg',width = 6,height = 3,dpi=300)

shp$RLI_new[!shp$land] <- NA
mypalette <- colorRampPalette(c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred"))
mycol <- rev(mypalette(100))
mycol <- mycol[c(1:50,seq(51,100,3))]

p <- ggplot(shp) +    
  geom_sf(aes(fill = RLI_new,colour=RLI_new)) + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='',
                       na.value = 'gray95') +
  scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide='none') +
  scale_x_continuous(limits = c(-170,170)) +
  theme_bw() + theme(panel.grid=element_blank(),
                     legend.position = c(0.08,0.3),
                     axis.text.x = element_blank(),
                     axis.text.y = element_blank(),
                     axis.ticks = element_blank(),
                     #legend.direction = 'horizontal',
                     legend.key.height = unit(0.12, "in"),
                     legend.key.width = unit(0.06, "in")) #+ 
p
ggsave('outputs/sfig/notIntCompare/figure2b.pdf',width = 4,height = 2)
ggsave('outputs/sfig/notIntCompare/figure2b.jpg',width = 4,height = 2,dpi=300)
