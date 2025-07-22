setwd('D:/work/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','RColorBrewer','stringr','patchwork'),require,character.only=T)#'rasterVis'

shp <- read_sf('data/map/data_1d/data_1d_land.shp')
shp <- shp[,c('cell_poll','land')]
dat <- read.csv('data/outputs/cell_1d.csv') %>% distinct()
shp <- left_join(shp,dat)

#shp$RLI[shp$SR<10] <- NA
########

mypalette <- colorRampPalette((c('#79171c','#934f24','#ac7e26','#c2b055', '#cbdda1','#a5d7ca','#5cb8c9','#2d8ab6','#185da3','#0f3188')))
#mypalette <- colorRampPalette(c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred"))
mycol <- rev(mypalette(100))
mycol <- mycol[c(seq(1,50,2),51:100)]

shp$SR_NTHR <- shp$SR-shp$SR_THR

p.list <- list()
for(i in 1:6){
  x <- c('SR','SR_THR','SR_NTHR','SR_VU','SR_EN','SR_CREX')[i]
  y <- c('(a) Total','(b) Threatened','(c) LC&NT','(d) VU','e EN','(f) CR&EX')[i]
  shp$value <- shp[[x]]
  shp$value[!shp$land] <- NA
  p <- ggplot(shp) +    
    geom_sf(aes(fill = value,colour=value)) + # ,lwd =0  
    scale_fill_gradientn(colors = mycol,
                         name='',
                         na.value = 'gray95') +
    scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide='none') +
    scale_x_continuous(limits = c(-170,170)) +
    labs(title=y)+
    theme_void()+
    theme_bw() + theme(panel.grid=element_blank(),
                       legend.position = 'bottom',
                       legend.direction = 'horizontal',
                       axis.ticks.x = element_blank(),
                       axis.text.x = element_blank(),
                       legend.key.height = unit(0.1, "in"),
                       legend.key.width = unit(0.5, "in"))
  
  p.list <- c(p.list,list(p))
}

windows(height = 7,width = 6)
p <- p.list[[1]] + p.list[[2]] +
  p.list[[3]] + p.list[[4]] +
  p.list[[5]] + p.list[[6]] +
  plot_layout(ncol=2)#+plot_layout(ncol=2,nrow=3,heights = c(1.5,1.5,1.5),widths = c(3,3))
p
ggsave('outputs/sfig/SR_Category.jpg',width = 6,height = 7)
ggsave('outputs/sfig/SR_Category.eps',width = 6,height = 7)

# p <- ggplot(shp) +    
#   geom_sf(aes(fill = value,colour=value)) + # ,lwd =0  
#   scale_fill_gradientn(colors = mycol,
#                        name=y,
#                        na.value = 'gray95') +
#   scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide='none') +
#   scale_x_continuous(limits = c(-170,170)) +
#   theme_bw() + theme(panel.grid=element_blank(),
#                      legend.position = c(0.05,0.35),
#                      #legend.direction = 'horizontal',
#                      legend.key.height = unit(0.25, "in"),
#                      legend.key.width = unit(0.1, "in"))
