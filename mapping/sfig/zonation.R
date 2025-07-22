# map ------------------------------------------------------
setwd('F:/luoao/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','RColorBrewer','patchwork'),require,character.only=T)


# load sp distribution data and join data --------------------------------------------------

rankmap <- raster('data/zonation/rankmap.tif')
rankmap <- data.frame(layer=1:64800,rank=rankmap[]) %>% na.omit()


shp <- read_sf('data/map/data_1d/data_1d_land.shp')

shp <- shp[,'layer']
shp <- left_join(shp,rankmap)

mypalette <- colorRampPalette(c('#5E4FA2','#66C2A5','#FFFFBF','#FDAE61','#9B0241'))
#mypalette <- colorRampPalette(c("#F2F1E6","#F3D78A","#F8984F","#C74647","#982C2C"))
mycol <- (mypalette(100))

p <- ggplot(shp) +    
  geom_sf(aes(fill = rank,colour=rank)) + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='Priority',
                       na.value = 'gray95') +
  scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide=FALSE) +
  scale_x_continuous(limits = c(-170,170)) +
  theme_bw() + theme(panel.grid=element_blank(),
                     legend.position = c(0.05,0.35),
                     #legend.direction = 'horizontal',
                     legend.key.height = unit(0.25, "in"),
                     legend.key.width = unit(0.1, "in"))

p

ggsave('results/figure/sfigs/zonation_abf.jpg',width = 7,height = 4.8)
ggsave('results/figure/sfigs/zonation_abf.eps',width = 7,height = 4.8)
