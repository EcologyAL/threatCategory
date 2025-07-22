setwd('D:/work/RLI/')
sapply(c('sf','reshape2','dplyr','ggplot2','raster','RColorBrewer','stringr','patchwork'),require,character.only=T)#'rasterVis'

shp <- read_sf('data/map/data_1d/data_1d_land.shp')
shp <- shp[,c('cell_poll','land')]

load('data/outputs/hab/hab_list.rdata')
hab.name <- read.csv('data/outputs/hab/hab_list_name.csv')
hab.name$Eco_Code <- paste0('ECO',hab.name$Eco_Code)

names(hab.list)
ecos <- colnames(hab.list[[1]])[-1]
ecos <- ecos[ecos%in%colnames(hab.list[[2]])[-1]];ecos <- ecos[ecos%in%colnames(hab.list[[3]])[-1]]
ecos <- ecos[!str_detect(ecos,'00')] 
ecos <- ecos[nchar(ecos)<7]

hab.name <- data.frame(Eco_Code=ecos) %>% left_join(hab.name)
hab.name$Eco_name %>% str_replace(' \xa8C ','_') %>% str_replace(' ?C ','_')

load('data/outputs/hab/hab_list.rdata')

areas <- colSums(hab.list[[1]][,-1],na.rm=T)
ecos <- ecos[ecos%in%names(areas)[areas > 100]] 

#cells <- hab.list[[i]][,1]
#mypalette <- colorRampPalette(c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred"))
#mycol <- rev(mypalette(100))
#mycol <- mycol[c(seq(1,50,2),51:100)]
mycol <- rev(terrain.colors(100))
mypalette <- colorRampPalette(c("darkblue","lightblue","lightyellow","darkred"))
mycol2 <- rev(mypalette(100))

p.list <- list()
for (eco in ecos) {
  p.sublist <- list()
  for (i in 1:3) {
    tmp <- hab.list[[i]]
    #if(any(colnames(tmp)%in%eco)) #tmp[[eco]] <- 0
    
    if(i!=3){
      shp.tmp <- left_join(shp,tmp[,c('cell_poll',eco)])
      shp.tmp$value <- shp.tmp[[4]]
      if(i==2) shp.tmp$value[abs(shp.tmp$value)<0.05] <-  0
      shp.tmp$value[!shp.tmp$land] <- NA
      
      p <- ggplot(shp.tmp) +    
        geom_sf(aes(fill = value,colour=value)) + # ,lwd =0  
        scale_fill_gradientn(colors = mycol,
                             name='',
                             na.value = 'gray95') +
        scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide='none') +
        scale_x_continuous(limits = c(-170,170)) +
        #labs(title=y)+
        theme_void()+
        theme_bw() + theme(panel.grid=element_blank(),
                           legend.position = 'bottom',
                           legend.direction = 'horizontal',
                           axis.ticks.x = element_blank(),
                           axis.text.x = element_blank(),
                           legend.key.height = unit(0.1, "in"),
                           legend.key.width = unit(0.3, "in"))
      if(i==1) p <- p + labs(title=hab.name$Eco_name[hab.name$Eco_Code%in%eco])
      p.sublist <- c(p.sublist,list(p))
    }else{
      tmp$value <- tmp[,eco]
      tmp$change <- tmp$value - hab.list[[2]][,eco]
      shp.tmp <- left_join(shp,tmp[,c('cell_poll','value','change')])
      shp.tmp$change[abs(shp.tmp$value)<0.05] <-  NA
      shp.tmp$changeRatio <- shp.tmp$change/shp.tmp$value
      
      shp.tmp$value <- shp.tmp$change
      shp.tmp$value[!shp.tmp$land] <- NA
      #shp.tmp$value <-  as.numeric(shp.tmp$change>=0)-0.5
      # p <- ggplot(shp.tmp) +    
      #   geom_sf(aes(fill = value,colour=value)) + # ,lwd =0  
      #   #labs(title=y)+
      #   theme_void()+
      #   theme_bw() + theme(panel.grid=element_blank(),
      #                      legend.position = 'bottom',
      #                      legend.direction = 'horizontal',
      #                      axis.ticks.x = element_blank(),
      #                      axis.text.x = element_blank(),
      #                      legend.key.height = unit(0.1, "in"),
      #                      legend.key.width = unit(0.3, "in"))
      # p
      
      p <- ggplot(shp.tmp) +    
        geom_sf(aes(fill = value,colour=value)) + # ,lwd =0  
        scale_fill_gradientn(colors = mycol2,
                             name='',
                             na.value = 'gray95') +
        scale_colour_gradientn(colors = mycol2,na.value = 'gray95',guide='none') +
        scale_x_continuous(limits = c(-170,170)) +
        #labs(title=y)+
        theme_void()+
        theme_bw() + theme(panel.grid=element_blank(),
                           legend.position = 'bottom',
                           legend.direction = 'horizontal',
                           axis.ticks.x = element_blank(),
                           axis.text.x = element_blank(),
                           legend.key.height = unit(0.1, "in"),
                           legend.key.width = unit(0.3, "in"))
      #p
      p.sublist <- c(p.sublist,list(p))
    }
  }
  p.list <- c(p.list,list(p.sublist))
}

for (i in 1:length(ecos)) {
  p.sublist <- p.list[[i]]
  if(i%%4==1){
    p <- p.sublist[[1]] + p.sublist[[2]] + p.sublist[[3]] + plot_layout(ncol=3)
  }else{
    p <- p + p.sublist[[1]] + p.sublist[[2]] + p.sublist[[3]]
  }

  if((i%%4==0)|(i==length(p.list))){
    ggsave(paste0('outputs/sfig/habitat/hab_',(i-1)%/%4,'.jpg'),width = 6,height = 7.5)
    ggsave(paste0('outputs/sfig/habitat/hab_',(i-1)%/%4,'.eps'),width = 6,height = 7.5)
  }
}
# windows(height = 7,width = 6)
# p

windows(height = 7.5,width = 6)
i <- 0
p <- p.list[[1+i*12]] + p.list[[2+i*12]] + p.list[[3+i*12]] + 
  p.list[[4+i*12]] + p.list[[5+i*12]] + p.list[[6+i*12]] +
  p.list[[7+i*12]] + p.list[[8+i*12]] + p.list[[9+i*12]] +
  p.list[[10+i*12]] + p.list[[11+i*12]] + p.list[[12+i*12]]+
  plot_layout(ncol=3)
p
ggsave('outputs/sfig/hab_1.jpg',width = 6,height = 7.5)
ggsave('outputs/sfig/hab_1.eps',width = 6,height = 7.5)

i <- 1
p <- p.list[[1+i*12]] + p.list[[2+i*12]] + p.list[[3+i*12]] + 
  p.list[[4+i*12]] + p.list[[5+i*12]] + p.list[[6+i*12]] +
  p.list[[7+i*12]] + p.list[[8+i*12]] + p.list[[9+i*12]] +
  p.list[[10+i*12]] + p.list[[11+i*12]] + p.list[[12+i*12]]+
  plot_layout(ncol=3)
p
ggsave('outputs/sfig/hab_2.jpg',width = 6,height = 7.5)
ggsave('outputs/sfig/hab_2.eps',width = 6,height = 7.5)

i <- 2
p <- p.list[[1+i*12]] + p.list[[2+i*12]] + p.list[[3+i*12]] + 
  p.list[[4+i*12]] + p.list[[5+i*12]] + p.list[[6+i*12]] +
  p.list[[7+i*12]] + p.list[[8+i*12]] + p.list[[9+i*12]] +
  p.list[[10+i*12]] + p.list[[11+i*12]] + p.list[[12+i*12]]+
  plot_layout(ncol=3)
p
ggsave('outputs/sfig/hab_3.jpg',width = 6,height = 7.5)
ggsave('outputs/sfig/hab_3.eps',width = 6,height = 7.5)

i <- 3
p <- p.list[[1+i*12]] + p.list[[2+i*12]] + p.list[[3+i*12]] + 
  p.list[[4+i*12]] + p.list[[5+i*12]]+
  plot_layout(ncol=3)
p
ggsave('outputs/sfig/hab_4.jpg',width = 6,height = 7.5)
ggsave('outputs/sfig/hab_4.eps',width = 6,height = 7.5)
