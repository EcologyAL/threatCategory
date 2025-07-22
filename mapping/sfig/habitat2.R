setwd('D:/work/RLI/')
sapply(c('sf','reshape2','dplyr','ggplot2','raster','RColorBrewer','stringr','patchwork'),require,character.only=T)#'rasterVis'

shp <- read_sf('data/map/data_1d/data_1d_land.shp')
shp <- shp[,c('cell_poll','land')]

load('data/outputs/hab/hab_list.rdata')
hab.name <- read.csv('data/outputs/hab/ecosysNames.csv')
#hab.name$code <- paste0('ECO',hab.name$code)

names(hab.list)
ecos <- colnames(hab.list[[1]])[-1]
ecos <- ecos[ecos%in%colnames(hab.list[[2]])[-1]];ecos <- ecos[ecos%in%colnames(hab.list[[3]])[-1]]
ecos <- ecos[!str_detect(ecos,'00')] 
ecos <- ecos[nchar(ecos)<7]

#ecos.name <- read.csv('data/outputs/hab/')
hab.name <- data.frame(code=ecos) %>% left_join(hab.name)
hab.name$ecoName_lv12 %>% str_replace(' \xa8C ','_') %>% str_replace(' ?C ','_')

load('data/outputs/hab/hab_list.rdata')

areas <- colSums(hab.list[[1]][,-1],na.rm=T)
ecos <- ecos[ecos%in%names(areas)[areas > 100]] 

mycol <- rev(terrain.colors(100))
mypalette <- colorRampPalette(c("darkblue","#222B9D","lightblue","gray95","lightyellow","#9D2924","darkred"))
mycol2 <- rev(mypalette(100))
mycol2 <- mycol2[c(1:24,seq(25,75,2),76:100)]


# legend ------------------------------------------------------------------


p.list <- list()
eco <-  ecos[1]
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
                           breaks = c(0, 0.5, 1),
                           limits = c(0, 1), 
                           na.value = 'gray95') +
      scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide='none') +
      scale_x_continuous(limits = c(-170,170)) +
      #labs(title=y)+
      theme_void()+
      theme_bw() + theme(panel.grid=element_blank(),
                         #legend.position='none',
                         legend.position = 'bottom',
                         legend.direction = 'horizontal',
                         legend.key.height = unit(0.1, "in"),
                         legend.key.width = unit(0.3, "in"),
                         axis.ticks.x = element_blank(),
                         axis.text.x = element_blank()
      )
    if(i==1) p <- p + labs(title=hab.name$ecoName_lv12[hab.name$code%in%eco],cex=3) + theme(title = element_text(size=8))
    p.sublist <- c(p.sublist,list(p))
  }else{
    tmp$value <- tmp[,eco]
    tmp$change <- tmp$value - hab.list[[2]][,eco]
    shp.tmp <- left_join(shp,tmp[,c('cell_poll','value','change')])
    shp.tmp$change[abs(shp.tmp$value)<0.05] <-  NA
    shp.tmp$changeRatio <- shp.tmp$change/shp.tmp$value
    
    shp.tmp$value <- shp.tmp$change
    shp.tmp$value[!shp.tmp$land] <- NA
    
    p <- ggplot(shp.tmp) +    
      geom_sf(aes(fill = value,colour=value)) + # ,lwd =0  
      scale_fill_gradientn(colors = mycol2,
                           name='',
                           breaks = c(-0.5, 0, 0.5),
                           limits = c(-1, 1), 
                           na.value = 'gray95') +
      scale_colour_gradientn(colors = mycol2,
                             breaks = c(-0.5, 0, 0.5),
                             limits = c(-1, 1), 
                             na.value = 'gray95',guide='none') +
      scale_x_continuous(limits = c(-170,170)) +
      #labs(title=y)+
      theme_void()+
      theme_bw() + theme(panel.grid=element_blank(),
                         # legend.position='none',
                         legend.position = 'bottom',
                         legend.direction = 'horizontal',
                         legend.key.height = unit(0.1, "in"),
                         legend.key.width = unit(0.3, "in"),
                         axis.ticks.x = element_blank(),
                         axis.text.x = element_blank())
    #p
    p.sublist <- c(p.sublist,list(p))
  }
}
#p.sublist
p <- p.sublist[[1]] + p.sublist[[2]] + p.sublist[[3]] + plot_layout(ncol=3)
ggsave(paste0('outputs/sfig/habitat/legend.jpg'),width = 5,height = 2)


# mapping -----------------------------------------------------------------


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
                             breaks = c(0, 0.5, 1),
                             limits = c(0, 1), 
                             na.value = 'gray95') +
        scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide='none') +
        scale_x_continuous(limits = c(-170,170)) +
        #labs(title=y)+
        theme_void()+
        theme_bw() + theme(panel.grid=element_blank(),
                           legend.position='none',
                           #legend.position = 'bottom',
                           #legend.direction = 'horizontal',
                           # legend.key.height = unit(0.1, "in"),
                           # legend.key.width = unit(0.3, "in"),
                           axis.ticks.x = element_blank(),
                           axis.text.x = element_blank()
                           )
      if(i==1) p <- p + labs(title=hab.name$ecoName_lv12[hab.name$code%in%eco],cex=3) + theme(title = element_text(size=8))
      p.sublist <- c(p.sublist,list(p))
    }else{
      tmp$value <- tmp[,eco]
      tmp$change <- tmp$value - hab.list[[2]][,eco]
      shp.tmp <- left_join(shp,tmp[,c('cell_poll','value','change')])
      shp.tmp$change[abs(shp.tmp$value)<0.05] <-  NA
      shp.tmp$changeRatio <- shp.tmp$change/shp.tmp$value
      
      shp.tmp$value <- shp.tmp$change
      shp.tmp$value[!shp.tmp$land] <- NA
      
      p <- ggplot(shp.tmp) +    
        geom_sf(aes(fill = value,colour=value)) + # ,lwd =0  
        scale_fill_gradientn(colors = mycol2,
                             name='',
                             breaks = c(-0.5, 0, 0.5),
                             limits = c(-1, 1), 
                             na.value = 'gray95') +
        scale_colour_gradientn(colors = mycol2,
                               breaks = c(-0.5, 0, 0.5),
                               limits = c(-1, 1), 
                               na.value = 'gray95',guide='none') +
        scale_x_continuous(limits = c(-170,170)) +
        #labs(title=y)+
        theme_void()+
        theme_bw() + theme(panel.grid=element_blank(),
                           legend.position='none',
                           # legend.position = 'bottom',
                           # legend.direction = 'horizontal',
                           # legend.key.height = unit(0.1, "in"),
                           # legend.key.width = unit(0.3, "in"),
                           axis.ticks.x = element_blank(),
                           axis.text.x = element_blank())
      #p
      p.sublist <- c(p.sublist,list(p))
    }
  }
  p.list <- c(p.list,list(p.sublist))
}

for (i in 1:length(ecos)) {
  print(i)
  p.sublist <- p.list[[i]]
  if(i%%7==1){
    p <- p.sublist[[1]] + p.sublist[[2]] + p.sublist[[3]] + plot_layout(ncol=3)
  }else{
    p <- p + p.sublist[[1]] + p.sublist[[2]] + p.sublist[[3]]
  }
  
  if((i%%7==0)|(i==length(p.list))){
    ggsave(paste0('outputs/sfig/habitat/hab_',(i-1)%/%7,'.jpg'),width = 6,height = 7.5)
    ggsave(paste0('outputs/sfig/habitat/hab_',(i-1)%/%7,'.eps'),width = 6,height = 7.5)
  }
}

