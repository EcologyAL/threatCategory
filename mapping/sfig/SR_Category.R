shp <- read_sf(paths['baseMap'])
shp <- shp[,c('cell_poll','land')];shp$land <- shp$land%in%c('T','TRUE','1')
dat <- read.csv(paths['cellInfo']) %>% distinct()
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
  y <- c('(a) Total','(b) Threatened','(c) LC&NT','(d) VU','(e) EN','(f) CR&EX')[i]
  shp$value <- shp[[x]]
  shp$value[!shp$land] <- NA
  
  p <- ggplot(shp) +
    geom_sf(aes(fill = value, colour = value)) +
    scale_fill_gradientn(colors = mycol,
                         name = '',
                         na.value = 'gray95',
                         guide = guide_colorbar(
                           direction      = 'vertical',
                           barwidth       = unit(0.1, 'in'),  # narrow bar
                           barheight      = unit(0.5, 'in'),  # short bar
                           ticks          = TRUE,
                           label.position = 'right'
                         )) +
    scale_colour_gradientn(colors = mycol, na.value = 'gray95', guide = 'none') +
    scale_x_continuous(limits = c(-170, 170)) +
    labs(title = y) +
    theme_bw() +
    theme(panel.grid            = element_blank(),
          axis.text             = element_blank(),
          axis.ticks            = element_blank(),
          axis.title            = element_blank(),
          legend.position       = c(0.02, 0.1),
          legend.justification  = c(0, 0),
          legend.background     = element_rect(fill = NA, color = NA),
          legend.text           = element_text(size = 5))  # small label text
  
  p.list <- c(p.list, list(p))
}

p <- p.list[[1]] + p.list[[2]] +
  p.list[[3]] + p.list[[4]] +
  p.list[[5]] + p.list[[6]] +
  plot_layout(ncol = 2)

p
ggsave(paths['sfig_sr_category'], width = 6, height = 5)

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
