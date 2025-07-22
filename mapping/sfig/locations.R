
spls <- read.csv('data/outputs/spInfo/sp_assess_all.csv')
spls <- spls[,c('num_coords_c','speciesKey')]
spls <- spls[spls$num_coords_c>0,]
spls$num_coords_c <- spls$num_coords_c-0.0001

nlabs <- c(1,3,5,10,20,100,100000)
p <- ggplot(data=spls, aes(x = log(num_coords_c))) +
  geom_histogram(
    aes(y = after_stat(count)),  
    breaks = log(nlabs),
    binwidth = 1,                  
    fill = "#69b3a2",              
    color = "#e9ecef",   
    #closed = 'left',
    alpha = 0.9                    
  ) +
  scale_x_continuous(breaks = log(nlabs),
                     labels = nlabs) +
  theme_minimal(base_size = 8) +
  labs(
    x = "Number of locations",
    y = "Number of Species",
  ) +
  
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = "grey85",
      linewidth = 0.2
    )
  )
p1 <- p
#ggsave(p,'outputs/sfig/locations/locations_a.csv')

# locations on map --------------------------------------------------------


shp <- read_sf('data/map/data_1d/data_1d_land.shp')

sp.ds.1d.raw <- read.csv('data/spcell_1d/sp_cellpoll_all.csv')
sp.ds.1d <- left_join(sp.ds.1d.raw,spls)
sp.ds.1d <- sp.ds.1d[sp.ds.1d$num_coords_c>0,]

loc <-  sp.ds.1d %>% group_by(cell_poll) %>% 
  mutate(numLoc=mean(num_coords_c)) %>% 
  ungroup() %>% 
  dplyr::select(cell_poll,numLoc) %>% distinct() 

shp <- shp[,c('cell_poll','land')]
shp <- left_join(shp,loc)
shp$numLoc[!shp$land] <- NA
shp$numLoc_ori <- shp$numLoc
shp$numLoc <- shp$numLoc_ori %>% log()
# mapping ---------------------------------------------------

# mycol <- hcl.colors(100, palette = "viridis")
# #mycol <- mycol[c(seq(1,50,2),51:100)]
# p <- ggplot(shp) +    
#   geom_sf(aes(fill = numLoc,colour=numLoc)) + # ,lwd =0  
#   scale_fill_gradientn(colors = mycol,
#                        name='',
#                        na.value = 'gray95') +
#   scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide='none') +
#   scale_x_continuous(limits = c(-170,170)) +
#   theme_bw() + theme(panel.grid=element_blank(),
#                      legend.position = c(0.08,0.35),
#                      axis.text.x = element_blank(),
#                      axis.text.y = element_blank(),
#                      axis.ticks = element_blank(),
#                      #legend.direction = 'horizontal',
#                      legend.key.height = unit(0.2, "in"),
#                      legend.key.width = unit(0.15, "in")) #+ 
# p
# ggsave('outputs/sfig/notIntCompare/figure2c_em.pdf',width = 6,height = 3)
# ggsave('outputs/sfig/notIntCompare/figure2c_em.jpg',width = 6,height = 3,dpi=300)
# 
# shp$RLI_new[!shp$land] <- NA
# mypalette <- colorRampPalette(c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred"))
# mycol <- rev(mypalette(100))
# mycol <- mycol[c(1:50,seq(51,100,3))]
# 
# p <- ggplot(shp) +    
#   geom_sf(aes(fill = RLI_new,colour=RLI_new)) + # ,lwd =0  
#   scale_fill_gradientn(colors = mycol,
#                        name='',
#                        na.value = 'gray95') +
#   scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide='none') +
#   scale_x_continuous(limits = c(-170,170)) +
#   theme_bw() + theme(panel.grid=element_blank(),
#                      legend.position = c(0.08,0.3),
#                      axis.text.x = element_blank(),
#                      axis.text.y = element_blank(),
#                      axis.ticks = element_blank(),
#                      #legend.direction = 'horizontal',
#                      legend.key.height = unit(0.12, "in"),
#                      legend.key.width = unit(0.06, "in")) #+ 
# p
# ggsave('outputs/sfig/notIntCompare/figure2b.pdf',width = 4,height = 2)
# ggsave('outputs/sfig/notIntCompare/figure2b.jpg',width = 4,height = 2,dpi=300)
# 
