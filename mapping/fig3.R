# load map data -----------------------------------------------------

dat <- read.csv(paths['cellInfo']) %>% distinct()
dat$p_threat[dat$p_threat==100] <- NA
dat$p_VU[dat$p_VU==100] <- NA
dat$p_EN[dat$p_EN==100] <- NA
dat$p_CREX[dat$p_CREX==100] <- NA

# mapping -----------------------------------------------------------------

shp <- read_sf(paths['baseMap'])
shp <- shp[,c('cell_poll','land','land2')]
shp <- left_join(shp,dat)

shp[!shp$land%in%'T',-1] <- NA
shp[shp$land2%in%FALSE,c('p_VU','p_EN','p_CREX')] <- NA

mycol <- hcl.colors(100, palette = "viridis")%>% rev()

p1 <- ggplot(shp) +    
  geom_sf(aes(fill = SR_THR),color='transparent') + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='',
                       na.value = 'gray95') +
  #scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide="none") +
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
  labs(title='  Threatened species richness')
p1

mycol <- hcl.colors(100, palette = "viridis")%>% rev()
p2 <- ggplot(shp) +    
  geom_sf(aes(fill = p_threat),color='transparent') + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='(%)',
                       na.value = 'gray95') +
  #scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide=FALSE) +
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
  labs(title='  Percent of threatened species')
p2

p3 <- ggplot(shp) +    
  geom_sf(aes(fill = p_VU),color='transparent') + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='(%)',
                       na.value = 'gray95') +
  #scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide=FALSE) +
  scale_x_continuous(limits = c(-170,170)) +
  scale_y_continuous(limits = c(-125,80)) +
  theme_bw() + 
  theme(panel.grid=element_blank(),
        legend.position = c(0.5,0.18),
        legend.title = element_text(size=5),
        legend.text =  element_text(size=5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = 'horizontal',
        legend.key.height = unit(0.025, "in"),
        legend.key.width = unit(0.2, "in")) +
  labs(title='  VU')
p3

p4 <- ggplot(shp) +    
  geom_sf(aes(fill = p_EN),color='transparent') + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='(%)',
                       na.value = 'gray95') +
  #scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide=FALSE) +
  scale_x_continuous(limits = c(-170,170)) +
  scale_y_continuous(limits = c(-125,80)) +
  theme_bw() + 
  theme(panel.grid=element_blank(),
        legend.position = c(0.5,0.18),
        legend.title = element_text(size=5),
        legend.text =  element_text(size=5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = 'horizontal',
        legend.key.height = unit(0.025, "in"),
        legend.key.width = unit(0.2, "in")) +
  labs(title='  EN')
p4

p5 <- ggplot(shp) +    
  geom_sf(aes(fill = p_CREX),color='transparent') + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='(%)',
                       na.value = 'gray95') +
  #scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide=FALSE) +
  scale_x_continuous(limits = c(-170,170)) +
  scale_y_continuous(limits = c(-125,80)) +
  theme_bw() + 
  theme(panel.grid=element_blank(),
        legend.position = c(0.5,0.18),
        legend.title = element_text(size=5),
        legend.text =  element_text(size=5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = 'horizontal',
        legend.key.height = unit(0.025, "in"),
        legend.key.width = unit(0.2, "in")) +
  labs(title='  CR and EX')
p5

#windows(height = 4.8,width = 7)
p1row <- p1+p2+plot_layout(nrow = 1,widths = c(3,3),heights = 2.4)
ggsave(paths['fig3a'],width = 6,height = 2.1)
#ggsave('results/v3/fig3/figure3a.jpg',width = 6,height = 2.1)
#ggsave('outputs/fig3/figure3a.eps',width = 6,height = 2.4)

p2row <- p3+p4+p5+plot_layout(nrow=1,widths = c(2,2,2),heights = 1.8)
#ggsave('outputs/fig3/figure3b.jpg',width = 6,height = 1.6)
ggsave(paths['fig3b'],width = 6,height = 1.6)

