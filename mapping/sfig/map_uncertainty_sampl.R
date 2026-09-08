library(viridis);library(ggpmisc)
# load map data -----------------------------------------------------
sp.ds.1d <- read.csv(paths["spds1d"])
cellInfo <- read.csv(paths["cellInfo"])

sp.tss <- read.csv(paths["spTSS"])
sp.ds.1d <- left_join(sp.ds.1d,sp.tss[,c('speciesKey','tss','auc')]) %>% na.omit()

sp.sampl <- read.csv(paths['spSampl']) %>% rename(SI_kNN=SI_knn) %>% 
  mutate(SI_GRID=SI_GRID_MAX) %>% dplyr::select(speciesKey,SI_GRID,SI_kNN)
  
cell.sampl <- left_join(sp.ds.1d,sp.sampl) %>% group_by(cell_poll) %>% 
  reframe(SI_GRID=round(mean(SI_GRID,na.rm=T),0),
          SI_kNN=round(mean(SI_kNN,na.rm=T),0)) %>% as.data.frame()

dat <- sp.ds.1d %>% group_by(cell_poll) %>% 
  reframe(SR_A=sum(!is.na(tss)),TSS=mean(tss,na.rm=T),AUC=mean(auc,na.rm=T))
dat <- left_join(dat,cellInfo[,c('RLI_A','cell_poll')]) %>% left_join(cell.sampl)
dat$RLI <- dat$RLI_A

# mapping -----------------------------------------------------------------

shp <- read_sf(paths['baseMap'])
shp <- shp[,c('cell_poll','land','land2')]
shp <- left_join(shp,dat)
shp[shp$land%in%c(0,'F','FALSE'),-(1:2)] <- NA
#shp[shp$RLI%in%1,-1] <- NA
shp[(shp$SR_A < 10)%in%TRUE,-1] <- NA

#mycol <- hcl.colors(100, palette = "viridis")%>% rev()
mycol <- plasma(100) %>% rev()


# TSS ---------------------------------------------------------------------


p1 <- ggplot(shp) +    
  geom_sf(aes(fill = TSS),color='transparent') + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='mean TSS',
                       na.value = 'gray95') +
  scale_x_continuous(limits = c(-170,170)) +
  scale_y_continuous(limits = c(-55,80)) +
  theme_bw() + 
  theme(panel.grid=element_blank(),
        legend.position = c(0.55,0.07),
        legend.title = element_text(size=5),
        legend.text =  element_text(size=5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = 'horizontal',
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        legend.key.height = unit(0.03, "in"),
        legend.key.width = unit(0.2, "in")) 
#p1

p2 <- ggplot(shp,aes(x=TSS,y=RLI)) +
  geom_point(size = 0.1, alpha = 0.1,shape=19)+
  xlab('mean TSS') +
  ylab(expression(RLI[INT])) +
  #ylab('RLI') +
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        aspect.ratio = 1,
        legend.key.height = unit(0.1, "inch"),
        legend.key.width = unit(0.1, "inch"),
        legend.background = element_rect(color='gray'), 
        text = element_text(size=7),,
        axis.text = element_text(size=7),) + 
  stat_correlation(aes(label = after_stat(rr.label)),
                   label.x = "left",        
                   label.y = "bottom",          
                   size = 2)
#p2
#p1 + p2 + plot_layout(nrow=1,width=c(3,1),heights = 1)



# AUC ---------------------------------------------------------------------


p3 <- ggplot(shp) +    
  geom_sf(aes(fill = AUC),color='transparent') + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='mean AUC',
                       na.value = 'gray95') +
  scale_x_continuous(limits = c(-170,170)) +
  scale_y_continuous(limits = c(-55,80)) +
  theme_bw() + 
  theme(panel.grid=element_blank(),
        legend.position = c(0.55,0.07),
        legend.title = element_text(size=5),
        legend.text =  element_text(size=5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = 'horizontal',
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        legend.key.height = unit(0.03, "in"),
        legend.key.width = unit(0.2, "in")) 
#p3

p4 <- ggplot(shp,aes(x=AUC,y=RLI)) +
  geom_point(size = 0.1, alpha = 0.1,shape=19)+
  xlab('mean AUC') +
  ylab(expression(RLI[INT])) +
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        aspect.ratio = 1,
        legend.key.height = unit(0.1, "inch"),
        legend.key.width = unit(0.1, "inch"),
        legend.background = element_rect(color='gray'), 
        text = element_text(size=7),,
        axis.text = element_text(size=7),) + 
  stat_correlation(aes(label = after_stat(rr.label)),
                   label.x = "left",        
                   label.y = "bottom",          
                   size = 2)
#p3 + p4 + plot_layout(nrow=1,width=c(3,1),heights = 1)

# SI_GRID ---------------------------------------------------------------------

#mycol <- viridis(100) %>% rev()

p5 <- ggplot(shp) +    
  geom_sf(aes(fill = SI_GRID),color='transparent') + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       trans  = "log10",
                       name='SI (grid_based)',
                       na.value = 'gray95') +
  scale_x_continuous(limits = c(-170,170)) +
  scale_y_continuous(limits = c(-55,80)) +
  theme_bw() + 
  theme(panel.grid=element_blank(),
        legend.position = c(0.6,0.07),
        legend.title = element_text(size=5),
        legend.text =  element_text(size=5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = 'horizontal',
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        legend.key.height = unit(0.03, "in"),
        legend.key.width = unit(0.2, "in")) 
#p5

p6 <- ggplot(shp,aes(x=SI_GRID,y=RLI)) +
  geom_point(size = 0.03, alpha = 0.1,shape=19)+
  xlab('SI (grid_based)') +
  ylab(expression(RLI[INT])) +
  theme_bw()+
  scale_x_log10()+
  theme(panel.grid.minor = element_blank(),
        aspect.ratio = 1,
        legend.key.height = unit(0.1, "inch"),
        legend.key.width = unit(0.1, "inch"),
        legend.background = element_rect(color='gray'), 
        text = element_text(size=7),,
        axis.text = element_text(size=7),) + 
  stat_correlation(aes(label = after_stat(rr.label)),
                   label.x = "left",        
                   label.y = "bottom",          
                   size = 2)
p6b <- ggplot(shp,aes(x=SI_GRID,y=TSS)) +
  geom_point(size = 0.03, alpha = 0.1,shape=19)+
  xlab('SI (distance_based)') +
  ylab('Mean TSS') +
  theme_bw()+
  scale_x_log10()+
  theme(panel.grid.minor = element_blank(),
        aspect.ratio = 1,
        legend.key.height = unit(0.1, "inch"),
        legend.key.width = unit(0.1, "inch"),
        legend.background = element_rect(color='gray'), 
        text = element_text(size=7),,
        axis.text = element_text(size=7),) + 
  stat_correlation(aes(label = after_stat(rr.label)),
                   label.x = "left",        
                   label.y = "top",          
                   size = 2)

#p5 + p6 + plot_layout(nrow=1,width=c(3,1),heights = 1)

# SI_kNN ---------------------------------------------------------------------


p7 <- ggplot(shp) +    
  geom_sf(aes(fill = SI_kNN),color='transparent') + # ,lwd =0  
  scale_fill_gradientn(colors = rev(mycol),
                       trans  = "log10",
                       guide = guide_colorbar(reverse = TRUE),
                       name='SI (distance_based)',
                       na.value = 'gray95') +
  scale_x_continuous(limits = c(-170,170)) +
  scale_y_continuous(limits = c(-55,80)) +
  theme_bw() + 
  theme(panel.grid=element_blank(),
        legend.position = c(0.6,0.07),
        legend.title = element_text(size=5),
        legend.text =  element_text(size=5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = 'horizontal',
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        legend.key.height = unit(0.03, "in"),
        legend.key.width = unit(0.2, "in")) 
#p7

p8 <- ggplot(shp,aes(x=SI_kNN,y=RLI)) +
  geom_point(size = 0.03, alpha = 0.1,shape=19)+
  #scale_x_continuous(trans = "reverse") +
  scale_x_log10()+
  xlab('SI (distance_based)') +
  ylab(expression(RLI[INT])) +
  theme_bw()+
  #labs(title = '(h)')+
  theme(panel.grid.minor = element_blank(),
        aspect.ratio = 1,
        legend.key.height = unit(0.1, "inch"),
        legend.key.width = unit(0.1, "inch"),
        legend.background = element_rect(color='gray'), 
        text = element_text(size=7),,
        axis.text = element_text(size=7),) + 
  stat_correlation(aes(label = after_stat(rr.label)),
                   label.x = "left",        
                   label.y = "bottom",          
                   size = 2)
p8b <- ggplot(shp,aes(x=SI_kNN,y=TSS)) +
  geom_point(size = 0.03, alpha = 0.1,shape=19)+
  #scale_x_continuous(trans = "reverse") +
  scale_x_log10()+
  xlab('SI (distance_based)') +
  ylab('Mean TSS') +
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        aspect.ratio = 1,
        legend.key.height = unit(0.1, "inch"),
        legend.key.width = unit(0.1, "inch"),
        legend.background = element_rect(color='gray'), 
        text = element_text(size=7),,
        axis.text = element_text(size=7),) +
  stat_correlation(aes(label = after_stat(rr.label)),
                   label.x = "left",        
                   label.y = "top",          
                   size = 2)

p <- p1 + p3 +
  plot_layout(ncol = 1,nrow=2,widths = 4,heights = c(2,2))
ggsave(paths['sfig_map_uncertainty'] %>% str_replace('.jpg','_TSS_AUC.pdf'),plot=p,width = 4,height = 4,units = "in")

p <- p2 + p4 +
  plot_layout(ncol = 1,nrow=2,widths = 2,heights = c(2,2))
ggsave(paths['sfig_map_uncertainty'] %>% str_replace('.jpg','_TSS_AUC_b.pdf'),plot=p,width = 2,height = 4,units = "in")

p <- p5 + p7 + 
  plot_layout(ncol = 1,nrow=2,widths = 4,heights = c(2,2))
  #plot_layout(ncol = 1,nrow=2,widths = 3.5,heights = c(1.8,1.8))
p
ggsave(paths['sfig_map_uncertainty'] %>% str_replace('.jpg','_SI.pdf'),plot=p,width = 4,height = 4,units = "in")
#ggsave(paths['sfig_map_uncertainty'] %>% str_replace('.jpg','_SI.pdf'),plot=p,width = 3.6,height = 3.6,units = "in")

p <- p6 + p8 +
  plot_layout(ncol = 1,nrow=2,widths = 2,heights = c(2,2))
ggsave(paths['sfig_map_uncertainty'] %>% str_replace('.jpg','__SI_b12.pdf'),plot=p,width = 2,height = 4,units = "in")

# p <- p6 + p6b + plot_layout(nrow=2)+ 
#   plot_layout(ncol = 2,nrow=1,heights = 1.8,widths = c(1.8,1.8))
# p
# ggsave(paths['sfig_map_uncertainty'] %>% str_replace('.jpg','_SI_b1.pdf'),plot=p,width = 3.6,height = 1.8,units = "in")
# 
# p <- p8 + p8b + plot_layout(nrow=2)+ 
#   plot_layout(ncol = 2,nrow=1,heights = 1.8,widths = c(1.8,1.8))
# p
# ggsave(paths['sfig_map_uncertainty'] %>% str_replace('.jpg','_SI_b2.pdf'),plot=p,width = 3.6,height = 1.8,units = "in")
