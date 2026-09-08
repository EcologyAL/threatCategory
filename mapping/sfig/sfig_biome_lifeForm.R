sp <-  read.csv(paths["spAssSim"])
lf <- read.csv(paths["lifeForm"])
colnames(lf)
lf <- lf[,c('speciesKey','lifeForm')]
# lf.list <- c("evergreen trees","drought_deciduous trees","cold_deciduous trees","needleleaf trees",
#              "evergreen shrubs","drought_deciduous shrubs","cold_deciduous shrubs",
#              "c3 grasses","c4 grasses","climbers","geophytes","succulents","therophytes","forbs")

lf$lifeForm <- lf$lifeForm %>% stringr::str_replace('drought_d','D') %>% 
  stringr::str_replace('cold_d','D') %>% 
  stringr::str_to_sentence()
lf.list <- c("Evergreen trees","Deciduous trees","Needleleaf trees",
             "Evergreen shrubs","Deciduous shrubs",
             "C3 grasses","C4 grasses","Climbers","Geophytes","Succulents","Therophytes","Forbs")

lf <- lf[lf$lifeForm%in%lf.list,]
table(lf$lifeForm)
lf$lifeForm <- factor(lf$lifeForm,lf.list)

dat <- left_join(sp,lf) %>% group_by(lifeForm) %>% 
  reframe(RLI=mean(wt_climC,na.rm=T),RLI_old=mean(wt_redlistC,na.rm=T),
          Delta.RLI=mean(wt_climC,na.rm=T)-mean(wt_redlistC,na.rm=T))
dat <- dat[!dat$lifeForm%in%NA,]


cols <- c("gray10","#1f77b4","#ff7f0e")
tmp <- dat
tmp$flag <- 'FCC'
dat2 <- dat

dat2$RLI <- dat2$RLI_old
dat2$flag <- 'IRL'
dat2 <- rbind(dat2,tmp)

gap <- 0.015
seg_dat <- dat %>%
  mutate(
    dy   = round(RLI - RLI_old,2),
    sgn  = sign(dy),
    y_start = RLI_old + sgn * gap,
    y_end   = RLI - sgn * gap,
    y_mid = (RLI_old + RLI)/2
  )

cols <- c("gray10","#1f77b4","#ff7f0e")
p1 <- ggplot(dat2, aes(x = lifeForm, y = RLI,colour = flag)) +
  geom_point(size=3)+
  xlab('')+
  labs(title='a')+
  scale_color_manual(values = cols) +
  theme_bw()+
  theme(
    legend.position = c(0.75,1.1),
    legend.direction = "horizontal",
    legend.margin = margin(t = 0.5, r = 1.5, b = 0.5, l = 1),
    legend.title = element_blank(),
    legend.background = element_rect(color = 'gray50'),
    legend.text = element_text(size = 8),
    legend.spacing = unit(1, "pt"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8)
  )+
  geom_text(data=seg_dat,
               aes(
                 x = lifeForm,
                 y = y_mid,
                 label = dy
               ), 
            angle=90,
            vjust=-0.25,
            size=3,
            inherit.aes = FALSE)+
  geom_segment(
    data = seg_dat,
    aes(
      x = lifeForm, xend = lifeForm,
      y = y_start, yend = y_end
    ),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(1.5, "mm"), type = "closed"),
    linewidth = 0.4,
    colour = "grey40"
  )
p1
#ggsave('results/sfig/lf.jpg',plot=p1,width = 5,height=3.5)

##
dat <-  read.csv(paths["spBiome"])
biome <- c("Tropical & Subtropical Moist Broadleaf Forests",
           "Deserts & Xeric Shrublands",                              
           "Tropical & Subtropical Grasslands, Savannas & Shrublands",
           "Tropical & Subtropical Dry Broadleaf Forests",            
           "Montane Grasslands & Shrublands",
           "Temperate Broadleaf & Mixed Forests",
           "Mediterranean Forests, Woodlands & Scrub",
           "Temperate Conifer Forests",
           "Mangroves",
           "Temperate Grasslands, Savannas & Shrublands",
           "Tropical & Subtropical Coniferous Forests",
           "Flooded Grasslands & Savannas",
           "Boreal Forests/Taiga",
           "Tundra")

sp <-  read.csv(paths["spAssSim"])
dat$biome <- factor(dat$biome,biome)


biome_2 <- c("Tropical & Subtropical \nMoist Broadleaf Forests",
             "Deserts & Xeric \nShrublands",                              
             "Tropical & Subtropical Grasslands, \nSavannas & Shrublands",
             "Tropical & Subtropical \nDry Broadleaf Forests",            
             "Montane Grasslands & Shrublands",
             "Temperate Broadleaf & Mixed Forests",
             "Mediterranean Forests, \nWoodlands & Scrub",
             "Temperate Conifer Forests",
             "Mangroves",
             "Temperate Grasslands, \nSavannas & Shrublands",
             "Tropical & Subtropical \nConiferous Forests",
             "Flooded Grasslands & \nSavannas",
             "Boreal Forests/Taiga",
             "Tundra")
levels(dat$biome) <- biome_2

dat <- left_join(sp,dat) %>% group_by(biome) %>% 
  reframe(RLI=mean(wt_climC,na.rm=T),RLI_old=mean(wt_redlistC,na.rm=T),
          Delta.RLI=mean(wt_climC,na.rm=T)-mean(wt_redlistC,na.rm=T))
dat <- dat[!dat$biome%in%NA,]

cols <- c("gray10","#1f77b4","#ff7f0e")
tmp <- dat
tmp$flag <- 'FCC'
dat2 <- dat
dat2$RLI <- dat2$RLI_old
dat2$flag <- 'IRL'
dat2 <- rbind(dat2,tmp)

gap <- 0.015  
seg_dat <- dat %>%
  mutate(
    dy   = round(RLI - RLI_old,2),
    sgn  = sign(dy),
    y_start = RLI_old + sgn * gap,
    y_end   = RLI - sgn * gap,
    y_mid = (RLI_old + RLI)/2
  )

cols <- c("gray10","#1f77b4","#ff7f0e")

p2 <- ggplot(dat2, aes(x = biome, y = RLI,colour = flag)) +
  geom_point(size=3)+
  xlab('')+
  labs(title='b')+
  scale_color_manual(values = cols) +
  theme_bw()+
  theme(
    legend.position = c(0.75,1.1),
    legend.direction = "horizontal",
    legend.margin = margin(t = 0.5, r = 1.5, b = 0.5, l = 1),
    legend.title = element_blank(),
    legend.background = element_rect(color = 'gray50'),
    legend.text = element_text(size = 8),
    legend.spacing = unit(1, "pt"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8)
  )+
  geom_text(data=seg_dat,
            aes(
              x = biome,
              y = y_mid,
              label = dy
            ), 
            angle=90,
            vjust=-0.25,
            size=3,
            inherit.aes = FALSE)+
  geom_segment(
    data = seg_dat,
    aes(
      x = biome, xend = biome,
      y = y_start, yend = y_end
    ),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(1.5, "mm"), type = "closed"),
    linewidth = 0.4,
    colour = "grey40"
  )
p2

p = p1+p2+plot_layout(ncol=1)
ggsave('results/sfig/rli_lifeForm_biome.jpg',plot=p,width = 5,height=6.5)
