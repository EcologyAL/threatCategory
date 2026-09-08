library(ggplot2)
sp <-  read.csv(paths["spAssSim"])
lf <- read.csv('../data/species/growthForm/species_lifefrom_NOgenusFill.csv')
colnames(lf)
lf <- lf[,c('speciesKey','lifeForm')]
lf.list <- c("evergreen trees","drought_deciduous trees","cold_deciduous trees","needleleaf trees",
                "evergreen shrubs","drought_deciduous shrubs","cold_deciduous shrubs",
                "c3 grasses","c4 grasses","climbers","geophytes","succulents","therophytes","forbs")
             
lf <- lf[lf$lifeForm%in%lf.list,]
table(lf$lifeForm)
lf$lifeForm <- factor(lf$lifeForm,lf.list)

dat <- left_join(sp,lf) %>% group_by(lifeForm) %>% 
  reframe(RLI=mean(wt_intC,na.rm=T),RLI_old=mean(wt_redlistC,na.rm=T),
          Delta.RLI=mean(wt_intC,na.rm=T)-mean(wt_redlistC,na.rm=T))
dat <- dat[!dat$lifeForm%in%NA,]


cols <- c("gray10","#1f77b4","#ff7f0e")
tmp <- dat
tmp$flag <- 'New ass.'
dat2 <- dat
dat2$RLI <- dat2$RLI_old
dat2$flag <- 'IUCN ass.'
dat2 <- rbind(dat2,tmp)

cols <- c("gray10","#1f77b4","#ff7f0e")
p1 <- ggplot(dat2, aes(x = lifeForm, y = RLI,colour = flag)) +
  geom_point(size=1)+
  xlab('')+
  scale_color_manual(values = cols) +
  #scale_x_continuous(breaks=faDatPlot$Family.num,labels=faDatPlot$Family)+
  theme_bw()+
  theme(
    legend.position = c(0.3,0.9),
    legend.direction = "horizontal",
    legend.margin = margin(t = 0.5, r = 1.5, b = 0.5, l = 1),
    legend.title = element_blank(),
    legend.background = element_rect(color = 'gray50'),
    legend.text = element_text(size = 8),
    legend.spacing = unit(1, "pt"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8)
  )
ggsave('results/sfig/lf.jpg',plot=p1,width = 5,height=3.5)
p2 <- ggplot(dat, aes(x = lifeForm, y = Delta.RLI)) +
  geom_point(size=1)+
  xlab('')+
  scale_color_manual(values = cols) +
  #scale_x_continuous(breaks=faDatPlot$Family.num,labels=faDatPlot$Family)+
  theme_bw()+
  theme(
    legend.position = c(0.75,0.7),
    legend.direction = "horizontal",
    legend.margin = margin(t = 0.5, r = 1.5, b = 0.5, l = 1),
    legend.title = element_blank(),
    legend.background = element_rect(color = 'gray50'),
    legend.text = element_text(size = 8),
    legend.spacing = unit(1, "pt"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8)
  )
ggsave('results/sfig/lf_delta.jpg',plot=p2,width = 5,height=3.5)

