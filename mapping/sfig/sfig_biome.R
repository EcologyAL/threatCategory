library(ggplot2)
setwd('~/myWorkshop/RLI/')
dat <-  read.csv('results/table/biome/biome.csv')
biome <- dat$Biome
#dat$Biome <- factor(dat$Biome,levels = unique(dat$Biome))
spls <- read.csv('data/outputs/spInfo/1dRnage/sp_biome.csv')
colnames(spls)
spls <- spls[,c(1,3)]

sp <-  read.csv(paths["spAssSim"])

spls <- spls[spls$biome%in%biome,]
#table(spls$biome)
spls$biome <- factor(spls$biome,biome)

dat <- left_join(sp,spls) %>% group_by(biome) %>% 
  reframe(RLI=mean(wt_intC,na.rm=T),RLI_old=mean(wt_redlistC,na.rm=T),
          Delta.RLI=mean(wt_intC,na.rm=T)-mean(wt_redlistC,na.rm=T))
dat <- dat[!dat$biome%in%NA,]


cols <- c("gray10","#1f77b4","#ff7f0e")
tmp <- dat
tmp$flag <- 'New ass.'
dat2 <- dat
dat2$RLI <- dat2$RLI_old
dat2$flag <- 'IUCN ass.'
dat2 <- rbind(dat2,tmp)

p1 <- ggplot(dat2, aes(x = biome, y = RLI,colour = flag)) +
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
ggsave('results/sfig/biome.jpg',plot=p1,width = 5,height=5)
p2 <- ggplot(dat, aes(x = biome, y = Delta.RLI)) +
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
ggsave('results/sfig/biome_delta.jpg',plot=p2,width = 5,height=5)
