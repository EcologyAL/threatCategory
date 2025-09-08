setwd('F:/luoao/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','rasterVis','RColorBrewer','stringr'),require,character.only=T)

dat <- read.csv('data/spinfo/sp_cty.csv')
iucn <- read.csv('data/IUCNcategory/sp_assess.csv')
sn <- 'ssp370_limit'
iucn$IUCN <- iucn[[sn]]

dat <- left_join(dat,iucn[,c('speciesKey','IUCN')])
dat <- dat[!is.na(dat$IUCN),]
dat$IUCN <- factor(dat$IUCN,levels=c('LC','VU','EN','CR','EX')) %>% as.numeric()

dat$wt <- 1-(dat$IUCN-1)/max(dat$IUCN-1)
#dat$sov_a3[dat$sovereignt%in%c('China',"Taiwan")] <- 'CHN'
dat <- dat %>% rename(income_grp=Income.group)
#dat$income_grp[dat$sovereignt%in%c('China',"Taiwan")]
# cls <- read.csv('data/spinfo/CLASS.csv')
# left_join(pdat,cls[,2:3])

pdat <- dat %>% group_by(adm0_a3_is) %>% mutate(TSR=sum(IUCN>0),RLI=mean(wt)) %>% ungroup() %>% 
  dplyr::select(TSR,RLI,income_grp,adm0_a3_is) %>% distinct() %>% na.omit()
pdat$income_grp <- pdat$income_grp %>% str_remove("[1-5]. ") %>% str_remove(": OECD") %>% str_remove(": nonOECD")

mycol <- c("#0F8554","#DB95D7","#994E95","#733B70")
pdat$income_grp <- factor(pdat$income_grp,levels = c('High income','Upper middle income','Lower middle income','Low income'))
pdat$mycol <- factor(pdat$income_grp,levels = c('High income','Upper middle income','Lower middle income','Low income'),labels=mycol)
pdat <- pdat[!pdat$adm0_a3_is%in%'PRI',]

p4a <- ggplot(pdat,aes(x=TSR,y=RLI,label = adm0_a3_is,color=income_grp)) +
  geom_text(size=2)+
  scale_color_manual(values=levels(pdat$mycol),name='Income group')+
  xlab('species number') + 
  #ylim(c(0.65,1))+
  scale_x_log10() + theme_bw()+
  theme(legend.position = c(0.16,0.21),
        legend.key.height = unit(0.1, "inch"),
        legend.key.width = unit(0.1, "inch"),
        legend.background = element_rect(color='gray'), 
        #legend.background = element_rect(fill='transparent'), 
        text = element_text(size=6))
# p4a
# ggsave('results/figure/figs/figure4a.jpg',width = 5,height = 3,units='in',dpi=300)

####
p4b <- ggplot(pdat, aes(income_grp, RLI,color=income_grp)) + geom_boxplot() + 
  scale_color_manual(values=levels(pdat$mycol),name='Income group',guide=FALSE) +
  xlab('') + theme_bw()+
  #ylim(c(0.65,1))+
  theme(#legend.position = c(0.2,0.25),
        #legend.background = element_rect(fill='transparent'), 
        axis.text.x = element_text(angle = 15, hjust = 1),
        text = element_text(size=6))
#ggsave('results/figure/figs/figure4b.jpg',width = 2,height = 3,units='in',dpi=300)

library(patchwork)
p4a+p4b+p4c+plot_layout(nrow=1,widths = c(4,1.5,1.5))
ggsave('results/figure/figs/figure4.jpg',width = 7,height = 3,units='in',dpi=300)

################
mycol <- c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred")
#n.all <- nrow(red.list)
y.i <- 'IUCN'
pdat2 <- dat[,c("speciesKey",'income_grp',"IUCN")] %>% distinct()
pdat2$IUCN <- factor(pdat2$IUCN,labels=c('LC','VU','EN','CR','EX'))
g <- ggplot(pdat2, aes(x=income_grp)) + geom_bar(aes(fill = IUCN))+
  scale_fill_manual(values = c('transparent',mycol)) + #brewer.pal(5,'Reds')
  xlab("Scenories") + ylab("Species number")+  labs(fill = '') +
  theme_bw()+
  theme(text=element_text(size=8),
        axis.text = element_text(size=5),
        legend.key.height = unit(0.5, "line"),
        legend.key.size = unit(0.5, "line"))
g
