setwd('F:/luoao/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','RColorBrewer','stringr'),require,character.only=T)#'rasterVis'

#### load data
#red.list <- read.csv('data/spinfo/spls_mark_all.csv')
red.list.short <- read.csv('data/outputs/spInfo/sp_assess_all.csv') #%>% na.omit()
red.list <- reshape2::melt(red.list.short[,1:10],id.vars='speciesKey')
red.list$value[is.na(red.list$value)] <- 'DD'
red.list <- cbind(do.call('rbind',str_split(red.list$variable,'_')),red.list) 
colnames(red.list) <- c('scenario','dispersal','speciesKey','group','category')

sp <- read.csv('data/outputs/spInfo/spls.csv')
sp <- sp$speciesKey[sp$class%in%c('Magnoliopsida','Liliopsida')]
red.list <- red.list[red.list$speciesKey%in%sp,]

pdat <- red.list
#pdat$category[is.na(pdat$category)] <- 0

pdat$category <- factor(pdat$category,levels=c('DD','LC','NT','VU','EN','CR','EX')) #,'EX'
pdat$dispersal <- factor(pdat$dispersal,levels=c('unlimit','limit','no'),
                         labels=c('Full dispersal','20 km/decade','No dispersal'))
pdat$scenario <- factor(pdat$scenario,levels=c('ssp126','ssp370','ssp585'),
                        labels=c('ssp1-2.6','ssp3-7.0','ssp5-8.5'))

mycol <- c("#D1D1C6","#70BE50","#CCE226","#FFF204","#F89D57","#ED1C2E","BLACK")
#mycol <- c("#FFF204","#F89D57","#ED1C2E","#8B0000")

n.all <- nrow(red.list.short)
g <- ggplot(pdat, aes(x=scenario))
g1 <- g + geom_bar(aes(fill = category))+
  scale_fill_manual(values = c(mycol)) +
  #xlab("") + ylab("Species number")+  labs(fill = '') +
  scale_y_continuous(breaks = c(0,n.all*0.25,n.all/2,n.all*0.75,n.all),labels = c(0,25,50,75,100)) +
  xlab("Socioeconomic pathways") + ylab("Percentage (%)")+  labs(fill = '') +
  facet_grid(.~dispersal) +
  theme_bw()+
  theme(text=element_text(size=8),
        axis.text = element_text(size=5),
        legend.key.height = unit(1.5, "line"),
        legend.key.size = unit(1, "line"))
g1
#ggsave('results/figure/sfigs/senarios.jpg',plot=g1,height = 3,width = 6)

#
pdat.s <- pdat
pdat.s$category <- as.character(pdat.s$category)%in%c('VU','EN','CR','EX')
n.sp <- length(unique(pdat$speciesKey))
ptable <- pdat.s %>% group_by(group,category) %>% mutate(n=n(),p=n()/n.sp) %>% ungroup() %>% 
  dplyr::select(group,category,n,p) %>% distinct() %>% filter(category)
range(ptable$p)
# 39.7%
# 31.8%
# 53.8%
ptable[7:9,'p'] - ptable[4:6,'p']
ptable[c(3,6,9),'p'] - ptable[c(1,4,7),'p']
