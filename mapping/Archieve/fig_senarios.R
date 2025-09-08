setwd('F:/luoao/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','RColorBrewer','stringr'),require,character.only=T)#'rasterVis'

#### load data
#red.list <- read.csv('data/spinfo/spls_mark_all.csv')
red.list.short <- read.csv('data/IUCNcategory/sp_assess.csv') %>% na.omit()
red.list <- reshape2::melt(red.list.short,id.vars='speciesKey')
red.list <- cbind(do.call('rbind',str_split(red.list$variable,'_')),red.list) 
colnames(red.list) <- c('scenario','dispersal','speciesKey','group','category')

pdat <- red.list
#pdat$category[is.na(pdat$category)] <- 0

pdat$category <- factor(pdat$category,levels=c('LC','NT','VU','EN','CR','EX')) #,'EX'
pdat$dispersal <- factor(pdat$dispersal,levels=c('unlimit','limit','no'),
                         labels=c('Full dispersal','20 km/decade','No dispersal'))
pdat$scenario <- factor(pdat$scenario,levels=c('ssp126','ssp370','ssp585'),
                        labels=c('ssp1-2.6','ssp3-7.0','ssp5-8.5'))

mycol <- c("#70BE50","#CCE226","#FFF204","#F89D57","#ED1C2E","BLACK")
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
ggsave('results/figure/sfigs/senarios.jpg',plot=g1,height = 3,width = 6)
