setwd('D:/work/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','rasterVis','RColorBrewer','stringr','ggbeeswarm'),require,character.only=T)

# read new assessment -----------------------------------------------------


red.list <- read.csv('data/outputs/sp_assess_simple.csv')

red.list$category <- factor(red.list$ensembleCategory,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()
#red.list$redlistCategory <- factor(red.list$redlistCategory,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()


# read country species list and join categroy list ------------------------


sp_cty <- read.csv('data/outputs/')

dat <- left_join(sp_cty,red.list[,c('speciesKey','category','redlistCategory','wt_new','wt_old')])

dat$wt <- dat$wt_new
dat <- dat %>% rename(income_grp=Income.group)

pdat <- dat %>% group_by(adm0_a3_is) %>% 
  mutate(TSR=sum(category>1,na.rm=T),
         SR=sum(category>0,na.rm=T),
         RLI=mean(wt,na.rm=T)) %>% 
  ungroup() %>% 
  dplyr::select(TSR,SR,RLI,income_grp,adm0_a3_is) %>% 
  distinct() %>% 
  na.omit()
pdat$income_grp <- pdat$income_grp %>% str_remove("[1-5]. ") %>% str_remove(": OECD") %>% str_remove(": nonOECD")
pdat$type <- 'ICCA'

pdat2 <- dat %>% filter(!is.na(wt_old)) %>% 
  group_by(adm0_a3_is) %>% 
  mutate(TSR=sum(category>1,na.rm=T),
         SR=sum(category>0,na.rm=T),
         RLI=mean(wt_old,na.rm=)) %>% 
  ungroup() %>% 
  dplyr::select(TSR,SR,RLI,income_grp,adm0_a3_is) %>% 
  distinct() %>% 
  na.omit()
pdat2$type <- 'ICCA_joint'
pdat <- rbind(pdat,pdat2)

pdat2 <- dat %>% filter(!is.na(wt_old)) %>% 
  group_by(adm0_a3_is) %>% 
  mutate(TSR=sum(category>1,na.rm=T),
         SR=sum(category>0,na.rm=T),
         RLI=mean(wt_new,na.rm=)) %>% 
  ungroup() %>% 
  dplyr::select(TSR,SR,RLI,income_grp,adm0_a3_is) %>% 
  distinct() %>% 
  na.omit()
pdat2$type <- 'IUCNA'
pdat <- rbind(pdat,pdat2)

#g.mean <- mean(red.list$wt_new,na.rm=T)
g.mean.ICCA <- mean(pdat$RLI[pdat$type%in%'ICCA'],na.rm=T)  
g.mean.JICCA <- mean(pdat$RLI[pdat$type%in%'ICCA_joint'],na.rm=T)  
g.mean.IUCNA <- mean(pdat$RLI[pdat$type%in%'IUCNA'],na.rm=T)  

mycol <- 2:5
# mycol <- c("#0F8554","#DB95D7","#994E95","#733B70")
pdat$income_grp <- factor(pdat$income_grp,levels = c('High income','Upper middle income','Lower middle income','Low income'))
pdat$mycol <- factor(pdat$income_grp,levels = c('High income','Upper middle income','Lower middle income','Low income'),labels=mycol)
pdat <- pdat[!pdat$adm0_a3_is%in%'PRI',]

p4a <- ggplot(pdat[pdat$type%in%'ICCA',],aes(x=SR,y=RLI,label = adm0_a3_is,color=income_grp)) +
  geom_text(size=2)+
  scale_color_discrete(name='Income group')+
  #scale_color_manual(values=levels(pdat$mycol),name='Income group')+
  xlab('Species richness') +
  geom_hline(yintercept=g.mean.ICCA,col=1,lty=2)+
  #ylim(c(0.65,1))+
  scale_x_log10() + theme_bw()+
  theme(legend.position = c(0.17,0.165),
        legend.key.height = unit(0.1, "inch"),
        legend.key.width = unit(0.1, "inch"),
        legend.background = element_rect(color='gray'), 
        #legend.background = element_rect(fill='transparent'), 
        text = element_text(size=8),
        axis.text = element_text(size=6))
p4a
# p4a
# ggsave('results/figure/figs/figure4a.jpg',width = 5,height = 3,units='in',dpi=300)

####
library(ggbeeswarm)
library(ggdist)
pdat$income_grp2 <- pdat$income_grp
levels(pdat$income_grp2) <- str_remove(levels(pdat$income_grp),' income')

p4b <- ggplot(pdat[pdat$type%in%"ICCA",], aes(income_grp2, RLI)) + 
  stat_slab(fill='gray90') +
  stat_dotsinterval(aes(color=income_grp),fill='gray90',side="bottom",
                    size=6,
                    dotsize=0.5)+
  xlab('Income group') +
  geom_hline(yintercept=g.mean.ICCA,col=1,lty=2)+
  theme(axis.text.x = element_text(angle = 15, vjust = 0.8, hjust = 0.5),
        text = element_text(size=8),
        axis.text = element_text(size=6),
        legend.position = 'none')
p4b

p4a
ggsave('results/figure/figs/figure4a.jpg',width = 4,height = 3,units='in',dpi=300)
ggsave('results/figure/figs/figure4a.eps',width = 4,height = 3,units='in')
p4b
ggsave('results/figure/figs/figure4b.jpg',width = 3,height = 3,units='in',dpi=300)
ggsave('results/figure/figs/figure4b.eps',width = 3,height = 3,units='in')

# p4b <- ggplot(pdat, aes(income_grp, RLI,color=income_grp)) + geom_boxplot() + 
#   scale_color_manual(values=levels(pdat$mycol),
#                      name='Income group',
#                      guide=FALSE) +
#   xlab('') + theme_bw()+
#   #ylim(c(0.65,1))+
#   theme(#legend.position = c(0.2,0.25),
#     #legend.background = element_rect(fill='transparent'), 
#     axis.text.x = element_text(angle = 15, hjust = 1),
#     text = element_text(size=6))
# p4b
#ggsave('results/figure/figs/figure4b.jpg',width = 2,height = 3,units='in',dpi=300)

library(patchwork)
p4a+p4b+plot_layout(nrow=1,widths = c(4,3))
ggsave('results/figure/figs/figure4.jpg',width = 7,height = 3,units='in',dpi=300)

p4a+p4b+p4c+plot_layout(nrow=1,widths = c(4,1.5,1.5))
ggsave('results/figure/figs/figure4.jpg',width = 7,height = 3,units='in',dpi=300)

################
mycol <- c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred")
#n.all <- nrow(red.list)
y.i <- 'category'
pdat2 <- dat[,c("speciesKey",'income_grp',"category")] %>% distinct()
pdat2$category <- factor(pdat2$category,labels=c('LC','VU','EN','CR','EX'))
g <- ggplot(pdat2, aes(x=income_grp)) + geom_bar(aes(fill = category))+
  scale_fill_manual(values = c('transparent',mycol)) + #brewer.pal(5,'Reds')
  xlab("Scenories") + ylab("Species number")+  labs(fill = '') +
  theme_bw()+
  theme(text=element_text(size=8),
        axis.text = element_text(size=5),
        legend.key.height = unit(0.5, "line"),
        legend.key.size = unit(0.5, "line"))
g
