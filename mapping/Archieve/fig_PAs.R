setwd('F:/luoao/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','RColorBrewer'),require,character.only=T)
#### load data
sp.assess <- read.csv('data/IUCNcategory/sp_assess.csv')
pa <- read.csv('data/spinfo/sp_PAs_ratio.csv')
sp.cty <- read.csv('data/spinfo/sp_cty.csv')

sp.assess <- left_join(sp.assess,pa) %>% na.omit() %>% left_join(sp.cty)

grps <- unique(sp.assess$Income.group)
grps <- c("Low income","Lower middle income","Upper middle income","High income")

sp.assess <- sp.assess[!sp.assess$ssp370_limit%in%'LC',]
dat.pa <- data.frame()
for (grp in grps) {
  sp.assess.tmp <- sp.assess[order(sp.assess$PAsRatio,decreasing = T),]
  sp.assess.tmp <- sp.assess.tmp[sp.assess.tmp$Income.group%in%grp,]
  sp.assess.tmp$cum <- 1:nrow(sp.assess.tmp)
  sp.assess.tmp$cum <- sp.assess.tmp$cum/nrow(sp.assess.tmp)
  
  tmp <-  sp.assess.tmp %>% group_by(PAsRatio) %>% 
    mutate(speciesNum=max(cum)) %>% ungroup() %>% 
    dplyr::select(PAsRatio,speciesNum) %>% distinct()
  tmp$grp <- grp
  dat.pa <- rbind(dat.pa,tmp)
}
dat.pa$grp <- dat.pa$grp %>% factor(levels = grps)

dat.pa$PAsRatio <- dat.pa$PAsRatio*100
dat.pa$speciesNum <- dat.pa$speciesNum*100
dat.pa <- dat.pa[dat.pa$PAsRatio>0,]
p4c <- ggplot(dat.pa) + 
  geom_line(aes(x=PAsRatio,y=speciesNum,group=grp,color=grp))+
  scale_color_manual(values=rev(mycol),guide=NULL)+
  ylim(c(0,100)) +
  xlab('PAs coverage threshold (%)') + ylab('Proportion of species exceeding PAs coverage threshold (%)')+
  theme_bw()+
  theme(text = element_text(size=6));p4c

# ggplot(dat) + 
#   geom_density(aes(PAsRatio,group=grp,fill=grp),alpha=.3)+
#   scale_color_manual(values=c("#0F8554","#DB95D7","#994E95","#733B70"))+
#   theme_bw()

grp.pa <- sp.assess %>% group_by(Income.group) %>% mutate(PAsRatio=mean(PAsRatio)) %>% 
  ungroup() %>% dplyr::select(Income.group,PAsRatio) %>% distinct() %>% na.omit()

ggplot(dat) + 
  geom_line(aes(x=PAsRatio,y=speciesNum,group=grp,color=grp))+
  scale_color_manual(values=rev(mycol))+
  geom_vline(data=grp.pa,aes(PAsRatio,col=Income.group))+
  theme_bw()


ggplot() + 
  #geom_line(aes(x=PAsRatio,y=speciesNum,group=grp,color=grp))+
  #scale_color_manual(values=rev(mycol))+
  geom_vline(grp.pa$PAsRatio[1])+
  theme_bw()
