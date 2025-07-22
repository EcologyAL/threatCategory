setwd('~/myWorkshop/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','rasterVis','RColorBrewer','stringr','ggbeeswarm'),require,character.only=T)

# read new assessment -----------------------------------------------------


red.list <- read.csv('data/outputs/spInfo/sp_assess_simple.csv')

red.list$intCategory <- factor(red.list$intCategory,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()
red.list$intCategory_com <- factor(red.list$intCategory_com,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()
red.list$redlistCategory <- factor(red.list$redlistCategory,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()


# read country species list and join categroy list ------------------------

sp_cty <- read.csv('data/outputs/spInfo/1dRnage/sp_cty.csv')
sp_cty2 <- read.csv('data/outputs/spInfo/test/sp_cty_eco.csv') %>% dplyr::select(speciesKey,sov_a3_num) %>% distinct() %>%
  group_by(speciesKey) %>% mutate(endemism=n()) %>% ungroup()
sp_cty2 <- sp_cty2[,c('speciesKey','endemism')] %>% distinct()
sp_cty <- left_join(dplyr::select(sp_cty,-endemism),sp_cty2)
sp_pa <- read.csv('data/outputs/spInfo/1dRnage/sp_PAs')

dat <- left_join(sp_cty,red.list[,c('speciesKey',
                                    'intCategory','intCategory_com','redlistCategory',
                                    'wt_intC','wt_intC_com','wt_redlistC')])
dat <- dat %>% rename(income_grp=Income.group)
dat <- dat %>% left_join(sp_pa[,c('speciesKey','Protected')])

dat$category <- dat$intCategory %>% as.numeric()
dat$wt <- dat$wt_intC

pdat <- dat %>% group_by(Code) %>% 
  mutate(SR=sum(category>1,na.rm=T),
         TSR=sum(category>3,na.rm=T),
         PTSR=sum(category>3 & Protected,na.rm=T),
         RLI=mean(wt,na.rm=T),
         ESR=sum(category>1  & endemism==1,na.rm=T),
         ETSR=sum(category>3 & endemism==1,na.rm=T),
         PETSR=sum(category>3 & endemism==1 & Protected,na.rm=T),
         ERLI=mean(wt[endemism==1],na.rm=T),
         RLI_com=mean(wt_intC_com,na.rm=T),
         RLI_IUCN=mean(wt_redlistC,na.rm=T)) %>% 
  ungroup()  %>% 
  mutate(
    T.S.P = round(TSR/SR * 100,1),
    T.E.S.P  = round(ETSR/ESR * 100,1),
    P.T.S.P  = round(PTSR/TSR * 100,1),
    P.T.E.S.P  = round(PETSR/ETSR * 100,1),
    deltaRLI = round(RLI_com - RLI_IUCN,3)
  )%>% 
  dplyr::select(Economy,Code,
                SR, TSR, P.T.S.P,
                ESR, ETSR, P.T.E.S.P,
                T.S.P, T.E.S.P,
                RLI,ERLI,RLI_com,RLI_IUCN,deltaRLI) %>% 
  distinct() %>% 
  na.omit()
colnames(pdat) <- c('Country', 'Code',
                    'S.', 'T.S.', 'Protected T.S.(%)',
                    'E.S.', 'T.E.S.', 'Protected T.E.S.(%)',
                    'T.S.(%)', 'T.E.S.(%)',
                    'RLI','RLI (E.S.)','RLI (Com.S.)','RLI (IUCN)','Delta RLI')

# pdat$`T.S.(%)` <- round(pdat$`T.S.`/pdat$`S.` * 100,1)
# pdat$`T.E.S.(%)` <- round(pdat$`T.E.S.`/pdat$`E.S.` * 100,1)
# pdat$`Protected T.S.(%)` <- round(pdat$`Protected T.S.(%)`/pdat$`T.S.` * 100,1)
# pdat$`Protected T.E.S.(%)` <- round(pdat$`Protected T.E.S.(%)`/pdat$`T.E.S.` * 100,1)
# pdat$'Delta RLI' <- pdat$`RLI (Com.S.)`-pdat$`RLI (IUCN)`
pdat <- pdat[order(pdat$S.,decreasing = T),]

g.info <- pdat[1,]
g.info[1,] <- NA
g.info[1,'Country'] <- c('Country average')
g.info[1,'T.S.(%)'] <- round(mean(pdat$`T.S.(%)`,na.rm=T),0) 
g.info[1,'T.E.S.(%)'] <- round(mean(pdat$`T.E.S.(%)`,na.rm=T),0) 
g.info[1,'RLI'] <- mean(pdat$RLI,na.rm=T)
g.info[1,'RLI (E.S.)'] <- mean(pdat$`RLI (E.S.)`,na.rm=T)
g.info[1,'RLI (Com.S.)'] <- mean(pdat$RLI,na.rm=T)
g.info[1,'RLI (IUCN)'] <- mean(pdat$`RLI (E.S.)`,na.rm=T)
g.info[1,'Delta RLI'] <- mean(pdat$`Delta RLI`,na.rm=T)

g.info[1,'Protected T.S.(%)'] <- round(mean(pdat$`Protected T.S.(%)`,na.rm=T),0) 
g.info[1,'Protected T.E.S.(%)'] <- round(mean(pdat$`Protected T.E.S.(%)`,na.rm=T),0) 
pdat <- rbind(g.info,pdat)

pdat$RLI <- pdat$RLI %>% round(3)
pdat$`RLI (E.S.)` <- pdat$`RLI (E.S.)` %>% round(3)
pdat$`RLI (Com.S.)` <- pdat$`RLI (Com.S.)` %>% round(3)
pdat$`RLI (IUCN)` <- pdat$`RLI (IUCN)` %>% round(3)
pdat$`Delta RLI` <- pdat$`Delta RLI` %>% round(3)
pdat
write.csv(pdat,'outputs/table/country/cty_2.csv',row.names = F,na = '')


# mapping PA and RLI/PTS --------------------------------------------------
library(ggpmisc) ; library(ggplot2) ; library(patchwork)
#pdat <- read.csv('mus/RLI/data S3_summary in the country level.csv')
pdat <- pdat[-1,]

p1 <- ggplot(pdat,aes(x=`T.S.(%)`,y=`Protected T.S.(%)`,label = Code)) +
  geom_text(size=2)+
  xlab('Threatened Species (%)') +
  ylab('Protected Threatened Species (%)') +
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = TRUE,              
              color = "firebrick")+
  theme_bw()+
  labs(title = '(a)')+
  theme(panel.grid.minor = element_blank(),
        legend.key.height = unit(0.1, "inch"),
        legend.key.width = unit(0.1, "inch"),
        legend.background = element_rect(color='gray'), 
        #legend.background = element_rect(fill='transparent'), 
        text = element_text(size=8),
        axis.text = element_text(size=5)) + 
  stat_poly_eq(
  formula = y ~ x,
  aes(label = paste(after_stat(rr.label),  
                    after_stat(p.value.label),  
                    sep = "~~~")),
  parse = TRUE,
  label.x = "right",        
  label.y = 1.2,#"top",          
  size = 3.5)
p1
p2 <- ggplot(pdat,aes(x=RLI,y=`Protected T.S.(%)`,label = Code)) +
  geom_text(size=2)+
  labs(title = '(b)')+
  xlab('RLI') +
  ylab('Protected Threatened Species (%)') +
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = TRUE,              
              color = "firebrick")+
  theme_bw()+
  theme(#legend.position = c(0.17,1.1),
        panel.grid.minor = element_blank(),
        legend.key.height = unit(0.1, "inch"),
        legend.key.width = unit(0.1, "inch"),
        legend.background = element_rect(color='gray'), 
        #legend.background = element_rect(fill='transparent'), 
        text = element_text(size=8),
        axis.text = element_text(size=5)) + stat_poly_eq(
          formula = y ~ x,
          aes(label = paste(after_stat(rr.label),  # R²值
                            after_stat(p.value.label),  # p值
                            sep = "~~~")),
          parse = TRUE,
          label.x = "left",        
          label.y = 1.2,#label.y = "top",          
          size = 3.5                
        )
p2

#p1+p2+plot_layout(nrow=2)
#ggsave('outputs/sfig/cty/cty_pa.jpg',width = 5,height=6.6,dpi='print')

pdat$`Delta RLI`
p3 <- ggplot(pdat,aes(x=log(`S.`),y=`Delta RLI`,label = Code)) +
  geom_text(size=2)+
  xlab('log(species richness)') +
  ylab('ΔRLI(%)') +
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = TRUE,              
              color = "firebrick")+
  theme_bw()+
  labs(title = '(c)')+
  theme(panel.grid.minor = element_blank(),
        legend.key.height = unit(0.1, "inch"),
        legend.key.width = unit(0.1, "inch"),
        legend.background = element_rect(color='gray'), 
        #legend.background = element_rect(fill='transparent'), 
        text = element_text(size=8),
        axis.text = element_text(size=5)) + 
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(after_stat(rr.label),  
                      after_stat(p.value.label),  
                      sep = "~~~")),
    parse = TRUE,
    label.x = "right",        
    label.y = 1.2,#"top",          
    size = 3.5)
p3
p4 <- ggplot(pdat,aes(x=`T.S.(%)`,y=`Delta RLI`,label = Code)) +
  geom_text(size=2)+
  labs(title = '(d)')+
  xlab('Proportion of threatened species (%)') +
  ylab('ΔRLI') +
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = TRUE,              
              color = "firebrick")+
  theme_bw()+
  theme(#legend.position = c(0.17,1.1),
    panel.grid.minor = element_blank(),
    legend.key.height = unit(0.1, "inch"),
    legend.key.width = unit(0.1, "inch"),
    legend.background = element_rect(color='gray'), 
    #legend.background = element_rect(fill='transparent'), 
    text = element_text(size=8),
    axis.text = element_text(size=5)) + stat_poly_eq(
      formula = y ~ x,
      aes(label = paste(after_stat(rr.label),  # R²值
                        after_stat(p.value.label),  # p值
                        sep = "~~~")),
      parse = TRUE,
      label.x = "right",        
      label.y = 1.2,#label.y = "top",          
      size = 3.5                
    )
p4

# p3+p4+plot_layout(nrow=2)
# ggsave('outputs/sfig/cty/cty_deltaRLI.jpg',width = 5,height=6.6,dpi='print')
p1 + p2 + p3+p4+plot_layout(nrow=2) 
ggsave('outputs/sfig/cty/cty_all.jpg',width = 7,height=6.6,dpi='print')
