# read new assessment -----------------------------------------------------
library(emmeans)

red.list <- read.csv(paths['spAssSim'])

red.list$intCategory <- factor(red.list$intCategory,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()
red.list$intCategory_com <- factor(red.list$intCategory_com,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()
red.list$redlistCategory <- factor(red.list$redlistCategory,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()

spds <- read.csv(paths["spds1d"])
sp_cty <- read.csv(paths['spCty']) 

cty <- read.csv('data/map/economy/cty_cell_5percent.csv')
bio1 <- raster('../data/chelsa/CHELSA_bio1_1981-2010_V.2.1.tif')
cty$bio1 <- extract(bio1,xyFromCell(raster(),cty$cell_poll))

cty <- cty %>% group_by(cell_poll) %>% 
  slice_max(order_by = p, n = 1)
cty <- cty[,c('cell_poll','cty','bio1')] %>% rename(ne_id=cty) %>% 
  left_join(distinct(sp_cty[,c('ne_id','Code','adm0_a3_is','Income.group')]))

rne <- rnaturalearth::ne_countries()
rne <- rne[,'adm0_a3'] %>% rename(adm0_a3_is=adm0_a3)
rne$adm0_a3_is[rne$adm0_a3_is%in%'TWN'] <- 'CHN'
rne <- st_transform(rne, crs = "+proj=moll")
rne$area_km2 <- st_area(rne) / 1e9
cty <- left_join(cty,st_drop_geometry(rne)) %>% 
  dplyr::select(-adm0_a3_is) %>% distinct() %>% na.omit()
cty <- cty %>% group_by(Code) %>% mutate(bio1=mean(bio1,na.rm=T)) %>% ungroup()

sp_cell_cty <- left_join(spds,cty)
#ylab(expression(Species~(per~10^10~m^2)))

dat <- left_join(sp_cell_cty,red.list[,c('speciesKey',
                                         'intCategory','intCategory_com','redlistCategory',
                                         'wt_intC','wt_intC_com','wt_redlistC')])
dat <- dat %>% rename(income_grp=Income.group)

dat$category <- dat$intCategory %>% as.numeric()
dat$wt <- dat$wt_intC

#
pdat <- dat %>% dplyr::select(-cell_poll) %>% 
  distinct() %>% group_by(Code) %>% 
  mutate(TSR=sum(category>3,na.rm=T),
         SR=sum(category>1,na.rm=T),
         SD=sum(category>1,na.rm=T),
         Area=area_km2,
         Temperature=bio1,
         income_grp=income_grp,
         RLI=mean(wt,na.rm=T)) %>% 
  ungroup() %>% 
  dplyr::select(TSR,SR,SD,RLI,Area,Temperature,income_grp,Code) %>% 
  distinct() %>% 
  na.omit()

pdat$income_grp <- pdat$income_grp %>% str_remove("[1-5]. ") %>% str_remove(": OECD") %>% str_remove(": nonOECD")
pdat$income_grp <- pdat$income_grp %in% 'High income'

pdat$income_grp[pdat$income_grp %in% c('Low income','Lower middle income','Upper middle income')] <- 'Low and middle income' 

#
m1 <- lm(RLI ~ income_grp + log(Area) + log(SR),
         data=pdat)
summary(m1)      
anova(m1)        

emm <- emmeans(m1, ~ income_grp, type = "response")
pairs(emm, type = "response")
confint(pairs(emm, adjust = "none"))   

#
m1 <- lm(RLI ~ income_grp + log(Area) + log(SR) + 
           income_grp*log(SR) + income_grp*log(Area) + log(Area)*log(SR),
         data=pdat)
summary(m1)      

m1 <- lm(RLI ~ income_grp + log(Area) + log(SR) + 
           income_grp*log(Area) + income_grp*log(SR),
         data=pdat)
summary(m1)      



#
m1 <- lm(RLI ~ income_grp + log(SR),
         data=pdat)
summary(m1)      
anova(m1)        

emm <- emmeans(m1, ~ income_grp, type = "response")
pairs(emm, type = "response")
confint(pairs(emm, adjust = "none"))   

#
m1 <- lm(RLI ~ income_grp + log(Area) + log(SR) + Temperature,
         data=pdat)
summary(m1)      
anova(m1)     

m1 <- lm(RLI ~ income_grp + log(Area) + log(SR) + Temperature  + 
           income_grp*log(Area) + income_grp*log(SR) + income_grp*Temperature,
         data=pdat)
summary(m1)      
anova(m1)     

emm <- emmeans(m1, ~ income_grp, type = "response")
pairs(emm, type = "response")
confint(pairs(emm, adjust = "none"))   