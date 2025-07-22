#LC VU EN CR EX
library(ggplot2);library(stringr);library(sf);library(dplyr);library(raster)
setwd('F:/luoao/RLI/')

A3c <- read.csv('data/IUCNcategory/A3c/A3c.csv')
colnames(A3c) <- colnames(A3c) %>% str_remove('_DS');colnames(A3c)[-1] <- paste0('A_',colnames(A3c)[-1])
B1ab <- read.csv('data/IUCNcategory/B1abiii/B1abiii.csv')

spls <- read.csv('data/spinfo/spls_mark_all.csv')
spls <- spls %>% left_join(A3c) %>% left_join(B1ab)
spls$D[(spls$num_coords_c>0)&(spls$num_coords_c<=5)] <- 'NT'

scenario <- read.csv('data/scenarios_list.csv')
scenario <- paste0(scenario$sn,'_',scenario$ds) %>% unique()# <- 
spls <- spls[,c('speciesKey',paste0('A_',scenario),paste0('B_',scenario),'D') ]
scenario_ab <- c(paste0('A_',scenario),paste0('B_',scenario),'D') 

for (sn in scenario_ab ) {
  spls[,sn] <- factor(spls[,sn],levels=c('LC','NT','VU','EN','CR','EX')) %>% as.numeric()
}

scenario_ab <- rbind(paste0('A_',scenario),paste0('B_',scenario),'D') 
sp.assess <- data.frame(speciesKey=spls$speciesKey) 

for (i in 1:ncol(scenario_ab)) {
  print(i)
  tmp <- spls[,scenario_ab[,i]] %>% apply(1, max, na.rm=T)
  sp.assess[[scenario[i]]] <- tmp %>% factor(levels=1:6,labels = c('LC','NT','VU','EN','CR','EX'))
}
write.csv(sp.assess,'data/IUCNcategory/sp_assess.csv',row.names = F)

# 2100 --------------------------------------------------------------------

spls <- read.csv('data/spinfo/spls_mark_all.csv')
spls <- spls %>% left_join(A3c) %>% left_join(B1ab)
spls$D[(spls$num_coords_c>0)&(spls$num_coords_c<=5)] <- 'NT'

scenario <- read.csv('data/scenarios_list.csv')
scenario2 <- paste0(scenario$sn,'_',scenario$ds,'_2100') %>% unique()
scenario <- paste0(scenario$sn,'_',scenario$ds) %>% unique()
spls <- spls[,c('speciesKey',paste0('A_',scenario2),paste0('B_',scenario),'D') ]
scenario_ab <- c(paste0('A_',scenario2),paste0('B_',scenario),'D') 

for (sn in scenario_ab ) {
  spls[,sn] <- factor(spls[,sn],levels=c('LC','NT','VU','EN','CR','EX')) %>% as.numeric()
}

scenario_ab <- rbind(paste0('A_',scenario2),paste0('B_',scenario),'D') 
sp.assess_2100 <- data.frame(speciesKey=spls$speciesKey) 

for (i in 1:ncol(scenario_ab)) {
  tmp <- spls[,scenario_ab[,i]] %>% apply(1, max, na.rm=T)
  sp.assess_2100[[scenario[i]]] <- tmp %>% factor(levels=1:6,labels = c('LC','NT','VU','EN','CR','EX'))
}


write.csv(sp.assess_2100,'data/IUCNcategory/sp_assess_2100.csv',row.names = F)

#### summary IUCN category

#### summary IUCN category
# spls$IUCN <- apply(spls[,c('A3c','B1abiii','D2')], 1, max,na.rm=T)
# spls$IUCN[is.infinite(spls$IUCN)] <- NA
# spls <- spls %>% filter(!is.na(IUCN))
# 
# spls$IUCN_2100 <- apply(spls[,c('A3c_2100','B1abiii_2100','D2')], 1, max,na.rm=T)
# spls$IUCN_2100[is.infinite(spls$IUCN_2100)] <- NA
# spls <- spls %>% filter(!is.na(IUCN))
# 
# flag <- as.numeric(table(spls$IUCN))/sum(table(spls$IUCN)) 
# smy <- data.frame(Categories=c('LC' ,'VU', 'EN', 'CR', 'EX'),
#                   species_num=table(spls$IUCN) %>% as.integer(),
#                   species_raio=round(flag,3))
# smy
# 
# #### summary IUCN category, all to 2071-2100
# spls$IUCN <- apply(spls[,c('A3c_2100','B1abiii_2100','D2')], 1, max,na.rm=T)
# spls$IUCN[is.infinite(spls$IUCN)] <- NA
# spls <- spls %>% filter(!is.na(IUCN))
# 
# flag <- as.numeric(table(spls$IUCN))/sum(table(spls$IUCN)) 
# smy <- data.frame(Categories=c('LC' ,'VU', 'EN', 'CR', 'EX'),
#                   species_num=table(spls$IUCN) %>% as.integer(),
#                   species_raio=round(flag,3))
# smy
# 
# ### only A3
# flag <- as.numeric(table(spls$A3c))/sum(table(spls$A3c)) 
# smy <- data.frame(Categories=c('LC' ,'VU', 'EN', 'CR', 'EX'),
#                   specie %>% %>% <- <- <- <- <- <- 