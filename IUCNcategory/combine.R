A3c <- read.csv(paths['A3c'])
colnames(A3c) <- colnames(A3c) %>% str_remove('_DS');colnames(A3c)[-1] <- paste0('A_',colnames(A3c)[-1])
B1ab <- read.csv(paths['B2'])

spls <- read.csv(paths['spMarkAll'])
spls <- spls %>% left_join(A3c) %>% left_join(B1ab)
spls$D[(spls$num_coords_c>0)&(spls$num_coords_c<=5)] <- 'NT'

scenario <- read.csv(paths[['scenario']])
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

spls <- read.csv(paths['spMarkAll'])
spls <- spls %>% left_join(A3c) %>% left_join(B1ab)
spls$D[(spls$num_coords_c>0)&(spls$num_coords_c<=5)] <- 'NT'
# for (sn in scenario_ab ) {
#   spls[,sn] <- factor(spls[,sn],levels=1:6,labels=c('LC','NT','VU','EN','CR','EX')) %>% as.character()
# }

sp.assess <- sp.assess %>% left_join(spls)
#sp.assess <-read.csv('data/IUCNcategory/sp_assess_all.csv')

sp <- read.csv(paths['spLS'])
table(sp$class)
sp <- sp$speciesKey[sp$class%in%c('Magnoliopsida','Liliopsida')]

sp.assess <- sp.assess[sp.assess$speciesKey%in%sp,]

sptss <- read.csv(paths["spTSS"])
sptss <- sptss$speciesKey[sptss$tss<=0.5]
sp.assess[sp.assess$speciesKey%in%sptss,str_detect(colnames(sp.assess),'(ssp)|A|B|D')] <- NA 
write.csv(sp.assess,paths['spAssAll'],row.names = F)
