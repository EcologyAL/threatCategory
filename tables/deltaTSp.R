# read new assessment -----------------------------------------------------

red.list <- read.csv(paths['spAssAll'])
sp <- read.csv(paths['spLS'])
sp <- sp$speciesKey[sp$class%in%c('Magnoliopsida','Liliopsida')]
red.list <- red.list[red.list$speciesKey%in%sp,]

red.list.iucn <- read.csv(paths['IUCNredList']) %>% na.omit() 
red.list.all <- left_join(red.list,red.list.iucn)


sns <- colnames(red.list)[2:10]
sn <- 'ssp370_limit'

dat <- data.frame()
for(sn in sns){
  red.list <- red.list.all
  red.list$climCategory <- red.list[,sn]
  red.list <-  red.list[,c('speciesKey','climCategory',"redlistCategory")]
  
  red.list$climCategory <- factor(red.list$climCategory,levels=c('DD','LC','NT','VU','EN','CR','EX'))
  red.list$redlistCategory <- factor(red.list$redlistCategory,levels=c('DD','LC','NT','VU','EN','CR','EX'))
  red.list$intCategory <- red.list$climCategory
  flag <- which(as.numeric(red.list$redlistCategory)>as.numeric(red.list$intCategory))
  red.list$intCategory[flag] <- red.list$redlistCategory[flag]
  flag <- which(is.na(red.list$climCategory)&(!is.na(red.list$redlistCategory)))
  red.list$intCategory[flag] <- red.list$redlistCategory[flag]
  red.list$intCategory <- factor(red.list$intCategory,levels=c('DD','LC','NT','VU','EN','CR','EX'))
  
  red.list <- red.list[,c('speciesKey','climCategory','redlistCategory','intCategory')]
  sp.rl <- red.list$speciesKey[!is.na(red.list$redlistCategory)] 
  
  p.int <- sum(red.list$intCategory%in%c('VU','EN','CR','EX'))/sum(red.list$intCategory%in%c('DD','LC','NT','VU','EN','CR','EX'))*100 
  p.rl <- sum(red.list$redlistCategory%in%c('VU','EN','CR','EX'))/sum(red.list$redlistCategory%in%c('DD','LC','NT','VU','EN','CR','EX'))*100
  p.intShareSp <- sum(red.list$intCategory[red.list$speciesKey%in%sp.rl]%in%c('VU','EN','CR','EX'))/
    sum(red.list$intCategory[red.list$speciesKey%in%sp.rl]%in%c('DD','LC','NT','VU','EN','CR','EX'))*100 
  
  dat<- rbind(dat,data.frame(sn=sn,
             INT=round(p.int,1),
             INT_SharedSp=round(p.intShareSp,1),
             IRL=round(p.rl,1),
             deltaINT=round(p.int-p.rl,1),
             deltaINT_SharedSp=round(p.intShareSp-p.rl,1)))
}
dat$sn <- dat$sn %>% 
  str_replace('ssp126','ssp1-2.6') %>% 
  str_replace('ssp379','ssp3-7.0') %>% 
  str_replace('ssp585','ssp5-8.5') %>% 
  str_replace('unlimit','Full dispersal') %>% 
  str_replace('limit','20 km/decade') %>% 
  str_replace('no','No dispersal') %>% 
  str_replace('_',', ')
for (i in 2:ncol(dat)) {
  dat[,i] <- paste0(round(dat[,i],1),'%')  
}

write.csv(dat,paths["stable_deltaTsp"],row.names = F)
