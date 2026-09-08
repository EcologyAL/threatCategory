sp.timespan <- read.csv(paths['spMarkAll'])
#sp.timespan$timespan%in%'' <- '2041-2070';sp.range$timespan[is.na(sp.range$timespan)] <- '2041-2070'
sp.range <- read.csv(paths['spRangeChange'])
sptss <- read.csv(paths["spTSS"])
sp <- sptss$speciesKey[sptss$tss>0.5]
sp.range <- sp.range[sp.range$speciesKey%in%sp,]

sp.range[,-(1:2)] <- (sp.range[,2]-sp.range[,-(1:2)])/sp.range[,2]*100
#sp.range <- sp.range %>% as.data.frame()
sp.range <- left_join(sp.range,sp.timespan)

for (i in which(str_detect(colnames(sp.range),'sn_20'))) {
  flag4 <- which(sp.range[,i]>=100)
  flag1 <- which(sp.range[,i]<=0)
  sp.range[flag4,i] <- 99.9
  sp.range[flag1,i] <- -0.1
  sp.range[,i] <- cut(sp.range[,i],c(-1,5,30,50,80,100)) %>% as.numeric()
  sp.range[,i] <- factor(sp.range[,i],levels=1:5,labels=c('LC','NT','VU','EN','CR')) %>% as.character()
}

sp.a3c <- sp.range
for (ds in c('DS_unlimit','DS_limit','DS_no')) {
  for (sn in 'ssp370') {
    sp.a3c$tmp <- NA
    for(year in c('2011-2040','2041-2070','2071-2100')){
      flag <- which(sp.a3c$timespan %in% year)
      sp.a3c[flag,'tmp'] <- sp.a3c[flag,paste0('sn_',str_replace(year,'-','.'),'_mean_',sn,'_',ds)]
    }
    colnames(sp.a3c)[colnames(sp.a3c)%in%'tmp'] <- paste0(sn,'_',ds)
    print(paste0(sn,'_',ds))
  }
}

for (ds in c('DS_unlimit','DS_limit','DS_no')) {
  for (sn in 'ssp370') {
    sp.a3c$tmp <- NA
    sp.a3c[,'tmp'] <- sp.a3c[,paste0('sn_','2071.2100','_mean_',sn,'_',ds)]
    colnames(sp.a3c)[colnames(sp.a3c)%in%'tmp'] <- paste0(sn,'_',ds,'_2100')
  }
}

cols <- c()
for (ds in c('DS_unlimit','DS_limit','DS_no')) {
  for (sn in 'ssp370') {
    cols <- c(cols,paste0(sn,'_',ds))
  }
}
for (ds in c('DS_unlimit','DS_limit','DS_no')) {
  for (sn in 'ssp370') {
    cols <- c(cols,paste0(sn,'_',ds,'_2100'))
  }
}

sp.a3c <- sp.a3c[,c('speciesKey',cols)]
write.csv(sp.a3c,paths['A3cNoEX'],row.names = F)

# sp.a3c <- read.csv('data/IUCNcategory/A3c/A3c_noEX.csv')
# EX <- read.csv('data/IUCNcategory/A3c/A3c_ex.csv')
# cols <- colnames(sp.a3c)[-1]
# for (col in cols) {
#   str_remove(col,c('sn_','_mean_'))
#   
#   EX$speciesKey[EX[,]]
#   flag <- sp.a3c$speciesKey%in%
#   sp.a3c[,col]
# }

# merge extinction species list -------------------------------------------


sp.a3c <- read.csv(paths['A3cNoEX'])
sp.ex <- read.csv(paths['A3cEX'])
sp.ex <- sp.ex[sp.ex$speciesKey%in%sp,]
sp.a3c <- left_join(sp.a3c,sp.timespan[,c('speciesKey','timespan')])

for (ds in c('DS_unlimit','DS_limit','DS_no')) {
  for (sn in 'ssp370') {
    for(year in c('2011-2040','2041-2070','2071-2100')){
      col <- paste0('sn_',str_replace(year,'-','.'),'_mean_',sn)
      
      flag <- which( (sp.a3c$timespan %in% year) &
                       (!is.na(sp.a3c[,paste0(sn,'_',ds)])) &
                       (sp.a3c$speciesKey%in%sp.ex$speciesKey[sp.ex[,col]]) )
      sp.a3c[flag,paste0(sn,'_',ds)] <- 'EX'
    }
    print(paste0(sn,'_',ds))
  }
}

# for (ds in c('DS_unlimit','DS_limit','DS_no')) {
#   for (sn in c('ssp126','ssp370','ssp585')) {
#     for(year in c('2011-2040','2041-2070','2071-2100')){
#       col <- paste0('sn_',str_replace(year,'-','.'),'_mean_',sn,'_',ds)
#       
#       flag <- which( (sp.a3c$timespan %in% year) & 
#                        (!is.na(sp.a3c[,paste0(sn,'_',ds)])) &
#                        (sp.a3c$speciesKey%in%sp.ex$speciesKey[sp.ex[,col]]) )
#       sp.a3c[flag,paste0(sn,'_',ds)] <- 'EX'
#     }
#     print(paste0(sn,'_',ds))
#   }
# }
# 
# for (ds in c('DS_unlimit','DS_limit','DS_no')) {
#   for (sn in c('ssp126','ssp370','ssp585')) {
#     col <- paste0('sn_',str_replace('2071.2100','-','.'),'_mean_',sn,'_',ds)
#     
#     flag <- which( (sp.a3c$timespan %in% year) & 
#                      (!is.na(sp.a3c[,paste0(sn,'_',ds,'_2100')])) &
#                      (sp.a3c$speciesKey%in%sp.ex$speciesKey[sp.ex[,col]]) )
#     sp.a3c[flag,paste0(sn,'_',ds,'_2100')] <- 'EX'
#   }
# }

write.csv(sp.a3c,paths['A3c'],row.names = F)
