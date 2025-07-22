library(ggplot2);library(stringr);library(sf);library(dplyr);library(raster)
setwd('F:/luoao/RLI/')
sp.timespan <- read.csv('data/spinfo/spls_mark_all.csv')
sp.timespan$timespan[is.na(sp.timespan$timespan)] <- '2041-2070'

sp.range <- read.csv('data/spinfo/rangeChange/spInfo_half.csv')
sp.range <- sp.range[,c('speciesKey','sn_1981.2010')]
sp.range <- sp.range[,]
sp.range$eoo <- cut(sp.range$sn_1981.2010,c(0,1.01,50,200))


spcell <- read.csv('E:/luoa/PRIME/output/summary/cellInfo_10km/spcell_0_10_10km_1d_cell.csv')
spcell <- spcell %>% dplyr::select(-X) %>% na.omit()

#load('data/ecosys/10km_eco_ratio_future.rdata')
eco.area <- raster('data/ecosys/10km_eco_area.tif')
eco.area <- data.frame(cell=1:length(eco.area[]),area=eco.area[]) %>% na.omit()

load('data/spinfo/sp_ecosys.rdata')

# sns <- names(pred_probs_list)
# data.frame(scenarios=sns)
# tmp <- do.call('rbind',str_split(sns,'_'))
# tmp <- tmp[,c(2,4,6)]
# colnames(tmp) <- c('year','sn','ds')
# write.csv(cbind(sns,tmp),'data/scenarios_list.csv',row.names = F)

sp.eco <- read.csv('data/spinfo/spls_mark_all.csv')
sp.eco$loc <- cut(sp.eco$num_coords_c,c(0,1.1,5.1,10.1))
#levels(sp.eco$loc) <- c("CR","EN","VU")


scenarios <- read.csv('data/scenarios_list.csv')
#scenarios <- scenarios[!scenarios$year%in%'2071-2100',]


# load current ecosystem ----------------------------------------------------------


load(paste0('data/ecosys/future/sn_1981.2010.rdata'))
eco_ratio <- pred_probs
eco.names <- colnames(eco_ratio)[1:46]
eco_ratio <- left_join(eco_ratio,eco.area)
eco_ratio <- eco_ratio[!is.na(eco_ratio$area),]
eco_ratio[,eco.names] <- eco_ratio[,eco.names]*eco_ratio[,'area']
rm(pred_probs)

eco_ratio.short <- reshape2::melt(eco_ratio,id.vars=c('cell'))
colnames(eco_ratio.short) <- c('cell','eco','area')

eco_ratio_2010 <- eco_ratio
eco_ratio.short_2010 <- eco_ratio.short

#lapply(scenarios$sns, function(scenario){
for(scenario in scenarios$sns){
  print(scenario)
  load(paste0('data/ecosys/future/',scenario,'.rdata'))
  eco_ratio <- pred_probs
  eco_ratio <- left_join(eco_ratio,eco.area)
  eco_ratio <- eco_ratio[!is.na(eco_ratio$area),]
  eco_ratio[,eco.names] <- eco_ratio[,eco.names]*eco_ratio[,'area']

  for (eco in eco.names) {
    eco_ratio[,eco] <- (eco_ratio_2010[,eco] - eco_ratio[,eco])
  }

  #eco_ratio[,eco.names] <- eco_ratio[,eco.names]*eco_ratio[,'area']
  eco_ratio.short <- reshape2::melt(eco_ratio,id.vars=c('cell'))
  colnames(eco_ratio.short) <- c('cell','eco','area_decline')

  tmp <- sp.ecosys %>% left_join(eco_ratio.short) %>% left_join(eco_ratio.short_2010) %>% #na.omit() %>%
    group_by(speciesKey) %>% mutate(area_decline_tol=sum(area_decline,na.rm=T),area_tol=sum(area,na.rm=T)) %>%
    dplyr::select(speciesKey,area_decline_tol,area_tol) %>% distinct()
  tmp <- tmp[tmp$area_tol>0,]
  sp <- tmp$speciesKey[tmp$area_decline_tol/tmp$area_tol > 0] %>% unique()
  print(length(sp))
  rm(pred_probs)

  sp.eco$tmp <- sp.eco$speciesKey%in%sp
  #sp.eco$loc[!sp.eco$speciesKey%in%sp] <- NA
  sp.eco <- sp.eco %>% rename(!!scenario :=tmp)
}
#save(sp.eco,file='data/IUCNcategory/B1abiii/sp_eco.rdata')

load(file='data/IUCNcategory/B1abiii/sp_eco.rdata')
sp.eco <- sp.eco %>% left_join(sp.range) 
flag <- which(sp.eco$num_coords_c%in%1:3)
sp.eco$sn_1981.2010[flag] <- sp.eco$num_coords_c[flag]#*100
sp.eco$eoo <- 4 - as.numeric(cut(sp.eco$sn_1981.2010,c(0,1,49.9,199.9))) 

sp.eco <- sp.eco[!is.na(sp.eco$eoo),]
sp.eco <- sp.eco[(sp.eco$num_coords_c<=10)&(sp.eco$num_coords_c>0),]
sp.eco$loc <- 4 - as.numeric(cut(sp.eco$num_coords_c,c(0,1.1,5.1,10.1)))

sp.eco$B3a <- apply(cbind(as.numeric(sp.eco$eoo),as.numeric(sp.eco$loc)),1,min)
sp.eco$B3a <- sp.eco$B3a+1

scenarios <- read.csv('data/scenarios_list.csv')
scenarios$sns2 <- paste0(scenarios$sn,'_',scenarios$ds) %>% unique()
sns <- scenarios$sns2 %>% unique()

for (sn in sns) {
  print(sn)
  flag <- apply(sp.eco[,scenarios$sns[scenarios$sns2%in%sn]], 1, function(x)sum(x)) 
  sp.eco[[sn]] <- sp.eco$B3a
  sp.eco[[sn]][flag<3] <- 1
  sp.eco[[sn]][flag<1] <- 0
  sp.eco[[sn]] <- factor(sp.eco[[sn]],levels=0:4,labels = c('LC','NT','VU','EN','CR'))
}


#sp.eco <- sp.eco[,c('speciesKey',scenarios$sns[str_detect(scenarios$sns,'2071.2100')])]
#save(sp.eco,file='data/IUCNcategory/B1abiii/sp_eco.rdata')
#colnames(sp.eco)[-1] <- colnames(sp.eco)[-1] %>% str_remove('sn_2071.2100_') %>% str_remove('DS_') %>% str_remove('mean_')
sp.eco <- sp.eco[,c('speciesKey',sns)]
colnames(sp.eco)[-1] <- paste0('B_',colnames(sp.eco)[-1])
write.csv(sp.eco,'data/IUCNcategory/B1abiii/B1abiii.csv',row.names = F)
