library(ggplot2);library(stringr);library(sf);library(dplyr);library(raster)
setwd('F:/luoao/RLI/')
sp.timespan <- read.csv('data/spinfo/spls_mark_all.csv')
sp.timespan$timespan[is.na(sp.timespan$timespan)] <- '2041-2070'

spcell <- read.csv('E:/luoa/PRIME/output/summary/cellInfo_10km/spcell_0_10_10km_1d_cell.csv')
spcell <- spcell %>% dplyr::select(-X) %>% na.omit()

#load('data/ecosys/10km_eco_ratio_future.rdata')
eco.area <- raster('data/ecosys/10km_eco_area.tif')
eco.area <- data.frame(cell=1:length(eco.area[]),area=eco.area[]) %>% na.omit()

# eco.100m <- raster('data/ecosys/iucn_habitatclassification_composite_lvl2_ver004/iucn_habitatclassification_composite_lvl2_ver004.tif')
# xy <- raster('E:/luoa/PRIME/data/CHELSA/10km_climate_cell_land.tif') %>% coordinates()
# xy <- xy[spcell$cell,]
# spcell$eco <- extract(eco.100m,xy)
# sp.ecosys <- spcell[,c('speciesKey','cell','eco')] %>% distinct()
# sp.ecosys$eco <- paste0('ECO',sp.ecosys$eco)
# save(sp.ecosys,file='data/spinfo/sp_ecosys.rdata')
load('data/spinfo/sp_ecosys.rdata')

# sns <- names(pred_probs_list)
# data.frame(scenarios=sns)
# tmp <- do.call('rbind',str_split(sns,'_'))
# tmp <- tmp[,c(2,4,6)]
# colnames(tmp) <- c('year','sn','ds')
# write.csv(cbind(sns,tmp),'data/scenarios_list.csv',row.names = F)

sp.eco <- read.csv('data/spinfo/spls_mark_all.csv')
sp.eco$loc <- cut(sp.eco$num_coords_c,c(0,1.1,5.1,10.1))
levels(sp.eco$loc) <- c("CR","EN","VU")

scenarios <- read.csv('data/scenarios_list.csv')
#scenarios <- scenarios[scenarios$year%in%'2071-2100',]


# load current ecosystem ----------------------------------------------------------


load(paste0('data/ecosys/future/sn_1981.2010.rdata'))
eco_ratio <- pred_probs;rm(pred_probs)
eco.names <- colnames(eco_ratio)[1:46]
eco_ratio <- left_join(eco_ratio,eco.area)
eco_ratio <- eco_ratio[!is.na(eco_ratio$area),]
eco_ratio[,eco.names] <- eco_ratio[,eco.names]*eco_ratio[,'area']

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
  sp <- tmp$speciesKey[tmp$area_decline_tol/tmp$area_tol > 0.05] %>% unique()
  print(length(sp))
  rm(pred_probs)
  
  sp.eco$tmp <- sp.eco$loc
  sp.eco$loc[!sp.eco$speciesKey%in%sp] <- NA
  sp.eco <- sp.eco %>% rename(!!scenario :=tmp)
}
save(sp.eco,file='data/IUCNcategory/B1abiii/sp_eco.rdata')
sp.eco <- sp.eco[(sp.eco$num_coords_c<=10)&(sp.eco$num_coords_c>0),c('speciesKey',scenarios$sns)]
sp.eco <- sp.eco[,c('speciesKey',scenarios$sns[str_detect(scenarios$sns,'2071.2100')])]
#save(sp.eco,file='data/IUCNcategory/B1abiii/sp_eco.rdata')

colnames(sp.eco)[-1] <- colnames(sp.eco)[-1] %>% str_remove('sn_2071.2100_') %>% str_remove('DS_') %>% str_remove('mean_')
colnames(sp.eco)[-1] <- paste0('B_',colnames(sp.eco)[-1])
write.csv(sp.eco,'data/IUCNcategory/B1abiii/B1abiii.csv',row.names = F)
