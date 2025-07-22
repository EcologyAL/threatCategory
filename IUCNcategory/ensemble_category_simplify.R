setwd('F:/luoao/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','RColorBrewer','stringr','circlize'),require,character.only=T)#'rasterVis'


# read new assessment -----------------------------------------------------


red.list <- read.csv('data/IUCNcategory/sp_assess_all.csv')# %>% na.omit()
sp <- read.csv('E:/LuoA/PRIME/splist/spls.csv')
sp <- sp$speciesKey[sp$class%in%c('Magnoliopsida','Liliopsida')]
red.list <- red.list[red.list$speciesKey%in%sp,]
sn <- 'ssp370_limit'

red.list$category <- red.list[,sn]
red.list$category[is.na(red.list$category)] <- 'DD'

# read iucn assessment ----------------------------------------------------


red.list.iucn <- read.csv('data/IUCNcategory/redlist_species_data/simple_summary_match_ag.csv') %>% 
  na.omit() %>% 
  right_join(red.list[,c('speciesKey','category')])
rm(red.list)


# ensemble category -------------------------------------------------------

red.list <- red.list.iucn
red.list$category <- factor(red.list$category,levels=c('DD','LC','NT','VU','EN','CR','EX'))
red.list$redlistCategory <- factor(red.list$redlistCategory,levels=c('DD','LC','NT','VU','EN','CR','EX'))

red.list$ensembleCategory <- red.list$category

red.list <- mutate(red.list,
                   ensembleCategory=ifelse(as.numeric(redlistCategory)>as.numeric(category),
                                           redlistCategory,
                                           category))
red.list$ensembleCategory <- factor(red.list$ensembleCategory,levels = (1:7),labels = (c('DD','LC','NT','VU','EN','CR','EX')))
red.list <- red.list[,c('species','speciesKey','category','redlistCategory','ensembleCategory')]


# calculate species weight ------------------------------------------------


red.list <- red.list %>%  
  mutate(redlistCategory2=as.numeric(redlistCategory)-2,
         category2=as.numeric(category)-2) %>% 
  mutate(wt_old = 1 - redlistCategory2/5,
         wt_new = 1 - category2/5) %>% 
  mutate(wt_old = ifelse(wt_old>1,NA,wt_old),
         wt_new = ifelse(wt_new>1,NA,wt_new)) 

ifelse(max(c(red.list$redlistCategory2,red.list$category2),na.rm = T)!=5,
       print(warning('MAX IS NOT 5!')),
       'Max is 5')

red.list <- red.list[,c('species','speciesKey','category','redlistCategory','ensembleCategory','wt_old','wt_new')]

write.csv(red.list,'data/IUCNcategory/sp_assess_simple.csv',row.names = F)
