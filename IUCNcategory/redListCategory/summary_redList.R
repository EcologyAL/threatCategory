setwd('F:/luoao/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','RColorBrewer','stringr'),require,character.only=T)#'rasterVis'
red.list.iucn <- read.csv('data/IUCNcategory/redlist_species_data/simple_summary.csv')

red.list.iucn$redlistCategoryRaw <- red.list.iucn$redlistCategory
red.list.iucn$redlistCategory[red.list.iucn$redlistCategory%in%c("Data Deficient")] <- "DD" 

red.list.iucn$redlistCategory[red.list.iucn$redlistCategory%in%c("Least Concern","Lower Risk/least concern")] <- "LC" 

red.list.iucn$redlistCategory[red.list.iucn$redlistCategory%in%c("Lower Risk/near threatened","Near Threatened",
                                                                 "Lower Risk/conservation dependent")] <- "NT"

red.list.iucn$redlistCategory[red.list.iucn$redlistCategory%in%c("Vulnerable")] <- "VU" 

red.list.iucn$redlistCategory[red.list.iucn$redlistCategory%in%"Endangered"] <- "EN" 

red.list.iucn$redlistCategory[red.list.iucn$redlistCategory%in%"Critically Endangered"] <- "CR" 

red.list.iucn$redlistCategory[red.list.iucn$redlistCategory%in%c("Extinct","Extinct in the Wild")] <- "EX"

red.list.iucn$redlistCategory <- factor(red.list.iucn$redlistCategory,levels=c('DD','LC','NT','VU','EN','CR','EX'))

red.list.iucn <- red.list.iucn[red.list.iucn$className%in%c('LILIOPSIDA','MAGNOLIOPSIDA'),]

red.list.iucn <- red.list.iucn[,c('scientificName','redlistCategory')]
colnames(red.list.iucn) <- c('species','redlistCategory')

splist <- read.csv('data/spinfo/spls_mark_all.csv')
splist <- splist[,c('speciesKey','species')]
red.list.iucn <- left_join(red.list.iucn,splist)
sum(is.na(red.list.iucn$speciesKey))

write.csv(red.list.iucn,'data/IUCNcategory/redlist_species_data/simple_summary_match_ag.csv',row.names = F)
