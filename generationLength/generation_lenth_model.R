# Set working directory and load packages

setwd("~")
sapply(c('raster','dplyr','stringr'), require, character.only=T)

# Read trait table and select variables
trait <- read.csv('data/trait/trait/Traits_Mean_GeFa.csv')
trait <- trait[,c('AccSpName','AccGenus','Height2','WD_mean','SM_mean')]
colnames(trait)[1:2] <- c('species','genus')


# Read life-form table and attach traits by species
spls.lf <- read.csv('data/lifeForm/species_lifefrom_NOgenusFill.csv')
spls.lf <- left_join(spls.lf,trait[,-2])


# fill missing trait values: first by genus mean, then by family mean
spls.lf2 <- spls.lf %>% group_by(genus) %>% mutate(Height2=mean(Height2,na.rm=T),
                                       WD_mean=mean(WD_mean,na.rm=T),
                                       SM_mean=mean(SM_mean,na.rm=T)) %>% ungroup()
spls.lf$Height2[is.na(spls.lf$Height2)] <- spls.lf2$Height2[is.na(spls.lf$Height2)]
spls.lf$WD_mean[is.na(spls.lf$WD_mean)] <- spls.lf2$WD_mean[is.na(spls.lf$WD_mean)]
spls.lf$SM_mean[is.na(spls.lf$SM_mean)] <- spls.lf2$SM_mean[is.na(spls.lf$SM_mean)]

spls.lf$Height2[is.nan(spls.lf$Height2)] <- NA
spls.lf$WD_mean[is.nan(spls.lf$WD_mean)] <- NA
spls.lf$SM_mean[is.nan(spls.lf$SM_mean)] <- NA

spls.lf2 <- spls.lf %>% group_by(family) %>% mutate(Height2=mean(Height2,na.rm=T),
                                                   WD_mean=mean(WD_mean,na.rm=T),
                                                   SM_mean=mean(SM_mean,na.rm=T)) %>% ungroup()
spls.lf$Height2[is.na(spls.lf$Height2)] <- spls.lf2$Height2[is.na(spls.lf$Height2)]
spls.lf$WD_mean[is.na(spls.lf$WD_mean)] <- spls.lf2$WD_mean[is.na(spls.lf$WD_mean)]
spls.lf$SM_mean[is.na(spls.lf$SM_mean)] <- spls.lf2$SM_mean[is.na(spls.lf$SM_mean)]

spls.lf$Height2[is.nan(spls.lf$Height2)] <- NA
spls.lf$WD_mean[is.nan(spls.lf$WD_mean)] <- NA
spls.lf$SM_mean[is.nan(spls.lf$SM_mean)] <- NA

#write.csv(spls.lf,'splist/spls_mark_trait.csv',row.names = F)


# Classify species into life-form/height types
lh <- read.csv('data/trait/trait/targetSpan.csv')
spls.lf$treetype <- cut(spls.lf$Height2,c(0,5,15,max(spls.lf$Height2,na.rm = T))) %>% as.numeric()
spls.lf$treetype[str_detect(spls.lf$lifeForm,'shrub')] <- 1
spls.lf$treetype[(is.na(spls.lf$treetype))&str_detect(spls.lf$lifeForm,'(tree)|(shrub)')] <- 4


# Divide species into ecological groups using functional traits
spls.lf$ecoGroup <- cut(spls.lf$WD_mean,quantile(spls.lf$WD_mean,c(0,0.25,0.5,0.75,1),na.rm = T)) %>% as.numeric()
tmp <- apply(cbind(spls.lf$ecoGroup,cut(spls.lf$SM_mean,quantile(spls.lf$SM_mean,c(0,0.25,0.5,0.75,1),na.rm = T)) %>% as.numeric()), 1, max,na.rm=T)
tmp[is.infinite(tmp)] <- NA
spls.lf$ecoGroup <- tmp
spls.lf$ecoGroup[(is.na(spls.lf$ecoGroup))&str_detect(spls.lf$lifeForm,'(tree)|(shrub)')] <- 5


# Reshape target table to long format and map (treetype, ecoGroup) → timespan
lh[,1] <- 1:4
colnames(lh)[2:6] <- 1:5
lh <- reshape2::melt(lh,id.vars='X')

# Assign target timespans for prediction
tmp <- data.frame(flag =lh[,1]*10+as.numeric(lh[,2]), timespan = lh[,3]) 
spls.lf$flag <- spls.lf$treetype*10+spls.lf$ecoGroup
spls.lf <- left_join(spls.lf,tmp)
spls.lf$timespan[(!str_detect(spls.lf$lifeForm,'(tree)|(shrub)'))|(is.na(spls.lf$lifeForm))] <- NA

# Assign a target timespan for prediction to non-woody species
spls.lf$timespan[spls.lf$lifeForm%in%c('therophytes')] <- '2011-2040'
spls.lf$timespan[is.na(spls.lf$timespan)] <- '2041-2070'

# Merge timespan labels back to the master species list
spls <- read.csv('data/spinfo/spls_info.csv')
spls <- left_join(spls,spls.lf[,c('speciesKey','lifeForm','timespan')])
write.csv(spls,'data/spinfo/spls_mark_all.csv',row.names = F)
