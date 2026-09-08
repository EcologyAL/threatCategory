# read new assessment -----------------------------------------------------

red.list <- read.csv(paths['spAssAll'])
sp <- read.csv(paths['spLS'])
sp <- sp$speciesKey[sp$class%in%c('Magnoliopsida','Liliopsida')]
red.list <- red.list[red.list$speciesKey%in%sp,]
sn <- 'ssp370_limit'

red.list$category <- red.list[,sn]
red.list$category[is.na(red.list$category)] <- 'DD'

# read iucn assessment ----------------------------------------------------


red.list.iucn <- read.csv(paths['IUCNredList']) %>% 
  na.omit() %>% 
  right_join(red.list[,c('speciesKey','category')])
rm(red.list)
#red.list.iucn$redlistCategory[red.list.iucn$category%in%'DD'] <- NA

# integrated category -------------------------------------------------------

red.list <- red.list.iucn
red.list$category <- factor(red.list$category,levels=c('DD','LC','NT','VU','EN','CR','EX'))
red.list$redlistCategory <- factor(red.list$redlistCategory,levels=c('DD','LC','NT','VU','EN','CR','EX'))

# red.list$intCategory <- red.list$category
# red.list <- mutate(red.list,
#                    intCategory=ifelse(as.numeric(redlistCategory)>as.numeric(category),
#                                            redlistCategory,
#                                            category))
# red.list$intCategory <- factor(red.list$intCategory,levels = (1:7),labels = (c('DD','LC','NT','VU','EN','CR','EX')))
red.list$intCategory <- red.list$category
flag <- which(as.numeric(red.list$redlistCategory)>as.numeric(red.list$intCategory))
red.list$intCategory[flag] <- red.list$redlistCategory[flag]
flag <- which((red.list$climCategory%in%'DD')&(!is.na(red.list$redlistCategory)))
red.list$intCategory[flag] <- red.list$redlistCategory[flag]
red.list <- red.list[,c('species','speciesKey','category','redlistCategory','intCategory')]


# calculate species weight ------------------------------------------------

red.list$climCategory <- red.list$category
red.list$climCategory[is.na(red.list$climCategory)] <- 'DD'
red.list$climCategory <- factor(red.list$climCategory,levels=c('DD','LC','NT','VU','EN','CR','EX'))
red.list$redlistCategory <- factor(red.list$redlistCategory,levels=c('DD','LC','NT','VU','EN','CR','EX'))

# red.list$intCategory <- red.list$climCategory
# flag <- which(as.numeric(red.list$wt_redlistC)<as.numeric(red.list$intCategory))
# red.list$intCategory[flag] <- red.list$redlistCategory[flag]
# flag <- which((red.list$climCategory%in%'DD')&(!is.na(red.list$redlistCategory)))
# red.list$intCategory[flag] <- red.list$redlistCategory[flag]
#red.list$intCategory <- factor(red.list$intCategory,levels=c('DD','LC','NT','VU','EN','CR','EX'))

red.list <- red.list %>% 
  mutate(wt_climC = 1 - (as.numeric(climCategory)-2)/5,
         wt_redlistC = 1 - (as.numeric(redlistCategory)-2)/5,
         wt_intC = 1 - (as.numeric(intCategory)-2)/5) %>% 
  mutate(wt_climC = ifelse(wt_climC>1,NA,wt_climC),
         wt_redlistC = ifelse(wt_redlistC>1,NA,wt_redlistC),
         wt_intC = ifelse(wt_intC>1,NA,wt_intC)) 

red.list$intCategory_com <- red.list$intCategory
red.list$wt_intC_com <- red.list$wt_intC
red.list$intCategory_com[is.na(red.list$redlistCategory)] <- NA
red.list$wt_intC_com[is.na(red.list$redlistCategory)] <- NA

red.list <- red.list[,c('species','speciesKey',
                        "climCategory",'redlistCategory',"intCategory",'intCategory_com',
                        "wt_climC",'wt_redlistC',"wt_intC",'wt_intC_com')]
#write.csv(red.list,'data/outputs/sp_assess_simple.csv',row.names = F)

#
#red.list <- read.csv('data/outputs/sp_assess_simple.csv')
red.list.all <- read.csv(paths['spAssAll'])
red.list.all <- red.list.all[,c('speciesKey','A_ssp370_limit','B_ssp370_limit')];colnames(red.list.all)[2:3]<-c('A','B')
red.list <- left_join(red.list,red.list.all)

red.list$A[is.na(red.list$A)] <- 'DD'
red.list$B[is.na(red.list$B)] <- 'DD'

red.list$A <- factor(red.list$A,levels=c('DD','LC','NT','VU','EN','CR','EX'))
red.list$B <- factor(red.list$B,levels=c('DD','LC','NT','VU','EN','CR','EX'))

red.list <- red.list %>% 
  mutate(wt_A = 1 - (as.numeric(A)-2)/5,
         wt_B = 1 - (as.numeric(B)-2)/5) %>% 
  mutate(wt_A = ifelse(wt_A>1,NA,wt_A),
         wt_B = ifelse(wt_B>1,NA,wt_B)) 
sptss <- read.csv(paths["spTSS"])
sptss <- sptss$speciesKey[sptss$tss<=0.5]

red.list[red.list$speciesKey%in%sptss,str_detect(colnames(red.list),'wt_')] <- NA
red.list[red.list$speciesKey%in%sptss,str_detect(colnames(red.list),'(Category)|A|B')] <- 'DD' 

write.csv(red.list,paths['spAssSim'],row.names = F)
