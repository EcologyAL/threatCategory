red.list <- read.csv('data/outputs/sp_assess_simple.csv')# %>% na.omit()

red.list$climCategory <- factor(red.list$category,levels=c('DD','LC','NT','VU','EN','CR','EX'))
red.list$ensembleCategory <- factor(red.list$ensembleCategory,levels=c('DD','LC','NT','VU','EN','CR','EX'))
red.list$redlistCategory <- factor(red.list$redlistCategory,levels=c('DD','LC','NT','VU','EN','CR','EX'))

red.list <- red.list %>% 
  mutate(wt_climC = 1 - (as.numeric(climCategory)-2)/5,
         wt_redlistC = 1 - (as.numeric(redlistCategory)-2)/5,
         wt_ensembleC = 1 - (as.numeric(ensembleCategory)-2)/5) %>% 
  mutate(wt_climC = ifelse(wt_climC>1,NA,wt_c),
         wt_redlistC = ifelse(wt_redlistC>1,NA,wt_redlistC),
         wt_ensembleC = ifelse(wt_ensembleC>1,NA,wt_ensembleC)) 

red.list <- red.list[,c('species','speciesKey',
                        "climCategory",'redlistCategory',"ensembleCategory",
                        "wt_climC",'wt_redlistC',"wt_ensembleC")]
write.csv(red.list,'data/outputs/sp_assess_simple.csv',row.names = F)
