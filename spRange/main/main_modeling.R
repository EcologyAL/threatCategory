setwd("~")
library(raster);library(dplyr)

####===================================
#### parallel processing setting
####===================================

spls <- read.csv('splist/spls_mark.csv')

cl <- makeCluster(100)
clusterEvalQ(cl,{
  setwd("～")
  spls <- read.csv('splist/spls_mark.csv')
  source('code/sdm_modeling/0_setting.R')
  NULL})

####===================================
#### select species
####===================================

flag <- parLapply(cl,1:nrow(spls),function(iii){
  fa <- spls$family[iii]
  id <- spls$speciesKey[iii]
  path_data <- paste0('data/F_SPP_Output/',fa,'/',id,'/',id,'_mod.rdata')
  !file.exists(path_data)
}) %>% unlist()
flag <- flag&(spls$num_coords>3)
flag <- flag&(!file.exists(paste0('data/F_SPP_Output_csv/mod6/',spls$speciesKey,'.csv')))
flag <- flag&(!file.exists(paste0('data/F_SPP_Output_csv/mod10/',spls$speciesKey,'.csv')))
flag.num <- which(flag);print(length(flag.num))

### =========================================================================
### modeling
### =========================================================================

resall <- parLapply(cl,flag.num,function(iii){
  
  tryCatch(
    expr = {
      fa <- spls$family[iii]
      id <- spls$speciesKey[iii]
      path_data <- paste0('data/F_SPP_Coord/',fa,'/',id,'/',id,'_coords','.csv')
      path_output_6 <- paste0('data/F_SPP_Output_csv/mod6/',id,'.csv')
      path_output_10 <- paste0('data/F_SPP_Output_csv/mod10/',id,'.csv')
      
      path_output_mod <- paste0('data/F_SPP_Output/',fa,'/',id,'/',id,'_mod.rdata')
      path_output_mod2 <- paste0('data/F_SPP_Output/',fa,'/',id,'/',id,'_mod2.rdata')
      
      source('code/sdm_modeling/1_premodeling.R',local=TRUE)
      if(spls$num_coords[iii]>=20) source('code/sdm_modeling/2_MT1_modeling.R',local=TRUE)
      if(spls$num_coords[iii]>3 & spls$num_coords[iii]<20) source('code/sdm_modeling/2_MT3_modeling.R',local=TRUE)
      
      
      if(file.exists(tempfile())){
        unlink(tempfile(),recursive = T)
      }
      
      if(dir.exists(tempdir())){
        unlink(tempdir(),recursive = T)
      }
      
      return(iii)
    },
    error = function(e) {
      return(NULL)
    }
  )
  
})

#### check outputs

test <- parLapply(cl,1:nrow(spls),function(iii){
  fa <- spls$family[iii]
  id <- spls$speciesKey[iii]
  
  path_output <- paste0('data/F_SPP_Output/',fa,'/',id,'/',id,'.csv')
  file.exists(path_output)
}) %>% unlist()
table(test)

stopCluster(cl)

