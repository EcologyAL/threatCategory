setwd("~")
library(raster);library(dplyr);library(alphahull)
source('code/sdm_modeling/0_setting.R')

####===================================
#### select spcies in Fagales
####===================================

spls <- read.csv('splist/spls_mark.csv')
flag <- file.exists(paste0('data/F_SPP_Output/',spls$family,'/',spls$speciesKey,'/',spls$speciesKey,'_mod.rdata'))
flag <- which(flag)

### =========================================================================
### model prediction
### =========================================================================

cl <- makeCluster(100)

clusterEvalQ(cl,{setwd("E:/LuoA/PRIME/");spls <- read.csv('splist/spls_mark.csv');NULL})
clusterEvalQ(cl,source('code/sdm_modeling/0_setting.R'))

#iii <- flag.num[2]
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
      
      source('code/sdm_modeling/3_prediction.R',local=TRUE)
      
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
stopCluster(cl)
