setwd("F:/luoao/RLI/")
library(glmnet);library(raster);library(dplyr);library(alphahull);library(parallel);library(stringr)

load(file='data/lifeForm/lf14_ratio.rdata')
cl <- makeCluster(54)
clusterEvalQ(cl,{
  set.seed(2024)
  setwd("F:/luoao/RLI/")
  library(glmnet);library(raster);library(dplyr);library(alphahull);library(parallel);library(stringr)
  
  load(file='data/lifeForm/lf14_ratio.rdata')
  lf.r <- lf.ratio %>% lapply( function(x)x[,'sn_1981.2010'] )
  lf.r <- do.call(cbind,lf.r)
  lf.r <- cbind(lf.ratio[[1]][,1],lf.r) 
  colnames(lf.r)[1] <- 'cell'
  lf.r <- as.data.frame(lf.r)
  
  lf.name <- colnames(lf.r)[-1]
  lf.name <- lf.name %>% str_replace_all(' ','_')
  colnames(lf.r)[-1] <- lf.name
  
  
  # eco.p <- eco.p[,-(1:2)]
  
  # load models -------------------------------------------------------------
  
  eco.names <- str_remove_all(list.files('data/ecosys/mods/'),'.rdata')
  mods <- lapply(eco.names,function(eco.name){
    #if(!is.null(m)) rm(m)
    load(paste0('data/ecosys/mods/',eco.name,'.rdata'))
    return(m)
  })
  names(mods) <- eco.names
})

# predict current probs -----------------------------------------------------------

sns <- colnames(lf.ratio[[1]])[-(1:2)]
#for (sn in sns) {
pred_probs_list <- parLapply(cl,sns, function(sn){
  lf.r <- lf.ratio %>% lapply( function(x)x[,sn] )
  lf.r <- do.call(cbind,lf.r)
  lf.r <- cbind(lf.ratio[[1]][,1],lf.r) 
  colnames(lf.r)[1] <- 'cell'
  lf.r <- as.data.frame(lf.r)
  
  lf.name <- colnames(lf.r)[-1]
  lf.name <- lf.name %>% str_replace_all(' ','_')
  colnames(lf.r)[-1] <- lf.name
  
  x.pred <- lf.r[,lf.name] %>% as.matrix()
  pred_probs <- lapply(eco.names,function(eco.name){
    pred_probs <- predict(mods[[eco.name]],newx=x.pred,type='response', s = "lambda.min") %>% as.vector()
    return(pred_probs)
  })
  pred_probs <- do.call('cbind',pred_probs)
  colnames(pred_probs) <- eco.names
  pred_probs <- as.data.frame(pred_probs)
  
  pred_probs$cell <- lf.r$cell
  save(pred_probs,file=paste0('data/ecosys/future/',sn,'.rdata'))
  #return(pred_probs)
})
names(pred_probs_list) <- sns

load('data/ecosys/10km_eco_ratio_future.rdata')
for(i in 1:length(pred_probs_list)){
  tmp <- pred_probs_list[[i]]
  tmp$cell <- lf.r$cell
  pred_probs_list[[i]] <- tmp
}

save(pred_probs_list,file='data/ecosys/10km_eco_ratio_future.rdata')

stopCluster(cl)
