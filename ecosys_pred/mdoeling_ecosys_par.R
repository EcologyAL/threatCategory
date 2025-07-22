setwd("F:/luoao/RLI/")
library(glmnet);library(raster);library(dplyr);library(alphahull);library(parallel);library(stringr)

cl <- makeCluster(54)

load(file='data/lifeForm/lf14_ratio.rdata')
lf.r <- lf.ratio %>% lapply( function(x)x[,'sn_1981.2010'] )
lf.r <- do.call(cbind,lf.r)
lf.r <- cbind(lf.ratio[[1]][,1],lf.r) 
colnames(lf.r)[1] <- 'cell'
lf.r <- as.data.frame(lf.r)

lf.name <- colnames(lf.r)[-1]
lf.name <- lf.name %>% str_replace_all(' ','_')
colnames(lf.r)[-1] <- lf.name

load('data/ecosys/10km_summary.rdata')
eco.fls <- list.files(('data/ecosys/lvl2_10km_ratio/'),full.names = T)
eco.fls.name <- paste0('ECO',unlist(lapply(eco.fls %>% stringr::str_split('_'), function(x)x[8])))
eco.fls.name <- eco.fls.name[nchar(eco.fls.name)<=6]
eco.p <- eco.p[,-(1:2)]

####
clusterEvalQ(cl,{
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
  
  load('data/ecosys/10km_summary.rdata')
  eco.fls <- list.files(('data/ecosys/lvl2_10km_ratio/'),full.names = T)
  eco.fls.name <- paste0('ECO',unlist(lapply(eco.fls %>% stringr::str_split('_'), function(x)x[8])))
  eco.fls.name <- eco.fls.name[nchar(eco.fls.name)<=6]
  eco.p <- eco.p[,-(1:2)]
  
  df <- left_join(lf.r,eco.p);rm(lf.r);rm(eco.p)
  df <- df[!is.na(df$area),]
  df <- df[!is.nan(df$`evergreen_trees`),]
  
  x <- as.matrix(df[,lf.name])
  return(NULL)
})

eco.names <- eco.fls.name
eco.names <- eco.names[!eco.names%in%str_remove_all(list.files('data/ecosys/mods/'),'.rdata')]  
m.list <- parLapply(cl,eco.names,function(eco.name){
  y <- df[,eco.name]
  y <- as.numeric(y>0.01)
  m = cv.glmnet(x, y, family = "binomial",nfolds = 10)
  save(m,file=paste0('data/ecosys/mods/',eco.name,'.rdata'))
  return(m)
})

# load models -------------------------------------------------------------


eco.names <- eco.fls.name
eco.names <- eco.names[eco.names%in%str_remove_all(list.files('data/ecosys/mods/'),'.rdata')]  
mods <- lapply(eco.names,function(eco.name){
  if(!is.null(m)) rm(m)
  load(paste0('data/ecosys/mods/',eco.name,'.rdata'))
  return(m)
})
names(mods) <- eco.names

# predict current probs -----------------------------------------------------------


x.pred <- df[,lf.name] %>% as.matrix()
pred_probs <- lapply(eco.names,function(eco.name){
  pred_probs <- predict(mods[[eco.name]],newx=x.pred,type='response', s = "lambda.min") %>% as.vector()
  return(pred_probs)
})
pred_probs <- do.call('cbind',pred_probs)
colnames(pred_probs) <- eco.names


# AUC ---------------------------------------------------------------------

x.pred <- df[,lf.name] %>% as.matrix()
library(pROC)
AUCs <- lapply(eco.names,function(eco.name){
  pred_probs <- predict(mods[[eco.name]],newx=x.pred,type='response', s = "lambda.min") %>% as.vector()
  dat <- data.frame(y.test = y.test,pred_probs=pred_probs) %>% na.omit()
  roc_curve <- roc(dat$y.test,dat$pred_probs)
  auc(roc_curve)
  return(auc(roc_curve))
}) %>% unlist()
AUCs

# plot(roc_curve, 
#      main="ROC Curve", 
#      col="darkred", 
#      lwd=2, 
#      xlab="False Positive Rate", 
#      ylab="True Positive Rate",
#      print.auc=TRUE,  
#      auc.polygon=TRUE,
#      grid=TRUE)

