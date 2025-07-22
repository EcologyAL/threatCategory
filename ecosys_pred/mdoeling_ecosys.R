library(dplyr);library(parallel);library(glmnet);library(stringr)
setwd('F:/luoao/RLI/')

lf.r <- read.csv('data/lifeForm/sr_ratio_lf.csv')
load('data/ecosys/10km_summary.rdata')
eco.fls <- list.files(('data/ecosys/lvl2_10km_ratio/'),full.names = T)
eco.fls.name <- paste0('ECO',unlist(lapply(eco.fls %>% stringr::str_split('_'), function(x)x[8])))
eco.fls.name <- eco.fls.name[nchar(eco.fls.name)<=6]
dat <- left_join(lf.r,eco.p[,-(1:2)])

lf.names <- colnames(dat[,3+(1:14)])
dat.output <- data.frame(cell=dat$cell)

cl <- makeCluster(60)
clusterEvalQ(cl,{
  library(dplyr);library(parallel);library(glmnet);library(stringr)
  setwd('F:/luoao/RLI/')
  lf.r <- read.csv('data/lifeForm/sr_ratio_lf.csv')
  load('data/ecosys/10km_summary.rdata')
  eco.fls <- list.files(('data/ecosys/lvl2_10km_ratio/'),full.names = T)
  eco.fls.name <- paste0('ECO',unlist(lapply(eco.fls %>% stringr::str_split('_'), function(x)x[8])))
  eco.fls.name <- eco.fls.name[nchar(eco.fls.name)<=6]
  dat <- left_join(lf.r,eco.p[,-(1:2)])
  
  lf.names <- colnames(dat[,3+(1:14)])
  dat.output <- data.frame(cell=dat$cell)
  return(NULL)
})

tmp <- parLapply(cl,eco.fls.name,function(eco.name){
  dat.output <- data.frame(cell=dat$cell)
  #print(eco.name)
  y=data.frame(y=dat[,eco.name]) 
  x=dat[,c('cell',lf.names)]
  m.dat <- data.frame(x,y) %>% na.omit()
  m <- glm(data=m.dat[,-1],y~.,family = 'binomial')
  
  for (year in c('_2010','_2040','_2070','_2100')) {
    dat.pred <- dat[,str_replace_all(lf.names,"_2010",year)]
    colnames(dat.pred) <- str_replace_all(colnames(dat.pred),year,'_2010')
    tmp <- data.frame(value=predict(m,dat.pred,type = "response"))
    colnames(tmp) <- paste0(eco.name,year)
    dat.output <- cbind(dat.output,tmp)
  }
  return(dat.output)
})
stopCluster(cl)

eco_ratio <- lapply(tmp,function(x)x[,-1])
eco_ratio <- do.call('cbind',eco_ratio)
eco_ratio <- cbind(data.frame(cell=dat$cell),eco_ratio)
save(eco_ratio,file='data/ecosys/10km_eco_ratio_future.rdata')

rst <- raster('data/ecosys/10km_eco_area.tif')
rst[] <- NA
rst[tmp$cell] <- tmp$value
plot(rst)

rst[dat$cell] <- dat[,eco.name]
plot(rst)


# library(glmnet);library(stringr)
# setwd('F:/luoao/RLI/')
# lf.r <- read.csv('data/lifeForm/sr_ratio_lf.csv')
# 
# # eco <- raster('data/ecosys/10km_eco_area.tif')
# # eco.p <- data.frame(coordinates(eco),cell=1:length(eco[]),area=eco[])
# # #colnames(eco.p)[3] <- 'area'
# # 
# # eco.fls <- list.files(('data/ecosys/lvl2_10km_ratio/'),full.names = T)
# # eco.fls.name <- paste0('ECO',unlist(lapply(eco.fls %>% stringr::str_split('_'), function(x)x[8])))
# # 
# # eco <- lapply(eco.fls, function(x){tmp <- raster(x);return(tmp[])})
# # eco <- do.call('cbind',eco)
# # colnames(eco) <- eco.fls.name
# # eco.p <- cbind(eco.p,eco) %>% as.data.frame()
# # eco.p <- eco.p[!is.na(eco.p$area),]
# # for (i in 5:ncol(eco.p)) {
# #   eco.p[is.na(eco.p[,i]),i] <- 0
# # }
# # save(eco.p,file='data/ecosys/10km_summary.rdata')
# load('data/ecosys/10km_summary.rdata')
# eco.fls <- list.files(('data/ecosys/lvl2_10km_ratio/'),full.names = T)
# eco.fls.name <- paste0('ECO',unlist(lapply(eco.fls %>% stringr::str_split('_'), function(x)x[8])))
# eco.fls.name <- eco.fls.name[nchar(eco.fls.name)<=6]
# 
# dat <- left_join(lf.r,eco.p[,-(1:2)])
# 
# lf.names <- colnames(dat[,3+(1:14)])
# dat.output <- data.frame(cell=dat$cell)
# 
# for (eco.name in eco.fls.name) {
#   for (year in c('_2010','_2040','_2071','_2100')) {
#     print(eco.name)
#     y=data.frame(y=dat[,eco.name]) 
#     x=dat[,c('cell',lf.names)]
#     m.dat <- data.frame(x,y) %>% na.omit()
#     
#     m <- glm(data=m.dat[,-1],y~.,family = 'binomial')
#     tmp <- data.frame(value=predict(m,dat[,str_],type = "response"))
#     colnames(tmp) <- paste0(eco.name,year)
#     dat.output <- cbind(dat.output,tmp)
#   }
# }
# 
# rst <- raster('data/ecosys/10km_eco_area.tif')
# rst[] <- NA
# rst[tmp$cell] <- tmp$value
# plot(rst)
# 
# rst[dat$cell] <- dat[,eco.name]
# plot(rst)
