`setwd("~")
library(raster);library(dplyr);library(alphahull);library(parallel);library(stringr)

# load l
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
library(glmnet)
df <- left_join(lf.r,eco.p)
df <- df[!is.na(df$area),]
df <- df[!is.nan(df$evergreen_trees),]

x <- as.matrix(df[,lf.name])
m.list <- list()
for (eco in eco.fls.name) {
  print(eco)
  print(Sys.time())
  y <- df[,lf]
  y <- as.numeric(y>0.01)
  
  m = cv.glmnet(x, y, family = "binomial",nfolds = 10)
  m <- c(m.list,list(m))
}
