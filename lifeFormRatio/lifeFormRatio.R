`setwd("F:/luoao/RLI/")
library(raster);library(dplyr);library(alphahull);library(parallel);library(stringr)

# rstland <- raster('E:/LuoA/PRIME/data/CHELSA/10km_climate_cell_land.tif')
# rstland.p <- rasterToPoints(rstland) %>% as.data.frame()
# rstland.p <-  data.frame(cell=rstland.p[,3]) 

# fls <- list.files('data/lifeForm/cellInfo/',full.names = T)
# lf.list <- c("evergreen trees","drought_deciduous trees","cold_deciduous trees","needleleaf trees",
#   "evergreen shrubs","drought_deciduous shrubs","cold_deciduous shrubs",
#   "c3 grasses","c4 grasses","climbers","geophytes","succulents","therophytes","forbs")%>% str_replace_all('  ',' ')
# 
# lf.sr <-  lapply(fls, read.csv)
# lf.sr <-  lapply(lf.sr, function(tmp)left_join(rstland.p,tmp))
# for (i in 1:length(lf.sr)) { 
#   for (j in 2:ncol(lf.sr[[1]])) {
#     flag <- is.na(lf.sr[[i]][,j])
#     lf.sr[[i]][flag,j] <- 0
#   } 
# }
# names(lf.sr) <- fls %>% str_remove_all('data/lifeForm/cellInfo/') %>% str_remove_all('.csv')
# save(lf.sr,file='data/lifeForm/lf_sr.rdata')
# 

# lf.sr <- lf.sr[lf.list]
# all.sr <- lf.sr[[1]]
# for (i in 2:length(lf.sr)) { all.sr[,-1] <- all.sr[,-1] + lf.sr[[i]][,-1] }
# 
# lf.ratio <- lf.sr
# for (i in 1:length(lf.ratio)) { lf.ratio[[i]][,-1] <-  lf.ratio[[i]][,-1]/all.sr[,-1] }
# save(lf.ratio,file='data/lifeForm/lf14_ratio.rdata')

#####
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
#library(glm2);library(caret)
df <- left_join(lf.r,eco.p)
df <- df[!is.na(df$area),]
df <- df[!is.nan(df$evergreen_trees),]

#  df <- df[sample(nrow(df),10000),]

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

# form.glm=as.formula(paste("ECO101","~",paste0(lf.name,collapse="+")))
# model_full <- glm(form.glm , data=df, family = binomial(link = "logit"));model_full
# summary(model_full)
# 
# # setting   
# control <- trainControl(method = "cv", number = 10)  # 10 fold  
# 
# # step_reg to select variable
# set.seed(2024)  
# model_stepwise <- train(form.glm , data=df, 
#                         method = "glmStepAIC",   
#                         family = binomial(link = "logit"),   
#                         trControl = control,   
#                         trace = FALSE)  
# 
# # check the best model 
# best_model <- model_stepwise$finalModel  
# summary(best_model)
