library(dplyr);library(parallel);library(glmnet);library(stringr)
setwd('F:/luoao/RLI/')
load('data/ecosys/10km_eco_ratio_future.rdata')
lf_ratio <- read.csv('data/lifeForm/sr_ratio_lf.csv')
rstland <- raster('E:/LuoA/PRIME/data/CHELSA/10km_climate_cell_land.tif')
eco_ratio

lf.list <- c("evergreen broadleaf trees","drought_deciduous broadleaf trees","cold_deciduous broadleaf trees","needleleaf trees",
             "evergreen broadleaf shrubs","drought_deciduous broadleaf shrubs","cold_deciduous broadleaf shrubs",
             "c3 grasses","c4 grasses","climbers","geophytes","succulents","therophytes","forbs")%>% str_replace_all('  ',' ') %>% 
  str_replace_all(' ','.')
windows(width=12,height=6)
par(mfcol=c(3,4),oma=c(0.2, 0.2, 0.2, 0.2),
    mar=c(5, 4, 4, 2) + 0.1)

for (lf in lf.list[1:3]) {
  for(year in c('_2010','_2040','_2070','_2100')) {
    rst <- rstland
    rst[] <- NA
    rst[lf_ratio$cell] <- round(lf_ratio[,paste0(lf,year)],3)
    plot(rst, xaxt="n", yaxt="n")
    title(paste0(lf,year))
  }
}

rst[lf_ratio$cell] <- round(lf_ratio[,paste0(lf,year)],3)

# CEO ---------------------------------------------------------------------
#colnames(eco_ratio)
lapply(str_split(fls,'_'), function(x)x[7]) %>% unlist()

fls <- list.files('data/ecosys/lvl2_10km_ratio/')
fls[,c(1,)]
eco.name <- lapply(str_split(fls,'_'), function(x)x[6]) %>% unlist()
eco.name <- paste0('ECO',eco.name)

par(mfcol=c(3,4))
for (i in c(2,18,33)){
#for (i in 1:length(eco.name)) {
  print(eco)
  eco <- eco.name[i]
  
  rst <- raster(paste0('data/ecosys/lvl2_10km_ratio/',fls[i]))
  plot(rst, xaxt="n", yaxt="n")
  title(paste0(str_remove(eco,'ECO'),year,' ','(observed)'))
  
  rst <- rstland
  rst[] <- NA
  rst[eco_ratio$cell] <- round(eco_ratio[,paste0(eco,'_2010')],3)
  rst[rst<0.05] <- NA
  plot(rst, xaxt="n", yaxt="n")
  title(paste0(str_remove(eco,'ECO'),year,' ','(predicted)'))
  rst1 <- rst
  
  rst <- rstland
  rst[] <- NA
  rst[eco_ratio$cell] <- round(eco_ratio[,paste0(eco,'_2100')],3)
  rst[rst<0.05] <- NA
  plot(rst, xaxt="n", yaxt="n")
  title(paste0(str_remove(eco,'ECO'),year,' ','(predicted)'))
  rst2 <- rst
  
  rst <- (rst2-rst1)/rst1
  rst[rst[] > -0.05] <- NA
  plot(rst, xaxt="n", yaxt="n")
  title(paste0(str_remove(eco,'ECO'),year,' change ','(predicted)'))
}
