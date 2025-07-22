setwd("E:/LuoA/PRIME/")
library(raster);library(dplyr);library(alphahull);library(parallel);library(stringr)


# set parallel ------------------------------------------------------------


cl <- makeCluster(100)
clusterEvalQ(cl,{
  setwd("E:/LuoA/PRIME/")
  library(raster);library(dplyr);library(alphahull);library(parallel);library(stringr)
  spls <- read.csv('splist/spls_outputs.csv')
  rst.land <- raster('data/CHELSA/10km_climate_cell_land.tif')
  rook0 <- sqrt(matrix((1:37 - 19)^2, ncol=37,nrow=37,byrow=T) + matrix((1:37 - 19)^2, ncol=37, nrow=37, byrow=F)) 
  
  rook6 <- rook0 <= 6 + sqrt(0.5)-0.5;rook6[!rook6] <- NA;rook6[19,19] <- FALSE;rook6 <- rook6[(19-6):(19+6),(19-6):(19+6)]
  rook12 <- rook0 <= 12 +  sqrt(0.5)-0.5;rook12[!rook12] <- NA;rook12[19,19] <- FALSE;rook12 <- rook12[(19-12):(19+12),(19-12):(19+12)]
  rook18 <- rook0 <= 18 + sqrt(0.5)-0.5;rook18[!rook18] <- NA;rook18[19,19] <- FALSE;rook18 <- rook18[(19-18):(19+18),(19-18):(19+18)]
  
})


# species to run ----------------------------------------------------------

setwd("E:/LuoA/PRIME/")
spls <- read.csv('splist/spls_outputs.csv')

flag.num <- which(!is.na(spls$path_range))

# get sp range size in each scenario and dispersal ability -----------------

splist <- parLapply(cl,flag.num,function(i){
  path <- spls$path_range[i]
  spds <- read.csv(path)
  if(str_detect(path,'mod6')){
    sp.range <- spds[,1:10] > 3 
  }else if(str_detect(path,'mod10')){
    sp.range <- spds[,1:10] > 5
  }else{
    return(NULL)
  }
  sp.range <- as.data.frame(sp.range)
  
  # unlimit -----------------------------------------------------------------
  
  sp.range.unlimit <- sp.range
  colnames(sp.range.unlimit)[2:10] <- paste0(colnames(sp.range.unlimit)[2:10],'_DS_unlimit')
  
  # 20km/decade ------------------------------------------------------------
  sp.range.limit <- sp.range
  
  j_s <- which(str_detect(colnames(sp.range),'2040') )
  cells <- spds$cell[sp.range$sn_1981.2010]
  cells.adj <- adjacent(rst.land,cells,rook6,include=T)[,2] %>% unique()
  for (j in j_s) {
    sp.range.limit[j] <- sp.range.limit[j]&(spds$cell%in%cells.adj)
  }
  
  j_s <- which(str_detect(colnames(sp.range),'2070') )
  cells <- spds$cell[sp.range$sn_1981.2010]
  cells.adj <- adjacent(rst.land,cells,rook12,include=T)[,2] %>% unique()
  for (j in j_s) {
    sp.range.limit[j] <- sp.range.limit[j]&(spds$cell%in%cells.adj)
  }
  
  j_s <- which(str_detect(colnames(sp.range),'2100') )
  cells <- spds$cell[sp.range$sn_1981.2010]
  cells.adj <- adjacent(rst.land,cells,rook18,include=T)[,2] %>% unique()
  for (j in j_s) {
    sp.range.limit[j] <- sp.range.limit[j]&(spds$cell%in%cells.adj)
  }
  
  colnames(sp.range.limit)[2:10] <- paste0(colnames(sp.range.limit)[2:10],'_DS_limit')
  
  # no dispersal ------------------------------------------------------------
  sp.range.no <- sp.range
  
  for (j in 2:10) {
    sp.range.no[j] <- sp.range.no[j]&(sp.range.no$sn_1981.2010)
  }
  
  colnames(sp.range.limit)[2:10] <- paste0(colnames(sp.range.limit)[2:10],'_DS_no')
  
  sp.range <- data.frame(specieKey=spls$speciesKey[i],spds[,'cell'],sp.range.limit,sp.range.unlimit[,-1],sp.range.no[,-1])
  flag <- sp.range[,-(1:2)] %>% rowSums()
  sp.range <- sp.range[flag>0,]
  
  path.out <- path %>% str_replace('data/F_SPP_Output_csv/','output/HSD/') %>% str_remove('mod6/') %>% str_remove('mod10/')
  
  write.csv(sp.range,path.out,row.names=F)
  
  # summary sp range
  sp.info <- sp.range[,-c(1,2)] %>% colSums()
  sp.info <- c(speciesKey=spls$speciesKey[i],sp.info)
  return(sp.info)
})
sp.info <- do.call('rbind',splist)
rm(splist)
write.csv(sp.info,'output')

cluster()