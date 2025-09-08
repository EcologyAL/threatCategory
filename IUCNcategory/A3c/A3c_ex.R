setwd("E:/LuoA/PRIME/")
library(raster);library(dplyr);library(alphahull);library(parallel);library(stringr)


# set parallel ------------------------------------------------------------

cl <- makeCluster(96)
clusterEvalQ(cl,{
  setwd("E:/LuoA/PRIME/")
  library(raster);library(dplyr);library(alphahull);library(parallel);library(stringr)
  spls <- read.csv('splist/spls_outputs.csv')
  
  rst.land <- raster('data/CHELSA/10km_climate_cell_land.tif')
  rook0 <- sqrt(matrix((1:37 - 19)^2, ncol=37,nrow=37,byrow=T) + matrix((1:37 - 19)^2, ncol=37, nrow=37, byrow=F)) 
  
  rook6 <- rook0 <= 6 + sqrt(0.5)-0.5;rook6[!rook6] <- NA;rook6[19,19] <- FALSE;rook6 <- rook6[(19-6):(19+6),(19-6):(19+6)]
  rook12 <- rook0 <= 12 +  sqrt(0.5)-0.5;rook12[!rook12] <- NA;rook12[19,19] <- FALSE;rook12 <- rook12[(19-12):(19+12),(19-12):(19+12)]
  rook18 <- rook0 <= 18 + sqrt(0.5)-0.5;rook18[!rook18] <- NA;rook18[19,19] <- FALSE;rook18 <- rook18[(19-18):(19+18),(19-18):(19+18)]
  
  return(NULL)
})

# species to run ----------------------------------------------------------

setwd("E:/LuoA/PRIME/")
spls <- read.csv('splist/spls_outputs.csv')

flag.num <- which(!is.na(spls$path_range))

# get sp range size in each scenario and dispersal ability -----------------

sp.ex <- parLapply(cl,flag.num,function(i){
  path <- spls$path_range[i]
  spds <- read.csv(path)
  if(str_detect(path,'mod6')){
    sp.range <- spds[,1:10] >= 3 
  }else if(str_detect(path,'mod10')){
    sp.range <- spds[,1:10] >= 5
  }else{
    return(NULL)
  }
  sp.range <- as.data.frame(sp.range)
  
  # unlimit -----------------------------------------------------------------
  
  sp.range.unlimit <- spds
  colnames(sp.range.unlimit)[2:10] <- paste0(colnames(sp.range.unlimit)[2:10],'_DS_unlimit')
  tmp <- colSums(sp.range.unlimit[,2:10])==0
  
  # 20km/decade ------------------------------------------------------------
  sp.range.limit <- spds
  
  j_s <- which(str_detect(colnames(sp.range),'2040') )
  cells <- spds$cell[sp.range$sn_1981.2010]
  cells.adj <- adjacent(rst.land,cells,rook6,include=T)[,2] %>% unique()
  for (j in j_s) {
    sp.range.limit[!sp.range.limit$cell%in%cells.adj,j] <- 0
  }
  
  j_s <- which(str_detect(colnames(sp.range),'2070') )
  cells <- spds$cell[sp.range$sn_1981.2010]
  cells.adj <- adjacent(rst.land,cells,rook12,include=T)[,2] %>% unique()
  for (j in j_s) {
    sp.range.limit[!sp.range.limit$cell%in%cells.adj,j] <- 0
  }
  
  j_s <- which(str_detect(colnames(sp.range),'2100') )
  cells <- spds$cell[sp.range$sn_1981.2010]
  cells.adj <- adjacent(rst.land,cells,rook18,include=T)[,2] %>% unique()
  for (j in j_s) {
    sp.range.limit[!sp.range.limit$cell%in%cells.adj,j] <- 0
  }
  
  colnames(sp.range.limit)[2:10] <- paste0(colnames(sp.range.limit)[2:10],'_DS_limit')
  tmp <- c(tmp,colSums(sp.range.limit[,2:10])==0) 
  
  # no dispersal ------------------------------------------------------------
  sp.range.no <- spds
  cells <- spds$cell[sp.range$sn_1981.2010]
  for (j in 2:10) {
    sp.range.no[!sp.range.no$cell%in%cells,j] <- 0
  }
  
  colnames(sp.range.no)[2:10] <- paste0(colnames(sp.range.no)[2:10],'_DS_no')
  tmp <- c(tmp,colSums(sp.range.no[,2:10])==0) 
  return(tmp)
})
sp.ex <- do.call('rbind',sp.ex) %>% as.data.frame()
sp.ex$speciesKey <- spls$speciesKey[flag.num]
write.csv(sp.ex,'F:/luoao/RLI/data/IUCNcategory/A3c/A3c_ex.csv',row.names = F)
stopCluster(cl)
   