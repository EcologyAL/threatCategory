setwd("E:/LuoA/PRIME/")
library(raster);library(dplyr);library(alphahull);library(parallel);library(stringr)


# set parallel ------------------------------------------------------------


cl <- makeCluster(98)
clusterEvalQ(cl,{
  setwd("E:/LuoA/PRIME/")
  library(raster);library(dplyr);library(alphahull);library(parallel);library(stringr)
  spls <- read.csv('splist/spls_outputs.csv')
  return(NULL)
})

clusterEvalQ(cl,{
  elev.rst <- raster('data/elevation/wc2.1_10km_elev.tif')
  ecosys.tb <- read.csv('data/ecoregion/bioreg_ll.csv')
  
  xy.cell <- raster('data/Map_World_Polygon/world_rst_bf.tif')
  xy.cell[] <- 1:length(xy.cell[])
  xy.cell <- rasterToPoints(xy.cell)
  colnames(xy.cell)[3] <- 'cell'
  
  rst.cell.poll <- raster('data/CHELSA/10km_climate_cell_land.tif')
  rst.cell.poll <- cellFromXY(raster(),coordinates(rst.cell.poll))
   
  NULL
})


# species to run ----------------------------------------------------------

setwd("E:/LuoA/PRIME/")
spls <- read.csv('splist/spls_outputs.csv')

flag <-  file.exists(spls$path_range %>%  str_replace('data/F_SPP_Output_csv/','output/HSD/') %>% str_remove('mod6/') %>% str_remove('mod10/'))
#flag.num <- which(flag&(!spls$speciesKey%in%spds.1d$speciesKey))
flag.num <- which(flag)

i <- flag.num[1]

# get sp range size in each scenario and dispersal ability -----------------

spds.1d.list <- parLapply(cl,flag.num,function(i){
  tryCatch(
    expr={
      id <- spls$speciesKey[i]
      path <- spls$path[i]
      path.out <- spls$path_range[i] %>% str_replace('data/F_SPP_Output_csv/','output/HSD/') %>% str_remove('mod6/') %>% str_remove('mod10/')
      
      spds <- read.csv(path.out)
      spds$cell_poll <- rst.cell.poll[spds$cell]
      
      spds_coord <- read.csv(path)
      
      elev <- range(extract(elev.rst,spds_coord[,c('decimalLongitude','decimalLatitude')] ),na.rm=T)
      ecosys <- ecosys.tb$ECO_CODE[ecosys.tb$layer %in% cellFromXY(raster(),spds_coord[,c('decimalLongitude','decimalLatitude')])] 
      ecosys <- ecosys.tb$layer[ecosys.tb$ECO_CODE %in% ecosys] 
      
      spds$elev <- elev.rst[spds$cell]
      spds$elev[is.na(spds$elev)] <- 0
      
      
      flag <- spds$elev<(elev[2]+500)
      flag <- flag&spds$elev>(elev[1]-500)
      flag <- flag&spds$cell_poll%in%ecosys
      
      flag <- flag&spds[,'sn_1981.2010']
      spds <- spds[flag,]
      
      write.csv(data.frame(x=1),paste('tmp/',i,'.csv'))
      # add in raw coordinates --------------------------------------------------
      
      
      if(nrow(spds)<=1){
        spds_1d <- data.frame(speciesKey=id,
                              cell_poll=cellFromXY(raster(),spds_coord[,c('decimalLongitude','decimalLatitude')]),
                              n=NA) %>% distinct() 
        return(spds_1d)
      }
      
      pts.new <- xy.cell[xy.cell[,3]%in%spds$cell,] %>% as.data.frame()
      ch <- dismo::convHull(spds_coord[,c('decimalLongitude','decimalLatitude')])
      cells <- pts.new[predict(ch, pts.new[,1:2])%in%1,3]
      cell_poll <- c(unique(spds$cell_poll[spds$cell%in%cells]),
                     cellFromXY(raster(),spds_coord[,c('decimalLongitude','decimalLatitude')]))
      cell_poll <- adjacent(raster(),cell_poll,8,include=T)[,2]
      
      spds <- spds[spds$cell_poll%in%cell_poll,]
      
      # spds$speciesKey <- id
      # spds <- spds[,c('speciesKey','cell_poll')] %>% group_by(cell_poll) %>% mutate(n=n()) %>% ungroup() #%>% filter(n>1)
      # 
      # spds_1d <- spds %>% distinct()
      # spds_1d <- rbind(spds_1d,data.frame(speciesKey=id,
      #                                     cell_poll=cellFromXY(raster(),spds_coord[,c('decimalLongitude','decimalLatitude')]),
      #                                     n=NA)) %>% distinct() 
      
      if(nrow(spds)>0){
        spds$speciesKey <- id
        
        spds <- spds[,c('speciesKey','cell_poll')] %>% group_by(cell_poll) %>% mutate(n=n()) %>% ungroup() #%>% filter(n>1)
        
        spds_1d <- spds %>% distinct()
        spds_1d <- rbind(spds_1d,data.frame(speciesKey=id,
                                            cell_poll=cellFromXY(raster(),spds_coord[,c('decimalLongitude','decimalLatitude')]),
                                            n=NA)) %>% distinct() 
      }else{
        spds_1d <- data.frame(speciesKey=id,
                              cell_poll=cellFromXY(raster(),spds_coord[,c('decimalLongitude','decimalLatitude')]),
                              n=NA) %>% distinct() 
      }
      
      #spds_1d 
      return(spds_1d)
    },
    error = function(e) {
      # id <- spls$speciesKey[i]
      # path <- spls$path_out[i]
      # 
      # spds_coord <- read.csv(path)
      # 
      # spds_1d <- data.frame(speciesKey=id,
      #                       cell_poll=cellFromXY(raster(),spds_coord[,c('decimalLongitude','decimalLatitude')]),
      #                       n=NA)
      return(NULL)
    }
  )
})
spds.1d.list <- c(spds.1d.list.raw,spds.1d.list)
spds.1d <- do.call('rbind',spds.1d.list)
length(unique(spds.1d$speciesKey))
write.csv(spds.1d,'output/summary/cellInfo_1d/sp_cellpoll.csv',row.names = F)

stopCluster(cl)

spds.1d.all <- read.csv('output/summary/cellInfo_10km/spcell_10km_1d_all_cell.csv')
spds.1d.all <- spds.1d.all[!spds.1d.all$cell%in%4991971,] 
spds.1d.all <- spds.1d.all[!spds.1d.all$speciesKey%in%spds.1d$speciesKey,] %>% 
  rename(cell_poll=cell_1d) %>% 
  dplyr::select(speciesKey,cell_poll) %>% 
  distinct()
spds.1d.all <- rbind(spds.1d.all,spds.1d[,c('speciesKey','cell_poll')]) %>% distinct()
write.csv(spds.1d.all,'output/summary/cellInfo_1d/sp_cellpoll_all.csv',row.names = F)
write.csv(spds.1d.all,'F:/luoao/RLI/data/spcell_1d/sp_cellpoll_all.csv',row.names = F)
