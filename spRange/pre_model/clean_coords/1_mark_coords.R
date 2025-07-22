######=====
######parallel
######=====
library(parallel);library(raster);library(dplyr);library(sf)
setwd('E:/LuoA/PRIME/')
spls <- read.csv('splist/spls.csv',encoding='UTF-8')

flag <- read.csv('splist/tmp_ea_spls.csv',encoding='UTF-8')
flag <- spls$speciesKey%in%flag$speciesKey
flag.num <- which(flag)
# flag <- lapply(spls$path_out,file.exists) %>% unlist
# flag <- which(!flag)

cl <- makeCluster(96)
clusterEvalQ(cl,{
  library(raster);library(dplyr);library(sf);library(stringr);library(rgeos);library(rnaturalearth)
  setwd('E:/LuoA/PRIME/')
  spls <- read.csv('splist/spls.csv',encoding='UTF-8')
  spls$path_archive <- paste0(getwd(),'/data/F_SPP_Data_Archive/',spls$family,'/',spls$speciesKey,'/',spls$speciesKey,'.csv')
  spls$path_out <- paste0(getwd(),'/data/F_SPP_Data_Archive/',spls$family,'/',spls$speciesKey,'/',spls$speciesKey,'_mark.csv')
  dataSource <- read.csv('data/dataSource/dataSource.csv')
  dataSource <- dataSource$institutionCode[dataSource$p%in%1]
  
  # load pkureg data and shp --------------------------------
  mappku <- raster('data/Map_World_Polygon/world_rst_bf.tif')
  sp.ds.gl.pku <- read.csv('data/Map_World_Polygon/Spdis_A&G/Spdis_A&G.txt')
  sp.ds.gl.pku <- sp.ds.gl.pku[,c('Species_E2','Adcode99')] %>% distinct()
  
  # cty centroid
  cty <- ne_countries()
  cty <- gCentroid(cty,byid=TRUE)
  
  cell.c <- cellFromXY(mappku,coordinates(cty))
  rm(cty)
  # load gift data and shp ----------------------------
  load('data/Map_GIFT/GIFT_checklists.RData') # gift_shape
  gift_shape <- read_sf('data/Map_GIFT/shape/gift_shape.shp')
  #gift_shape <- st_make_valid(gift_shape)%>%filter(area<5000000)
  gift_shape <- st_make_valid(gift_shape)%>%filter(area<6700000)
  
  # load wcvp data and shp --------------------------------
  mapkew <- raster('data/Map_kew/shp/kew_rst_bf.tif')
  
  mapkew_tb <- read.csv('data/Map_kew/shp/kew_rst_tb.csv')
  sp.ds.gl.kew <- read.csv('data/Map_kew/WCVP/wcvp_distribution.csv',sep = '|')
  sp.ds.gl.kew <- sp.ds.gl.kew%>%filter(!introduced%in%1)%>%filter(!extinct%in%1)
  sp.ds.gl.kew <- left_join(sp.ds.gl.kew,mapkew_tb,c('area_code_l3' = 'LEVEL3_COD'))
  
  kew.splist <- read.csv('data/Map_kew/WCVP/wcvp_names.csv',sep = '|')%>% 
    dplyr::select(plant_name_id,taxon_name,taxon_rank)%>%distinct()
  kew.splist$plant_name_id <- kew.splist$plant_name_id %>% as.numeric()
  
  sp.ds.gl.kew <- left_join(sp.ds.gl.kew,kew.splist)
  sp.ds.gl.kew.genus <- sp.ds.gl.kew %>% filter(taxon_rank%in%'Genus')
  
  # load wcvp introduce data ---------------------------------
  
  sp.nds.gl.kew <- read.csv('data/Map_kew/kew_introduce_clean.csv')
  sp.nds.gl.kew.genus <- read.csv('data/Map_kew/kew_introduce_clean_genus.csv')
  sp.nds.gl.kew <- left_join(sp.nds.gl.kew,mapkew_tb,c('area_code_l3' = 'LEVEL3_COD'))
  sp.nds.gl.kew.genus <- left_join(sp.nds.gl.kew.genus,mapkew_tb,c('area_code_l3' = 'LEVEL3_COD'))
  
  # load GolNAF aline species ------------------------------------
  mapnaf <- raster('data/Map_GolNAF/GloNAF_Shapefile/GloNAF_map.tif')
  sp.nds.naf <- read.csv('data/Map_GolNAF/GLONAF/spdsNAF.csv')
  sp.nds.naf <- sp.nds.naf %>% mutate(OBJIDsic=as.numeric(OBJIDsic)) %>% filter(!is.na(OBJIDsic))
  return(0)
})

test <- parLapply(cl,flag.num,function(i){
#test <- parLapply(cl,1:nrow(spls),function(i){
  sp <- spls$species[i]
  ge <- spls$genus[i]
  
  # take care of this file name
  path.load <- spls$path_archive[i]
  path.out <- spls$path_out[i]
  
  if(!file.exists(path.load)) return(0)

  spds <- read.csv(path.load)
  spds$decimalLongitude <- as.numeric(spds$decimalLongitude)
  spds$decimalLatitude <- as.numeric(spds$decimalLatitude)
  spds <- spds[!is.na(spds$decimalLongitude),]
  spds <- spds[!is.na(spds$decimalLatitude),]
  
  # pass none data
  if(nrow(spds)<1) {
    write.csv(spds,path.out,row.names=F)
    return(0)
  }
  
  # mark gbif accepted ------------------------------------------------------
  spds$gbif_mark <- TRUE
  spds$gbif_mark[which(spds$coordinateUncertaintyInMeters>10000)] <- FALSE
  spds$gbif_mark[which(spds$year<=1945)] <- FALSE
  
  # mark pkureg accepted ------------------------------------------------------
  pkuregion_spds <- sp.ds.gl.pku$Adcode99[sp.ds.gl.pku$Species_E2%in%sp]
  
  xys <- extract(mappku,spds[,c('decimalLongitude','decimalLatitude')]);
  flag <- xys %in% pkuregion_spds
  
  spds$region_pku_mark <- flag
  spds$region_mark <- flag
  
  # mark gift accepted ------------------------------------------------------
  gift_spds <- GIFT_database$checklists$entity_ID[GIFT_database$checklists$work_species%in%sp]
  
  if(length(gift_spds)>0){
    spds_sf <- st_as_sf(spds,coords = c('decimalLongitude','decimalLatitude'))
    st_crs(spds_sf) <- st_crs(gift_shape)
    spds_sf <- st_make_valid(spds_sf)
    gift_shape_subset <- gift_shape[gift_shape$entt_ID%in%gift_spds,]
    spds_entt_id <- st_within(spds_sf,gift_shape_subset)
    flag <- spds_entt_id%>%lapply(function(x){length(x)>0})%>%unlist()
    spds$region_gift_mark <- flag
    spds$region_mark <- spds$region_mark|flag
  } 
  # else{
  #   spds$GIFT <- FALSE
  # }
  
  # mark kew accepted ------------------------------------------------------
  
  kew_spds <- sp.ds.gl.kew$LEVEL3_COD_id[sp.ds.gl.kew$taxon_name%in%sp]
  kew_spds_genus <- sp.ds.gl.kew.genus$LEVEL3_COD_id[sp.ds.gl.kew.genus$taxon_name%in%ge]
  
  if(length(kew_spds)>0){
    xys <- extract(mapkew,spds[,c('decimalLongitude','decimalLatitude')]);
    flag <- xys %in% kew_spds
    
    spds$region_wcvp_mark <- flag
    spds$region_mark <- spds$region_mark|flag
    spds$wcvp_genus_mark <- flag
  } else if(length(kew_spds_genus)>0){
    xys <- extract(mapkew,spds[,c('decimalLongitude','decimalLatitude')]);
    flag <- xys %in% kew_spds_genus
    
    #spds$wcvp <- FALSE
    spds$wcvp_genus_mark <- flag
  } else if( (length(kew_spds_genus) + length(kew_spds)) <=0){
    #spds$wcvp <- FALSE
    spds$wcvp_genus_mark <- FALSE
  }
  
  # mark alien speiceis  -----------------------------------------------------
  
  spds$is_alien <- FALSE

  kew_spds_clean <- sp.nds.gl.kew$LEVEL3_COD_id[sp.nds.gl.kew$taxon_name_spp%in%sp]
  kew_spds_clean_genus <- sp.nds.gl.kew.genus$LEVEL3_COD_id[sp.nds.gl.kew.genus$genus%in%ge]
  
  if(length(kew_spds_clean)>0){
    xys <- raster::extract(mapkew,spds[,c('decimalLongitude','decimalLatitude')]);
    flag <- xys %in% kew_spds_clean
    spds$is_alien <- flag
  } else if(length(kew_spds_clean_genus)>0){
    xys <- raster::extract(mapkew,spds[,c('decimalLongitude','decimalLatitude')]);
    flag <- xys %in% kew_spds_clean
    
    spds$is_alien <- flag
  }
  
  # mark alien speiceis by GolNAF  -----------------------------------------------------  
  naf_spds_clean <- sp.nds.naf$OBJIDsic[sp.nds.naf$taxon_orig%in%sp]
  
  xys <- raster::extract(mapnaf,spds[,c('decimalLongitude','decimalLatitude')])
  xys[is.na(xys)] <- FALSE
  flag <- xys %in% naf_spds_clean
  spds$is_alien <- spds$is_alien|flag

  # mark countries centroids  -----------------------------------------------------    
  rst.c <- cellFromXY(mappku,spds[,c('decimalLongitude','decimalLatitude')])
  #spds$is_cty_c_mark <- rst.c%in%cell.c
  #spds$is_outlier <- rst.c%in%c(4917686,4831031) #4826708
  spds$is_outlier <- rst.c%in%c(4917686,4831031,3988730,4152806,4913363,6187621,
                                3169676,2294821,1797801,1276777,3158857,
                                2294821,5219947,1065009)
  # delect longtitude == latitude  -----------------------------------------------------    
  spds <- spds[abs(spds$decimalLatitude-spds$decimalLongitude)>0.001,]
  
  write.csv(spds,path.out,row.names=F)
  
  if(nrow(spds)<1) return(NULL)
  spds2 <- spds %>% filter(!is_alien) %>% filter(!is_outlier) %>% 
    filter(gbif_mark) %>% filter(region_mark)
  tmp <- data.frame(species=spls$species[i],family=spls$family[i],
                    num_ori=nrow(spds),num_region=sum(spds$region_mark),num_clean=nrow(spds2))
  return(tmp)
  #return(sum(spds$region_mark))
}) 
stopCluster(cl)

write.csv(spls,'splist/spls_mark.csv',row.names = F)