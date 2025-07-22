al.range.fun <- function (species_name,
                          occ_coord, 
                          proj = proj,
                          rst = rst.poll,
                          degrees_outlier=10,
                          buffer_width=500000,
                          clustered_points_outlier=3
){
  cat("## Start of computation for species: ",species_name," ###", "\n") 
  
  ### =========================================================================
  ### Identify outliers
  ### =========================================================================
  
  #create distance matrix...
  occ_coord <- occ_coord[,c('x','y')]
  mat_dist <- as.matrix(knn.dist(occ_coord[,c('x','y')], k=clustered_points_outlier))
  
  #mark outliers
  cond <- apply(mat_dist, 1, function(x) x[clustered_points_outlier])>degrees_outlier
  rm(mat_dist) 
  
  cat(paste0(sum(cond), " outlier's from " ,nrow(occ_coord), " | proportion from total points: ", round((sum(cond)/length(occ_coord))*100,0), "%\n"))
  
  occ_coord_mod <- occ_coord[!cond,]
  occ_coord_mod$x <- floor(occ_coord_mod$x)+0.5
  occ_coord_mod$y <- floor(occ_coord_mod$y)+0.5
  occ_coord_mod <- distinct(occ_coord_mod)
  ### =========================================================================
  ### thin points
  ### =========================================================================
  
  # Stop if too many outliers in data set
  if(nrow(occ_coord_mod)==0){
    stop('Too few occurrences within outlier threshold!')
  }
  occ_coord_mod <-  st_as_sf(occ_coord_mod,coords=c('x','y'),crs=proj)
   
  ### =========================================================================
  ### Extract geopolitical units
  ### =========================================================================
  
  gppoly.sp <- st_buffer(occ_coord_mod,dist=buffer_width)
  gppoly.sp <- st_union(gppoly.sp)
  
  gppoly.sp <- gppoly.sp %>% st_as_sf() %>% st_make_valid()
  gppoly.sp.cell <- st_intersects(gppoly.sp,rst)
  gppoly.sp.cell <- unlist(gppoly.sp.cell) %>% unique()
  
  cat("## End of computation for species: ",species_name," ###", "\n") 
  
  return(gppoly.sp.cell)
}
