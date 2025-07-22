### =========================================================================
### Preparations
### =========================================================================

# Get species name
sps <- read.csv(path_data)
spi_name <- spls$species[iii]

obs = sps %>% dplyr::select(decimalLongitude,decimalLatitude) %>% na.omit()

templ_dir = enstr_path = temp_dir = paste0('Temp/Temp',sample(1:9999999999,1),'/')
if(!dir.exists(enstr_path)){
  dir.create(enstr_path, recursive = T)
}

### =========================================================================
### coordinates to cell number
### =========================================================================

obs_c <- obs
obs_c$c <- cellFromXY(rst.env,obs)
if(length(unique(obs_c$c))>20){
  obs_c <- unique(obs_c$c)
}else{
  obs_c <- obs_c$c
}
obs = xyFromCell(rst.env,obs_c) %>% as.data.frame()
obs$cell <- obs_c

spp <- st_as_sf(obs,coords=c('x','y'),crs=proj)


### =========================================================================
### Run the range function
### =========================================================================

cat('running range function...\n')

finalpol=al.range.fun(species_name = spi_name,
        occ_coord=obs,
        proj = proj,
        rst = rst.poll,
        degrees_outlier=10,
        buffer_width= 1000000)

### =========================================================================
### extract env in finial pool
### =========================================================================

# cat('reading env predictors...\n')
env.df <- filter(rstlist,cell_poll%in%finalpol)
