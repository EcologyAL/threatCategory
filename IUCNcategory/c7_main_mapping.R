library(ggplot2);library(patchwork);library(stringr);library(sf);library(dplyr)

setwd("E:/luoa/")
source('data/map/Robin/MapRobin.R')
source('Chapter7/code/func/func_c7_mapping.R')
world <- read_sf('data/map/rst_shape/raw/raster_boarder.shp')
#load('data/worldMap.rdata')
world.map <- read.csv('Chapter3/data/mapinfo/worldMap.csv')
world.map <- left_join(world,world.map)
#world.map <- world.map[!is.na(world.map$SR),]
#world.map[as.numeric(st_area(world.map))<10^8,]
world.map[is.na(world.map$SR),-(1:2)] <- 0
world.map.proj <- st_transform(world.map, crs = "+proj=robin")
world.map <- world.map.proj

ft <- read.csv('data/future/cellInfo_future.csv')
vars <- c("X2070_ssp126","X2070_ssp126_limit","X2070_ssp126_current",
          "X2070_ssp370","X2070_ssp370_limit","X2070_ssp370_current",
          "X2070_ssp585","X2070_ssp585_limit","X2070_ssp585_current",
          "X2100_ssp126","X2100_ssp126_limit","X2100_ssp126_current",
          "X2100_ssp370","X2100_ssp370_limit","X2100_ssp370_current",
          "X2100_ssp585","X2100_ssp585_limit","X2100_ssp585_current") %>% 
  str_remove('X') %>% paste0('_unlimit') %>% str_replace('_limit_unlimit','_limit') %>% str_replace('_current_unlimit','_nondispersal')

colnames(ft)[-(1:3)] <- colnames(ft)[-(1:3)] %>% str_remove('X') %>% paste0('_unlimit') %>% str_replace('_limit_unlimit','_limit') %>% str_replace('_current_unlimit','_nondispersal')
ft[,vars] <- (ft[,vars]-ft[,'current'])/(ft[,'current'])

world.map <- left_join(world.map.proj,ft)

value <- vars[14]
p <- mapRobin.f.1(value,world.map,"")
ggsave(paste0('Chapter7/figures/0_',value,'.jpg'),p, width=14.65, height=7.34, units="cm")

# p.list <- list(mapRobin("PD",world.map,"谱系多样性"),mapRobin("FD",world.map,"功能多样性"))
# combined_plots <- wrap_plots(p.list, ncol = 1) 
# plot(combined_plots)
# ggsave(paste0('Chapter3/figures/','2_PD_FD.jpg'),combined_plots, width=14.65, height=7.34/1*2, units="cm")

values <- vars
p.list <- lapply(values,mapRobin.f.3,world.map)
# values.cn <- values %>% str_replace('SR','物种多样性') %>% 
#   str_replace('PD','谱系多样性') %>% str_replace('FD','功能多样性') %>% 
#   str_replace("_W",'（木本）') %>% str_replace("_H",'（草本）')
values.cn <- vars
for (i in 1:length(values)) {p.list[[i]] <- p.list[[i]] + labs(title=values.cn[i])}
combined_plots <- wrap_plots(p.list[1:9], ncol = 3) 
#plot(combined_plots)
ggsave(paste0('Chapter7/figures/','F2.jpg'),combined_plots, width=14.65*2, height=7.34*2+2, units="cm")

combined_plots <- wrap_plots(p.list[10:18], ncol = 3) 
#plot(combined_plots)
ggsave(paste0('Chapter7/figures/','F3.jpg'),combined_plots, width=14.65*2, height=7.34*2+2, units="cm")

## phylo func
world.map <- world.map.2100
world.map$SR2100 <- (world.map$SR2100-world.map$SR)/world.map$SR
world.map$PD2100 <- (world.map$PD2100-world.map$PD)/world.map$PD
world.map$FD2100 <- (world.map$FD2100-world.map$FD)/world.map$FD

p1 <- mapRobin.f.1('SR2100',world.map,"")
p2 <- mapRobin.f.1('PD2100',world.map,"谱系多样性的变化");p2 <- p2 + labs(title="谱系多样性变化比例")
p3 <- mapRobin.f.1('FD2100',world.map,"功能多样性的变化");p3 <- p3 + labs(title="功能多样性变化比例")

# world.map$TEST <- world.map$FD2100-world.map$SR2100
# p4 <- mapRobin.f.1('TEST',world.map,"")

combined_plots <- wrap_plots(p2,p3, ncol = 1)
ggsave(paste0('Chapter7/figures/','F5.jpg'),combined_plots, width=14.65, height=7.34*2, units="cm")


# gain & loss -------------------------------------------------------------

# dat <- read.csv('data/spc/future/cellInfo_future.csv')
# dat <- dat[,c('cell','gain','loss')]
# world.map <- left_join(world.map,dat)
world <- read_sf('data/map/rst_shape/raw/raster_boarder.shp')
world.map <- left_join(world,cell.info)

world.map$SR2100 <- world.map$SR_f-world.map$SR_c
world.map$SR2100_p <- (world.map$SR_f - world.map$SR_c)/world.map$SR_c
world.map$gain_p <- (world.map$gain)/world.map$SR_c
world.map$loss_p <- (world.map$loss)/world.map$SR_c

p1 <- mapRobin.f.1.plus('SR2100',world.map);p1 <- p1 + labs(title="物种丰富度变化")
p2 <- mapRobin.f.1('SR2100_p',world.map);p2 <- p2 + labs(title="物种丰富度变化比例")

combined_plots <- wrap_plots(list(p1,p2),ncol = 1)
ggsave(paste0('Chapter7/figures/','F5_1.jpg'),combined_plots, width=14.65, height=7.34*2, units="cm")

p3 <- mapRobin.f.g.3.plus('gain',world.map);p3 <- p3 + labs(title="迁入物种数")
p4 <- mapRobin.f.g.3('gain_p',world.map);p4 <- p4 + labs(title="迁入物种比例")

p5 <- mapRobin.f.l.3.plus('gain',world.map);p5 <- p5 + labs(title="迁出物种数")
p6 <- mapRobin.f.l.3('loss_p',world.map);p6 <- p6 + labs(title="迁出物种比例")


combined_plots <- wrap_plots(list(p3,p4,p5,p6),nrow = 2)
ggsave(paste0('Chapter7/figures/','F5_2.jpg'),combined_plots, width=14.65, height=7.34/2*2, units="cm")
