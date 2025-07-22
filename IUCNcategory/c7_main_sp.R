library(ggplot2);library(patchwork);library(stringr);library(sf);library(dplyr)
setwd('E:/luoa/')
sp.info <- read.csv('data/future/spInfo_future.csv')

vars <- c("X2070_ssp126","X2070_ssp126_limit","X2070_ssp126_current",
          "X2070_ssp370","X2070_ssp370_limit","X2070_ssp370_current",
          "X2070_ssp585","X2070_ssp585_limit","X2070_ssp585_current",
          "X2100_ssp126","X2100_ssp126_limit","X2100_ssp126_current",
          "X2100_ssp370","X2100_ssp370_limit","X2100_ssp370_current",
          "X2100_ssp585","X2100_ssp585_limit","X2100_ssp585_current") %>% 
  str_remove('X') %>% paste0('_unlimit') %>% str_replace('_limit_unlimit','_limit') %>% str_replace('_current_unlimit','_nondispersal')

colnames(sp.info)[-(1:4)] <- colnames(sp.info)[-(1:4)] %>% str_remove('X') %>% paste0('_unlimit') %>% str_replace('_limit_unlimit','_limit') %>% str_replace('_current_unlimit','_nondispersal')

sp.rank <- data.frame()
sp.rank.2 <- data.frame()
for (i in vars) {
  tmp <- (sp.info[,i]-sp.info[,'current'])/sp.info[,'current']
  tmp[is.na(tmp)] <- -1
  
  EX <- which(tmp == -1)
  CR <- which(tmp < (-0.8))
  EN <- which(tmp < (-0.5))
  VU <- which(tmp < (-0.3))
  LC <- which(tmp >= (-0.3))
  
  sp.info.i <- data.frame(speciesKey=sp.info$speciesKey,rank=NA)
  sp.info.i$rank[LC] <- 'LC'
  sp.info.i$rank[VU] <- 'VU'
  sp.info.i$rank[EN] <- 'EN'
  sp.info.i$rank[CR] <- 'CR'
  sp.info.i$rank[EX] <- 'EX'
  
  sp.info.i$year <- str_split(i,'_')[[1]][1]
  sp.info.i$scenarios <- str_split(i,'_')[[1]][2]
  sp.info.i$dispersal <- str_split(i,'_')[[1]][3]
  
  sp.rank <- rbind(sp.rank,sp.info.i)
  if(nrow(sp.rank.2)==0) {
    sp.rank.2 <- data.frame(speciesKey=sp.info$speciesKey,rank=sp.info.i$rank)
    colnames(sp.rank.2)[2] <- i
  }else {
    tmp <- data.frame(rank=sp.info.i$rank)
    colnames(tmp) <- i
    sp.rank.2 <- cbind(sp.rank.2,tmp)
  }
}

sp.rank$rank <- factor(sp.rank$rank,levels=c('LC','VU','EN','CR','EX'))
sp.rank$dispersal <- factor(sp.rank$dispersal,levels=c('unlimit','limit','nondispersal'))
levels(sp.rank$dispersal) <- c('完全扩散','有限扩散','不扩散') 

spls$X2100_ssp585_nondispersal
# GF <- read.csv('data/spInfo/growthForm/Growthform_Allplants.csv')
# GF$Accepted_SpName1 %<>% stringr::str_replace_all(' ','_')
# load('data/spc/spc.rdata');spc %<>% na.omit()
# splist <- spc[,-2] %>% distinct()

# rank map ----------------------------------------------------------------

sp.rank.2100 <- sp.rank[sp.rank$year%in%2100,]
sp.rank.2100 <- sp.rank.2100[sp.rank.2100$scenarios%in%'ssp370',]
sp.rank.2100 <- sp.rank.2100[sp.rank.2100$dispersal%in%'有限扩散',]

load('data/spc/spc.rdata');spc %<>% na.omit()
source('Chapter3/code/func/func.R')
spc.all <- spc

#world <- read_sf('data/map/rst_shape/raw/raster_boarder.shp')
world <- read_sf('data/worldMap/worldMap.shp')
world <- world[,c('cell','flag')]

sr.func <- function(spc){
  sr <- spc$cell %>% table() %>% as.data.frame()
  colnames(sr) <- c('cell','SR')
  sr$cell %<>% as.character() %>% as.numeric()
  #world.map <- world %>% left_join(sr)
  return(sr)
}

sp <- sp.rank.2100$speciesKey[sp.rank.2100$rank%in%'LC']
sr <- sr.func(spc[spc$speciesKey%in%sp,])
colnames(sr)[2]<-'SR_LC'
world.map <- left_join(world,sr)

sp <- sp.rank.2100$speciesKey[sp.rank.2100$rank%in%'VU']
sr <- sr.func(spc[spc$speciesKey%in%sp,])
colnames(sr)[2]<-'SR_VU'
world.map <- left_join(world.map,sr)

sp <- sp.rank.2100$speciesKey[sp.rank.2100$rank%in%'EN']
sr <- sr.func(spc[spc$speciesKey%in%sp,])
colnames(sr)[2]<-'SR_EN'
world.map <- left_join(world.map,sr)

sp <- sp.rank.2100$speciesKey[sp.rank.2100$rank%in%c('CR','EX')]
sr <- sr.func(spc[spc$speciesKey%in%sp,])
colnames(sr)[2]<-'SR_CR'
world.map <- left_join(world.map,sr)

sr <- sr.func(spc)
world.map <- left_join(world.map,sr)


function(value,world.map.proj,legend.name='') {
  if(legend.name%in%"")legend.name<-value
  p <- ggplot(world.map.proj) + 
    geom_sf(aes(fill=!!as.name(value)),color='transparent')+ labs(fill = legend.name) +
    scale_fill_viridis_c(direction=-1,na.value = 'gray90',position='bottom') + 
    geom_sf(data=NE_box_rob, colour="gray50", fill="transparent", size = 0.25)+
    geom_sf(data=NE_graticules_rob, linetype="dotted", color="grey50",alpha=0.5, size = 0.25)+
    #geom_text(data = lbl.Y.prj, aes(x = X.prj, y = Y.prj, label = lbl), color="grey50", size=2) +
    #geom_text(data = lbl.X.prj, aes(x = X.prj, y = Y.prj, label = lbl), color="grey50", size=2) +
    theme_void()+
    theme(legend.position = c(0.5,0.12),
          legend.direction = 'horizontal',
          #legend.background = element_rect(fill = "white", color = "black"),
          legend.key.height = unit(0.8, "line"),
          legend.key.width = unit(1.6, "line"))
  #p
  return(p)
}

world.map <- st_transform(world.map, crs = "+proj=robin")
mapRobin3<-function(value,world.map.proj,legend.name='') {
  if(legend.name%in%"")legend.name<-value
  #b.max <- world.map.proj[,value] %>% st_drop_geometry() %>% max(na.rm = T) %>% floor()
  #b.min <- world.map.proj[,value] %>% st_drop_geometry() %>% min(na.rm = T) %>% ceiling()
  p <- ggplot(world.map.proj) + 
    #labs(title = value) +
    geom_sf(aes(fill=!!as.name(value)),color='transparent')+ labs(fill = ' ') +
    scale_fill_viridis_c(direction=-1,na.value = 'gray90',position='bottom'#,breaks=c(b.min,b.max)
    )+ 
    geom_sf(data=NE_box_rob, colour="gray50", fill="transparent", size = 0.25)+
    theme_void()+
    theme(legend.position = c(0.5,0.12),
          title = element_text(size = 8),
          plot.title = element_text(hjust = 0.5),
          legend.direction = 'horizontal',
          legend.text = element_text(size = 6),
          legend.key.height = unit(0.2, "line"),
          legend.key.size = unit(1, "line"))
  #p
  return(p)
}

value <- 'SR_LC'
world.map[[value]] <- world.map[[value]]/world.map[['SR']]
world.map[[value]][world.map$flag==0] <- NA
p1 <- mapRobin3(value,world.map,"无危物种丰富度比例")
p1 <- p1 + labs(title='无危物种丰富度比例')

value <- 'SR_VU'
world.map[[value]] <- world.map[[value]]/world.map[['SR']]
world.map[[value]][world.map$flag==0] <- NA
p2 <- mapRobin3(value,world.map,"易危物种丰富度比例")
p2 <- p2 + labs(title='易危物种丰富度比例')

value <- 'SR_EN'
world.map[[value]] <- world.map[[value]]/world.map[['SR']]
world.map[[value]][world.map$flag==0] <- NA
world.map$SR_EN[world.map[[value]]%in%1]<-NA
p3 <- mapRobin3(value,world.map,"濒危物种丰富度比例")
p3 <- p3 + labs(title='濒危物种丰富度比例')

value <- 'SR_CR'
world.map[[value]] <- world.map[[value]]/world.map[['SR']]
world.map[[value]][world.map$flag==0] <- NA
p4 <- mapRobin3(value,world.map,"极危及以上物种丰富度比例")
p4 <- p4 + labs(title='极危及以上物种丰富度比例')

combined_plots <- wrap_plots(list(p1,p2,p3,p4), ncol = 2) 
ggsave(paste0('Chapter7/figures/','F8.jpg'),combined_plots, width=14.65, height=7.34, units="cm",dpi=300)

#ggsave(paste0('Chapter3/figures/',value,'.jpg'),p, width=14.65, height=7.34, units="cm")

# splist ------------------------------------------------------------------


length(unique(sp.rank$speciesKey))

options (scipen=200)
g <- ggplot(sp.rank[sp.rank$year%in%2100,], aes(scenarios))
g1 <- g + geom_bar(aes(fill = rank))+
  scale_fill_manual(values = brewer.pal(5,'Reds')) +
  scale_y_continuous(breaks = c(0,146395*0.25,146395/2,146395*0.75,146395),labels = c(0,25,50,75,100)) + 
  xlab("未来气候情景") + ylab("百分比（%）")+  labs(fill = '受威胁等级') +
  facet_grid(.~dispersal) + labs(title = '2100')+
  theme_bw()+
  theme(text=element_text(size=8),
        axis.text = element_text(size=5),
        legend.key.height = unit(0.5, "line"),
        legend.key.size = unit(0.5, "line"))

g <- ggplot(sp.rank[sp.rank$year%in%2070,], aes(scenarios))
g2 <- g + geom_bar(aes(fill = rank))+
  scale_fill_manual(values = brewer.pal(5,'Reds')) +
  scale_y_continuous(breaks = c(0,146395*0.25,146395/2,146395*0.75,146395),labels = c(0,25,50,75,100)) + 
  xlab("未来气候情景") + ylab("百分比（%）")+  labs(fill = '受威胁等级') +
  facet_grid(.~dispersal) + labs(title = '2070')+
  theme_bw()+
  theme(text=element_text(size=8),
        axis.text = element_text(size=5),
        legend.key.height = unit(0.5, "line"),
        legend.key.size = unit(0.5, "line"))

#combined_plots <- wrap_plots(c(list(g1),list(g2)), ncol = 2) 
ggsave(paste0('Chapter7/figures/','F6_1.jpg'),g1, width=10, height=7, units="cm",dpi=300)
ggsave(paste0('Chapter7/figures/','F6_2.jpg'),g2, width=10, height=7, units="cm",dpi=300)

# sp.info.data <- read.csv('data/spInfo/spInfo.csv')
# sp.info.data <- left_join(sp.info.data,sp.rank.2)

#write.csv(sp.info.data,'data/spInfo/spInfo.csv',row.names = F)

# IUCN --------------------------------------------------------------------

sp.rank.2100 <- sp.rank[sp.rank$year%in%2100,]
sp.rank.2100 <- sp.rank.2100[sp.rank.2100$scenarios%in%'ssp370',]
sp.rank.2100 <- sp.rank.2100[sp.rank.2100$dispersal%in%'有限扩散',]

iucn <- read.csv('Chapter7/data/redlist_species_data_407c4094-0f37-41e5-a43e-a10aba3afc28/assessments.csv')
iucn <- iucn[,c("scientificName","redlistCategory")]
colnames(iucn)[1] <- 'species'

iucn$redlistCategory[iucn$redlistCategory%in%c("Near Threatened","Near Threatened",
                                               "Least Concern","Lower Risk/least concern",
                                               "Lower Risk/conservation dependent",
                                               "Lower Risk/near threatened")] <- "LC" 
iucn$redlistCategory[iucn$redlistCategory%in%"Vulnerable"] <- "VU" 
iucn$redlistCategory[iucn$redlistCategory%in%"Endangered"] <- "EN" 
iucn$redlistCategory[iucn$redlistCategory%in%"Critically Endangered"] <- "CR" 
iucn$redlistCategory[iucn$redlistCategory%in%"Data Deficient"] <- NA
iucn$redlistCategory[iucn$redlistCategory%in%c("Extinct","Extinct in the Wild")] <- "EX"
iucn$redlistCategory <- factor(iucn$redlistCategory,levels=c('LC','VU','EN','CR','EX'))

spls <- read.csv('data/splist/spls_all.csv')
colnames(spls)[3] <- 'speciesKey'
spls <- spls[,1:3]

spls <- left_join(spls,iucn)
spls <- left_join(sp.rank,spls)

spls$change <- as.numeric(spls$rank) - as.numeric(spls$redlistCategory)
spls <- spls[!is.na(spls$change),]
spls$change <- spls$change>0

g1 <- ggplot(spls[spls$year%in%2070,], aes(scenarios)) + geom_bar(aes(fill = change))+
  scale_fill_manual(values = brewer.pal(5,'Reds'),labels=c("未上升","上升")) +
  scale_y_continuous(breaks = c(0,32364*0.25,32364/2,32364*0.75,32364),labels = c(0,25,50,75,100)) + 
  xlab("未来气候情景") + ylab("百分比（%）")+  labs(fill = '受威胁等级') +
  facet_grid(.~dispersal) + labs(title = '2070')+
  theme_bw()+
  theme(text=element_text(size=8),
        axis.text = element_text(size=5),
        legend.key.height = unit(0.5, "line"),
        legend.key.size = unit(0.5, "line"))
g2 <- ggplot(spls[spls$year%in%2100,], aes(scenarios)) + geom_bar(aes(fill = change))+
  scale_fill_manual(values = brewer.pal(5,'Reds'),labels=c("未上升","上升")) +
  scale_y_continuous(breaks = c(0,32364*0.25,32364/2,32364*0.75,32364),labels = c(0,25,50,75,100)) + 
  xlab("未来气候情景") + ylab("百分比（%）")+  labs(fill = '受威胁等级') +
  facet_grid(.~dispersal) + labs(title = '2100')+
  theme_bw()+
  theme(text=element_text(size=8),
        axis.text = element_text(size=5),
        legend.key.height = unit(0.5, "line"),
        legend.key.size = unit(0.5, "line"))
ggsave(paste0('Chapter7/figures/','F7_1.jpg'),g1, width=10, height=7, units="cm",dpi=300)
ggsave(paste0('Chapter7/figures/','F7_2.jpg'),g2, width=10, height=7, units="cm",dpi=300)
