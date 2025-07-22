setwd('D:/work/RLI/')
sapply(c('foreign','sf','dplyr','ggplot2','raster','RColorBrewer','stringr','patchwork'),require,character.only=T)#'rasterVis'

mapdat <- read.csv('data/outputs/cell_1d.csv') 

shp.data <- read.dbf('data/map/data_1d/data_1d_land.dbf')
shp.data <- left_join(shp.data,dplyr::select(mapdat,-land))

shp.data$Priority2 <- shp.data$Priority
shp.data$Priority2[shp.data$Priority2%in%c('NP_NP','NP_P')] <- 'NP'
shp.data$Priority2[shp.data$Priority2%in%c('PAEP_G')] <- 'PAE30'
shp.data$Priority2[shp.data$Priority2%in%c('RLIP_G','PAEP_P','RLIP_P')] <- 'RLI30'
#write.dbf(shp.data,'data/outputs/map1d/data_1d_land.dbf')

mapdat <- mapdat[,c('cell_poll','PAs30')] %>% distinct() %>% na.omit()

sp.cell <- read.csv('data/spcell_1d/sp_cellpoll_all.csv') %>% distinct()
sp.cell <- left_join(sp.cell,mapdat)

sp.pa <- sp.cell %>% na.omit() %>% group_by(speciesKey) %>% mutate(PAsR=sum(PAs30)/n(),N=n(),PAsN=sum(PAs30)) %>% ungroup() %>% 
  dplyr::select(-cell_poll,-PAs30) %>% distinct()
#sp.pa$Protected <- sp.pa$PAsR>0.3|sp.pa$PAsN>10
log_linear_interpolate <- function(x_interp, y0=0.15, y1=1,  x0=25,  x1=1) {
  log_y0 <- log(y0)
  log_y1 <- log(y1)
  
  log_y_interp <- log_y0 + (log_y1 - log_y0) * (x_interp - x0) / (x1 - x0)
  y_interp <- exp(log_y_interp)
  return(y_interp)
}
sp.pa$flag <- log_linear_interpolate(sp.pa$N)
sp.pa$flag[sp.pa$N>25] <- 0.15
sp.pa$Protected <- sp.pa$PAsR>=sp.pa$flag

# sp.cty <- read.csv('data/outputs/spInfo/sp_cty.csv') 
# spm.cty <- distinct(sp.cty[,c('speciesKey','CONTINENT')])
# sp.pa <- left_join(sp.pa,sp.cty)
sp.info <- read.csv('data/outputs/spInfo/sp_continent.csv')
sp.info <- distinct(sp.info[,c('speciesKey','CONTINENT')])
sp.pa <- left_join(sp.pa,sp.info)

sp.assess <- read.csv('data/outputs/spInfo/sp_assess_simple.csv')
sp.pa <- left_join(sp.pa,sp.assess[,c('speciesKey','intCategory')])
sp.pa$TH <- sp.pa$intCategory%in%c('VU','EN','CR','EX')

gl.pa <- sp.pa %>% 
  dplyr::select(speciesKey,Protected,TH) %>%  
  distinct() %>% na.omit() %>% 
  mutate(TH_P=sum(Protected&TH,na.rm=T),TH_NP=sum((!Protected)&TH,na.rm=T),
         NTH_P=sum(Protected&(!TH),na.rm=T),NTH_NP=sum((!Protected)&(!TH)),na.rm=T) %>% 
  dplyr::select(TH_P,TH_NP,NTH_P,NTH_NP) %>% distinct()
gl.pa

options(scipen = 200)
tmp <- unlist(gl.pa)
tmp <- data.frame(x=c(1,1,2,2),y=as.numeric(tmp)) 
tmp[c(2,4),2] <- -tmp[c(2,4),2]
#tmp$color <- c('blue','red','lightblue','orange')
#tmp$color <- tmp$y>0
#tmp$color <- c('darkgreen','#7D4766','#65B988','#A75E88')
tmp$color <- c('#A88B56','#17356A','#E0B972','#3B588D')

ggplot(data=tmp)+
  geom_col(aes(x=x,y=y),fill = tmp$color,width = 1)+
  geom_hline(yintercept = 0)+
  #geom_vline(xintercept = 1.5)+
  theme_classic()+
  ylab('')+
  theme(text = element_text(size=6),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank())
ggsave('outputs/fig5/sppa/sppa_gl.eps',width = .8,height=.8,units='in',dpi=300)
ggsave('outputs/fig5/sppa/sppa_gl.jpg',width = .8,height=.8,units='in',dpi=300)

# map CONTINENT protected status ---------------------------------------------

sp.pa$CONTINENT[sp.pa$CONTINENT%in%'Australia'] <- 'Oceania'
rg.pa <- sp.pa %>% 
  dplyr::select(CONTINENT,speciesKey,Protected,TH) %>%  
  distinct() %>% na.omit() %>% 
  group_by(CONTINENT) %>%
  mutate(TH_P=sum(Protected&TH,na.rm=T),TH_NP=sum((!Protected)&TH,na.rm=T),
         NTH_P=sum(Protected&(!TH),na.rm=T),NTH_NP=sum((!Protected)&(!TH)),na.rm=T) %>% 
  ungroup() %>% 
  dplyr::select(CONTINENT,TH_P,TH_NP,NTH_P,NTH_NP) %>% distinct()
rg.pa <- rg.pa[!rg.pa$CONTINENT%in%'Antarctica',]
rg.pa <- rg.pa[order(rg.pa$CONTINENT),]
rg.pa$p <- rg.pa$TH_P/(rg.pa$TH_P + rg.pa$TH_NP)

for (i in 1:6) {
  tmp <- rg.pa[i,] %>% unlist()
  rg <- tmp[1]
  tmp <- data.frame(x=c(1,1,2,2),y=as.numeric(tmp[-1])) 
  tmp[c(2,4),2] <- -tmp[c(2,4),2]
  tmp$color <- c('#A88B56','#17356A','#E0B972','#3B588D')
  
  #tmp$color <- tmp[,2]>0
  ggplot(data=tmp)+
    geom_col(aes(x=x,y=y),fill = tmp$color,width = 1)+
    geom_hline(yintercept = 0,linewidth=0.2)+
    #geom_vline(xintercept = 1.5)+
    theme_classic()+
    #ylab('S.N.')+
    #labs(title=rg)+
    theme(text = element_text(size=3),
          line =  element_line(linewidth=0.1),
          # axis.line.x = element_line(lwd=0.1),
          # axis.ticks.x = element_blank(lwd=0.1),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title = element_blank())
  ggsave(paste0('outputs/fig5/sppa/sppa_',rg.pa$CONTINENT[i],'.png'),width = .5,height=.6,units='in',dpi=300)
  ggsave(paste0('outputs/fig5/sppa/sppa_',rg.pa$CONTINENT[i],'.eps'),width = .5,height=.6,units='in',dpi=300)
}

