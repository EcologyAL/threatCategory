# read new assessment -----------------------------------------------------

red.list <- read.csv(paths["spAssSim"])# %>% na.omit()
sp <- read.csv(paths["spLS"])
sp <- sp$speciesKey[sp$class%in%c('Magnoliopsida','Liliopsida')]
red.list <- red.list[red.list$speciesKey%in%sp,]

red.list$category <- red.list$intCategory
red.list$category[is.na(red.list$category)] <- 'DD'

# int assessment -----------------------------------------------------

red.list$intCategory <- factor(red.list$category,levels=c('DD','LC','NT','VU','EN','CR','EX'))
red.list$redlistCategory <- factor(red.list$redlistCategory,levels=c('DD','LC','NT','VU','EN','CR','EX'))

red.list.cp <- red.list[,c('intCategory','redlistCategory')] %>% na.omit() %>% 
  group_by(intCategory,redlistCategory) %>% mutate(n=n()) %>% distinct()
red.list.cp <- red.list.cp[order(red.list.cp$intCategory,decreasing = T),]
red.list.cp <- red.list.cp[order(red.list.cp$redlistCategory),]
if(!red.list.cp[1,1]%in%'EX'){
  tmp <- red.list.cp[1,];tmp[1,1] <- 'EX';tmp[1,3] <- 0;red.list.cp <- rbind(tmp,red.list.cp)
}
levels(red.list.cp$redlistCategory) <- c('DD ','LC ','NT ','VU ','EN ','CR ',' EX ')

# map circle --------------------------------------------------------------


mycol <- c("#D1D1C6","#70BE50","#CCE226","#FFF204","#F89D57","#ED1C2E","BLACK");mycol <- c(rev(mycol),mycol)

{
  #cairo_ps(paste0(paths['fig2_dir'],'fig2a_Ensem.ps'),width=2,height=2,pointsize=6)
  pdf(
    file = paste0(paths["fig2_dir"], "fig2a_Ensem.pdf"),
    width = 2,
    height = 2,
    pointsize=6,
    useDingbats = FALSE
  )
  #jpeg('outputs/fig2a_noEnsem.jpg',width=3.6,height=3.6,units = 'in',res=300)
  circos.par(start.degree = 0)
  chordDiagram(red.list.cp,big.gap = 15,
               annotationTrack = c('name','grid'),
               annotationTrackHeight = mm_h(c(2, 2)),
               grid.col =  mycol)
  
  circos.track(
    track.index = 1, 
    panel.fun = function(x, y) {
      xlim = get.cell.meta.data("xlim")
      ylim = get.cell.meta.data("ylim")
      sector.name = get.cell.meta.data("sector.index")
      xplot = get.cell.meta.data("xplot")
      #by = ifelse(abs(xplot[2] - xplot[1]) > 30, 0.25, 0.5)
      by = if(abs(xplot[2] - xplot[1]) > 30){
        0.25
      }else if(abs(xplot[2] - xplot[1]) > 5){
        0.5
      } else{
          1
        }
      for (p in seq(by, 1, by = by)[-length(seq(by, 1, by = by))]) {
        circos.text(
          p * (xlim[2] - xlim[1]) + xlim[1],
          mean(ylim)-0.5,
          paste0(p * 100, "%"),
          cex = 0.5,
          adj = c(0.5, 0),
          niceFacing = TRUE
        )
      }
    }, bg.border = NA)
  circos.clear()
  dev.off()
}

# load shp and sp.ds.1d ---------------------------------------------------
shp <- read_sf(paths["baseMap"])
tmp <- st_coordinates(shp) %>% as.data.frame() %>% group_by(L3) %>% mutate(Y=max(Y)) %>% ungroup() %>% 
  dplyr::select(L3,Y) %>% distinct()
shp$lat <- tmp$Y
shp$land <- shp$land%in%c('T','TRUE','1')

cellInfo <- read.csv(paths[['cellInfo']])
#cellInfo <- read.csv('data_modOutput/v4/cellInfo.csv')
#colnames(rli)[1] <- 'layer'
shp <- shp[,c('cell_poll','land')]
shp <- left_join(shp,cellInfo)
shp$deltaRLI[!shp$land] <- NA
# shp$deltaRLI[(shp$SR<10)&(shp$lat>55)] <- NA
# shp$deltaRLI[which(shp$deltaRLI>0)] <- NA


library(scales)
brks <- pretty(range(shp$deltaRLI[which(shp$deltaRLI<=0)], na.rm = TRUE), n = 9)
brks <- c(brks,max(shp$deltaRLI,na.rm = T))
#brks <- pretty(range(shp$deltaRLI, na.rm = TRUE), n = 9)
print(length(brks))

shp <- shp %>%
  mutate(RLI_bin9 = cut(deltaRLI, breaks = brks, include.lowest = TRUE, right = TRUE))
#cols9 <- c(heat.colors(7),"white")
print(length(unique(brks)))

levels(shp$RLI_bin9) <- gsub(
  "\\(|\\]|\\[", "",
  gsub(",", " ~ ", levels(shp$RLI_bin9))
)
cols9 <- c(heat.colors(length(brks))[1:(length(brks)-2)],"#FFFDE7")

p <- ggplot(shp) +
  geom_sf(aes(fill = RLI_bin9), colour = NA) +
  scale_fill_manual(values = cols9, 
                    na.value = "gray95", 
                    na.translate = FALSE,
                    drop = TRUE,
                    name = "") +
  scale_x_continuous(limits = c(-170,170)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(
          fill = "#EAF4FB",
          colour = NA
        ),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position = c(0.08,0.3),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.key.height = unit(0.1, "in"),
        legend.key.width  = unit(0.05, "in"))
p

ggsave(paste0(paths["fig2_dir"],'figure2c_em.pdf'),plot = p,width = 6,height = 3)
ggsave(paste0(paths["fig2_dir"],'figure2c_em.jpg'),plot = p,width = 6,height = 3,dpi=300)

shp$RLI_old[!shp$land] <- NA
mypalette <- colorRampPalette(c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred"))
mycol <- rev(mypalette(100))
mycol <- mycol[c(1:50,seq(51,100,3))]
#mycol <- mycol[c(seq(1,50,2),51:100)]
#mycol[c(seq(1,50,2),51:100)]

p <- ggplot(shp) +    
  geom_sf(aes(fill = RLI_old,colour=RLI_old)) + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='',
                       na.value = 'gray95') +
  scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide='none') +
  scale_x_continuous(limits = c(-170,170)) +
  theme_bw() + theme(panel.grid=element_blank(),
                     legend.position = c(0.08,0.3),
                     panel.background = element_rect(
                       fill = "#EAF4FB",
                       colour = NA
                     ),
                     legend.background = element_rect(fill = "transparent", colour = NA),
                     axis.text.x = element_blank(),
                     axis.text.y = element_blank(),
                     axis.ticks = element_blank(),
                     #legend.direction = 'horizontal',
                     legend.key.height = unit(0.1, "in"),
                     legend.key.width = unit(0.05, "in")) #+ 
#labs(title='(c) RLI anomaly')
p
ggsave(paste0(paths["fig2_dir"],'figure2b.pdf'),width = 4,height = 2)
ggsave(paste0(paths["fig2_dir"],'figure2b.jpg'),width = 4,height = 2,dpi=300)
