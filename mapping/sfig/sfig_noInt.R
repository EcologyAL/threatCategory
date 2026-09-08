# read new assessment -----------------------------------------------------

# red.list <- read.csv(paths["spAssSim"])# %>% na.omit()
# sp <- read.csv(paths["spLS"])
# sp <- sp$speciesKey[sp$class%in%c('Magnoliopsida','Liliopsida')]
# red.list <- red.list[red.list$speciesKey%in%sp,]

#red.list$category <- red.list$intCategory
#red.list$category[is.na(red.list$category)] <- 'DD'

# int assessment -----------------------------------------------------

#red.list$intCategory <- factor(red.list$climCategory,levels=c('DD','LC','NT','VU','EN','CR','EX'))
# red.list$intCategory <- factor(red.list$intCategory,levels=c('DD','LC','NT','VU','EN','CR','EX'))
# red.list$redlistCategory <- factor(red.list$redlistCategory,levels=c('DD','LC','NT','VU','EN','CR','EX'))
# 
# red.list.cp <- red.list[,c('intCategory','redlistCategory')] %>% na.omit() %>% 
#   group_by(intCategory,redlistCategory) %>% mutate(n=n()) %>% distinct()
# red.list.cp <- red.list.cp[order(red.list.cp$intCategory,decreasing = T),]
# red.list.cp <- red.list.cp[order(red.list.cp$redlistCategory),]
# levels(red.list.cp$redlistCategory) <- c('DD ','LC ','NT ','VU ','EN ','CR ',' EX ')

# map circle --------------------------------------------------------------


# mycol <- c("#D1D1C6","#70BE50","#CCE226","#FFF204","#F89D57","#ED1C2E","BLACK");mycol <- c(rev(mycol),mycol)
# 
# {
#   cairo_ps(paths['sfig_noInt_a'],width=2,height=2,pointsize=6)
#   #jpeg('outputs/fig2a_noEnsem.jpg',width=3.6,height=3.6,units = 'in',res=300)
#   circos.par(start.degree = 0)
#   chordDiagram(red.list.cp,big.gap = 15,
#                annotationTrack = c('name','grid'),
#                annotationTrackHeight = mm_h(c(2, 2)),
#                grid.col =  mycol)
#   
#   circos.track(
#     track.index = 1, 
#     panel.fun = function(x, y) {
#       xlim = get.cell.meta.data("xlim")
#       ylim = get.cell.meta.data("ylim")
#       sector.name = get.cell.meta.data("sector.index")
#       xplot = get.cell.meta.data("xplot")
#       #by = ifelse(abs(xplot[2] - xplot[1]) > 30, 0.25, 0.5)
#       by = if(abs(xplot[2] - xplot[1]) > 30){
#         0.25
#       }else if(abs(xplot[2] - xplot[1]) > 5){
#         0.5
#       } else{
#         1
#       }
#       for (p in seq(by, 1, by = by)[-length(seq(by, 1, by = by))]) {
#         circos.text(
#           p * (xlim[2] - xlim[1]) + xlim[1],
#           mean(ylim)-0.5,
#           paste0(p * 100, "%"),
#           cex = 0.5,
#           adj = c(0.5, 0),
#           niceFacing = TRUE
#         )
#       }
#     }, bg.border = NA)
#   circos.clear()
#   dev.off()
# }

# load shp and sp.ds.1d ---------------------------------------------------
shp <- read_sf(paths["baseMap"])
shp <- shp[,c('cell_poll','land')];shp$land <- shp$land%in%c('T','TURE','1')

cellInfo <- read.csv(paths["cellInfo"])
shp <- left_join(shp,cellInfo)
shp$RLI_int_shareSp[!shp$land] <- NA
shp$deltaRLI_int_shareSp[!shp$land] <- NA
shp$deltaRLI_climC[!shp$land] <- NA

#shp$deltaRLI[which(shp$SR<10)] <- NA
#shp$deltaRLI[(shp$SR<10)&(shp$lat>55)] <- NA
#shp$deltaRLI[which(shp$deltaRLI>0)] <- 0.022

# mapping ---------------------------------------------------

library(scales)
rg <- range(c(shp$RLI_int,shp$RLI_int_shareSp),na.rm = T)
mypalette <- colorRampPalette(c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred"))
mycol <- rev(mypalette(100))
#mycol <- mycol[c(1:50,seq(51,100,2))]
p <- ggplot(shp) +    
  geom_sf(aes(fill = RLI_int,colour=RLI_int)) + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       limits=rg,
                       name='',
                       na.value = 'gray95') +
  scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide='none') +
  scale_x_continuous(limits = c(-170,170)) +
  theme_bw() + theme(panel.grid=element_blank(),
                     legend.position = c(0.08,0.35),
                     axis.text.x = element_blank(),
                     axis.text.y = element_blank(),
                     axis.ticks = element_blank(),
                     legend.text = element_text(size=6),
                     legend.key.height = unit(0.1, "in"),
                     legend.key.width = unit(0.03, "in")) #+ 
p1 <- p

mypalette <- colorRampPalette(c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred"))
mycol <- rev(mypalette(100))
#mycol <- mycol[c(1:50,seq(51,100,2))]
p <- ggplot(shp) +    
  geom_sf(aes(fill = RLI_int_shareSp,colour=RLI_int_shareSp)) + # ,lwd =0  
  scale_fill_gradientn(colors = mycol,
                       name='',
                       limits=rg,
                       na.value = 'gray95') +
  scale_colour_gradientn(colors = mycol,na.value = 'gray95',guide='none') +
  scale_x_continuous(limits = c(-170,170)) +
  theme_bw() + theme(panel.grid=element_blank(),
                     legend.position = c(0.08,0.35),
                     axis.text.x = element_blank(),
                     axis.text.y = element_blank(),
                     axis.ticks = element_blank(),
                     #legend.direction = 'horizontal',
                     legend.text = element_text(size=6),
                     legend.key.height = unit(0.1, "in"),
                     legend.key.width = unit(0.03, "in")) #+ 
p2 <- p

brks <- pretty(range(shp$deltaRLI_int_shareSp, na.rm = TRUE), n = 9)
shp <- shp %>%
  mutate(RLI_bin9 = cut(deltaRLI_int_shareSp, breaks = brks, include.lowest = TRUE, right = TRUE))
#cols9 <- c(heat.colors(7),"white")

levels(shp$RLI_bin9) <- gsub(
  "\\(|\\]|\\[", "",
  gsub(",", " ~ ", levels(shp$RLI_bin9))
)
cols9 <- c(heat.colors(10)[1:8])
#cols9 <- c(heat.colors(8),"#FFFDE7")

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
        legend.position = c(0.1,0.35),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=6),
        legend.key.height = unit(0.06, "in"),
        legend.key.width = unit(0.03, "in"))
p3 <- p

brks <- pretty(range(shp$deltaRLI_climC[which(shp$deltaRLI_climC<=0)], na.rm = TRUE), n = 8)
brks <- c(brks,max(shp$deltaRLI_climC,na.rm = T))
#brks <- pretty(range(shp$deltaRLI_climC, na.rm = TRUE), n = 9)

shp <- shp %>%
  mutate(RLI_bin9 = cut(deltaRLI_climC, breaks = brks, include.lowest = TRUE, right = TRUE))
#cols9 <- c(heat.colors(7),"white")

levels(shp$RLI_bin9) <- gsub(
  "\\(|\\]|\\[", "",
  gsub(",", " ~ ", levels(shp$RLI_bin9))
)
cols9 <- c(heat.colors(9)[1:7],"#FFFDE7")

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
        legend.position = c(0.1,0.35),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=6),
        legend.key.height = unit(0.06, "in"),
        legend.key.width = unit(0.03, "in"))
p4 <- p

p1+p2+p4+p3+plot_layout()
ggsave(str_replace(paths["sfig_noInt_b"],'jpg','pdf'),width = 7,height = 3.8)
ggsave(paths["sfig_noInt_b"],width = 7,height = 3.8,dpi=300)
