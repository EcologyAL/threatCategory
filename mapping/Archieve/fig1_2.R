setwd('F:/luoao/RLI/')
sapply(c('sf','dplyr','ggplot2','raster','RColorBrewer','stringr'),require,character.only=T)#'rasterVis'

#### load data
#red.list <- read.csv('data/spinfo/spls_mark_all.csv')
red.list.short <- read.csv('data/IUCNcategory/sp_assess.csv') %>% na.omit()
red.list <- reshape2::melt(red.list.short,id.vars='speciesKey')
red.list <- cbind(do.call('rbind',str_split(red.list$variable,'_')),red.list) 
colnames(red.list) <- c('scenario','dispersal','speciesKey','group','category')

pdat <- red.list
#pdat$category[is.na(pdat$category)] <- 0

pdat$category <- factor(pdat$category,levels=c('LC','VU','EN','CR','EX')) #,'EX'
pdat$dispersal <- factor(pdat$dispersal,levels=c('unlimit','limit','no'),
                         labels=c('Full dispersal','20 km/decade','No dispersal'))
pdat$scenario <- factor(pdat$scenario,levels=c('ssp126','ssp370','ssp585'),
                        labels=c('ssp1-2.6','ssp3-7.0','ssp5-8.5'))

mycol <- c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred")
#mycol <- c("#FFF204","#F89D57","#ED1C2E","#8B0000")

n.all <- nrow(red.list.short)
g <- ggplot(pdat, aes(x=scenario))
g1 <- g + geom_bar(aes(fill = category))+
  scale_fill_manual(values = c(mycol)) +
  #xlab("") + ylab("Species number")+  labs(fill = '') +
  scale_y_continuous(breaks = c(0,n.all*0.25,n.all/2,n.all*0.75,n.all),labels = c(0,25,50,75,100)) +
  xlab("Socioeconomic pathways") + ylab("Percentage (%)")+  labs(fill = '') +
  facet_grid(.~dispersal) +
  theme_bw()+
  theme(text=element_text(size=8),
        axis.text = element_text(size=5),
        legend.key.height = unit(1.5, "line"),
        legend.key.size = unit(1, "line"))
g1
ggsave('results/figure/figure1.png',plot=g1,height = 3,width = 6)

################################################################
#### fig 2 Compare
################################################################
red.list.iucn <- read.csv('data/IUCNcategory/redlist_species_data_407c4094-0f37-41e5-a43e-a10aba3afc28/Sp_redlist.csv')
red.list.iucn$redlistCategory[red.list.iucn$redlistCategory%in%c("Near Threatened","Near Threatened",
                                                                 "Least Concern","Lower Risk/least concern",
                                                                 "Lower Risk/conservation dependent",
                                                                 "Lower Risk/near threatened")] <- "LC" 
red.list.iucn$redlistCategory[red.list.iucn$redlistCategory%in%c("Vulnerable","Near Threatened")] <- "VU" 
red.list.iucn$redlistCategory[red.list.iucn$redlistCategory%in%"Endangered"] <- "EN" 
red.list.iucn$redlistCategory[red.list.iucn$redlistCategory%in%"Critically Endangered"] <- "CR" 
red.list.iucn$redlistCategory[red.list.iucn$redlistCategory%in%"Data Deficient"] <- NA
red.list.iucn$redlistCategory[red.list.iucn$redlistCategory%in%c("Extinct","Extinct in the Wild")] <- "EX"
red.list.iucn$redlistCategory <- factor(red.list.iucn$redlistCategory,levels=c('LC','VU','EN','CR','EX'))
colnames(red.list.iucn) <- c('species','redlistCategory')

# library(rgbif)
# #spKey <- lapply(red.list.iucn$speciesKey, name_suggest)


#########################################################
library(circlize) 

spls <- read.csv('data/spinfo/spls_info.csv')
spls <- spls[,c('species','speciesKey')]
red.list <- red.list %>% left_join(spls) %>% left_join(red.list.iucn) 
red.list$IUCN <- factor(red.list$category,levels=c('LC','VU','EN','CR','EX'))
red.list$IUCN_new <-  ifelse(as.numeric(red.list$redlistCategory)>as.numeric(red.list$IUCN),
                             as.numeric(red.list$redlistCategory),
                             as.numeric(red.list$IUCN))

red.list$IUCN_new <- factor(red.list$IUCN_new,levels = rev(1:5),labels = rev(c(' LC ','VU ','EN ','CR ','EX')))

red.list.cp <- red.list[red.list$group%in%'ssp126_limit',c('IUCN_new','redlistCategory')] %>% na.omit() %>% 
  group_by(IUCN_new,redlistCategory) %>% mutate(n=n()) %>% distinct()
red.list.cp <- red.list.cp[order(red.list.cp$IUCN_new),]
red.list.cp <- red.list.cp[order(red.list.cp$redlistCategory),]
levels(red.list.cp$redlistCategory) <- c('LC','VU','EN','CR',' EX')

mycol <- c("#56BA48","#FFF204","#F89D57","#ED1C2E","darkred");mycol <- c(rev(mycol),mycol)

{
  jpeg('results/figure//figure2.jpg',width=3.6,height=3.6,units = 'in',res=300)
  circos.par(start.degree = 90)
  chordDiagram(red.list.cp,big.gap = 15,
               annotationTrack = c('name','grid'),
               grid.col =  mycol)
  
  circos.track(
    track.index = 1, 
    panel.fun = function(x, y) {
      xlim = get.cell.meta.data("xlim")
      ylim = get.cell.meta.data("ylim")
      sector.name = get.cell.meta.data("sector.index")
      xplot = get.cell.meta.data("xplot")
      by = ifelse(abs(xplot[2] - xplot[1]) > 30, 0.2, 0.5)
      for (p in seq(by, 1, by = by)) {
        circos.text(
          p * (xlim[2] - xlim[1]) + xlim[1],
          mean(ylim)-0.5,
          paste0(p * 100, "%"),
          cex = 0.35,
          adj = c(0.5, 0),
          niceFacing = TRUE
        )
      }
    }, bg.border = NA)
  circos.clear()
  dev.off()
}
