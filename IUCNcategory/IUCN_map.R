library(raster);library(ggplot2);library(stringr);library(sf);library(dplyr)#;library(patchwork)
setwd('F:/luoao/RLI/')
rstland <- raster('E:/LuoA/PRIME/data/Map_land/landboundary.tif')
load('data/spcell_1d/sp_ds_1d.rdata')
spIUCN <- read.csv('data/IUCNcategory/sp_iucn.csv')

sr <- sp.ds.1d %>% filter(sn_1981.2010>0) %>% dplyr::select(cell_poll,speciesKey) %>% 
  distinct() %>% group_by(cell_poll) %>% summarise(sr=n()) %>% as.data.frame()

RLI.tmp <- sp.ds.1d %>% filter(sn_1981.2010>0) %>% dplyr::select(cell_poll,speciesKey) %>% 
  left_join(spIUCN) %>% distinct() 
RLI.tmp$wt <- 1-(RLI.tmp$IUCN-1)/4
RLI <- RLI.tmp %>% 
  group_by(cell_poll) %>% mutate(sr=n(),RLI=mean(wt,na.rm=T)) %>% 
  ungroup() %>% dplyr::select(cell_poll,sr,RLI) %>% distinct()
rst <- raster()  
rst[RLI$cell_poll] <- RLI$RLI
rst[is.na(rstland)] <- NA
rst.rli <- rst
plot(rst.rli);title("RLI (ALL)")

RLI.tmp$wt <- 1-(RLI.tmp$IUCN_2100-1)/4
RLI <- RLI.tmp %>% 
  group_by(cell_poll) %>% mutate(sr=n(),RLI=mean(wt,na.rm=T)) %>% 
  ungroup() %>% dplyr::select(cell_poll,sr,RLI) %>% distinct()
rst <- raster()  
rst[RLI$cell_poll] <- RLI$RLI
rst[is.na(rstland)] <- NA
rst.rli <- rst
plot(rst.rli);title("RLI (2071-2100)")

RLI.tmp$wt <- 1-(RLI.tmp$A3c-1)/4
RLI <- RLI.tmp %>% 
  group_by(cell_poll) %>% mutate(sr=n(),RLI=mean(wt,na.rm=T)) %>% 
  ungroup() %>% dplyr::select(cell_poll,sr,RLI) %>% distinct()
rst <- raster()  
rst[RLI$cell_poll] <- RLI$RLI
rst[is.na(rstland)] <- NA
rst.rli <- rst
plot(rst.rli);title("RLI (Only A3c)")

RLI.tmp$wt <- 1-(RLI.tmp$B1abiii-1)/4
RLI <- RLI.tmp %>% 
  group_by(cell_poll) %>% mutate(sr=n(),RLI=mean(wt,na.rm=T)) %>% 
  ungroup() %>% dplyr::select(cell_poll,sr,RLI) %>% distinct()
rst <- raster()  
rst[RLI$cell_poll] <- RLI$RLI
rst[is.na(rstland)] <- NA
rst.rli <- rst
plot(rst.rli);title("RLI (Only B1abiii)")

# rst <- raster()  
# rst[RLI$cell_poll] <- RLI$sr
# rst[is.na(rstland[])] <- NA
# rst.sr <- rst
# plot(rst.sr);title("Species Richness")

##########################################################################################
#### take care of lifespan
##########################################################################################
sapply(c('dplyr','sf','stringr','raster','graphics','funspace'), require,character.only=T)
rst <- raster()

spinfo <- read.csv('data/IUCNcategory/sp_iucn.csv')
spinfo$wt <- spinfo$IUCN-1
#spinfo$wt <- spinfo$IUCN_2100-1
spinfo$wt <- (4-spinfo$wt)/4

tmp <- read.csv('data/spinfo/spls_mark_all.csv')
spinfo <- left_join(spinfo,tmp[,c('family','species','speciesKey')])

load('data/spcell_1d/sp_ds_1d.rdata')
spc <- sp.ds.1d %>% rename(cell=cell_poll) %>% filter(sn_1981.2010) %>% dplyr::select(speciesKey,cell) %>% left_join(spinfo[,c('speciesKey','wt')]) %>% na.omit() %>% distinct()
# rm(sp.ds.1d)
# mapdata <- spc %>% group_by(cell) %>% mutate(RLI=mean(wt),SR=n()) %>% ungroup() %>% 
#   dplyr::select(cell,SR,RLI) %>% distinct()
# mapdata <- mapdata[mapdata$SR>10,]
# 
# rstland <- raster('data/map/Map_land/landboundary.tif')
# RLI <- rst
# RLI[is.na(rstland)] <- NA
# RLI[mapdata$cell] <- mapdata$RLI
# plot(RLI)

####1_3
# fadata <- spinfo %>% group_by(family) %>% 
#   mutate(RLI=mean(wt),SR=sum(wt<=1)) %>% ungroup() %>% 
#   dplyr::select(family,SR,RLI) %>% distinct()
# fadata <- fadata[fadata$SR>10,]
# fadata <- fadata[order(fadata$SR,decreasing = T),]
# fadata <- fadata[1:20,]

# fadata2 <- spinfo %>% group_by(family) %>% 
#   mutate(RLI=mean(wt),SR=sum(wt<=1)) %>% ungroup() %>% 
#   dplyr::select(family,SR,RLI) %>% distinct()
# 
# fadata$family <- factor(fadata$family,fadata$family)
# yylim <- range(c(fadata$RLI_min,fadata$RLI_max))
# 
# plot(fadata$family, fadata$RLI_min, las = 2,ylim=yylim)
# plot(fadata$family, fadata$RLI_max, las = 2, add=T)
# for (i in 1:20) lines(rep(fadata$family[i],2),c(fadata$RLI_min[i],fadata$RLI_max[i]))
# abline(a=fadata2$RLI_max,b=0,lty=3)
# abline(a=fadata2$RLI_min,b=0,lty=3)

#### 2_1
# funs <- read.csv('data/spInfo/funcTre/Trait data/trait1.csv')
# for (i in 2:7) funs[,i] <- scale(funs[,i])  

funs <- read.csv('data/spInfo/funcTre/Trait data/Traits_A_dis.csv')
funs <- funs[,c(1,9,11:15)]

colnames(funs) <- c('species','Height','SM','LC','LN','LP','SLA')
#funs$species <- str_replace_all(funs$species,' ','_')
funs.p <- princomp(funs[,-1])
x <- funs.p.sp <- funspace(funs.p)

nseq <- 100

mn <- range(funs.p$scores[,1])[1];mx <- range(funs.p$scores[,2])[2]+1
xx <- c(mn,mn + (1:nseq)*(mx-mn)/nseq)
mn <- range(funs.p$scores[,1])[1]+1;mx <- range(funs.p$scores[,2])[2]
yy <- c(mn,mn + (1:nseq)*(mx-mn)/nseq)

sp.fun <- data.frame(species = funs$species,
                     x = funs.p$scores[,1],
                     y = funs.p$scores[,2])

im <- matrix(NA,nrow=nseq,ncol=nseq)
for (xi in 1:nseq) {
  print(xi)
  for (yi in 1:nseq) {
    flag <- between(sp.fun$x,xx[xi],xx[xi+1])&between(sp.fun$y,yy[yi],yy[yi+1])
    #if(sum(flag)==0) next
    sp <- sp.fun$species[flag]
    flag <- spinfo$species%in%sp
    
    if(sum(flag)<10) next
    #print(sum(flag))
    im[xi,yi] <- mean(spinfo$wt[flag],na.rm=T)
  }
}

arrows.head <- 0.08; arrows.length = 0.8;arrows.col = "black";
arrows.label.col = "black";arrows.label.pos = 1.2;arrows.label.cex = 1

# colors <- c("lightyellow", "red"); ncolors = 8;
# gradientColors <- grDevices::colorRampPalette(colors, space = "Lab")
# ColorRamp <- rev(gradientColors(ncolors))
# ColorRamp_ex <- ColorRamp
ColorRamp_ex <- heat.colors(6) %>% rev(); ncolors = 6;

z_range <- range(im,na.rm=T)  
z_ticks <- seq(from = z_range[1], to = z_range[2], length.out = ncolors) 

graphics::image(x = xx[-1],
                y = yy[-1],
                z = im,
                col = ColorRamp_ex, xaxs="r", yaxs="r", xlab = "PC1(46.69%)", ylab = "PC(26.22%)",
                axes=F, main = "")
graphics::box(which="plot")
graphics::axis(1, tcl=0.3, lwd=0.8)
graphics::axis(2, las=1, tcl=0.3, lwd=0.8)
graphics::arrows(x0 = 0, y0 = 0,
                 x1 = x$PCAInfo$fit[[1]][, 1] * arrows.length,
                 y1 = x$PCAInfo$fit[[1]][, 2] * arrows.length,
                 length = arrows.head, col = arrows.col)
graphics::text(x = x$PCAInfo$fit[[1]][, 1] * arrows.length * arrows.label.pos,
               y = x$PCAInfo$fit[[1]][, 2] * arrows.length * arrows.label.pos,
               labels = rownames(x$PCAInfo$fit[[1]]),
               col = arrows.label.col, cex = arrows.label.cex)
graphics::legend("topright", legend = round(z_ticks, 2), fill = ColorRamp_ex,  
                 title = "RLI", bty = "n")

#### 2_2
nseq <- 50

#load('data/spds/spc.rdata')
spc.mark <- spc %>% left_join(spinfo[,c('speciesKey','wt')]) %>% na.omit()

idx <- read.csv('data/index/vars1degree.csv')
idx <- idx[,c('cell','BIO1','BIO12')]

tmp <- raster('data/index/future/CHELSA_bio1_2071-2100_mean_ssp585.tif')
idx$BIO1_F <- extract(tmp,coordinates(raster()))
tmp <- raster('data/index/future/CHELSA_bio12_2071-2100_mean_ssp585.tif')
idx$BIO12_F <- extract(tmp,coordinates(raster()))

tmp <- raster('data/index/future/CHELSA_bio1_anomaly_2071-2100_mean_ssp585.tif')
idx$BIO1A <- extract(tmp,coordinates(raster()))
tmp <- raster('data/index/future/CHELSA_bio12_anomaly_2071-2100_mean_ssp585.tif')
idx$BIO12A <- extract(tmp,coordinates(raster()))

idx <- idx %>% na.omit()
spc.mark <- left_join(spc.mark,idx)

mn <- range(spc.mark$BIO1,na.rm=T)[1];mx <- range(spc.mark$BIO1,na.rm=T)[2]
xx <- c(mn,mn + (1:nseq)*(mx-mn)/nseq)
mn <- range(spc.mark$BIO12,na.rm=T)[1];mx <- range(spc.mark$BIO12,na.rm=T)[2]
yy <- c(mn,mn + (1:nseq)*(mx-mn)/nseq)

sp.fun <- data.frame(speciesKey = spc.mark$speciesKey,
                     x = spc.mark$BIO1,
                     y = spc.mark$BIO12) %>% na.omit()

im <- matrix(NA,nrow=nseq,ncol=nseq)
for (xi in 1:nseq) {
  print(xi)
  for (yi in 1:nseq) {
    flag <- between(sp.fun$x,xx[xi],xx[xi+1])&between(sp.fun$y,yy[yi],yy[yi+1])
    #if(sum(flag)==0) next
    sp <- sp.fun$speciesKey[flag]
    #print(length(sp))
    flag <- spinfo$speciesKey%in%sp
    
    if(sum(flag)<100) next
    #print(sum(flag))
    im[xi,yi] <- mean(spinfo$wt[flag],na.rm=T)
  }
}

colors <- c("lightyellow", "red"); ncolors = 5;
arrows.head <- 0.08; arrows.length = 0.8;arrows.col = "black";
arrows.label.col = "black";arrows.label.pos = 1.2;arrows.label.cex = 1

gradientColors <- grDevices::colorRampPalette(colors, space = "Lab")
ColorRamp <- rev(gradientColors(ncolors))
ColorRamp_ex <- ColorRamp

z_range <- range(im,na.rm=T)  
z_ticks <- seq(from = z_range[1], to = z_range[2], length.out = ncolors) 

graphics::image(x = xx[-1],
                y = yy[-1],
                z = im,
                xlim =c(min(xx),max(xx)),
                col = ColorRamp_ex, xaxs="r", yaxs="r", xlab = "bio1", ylab = "bio12",
                axes=F, main = "")
graphics::box(which="plot")
graphics::axis(1, tcl=0.3, lwd=0.8)
graphics::axis(2, las=1, tcl=0.3, lwd=0.8)
graphics::legend("topleft", legend = round(z_ticks, 2), fill = ColorRamp_ex,  
                 title = "RLI", bty = "n")

#### 2_3
#load('data/spds/spc.rdata')
spc.mark <- spc %>% left_join(spinfo[,c('speciesKey','wt')]) %>% na.omit()

idx <- read.csv('data/index/vars1degree.csv')
idx <- idx[,c('cell','BIO1','BIO12')]

tmp <- raster('data/index/future/CHELSA_bio1_2071-2100_mean_ssp585.tif')
idx$BIO1_F <- extract(tmp,coordinates(raster()))
tmp <- raster('data/index/future/CHELSA_bio12_2071-2100_mean_ssp585.tif')
idx$BIO12_F <- extract(tmp,coordinates(raster()))

#tmp <- raster('data/index/future/CHELSA_bio1_anomaly_2071-2100_mean_ssp585.tif')
#idx$BIO1A <- extract(tmp,coordinates(raster()))
#tmp <- raster('data/index/future/CHELSA_bio12_anomaly_2071-2100_mean_ssp585.tif')
#idx$BIO12A <- extract(tmp,coordinates(raster()))
idx$BIO1A <- idx$BIO1_F - idx$BIO1
idx$BIO12A <- idx$BIO12_F - idx$BIO12
idx$BIO12A <- idx$BIO12A/idx$BIO12

idx <- idx %>% na.omit()
spc.mark <- left_join(spc.mark,idx)

mn <- range(spc.mark$BIO1A,na.rm=T)[1];mx <- range(spc.mark$BIO1A,na.rm=T)[2]
xx <- c(mn,mn + (1:nseq)*(mx-mn)/nseq)
mn <- range(spc.mark$BIO12A,na.rm=T)[1];mx <- range(spc.mark$BIO12A,na.rm=T)[2]
yy <- c(mn,mn + (1:nseq)*(mx-mn)/nseq)

sp.fun <- data.frame(speciesKey = spc.mark$speciesKey,
                     x = spc.mark$BIO1A,
                     y = spc.mark$BIO12A) %>% na.omit()

im <- matrix(NA,nrow=nseq,ncol=nseq)
for (xi in 1:nseq) {
  print(xi)
  for (yi in 1:nseq) {
    flag <- between(sp.fun$x,xx[xi],xx[xi+1])&between(sp.fun$y,yy[yi],yy[yi+1])
    #if(sum(flag)==0) next
    sp <- sp.fun$speciesKey[flag]
    flag <- spinfo$speciesKey%in%sp
    
    if(sum(flag)<10) next
    #print(sum(flag))
    im[xi,yi] <- mean(spinfo$wt[flag],na.rm=T)
  }
}

colors <- c("lightyellow", "red"); ncolors = 5;
arrows.head <- 0.08; arrows.length = 0.8;arrows.col = "black";
arrows.label.col = "black";arrows.label.pos = 1.2;arrows.label.cex = 1

gradientColors <- grDevices::colorRampPalette(colors, space = "Lab")
ColorRamp <- rev(gradientColors(ncolors))
ColorRamp_ex <- ColorRamp

z_range <- range(im,na.rm=T)  
z_ticks <- seq(from = z_range[1], to = z_range[2], length.out = ncolors) 

graphics::image(x = xx[-1],
                y = yy[-1],
                z = im,
                xlim =c(min(xx),max(xx)),
                col = ColorRamp_ex, xaxs="r", yaxs="r", xlab = "bio1_anomaly", ylab = "bio12_anomaly",
                axes=F, main = "")
graphics::box(which="plot")
graphics::axis(1, tcl=0.3, lwd=0.8)
graphics::axis(2, las=1, tcl=0.3, lwd=0.8)
graphics::legend("bottomright", legend = round(z_ticks, 2), fill = ColorRamp_ex,  
                 title = "RLI", bty = "n")

