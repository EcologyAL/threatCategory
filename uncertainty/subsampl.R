dir.path <- 'data_modOutput/v4/subSampling/'
set.seed(456)

sp.info <- data.frame()
for (n1 in c('GRID','kNN')) {
  for (n2 in c('75','50','25')) {
    sp.range <- read.csv(paste0(dir.path,n1,'_',n2,'_spInfo.csv')) 
    sp.timespan <- read.csv(paths['spMarkAll'])
    
    sp.range <- sp.range[,c("speciesKey","sn_1981.2010",
      "sn_2011.2040_mean_ssp370_DS_limit",
      "sn_2041.2070_mean_ssp370_DS_limit",
      "sn_2071.2100_mean_ssp370_DS_limit")]

    sp.range[,-(1:2)] <- (sp.range[,2]-sp.range[,-(1:2)])/sp.range[,2]*100
    sp.range <- left_join(sp.range,sp.timespan)
    
    for (i in 3:5) {
      flag4 <- which(sp.range[,i]>=100)
      flag1 <- which(sp.range[,i]<=0)
      sp.range[flag4,i] <- 99.9
      sp.range[flag1,i] <- -0.1
      sp.range[,i] <- cut(sp.range[,i],c(-1,5,30,50,80,100)) %>% as.numeric()
      sp.range[,i] <- factor(sp.range[,i],levels=1:5,labels=c('LC','NT','VU','EN','CR')) %>% as.character()
    }
    
    sp.a3c <- sp.range
    #for (ds in c('DS_unlimit','DS_limit','DS_no')) {
    #for (sn in c('ssp126','ssp370','ssp585')) {
    for (ds in c('DS_limit')) {
      for (sn in c('ssp370')) {
        sp.a3c$tmp <- NA
        for(year in c('2011-2040','2041-2070','2071-2100')){
          flag <- which(sp.a3c$timespan %in% year)
          sp.a3c[flag,'tmp'] <- sp.a3c[flag,paste0('sn_',str_replace(year,'-','.'),'_mean_',sn,'_',ds)]
        }
        colnames(sp.a3c)[colnames(sp.a3c)%in%'tmp'] <- paste0(sn,'_',ds)
        print(paste0(sn,'_',ds))
      }
    }
    cols <- c()
    for (ds in c('DS_limit')) {
      for (sn in c('ssp370')) {
        cols <- c(cols,paste0(sn,'_',ds))
      }
    }
    sp.a3c <- sp.a3c[,c('speciesKey',cols)]
    
    # merge extinction species list -------------------------------------------

    sp.ex <- read.csv(paste0(dir.path,n1,'_',n2,'_spInfo.csv')) 
    sp.ex <- sp.ex[,c("speciesKey","forEX_sn_1981.2010",
                      "forEX_sn_2011.2040_mean_ssp370",
                      "forEX_sn_2041.2070_mean_ssp370",
                      "forEX_sn_2071.2100_mean_ssp370")] 
    for (i in 3:5) {
      sp.ex[,i] <- (sp.ex[,i]==0)&(sp.ex[,2]!=0)
    }
    sp.a3c <- left_join(sp.a3c,sp.timespan[,c('speciesKey','timespan')])
    
    for (ds in c('DS_limit')) {
      for (sn in c('ssp370')) {
        for(year in c('2011-2040','2041-2070','2071-2100')){
          col <- paste0('sn_',str_replace(year,'-','.'),'_mean_',sn)
          
          flag <- which( (sp.a3c$timespan %in% year) &
                           (!is.na(sp.a3c[,paste0(sn,'_',ds)])) &
                           (sp.a3c$speciesKey%in%sp.ex$speciesKey[sp.ex[,str_replace(col,'sn_','forEX_sn_')]]) )
          sp.a3c[flag,paste0(sn,'_',ds)] <- 'EX'
        }
        print(paste0(sn,'_',ds))
      }
    }
    
    sp.a3c <- sp.a3c[,1:2]
    colnames(sp.a3c)[2] <- paste(n1,n2,sep='_')
    if(nrow(sp.info)>0){
      sp.info <- left_join(sp.info,sp.a3c)
    }else{
      sp.info <- sp.a3c
    }
  }
}

sp.info <- sp.info[sample(nrow(sp.info),2e4),]
cell.info <- data.frame()
for (n1 in c('GRID','kNN')) {
  for (n2 in c('75','50','25')) {
    sp.ds.1d.raw <- sp.ds.1d <- read.csv(paths["spds1d"])
    #sp.ds.1d.raw <- sp.ds.1d <- read.csv(paste0(dir.path,n1,'_',n2,'_spRange.csv'))
    
    red.list <- sp.info
    red.list$IUCN <- red.list[[paste0(n1,'_',n2)]]
    
    sp.ds.1d <- left_join(sp.ds.1d.raw,red.list[,c('speciesKey','IUCN')])
    sp.ds.1d <- sp.ds.1d[!is.na(sp.ds.1d$IUCN),]
    sp.ds.1d$IUCN <- factor(sp.ds.1d$IUCN,levels=c('LC','NT','VU','EN','CR','EX')) %>% as.numeric()
    sp.ds.1d$wt <- 1-(sp.ds.1d$IUCN-1)/max(sp.ds.1d$IUCN-1,na.rm=T)
    rli <-  sp.ds.1d %>% group_by(cell_poll) %>% mutate(RLI=mean(wt,na.rm=T),SR=n()) %>% ungroup() %>%
      dplyr::select(cell_poll,RLI,SR) %>% distinct()
    rli <- rli[rli$SR>10,]
    rli <- rli[,c('cell_poll','RLI')]
    
    colnames(rli)[c(1,2)] <- c('cell_poll',paste(n1,n2,sep='_'))
  
    if(nrow(cell.info)>0){
      cell.info <- left_join(cell.info,rli)
    }else{
      cell.info <- rli
    }
  }
}

#
sp.ds.1d.raw <- sp.ds.1d <- read.csv(paths["spds1d"])
red.list <- read.csv(paths["A3c"])
red.list$IUCN <- red.list[["ssp370_DS_limit"]]
red.list <- red.list[red.list$speciesKey%in%sp.info$speciesKey,]

sp.ds.1d <- left_join(sp.ds.1d.raw,red.list[,c('speciesKey','IUCN')])
sp.ds.1d <- sp.ds.1d[!is.na(sp.ds.1d$IUCN),]
sp.ds.1d$IUCN <- factor(sp.ds.1d$IUCN,levels=c('LC','NT','VU','EN','CR','EX')) %>% as.numeric()
sp.ds.1d$wt <- 1-(sp.ds.1d$IUCN-1)/max(sp.ds.1d$IUCN-1,na.rm=T)
rli <-  sp.ds.1d %>% group_by(cell_poll) %>% mutate(RLI=mean(wt,na.rm=T),SR=n()) %>% ungroup() %>%
  dplyr::select(cell_poll,RLI,SR) %>% distinct()
rli <- rli[rli$SR>10,]
rli <- rli[,c('cell_poll','RLI')]
cell.info <- left_join(rli,cell.info)

colnames(cell.info)
for(i in 3:ncol(cell.info)){
  m <- lm(cell.info[[i]]~cell.info[[2]]) %>% summary()
  print(m$r.squared)
}

# mapping -----------------------------------------------------------------
mypalette <- colorRampPalette(c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred"))
mycol <- rev(mypalette(100))
vals <- seq(0.4, 1, length.out = length(mycol))

shp <- read_sf(paths['baseMap'])
shp$land <- shp$land%in%c(1,'T','TRUE')
shp <- shp[,c('cell_poll','land')]
shp <- left_join(shp,cell.info)
#shp$RLI[shp$SR<10] <- NA
shp$RLI[!shp$land] <- NA
colnames(shp)

colnames(shp)[4] <- 'Full'
p.list <- list()
for(i in c('Full',
           "GRID_75","GRID_50","GRID_25",
           "kNN_75","kNN_50","kNN_25")){
  shp$value <- shp[[i]]
  p <- ggplot(shp) +    
    geom_sf(aes(fill = value,colour=value)) +  
    scale_fill_gradientn(colors = mycol,
                         values = scales::rescale(vals, from = c(0.4, 1)),
                         limits = c(0.4, 1),
                         oob = scales::squish,
                         na.value = "gray95",
                         name = ""
    ) +
    scale_colour_gradientn(colors = mycol,
                           na.value = 'gray95',
                           values = scales::rescale(vals, from = c(0.4, 1)),
                           limits = c(0.4, 1),
                           oob = scales::squish,
                           guide='none') +
    scale_x_continuous(limits = c(-170,170)) +
    labs(title=i)+
    theme_void()+
    theme_bw() + theme(panel.grid=element_blank(),
                       legend.position = 'bottom',
                       legend.direction = 'horizontal',
                       axis.ticks.x = element_blank(),
                       axis.text.x = element_blank(),
                       legend.key.height = unit(0.1, "in"),
                       legend.key.width = unit(0.3, "in"))
  
  # p <- ggplot(shp) +    
  #   geom_sf(aes(fill = value),color='transparent') +  
  #   scale_fill_gradientn(colors = mycol,
  #                        name=i
  #                        ,
  #                        na.value = 'gray95') +
  #   scale_x_continuous(limits = c(-170,170)) +
  #   scale_y_continuous(limits = c(-108,80)) +
  #   theme_bw() + 
  #   theme(panel.grid=element_blank(),
  #         legend.position = c(0.5,0.125),
  #         legend.title = element_text(size=5),
  #         legend.text =  element_text(size=5),
  #         axis.text.x = element_blank(),
  #         axis.text.y = element_blank(),
  #         axis.ticks = element_blank(),
  #         legend.direction = 'horizontal',
  #         legend.key.height = unit(0.05, "in"),
  #         legend.key.width = unit(0.3, "in")) +
  #   labs(title='(a)')
  p.list <- c(p.list,list(p))
}
p0 <- ggplot()+theme_void()
p <- p.list[[1]] + p.list[[2]] + p.list[[3]] + 
  p.list[[4]]  + p0 + p.list[[5]] + p.list[[6]] +
  p.list[[7]] + 
  plot_layout(ncol=2,byrow = F)
ggsave(paths["sfig_subsumpl_map"],plot=p,width = 6,height = 10)

# compare species 
red.list <- read.csv(paths["A3c"])
red.list$Full <- red.list[["ssp370_DS_limit"]]
red.list <- red.list[red.list$speciesKey%in%sp.info$speciesKey,]
sp.info.all <- left_join(red.list[,c('speciesKey','Full')],sp.info) 

for (i in 2:ncol(sp.info.all)) {
  sp.info.all[,i] <- sp.info.all[,i] %in% c('VU','EN','CR','EX')
}

library(caret)
y_true<-sp.info.all$Full %>% as.numeric()
subsampl <- data.frame()
for (i in 3:ncol(sp.info.all)) {
  y_pred<-sp.info.all[,i] %>% as.numeric()
  df <- data.frame(
    y_true = y_true,   
    y_pred = y_pred
  )
  
  cm <- confusionMatrix(
    factor(df$y_pred),
    factor(df$y_true),
    positive = "1"
  )
  
  metrics <- data.frame(
    Strategy = colnames(sp.info.all)[i],
    Accuracy     = unname(cm$overall["Accuracy"]),
    Kappa        = unname(cm$overall["Kappa"]),
    Sensitivity  = unname(cm$byClass["Sensitivity"]),
    Specificity  = unname(cm$byClass["Specificity"]),
    Precision    = unname(cm$byClass["Precision"]),
    Recall       = unname(cm$byClass["Recall"]),
    F1           = unname(cm$byClass["F1"]),
    BalancedAcc  = unname(cm$byClass["Balanced Accuracy"]),
    TSS          = unname(cm$byClass["Sensitivity"] +
                            cm$byClass["Specificity"] - 1)
  )
  subsampl <- rbind(subsampl,metrics)
}
subsampl[,-1] <- round(subsampl[,-1],2)
subsampl[,c('Strategy','Accuracy','Precision','Recall','F1')]
write.csv(subsampl,str_replace(paths["sfig_subsumpl_map"],'jpg','csv'),row.names = F)
