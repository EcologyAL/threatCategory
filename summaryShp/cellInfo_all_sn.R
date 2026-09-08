red.list <- read.csv(paths["spAssAll"])
sp.ds.1d.raw <- sp.ds.1d <- read.csv(paths['spds1d'])
cellInfo <- data.frame(cell_poll=unique(sp.ds.1d.raw$cell_poll))

for (sn in c("ssp126_limit","ssp370_limit","ssp585_limit",
             "ssp126_unlimit","ssp370_unlimit","ssp585_unlimit",
             "ssp126_no","ssp370_no","ssp585_no")) {
  red.list$category <- red.list[[sn]]
  sp.ds.1d <- left_join(sp.ds.1d.raw,red.list[,c('speciesKey','category')])
  sp.ds.1d <- sp.ds.1d[!is.na(sp.ds.1d$category),]
  sp.ds.1d$category <- factor(sp.ds.1d$category,levels=c('LC','NT','VU','EN','CR','EX')) %>% as.numeric()
  sp.ds.1d$wt <- 1-(sp.ds.1d$category-1)/5
  rli <-  sp.ds.1d %>% group_by(cell_poll) %>% 
    mutate(RLI=mean(wt,na.rm=T),
           SR=n()) %>% 
    ungroup() %>%
    dplyr::select(cell_poll,RLI) %>% distinct() %>% as.data.frame()
  colnames(rli) <- c('cell_poll',sn)
  
  cellInfo <- left_join(cellInfo,rli) 
}
write.csv(cellInfo,paths['cellInfo_all_sn'],row.names = F)