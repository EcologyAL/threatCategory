pa <- foreign::read.dbf(paths["PAs.dbf"])
pa$PAs <- pa$PAs30%in%c('T','TRUE','1')

sp.ds.1d <- read.csv(paths['spds1d'])
sp.ds.1d <- sp.ds.1d %>% left_join(pa[,c('cell_poll','PAs')]) %>% na.omit()

sp.pa <- sp.ds.1d %>% group_by(speciesKey) %>% 
  reframe(PAsR=sum(PAs,na.rm=T)/n(),
          N=n()) %>% as.data.frame()
sp.pa[is.na(sp.pa$PAsR),]

# fun.thr <- function(x) {
#   tmp <- 1 - 0.85 * (log(x) / log(25))
#   tmp[tmp<0.15] <- 0.15
#   return(tmp)
#   }
# sp.pa$thr <- fun.thr(sp.pa$N)
# sp.pa$Protected <- sp.pa$PAsR >= sp.pa$thr
# 
# write.csv(sp.pa,paths["spPA"],row.names = F)

log_linear_interpolate <- function(x_interp, y0=0.15, y1=1,  x0=25,  x1=1) {
  log_y0 <- log(y0)
  log_y1 <- log(y1)
  
  log_y_interp <- log_y0 + (log_y1 - log_y0) * (x_interp - x0) / (x1 - x0)
  y_interp <- exp(log_y_interp)
  return(y_interp)
}
sp.pa$thr <- log_linear_interpolate(sp.pa$N)
sp.pa$thr[sp.pa$N>25] <- 0.15
sp.pa$Protected <- sp.pa$PAsR>=sp.pa$thr
write.csv(sp.pa,paths["spPA"],row.names = F)