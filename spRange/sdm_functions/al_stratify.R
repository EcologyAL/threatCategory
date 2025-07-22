al_stratify <- function(spp.df,type,sampsiz){
  
  # Identify number of points per stratum
  spp.df= spp.df[sample(1:nrow(spp.df)),]
  
  tb=table(spp.df$recl)
  stb=sort(tb)
  cms=cumsum(stb)
  
  rat=sampsiz/sum(log(tb))

  tarsiz=optimize(function(x,sz,tb){
    vls=cbind(log(tb)*x,tb)
    sm=sum(apply(vls,1,min))
    return(abs(sz-sm))
  },tb=tb,sz=sampsiz,interval=c(rat,5*rat))
  
  gregr=aggregate(spp.df[,c('x','y')],
                  by=list(lay=spp.df$recl),function(x,y){
                    out=x[1:min(ceiling(log(length(x))*y),length(x))]
                    return(out)
                  },y=ceiling(tarsiz$minimum))
  
  smpsiz=sapply(gregr[,2],function(x){
    length(x)
  })
  
  # if there is only one sample per layer just take as is
  
  if(is.numeric(gregr[,2])){
    colnames(gregr) <- c('recl','x','y')
    #bgstrt2 <- st_as_sf(gregr,coords = c('x','y'))
  } else {
    # otherwise reformat
    bgstrt2 <- cbind(do.call("c",gregr[,2]),do.call("c",gregr[,3]),rep(gregr$lay,smpsiz)) %>% as.data.frame()
    colnames(bgstrt2) <- c('x','y','recl')
    #bgstrt2 <- st_as_sf(bgstrt2,coords = c('x','y'))
  }
  return(bgstrt2)
}
