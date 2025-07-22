al_create_envstrat <- function(env.only.df,
                               pred_sdm,
                            poolsiz=5*10^6,
                            sampsiz=400000,
                            type){
  
  recli=list()
  # loop over environmental preds
  for(i in 1:length(pred_sdm)){
    
    # select preds
    rsti <- env.only.df[,pred_sdm[i]]
    
    # take a sample to check if raster has less than 10 levels 
    smp=sample(rsti,1000)
    
    if(length(unique(smp))<10){
      
      warning(paste("Less than 10 unique values found for layer",names(env.stk)[i],"no reclassification applied.."))
      values(rsti)=as.numeric(as.factor(values(rsti)))
      recli[[i]]=rsti
      
    } else {
      
      # Define five equidistant bins
      strt_i=seq(from=min(rsti),
                 to=max(rsti),
                 length.out=6)
      
      # Reclassify raster according to these bins
      #rclm=matrix(c(strt_i[1:5],strt_i[2:6],1:5*10^(i-1)),ncol=3)
      #recli[[i]]=reclassify(rsti,rclm,include.lowest=TRUE)
      recli[[i]]=cut(rsti,strt_i,include.lowest=TRUE) %>% as.numeric()
      recli[[i]] <- recli[[i]] * 10^i
    }
    
    
    cat(paste0("reclassified preds ",i,"...\n"))
  }
  
  # create one raster layer representing combined bins
  recl.m=do.call('cbind',recli)
  env.only.df <- dplyr::mutate(env.only.df,recl=round(rowSums(recl.m)))
  
  # Create ca 400000 stratfied point samples
  # if(rAll){
  #   sptz=as(recl,"SpatialPixelsDataFrame")
  #   poolsiz=nrow(sptz)
  # } else {
  #   sptz=sampleRandom(recl,poolsiz,sp=TRUE)
  # }
  if(poolsiz<nrow(env.only.df)){
    env.only.df <- env.only.df[sample(1:nrow(env.only.df),poolsiz),]
  }
  
  strpts=al_stratify(env.only.df,type,sampsiz)
  
  return(strpts)
}
