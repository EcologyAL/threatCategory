wsl.samplePseuAbs.geo.strat <- function(n=10000,
                             rst,
                             env,
                             pred_sdm,
                             pres,
                             type="geo.strat",
                             add.strat=0,
                             #pres=numeric(),
                             taxon=character(),
                             geodist_fact=1,
                             geores_fact=20,
                             template_dir=NA,
                             geo_nrep=7,
                             target.group_dir=NA,
                             env.strat_path=NA,
                             rAll=TRUE,
                             force_spat_thin="no",
                             limdist=NA,
                             set_max_npres_to_nabs=TRUE){
  
  ### ------------------------
  ### Check input and prepare
  ### ------------------------
  
  if(add.strat<0 | add.strat>1){
    stop("add.strat represents the fraction of pseudoabsences that are
         sampled environmentally stratified and should be between 0 and 1!")
  }
  
  possibtype=c("geo.strat")
  if(length(type)!=1 | !(type%in%possibtype)){
    stop("Invalid specification of pseudoabsence sampling type!")
  }
  
  possibthin=c("no","presences","absences","both")
  if(length(type)!=1 | !(force_spat_thin%in%possibthin)){
    stop("Invalid specification of spatial thinning method!")
  }
  
  ### ------------------------
  ### generate wsl.pseudoabsences object and add meta info
  ### ------------------------
  
  out<-preva.meta(type="pseudoabsence")
  
  tpnam=type
  if(tpnam=="geographic"){
    tpnam=paste0(tpnam,"_w",geodist_fact)
  }
  if(add.strat>0 && !grepl("env",type)){
    tpnam=paste0(tpnam,"_x_",add.strat,"env_strata")
  }
  
  out@meta$type=tpnam
  out@meta$taxon=taxon
  out@meta$force_spat_thin=force_spat_thin
  out@meta$template_file=NA
  
  call=match.call()
  out@call<-call
  
  ### ------------------------
  ### Prepare templat | data.frame don't need it.
  ### ------------------------

  rst[] <- NA
  rst[env$cell] <- env$cell
  
  ### ------------------------
  ### raster::raster::extract and refine presences
  ### ------------------------
  
  if(n=="auto"){
    if(n=="auto"){
      if( ( (0.8*sum(!is.na(rst[]))) > max(1000,10*nrow(pres)))|((0.8*sum(!is.na(rst[])))>50000) ){
        n=ifelse(nrow(pres)<100,1000,ifelse(nrow(pres)>=100 & nrow(pres)<5000,10*nrow(pres),50000))
      }else{
        n=0.8*sum(!is.na(rst[]))
      }
    }
  }
  
  ### ------------------------
  ### Do the geographic sampling
  ### ------------------------
  
  if (type=="geo.strat"){
    
    if(grepl("longlat",crs(rst))){
      
      # sample regularly on the surface of a sphere
      fglb=(extent(rst)@xmax-extent(rst)@xmin)*(extent(rst)@ymax-extent(rst)@ymin)/(360*180)
      N=round(1.1*n*(1-add.strat)*length(values(rst))/length(which(!is.na(values(rst))))/fglb)
      r=1
      Nc=0
      a=4*pi*r^2/N
      d=sqrt(a)
      Mx=round(pi/d)
      dx=pi/Mx
      dy=a/dx
      pts=matrix(NA,ncol=3,nrow=N*5)
      
      for(m in 0:(Mx-1)){
        xx=pi*(m+0.5)/Mx
        My=round(2*pi*sin(xx)/dy)
        for(nn in 0:(My-1)){
          yy=2*pi*nn/My
          pts[Nc+1,]=c(r*sin(xx)*cos(yy),r*sin(xx)*sin(yy),r*cos(xx))
          Nc=Nc+1
        }
      }
      pts=na.omit(pts)
      
      # Transform to longitude latitude
      lat=atan(sqrt(pts[,2]^2+pts[,1]^2)/pts[,3])
      lat=ifelse(lat<0,lat+max(lat,na.rm = TRUE),lat-max(lat,na.rm=TRUE))
      lon=atan(pts[,2]/pts[,1])
      regpt=na.omit(unique(cbind(lon,lat)))
      regpt[,1]=regpt[,1]/pi*360
      regpt[,2]=regpt[,2]/pi*180
      
      sppt=raster::extract(rst,regpt) %>% unique()
      
    } else {
      stop('Stop! Not longlat.')
    }
    
    # Preprare output
    sp_abs = env[env$cell%in%sppt,]
    sp_abs = sp_abs[!sp_abs$cell%in%pres[,'cell'],]
  } 
  
  if(exists("sp_abs")){
    # Subsample and remove NAs
    nosna=apply(sp_abs,1,function(x){
      all(!is.na(x))
    })
    
    sp_abs=sp_abs[which(nosna),]
    if(nrow(sp_abs)>round(n*(1-add.strat))){
      sp_abs=sp_abs[sample(1:nrow(sp_abs),round(n*(1-add.strat))),]
    }
    
  }
  
  ### ------------------------
  ### Do the env.strat sampling
  ### ------------------------
  
  if (grepl("env",type) | add.strat>0){
    
    # define env strat sampling strategy
    if(add.strat<1){
      tyyp="env.strat"
    } else {
      tyyp=type
    }
    
    # if no strata directory is supplied, just
    # calculate from scratch
    {
      strpts = al_create_envstrat(env.only.df=env,
                               pred_sdm=pred_sdm,
                               type=tyyp)
    }
    
    strfy=al_stratify(strpts,tyyp,n*add.strat*1.1)
    
    # if(force_spat_thin%in%c("absences","both")){
    #   
    #   strfy=upsample_thin(strpts,limdist,n*add.strat*1.1)
    #   
    # } else {
    #   if(type=="env.semi.strat"){
    #     strfy=strpts[sample(1:nrow(strpts),n*add.strat*1.1),]
    #   } else {
    #     strfy=stratify(strpts,tyyp,n*add.strat*1.1)
    #   }
    # }
    
    # sp_abse=SpatialPointsDataFrame(coords=coordinates(strfy),
    #                                data=as.data.frame(raster::extract(env.stack,strfy)),
    #                                proj4string =strpts@proj4string)
    
    strfy=raster::extract(rst,strfy[,c('x','y')]) %>% unique()
    sp_abse = env[env$cell%in%strfy,]
    
    nosna=apply(sp_abse,1,function(x){
      all(!is.na(x))
    })
    
    sp_abse=sp_abse[which(nosna),]
    sp_abse=sp_abse[!sp_abse$cell%in%pres[,'cell'],]
    
    if(round(n*add.strat)<nrow(sp_abse)) sp_abse=sp_abse[sample(1:nrow(sp_abse),round(n*add.strat)),]
    
    if(grepl("env",type)){
      sp_abs=sp_abse
    } else {
      sp_abs=rbind(sp_abse,sp_abs)
    }
    
  }
  
  ### ------------------------
  ### Prepare output
  ### ------------------------
  
  if(nrow(pres)>0){
    pres <- env[env$cell%in%pres[,'cell'],]
    ptout=rbind(pres,sp_abs)
    #ptout=rbind(as(pres,"SpatialPoints"),as(sp_abs,"SpatialPoints"))
    
    out@pa=c(rep(1,nrow(pres)),
             rep(0,nrow(sp_abs)))
    
    out@env_vars=ptout[,pred_sdm] #as.data.frame(rbind(xt_pres,sp_abs@data))
    out@xy=ptout[,c('x','y')] %>% as.matrix() #rbind(coordinates(pres),coordinates(sp_abs))
  } else {
    ptout=sp_abs
    
    out@pa=rep(0,nrow(sp_abs))
    
    out@env_vars=ptout[,pred_sdm] #as.data.frame(rbind(xt_pres,sp_abs@data))
    out@xy=ptout[,c('x','y')] %>% as.matrix() #rbind(coordinates(pres),coordinates(sp_abs))
  }
  
  # return
  return(out)
  
}
