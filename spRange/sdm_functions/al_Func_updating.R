ceval <- function(f,pa,tesdat,crit,tre=numeric()){
  
  # If there are any presences in the evaluation data
  if(any(pa%in%1)){
    
    z<-prediction(f,pa)
    # AUC
    auc<-performance(z,measure="auc")@y.values[[1]]
    rmse=performance(z,measure="rmse")@y.values[[1]]
    
    # optimum Threshold for conversion into binary outputs
    prbos<-seq(from=0,to=1,length.out=length(pa))
    zz<-performance(z,measure="sens",x.measure="spec",prbe=100000)
    zzz<-performance(z,measure="fpr",x.measure="fnr",prbe=100000)
    all.tss=zz@x.values[[1]]+zz@y.values[[1]]-1
    all.ppv<-performance(z,measure="ppv",prbe=100000)
    all.acc<-performance(z,measure="acc",prbe=100000)
    pn=zz@x.values[[1]]*zzz@x.values[[1]]
    py=zz@y.values[[1]]*zzz@y.values[[1]]
    
    all.kappa=(all.acc@y.values[[1]]-py*pn)/(1-py*pn)
    
    
    if(crit=="maxTSS"){
      thr=z@cutoffs[[1]][which.max(all.tss)]
      
    } else if(crit=="pp=op"){
      thr=quantile(f,probs=1-mean(pa))
      
    }else if(length(tre)!=0){
      thr=tre
      
    }
    
    wi=which.min(abs(thr-z@cutoffs[[1]]))
    ppv=all.ppv@y.values[[1]][wi]
    tss=all.tss[wi]
    acc=all.acc@y.values[[1]][wi]
    kappa=all.kappa[wi]
    
    # Return evaluation metrics
    weg=c(auc=auc,rmse=rmse,ppv=ppv,tss=tss,acc=acc,kappa=kappa,threshold=as.numeric(thr))
    
    return(weg)
  }
}

create_envstrat <- function(env.stk,
                            rAll=TRUE,
                            save_it=TRUE,
                            strat_dir=NA,
                            poolsiz=5*10^6,
                            sampsiz=400000,
                            type){
  
  recli=list()
  # loop over environmental layers
  for(i in 1:nlayers(env.stk)){
    
    # read layer
    if(rAll){
      rsti=readAll(env.stk[[i]])
    } else {
      rsti=raster(env.stk[[i]])
    }
    
    # take a sample to check if raster has less than 10 levels 
    smp=sampleRandom(rsti,min(1000,ncell(rsti)))
    
    if(length(unique(smp))<10){
      
      warning(paste("Less than 10 unique values found for layer",names(env.stk)[i],"no reclassification applied.."))
      values(rsti)=as.numeric(as.factor(values(rsti)))
      recli[[i]]=rsti
      
    } else {
      
      # Define five equidistant bins
      strt_i=seq(from=rsti@data@min,
                 to=rsti@data@max,
                 length.out=6)
      
      # Reclassify raster according to these bins
      rclm=matrix(c(strt_i[1:5],strt_i[2:6],1:5*10^(i-1)),ncol=3)
      recli[[i]]=reclassify(rsti,rclm,include.lowest=TRUE)
      
    }
    
    
    cat(paste0("reclassified layer",i,"...\n"))
  }
  
  # create one raster layer representing combined bins
  recl=sum(stack(recli))
  
  # Create ca 400000 stratfied point samples
  if(rAll){
    sptz=as(recl,"SpatialPixelsDataFrame")
    poolsiz=nrow(sptz)
  } else {
    sptz=sampleRandom(recl,poolsiz,sp=TRUE)
  }
  crs(sptz)=crs(env.stk)#sptz@proj4string=env.stk@crs
  
  strpts=stratify(sptz,type,sampsiz)
  
  if(save_it){
    lyn=paste(names(env.stk),collapse="_")
    save(strpts,file=paste0(strat_dir,type,"_",lyn,".RData"))
  }
  
  return(strpts)
}

df_or_rast <- function(mod,nwdat,...){
  
  if("randomForest"%in%class(mod)){
    index=2
  }else{
    index=1
  }
  
  
  if(class(nwdat)%in%c("RasterStack","RasterBrick","RasterLayer")){
    
    # add.arg=list(...)
    # beginCluster(5)
    # cl=getCluster()
    # clusterExport(cl,varlist=list("nwdat","mod","add.arg","index"),envir=environment())
    # out=clusterR(nwdat,predict,args=c(list(model=mod,index=index),add.arg))
    # 
    # returnCluster()
    out=raster::predict(nwdat,mod,...)
    #endCluster()
    
  } else if(class(nwdat)=="data.frame"){
    out=predict(mod,newdata=nwdat,...)
    
    if("randomForest"%in%class(mod)){
      out=out[,2]
    }
  }
  
  return(out)
  
}

wsl.samplePseuAbs.SPD <- function(n=10000,
                                  env.stack,
                                  type="geographic",
                                  add.strat=0,
                                  pres=numeric(),
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
  
  possibtype=c("geographic","random","target.group","geo.strat","density",
               "env.strat","env.semi.strat")
  if(length(type)!=1 | !(type%in%possibtype)){
    stop("Invalid specification of pseudoabsence sampling type!")
  }
  
  possibthin=c("no","presences","absences","both")
  if(length(type)!=1 | !(force_spat_thin%in%possibthin)){
    stop("Invalid specification of spatial thinning method!")
  }
  
  if(grepl("env",type)){
    add.strat=1
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
  ### Prepare template raster
  ### ------------------------
  
  if(type%in%c("geographic","random","geo.strat","density") | force_spat_thin%in%c("presences","both")){
    
    # no template directory is supplied, just
    # calculate from scratch
    if(is.na(template_dir)){
      
      rst=aggregate(env.stack[[1]],
                    fact=geores_fact,
                    fun=function(x,na.rm){
                      ifelse(all(is.na(x)),NA,1)
                    },na.rm=T)
      # if template directory is supplied load if file exists
      # otherwise writeRaster
    } else {
      ptrn=paste0("template",geores_fact,".tif")
      tmfl=list.files(template_dir,pattern=ptrn,full.names=TRUE)
      
      if(length(tmfl)>0){
        rst=raster(tmfl)
      } else {
        tmfl=paste0(template_dir,"/",ptrn)
        rst=aggregate(env.stack[[1]],
                      fact=geores_fact,
                      fun=function(x,na.rm){
                        ifelse(all(is.na(x)),NA,1)
                      },na.rm=T,filename=tmfl,overwrite=TRUE)
      }
    }
    crs(rst)=crs(env.stack)
    
    # Calculate (latitudinal) distance between cells for potential
    # downstream analyses if no minim is provided
    if(is.na(limdist)){
      proje=grepl("longlat",crs(rst))
      dpp=SpatialPoints(coordinates(rst)[c(1,1+dim(rst)[2]),],
                        proj4string = crs(rst))
      limdist=spDists(dpp,longlat = proje)[1,2]
    }
    
    
    # Write path to template file into meta information
    out@meta$template_file=paste0(template_dir,"/template",geores_fact,".tif")
  }
  
  ### ------------------------
  ### raster::raster::extract and refine presences
  ### ------------------------
  
  if(length(pres)>0){
    
    xt_pres=raster::extract(env.stack,pres)
    sna=apply(xt_pres,1,function(x){
      any(is.na(x))
    })
    
    if(length(which(sna))>0){
      pres=pres[-which(sna)]
      xt_pres=xt_pres[-which(sna),]
      cat(paste0(length(which(sna))," non-matching presences removed..\n"))
    }
    
    if(force_spat_thin%in%c("presences","both")){
      
      n_mx=ifelse(n=="auto",50000,n)
      
      pres=SpatialPointsDataFrame(pres,data=data.frame(ID=1:length(pres)))
      if(nrow(pres@coords)<3000){
        tpp=thin_them(pres,limdist)
      } else {
        if(set_max_npres_to_nabs & nrow(pres@coords)<7*n_mx & nrow(pres@coords) >= n_mx){
          tpp=upsample_strategic(pres,limdist,n_mx,warnig=FALSE)
        }else if(set_max_npres_to_nabs & nrow(pres@coords)>=7*n_mx){
          tpp=upsample_thin(pres,limdist,n_mx)
        }else{
          tpp=upsample_strategic(pres,limdist,nrow(pres@coords),warnig=FALSE)
        }
      }
      
      cat(paste0(length(pres)-length(tpp)," presences removed to obtain min distance of ",
                 round(limdist,digits=2),"..\n"))
      pres=tpp
      xt_pres=xt_pres[pres$ID,]
      
      if(n=="auto"){
        if( ( (0.8*sum(!is.na(env.stack[[1]][]))) > max(1000,10*length(pres)))|((0.8*sum(!is.na(env.stack[[1]][])))>50000) ){
          n=ifelse(length(pres)<100,1000,ifelse(length(pres)>=100 & length(pres)<5000,10*length(pres),50000))
        }else{
          n=0.8*sum(!is.na(env.stack[[1]][]))
        }
      }
      
    } else {
      
      if(n=="auto"){
        if(n=="auto"){
          if( ( (0.8*sum(!is.na(env.stack[[1]][]))) > max(1000,10*length(pres)))|((0.8*sum(!is.na(env.stack[[1]][])))>50000) ){
            n=ifelse(length(pres)<100,1000,ifelse(length(pres)>=100 & length(pres)<5000,10*length(pres),50000))
          }else{
            n=0.8*sum(!is.na(env.stack[[1]][]))
          }
        }
      }
      
      if(set_max_npres_to_nabs & length(pres)>n){
        pres=pres[sample(1:length(pres),n)]
      }
    }
  }
  
  ### ------------------------
  ### Do the geographic sampling
  ### ------------------------
  
  if(type=="geographic"){
    
    # Sample a regular grid of abence points with
    # n_presences x geodist_fact points
    abs=sampleRegular(rst,round(length(pres)*geodist_fact),sp=T)
    
    # create geo absences from geo_nrep times jittering regular samples
    geo.pts=list()
    for(i in 1:geo_nrep){
      
      pt.abs=abs@coords[,c("x","y")]
      pt.abs=apply(pt.abs,2,jitter,factor=3)
      
      model.idw <- geoIDW(p=as.data.frame(pres@coords),
                          a=as.data.frame(pt.abs))
      
      prd <- predict(rst, model.idw,mask=TRUE)
      
      # Sample cell centers proportional to interpolated presence
      # probability
      nonaval=which(!is.na(values(prd)))
      
      prb=round(values(prd)[nonaval]*10^4)
      
      smp=sample(1:length(prb),size=round(n*(1-add.strat+.1)/geo_nrep),prob=prb,replace=F)
      
      geo.pts[[i]]=coordinates(prd)[nonaval[smp],]
    }
    
    # combine
    df.pseu=SpatialPoints(do.call("rbind",geo.pts),
                          proj4string = crs(rst))
    
    # raster::extract and subsample to match desired number
    sp_abs=as(df.pseu,"SpatialPointsDataFrame")
    sp_abs@data=as.data.frame(raster::extract(env.stack,df.pseu))
    
    ### ------------------------
    ### Do the random sampling
    ### ------------------------
  } else if (type=="random"){
    
    nona=which(!is.na(values(rst)))
    
    rnd.pts=sample(nona,
                   size=n*(1-add.strat)*1.5,
                   prob=values(suppressWarnings(raster::area(rst)))[nona])
    crds=coordinates(rst)[rnd.pts,]
    sp_abs=SpatialPointsDataFrame(coords=crds,
                                  data=as.data.frame(raster::extract(env.stack,crds)),
                                  proj4string =crs(rst))
    
    ### ------------------------
    ### Do the target.group sampling
    ### ------------------------
  } else if (type=="target.group"){
    
    if(is.na("target.group_dir")){
      stop("target.group_dir has to be supplied for sampling type target.group!")
    }
    
    fls=list.files(target.group_dir,full.names=T)
    ltarpt=lapply(fls,read.table,header=TRUE)
    dftarpt=do.call("rbind",ltarpt)
    sptarpt=SpatialPoints(dftarpt,
                          proj4string = crs(env.stack))
    
    if(force_spat_thin%in%c("absences","both")){
      
      if(nrow(dftarpt)>7*n*(1-add.strat)*1.1){
        
        crds=upsample_thin(sptarpt,limdist,n*(1-add.strat)*1.1)
        
      } else {
        
        crds=upsample_strategic(sptarpt,limdist,n*(1-add.strat)*1.1)
      }
    } else {
      if(nrow(dftarpt)>n*(1-add.strat)*1.1){
        crds=sptarpt[sample(1:nrow(dftarpt),n*(1-add.strat)*1.1,replace=FALSE),]
        
      } else {
        crds=sptarpt[sample(1:nrow(dftarpt),n*(1-add.strat)*1.1,replace=TRUE),]
        cat(paste("Less target.group points available than requested. Sampling with replacemnent..."))
        
      }
      
    }
    
    sp_abs=SpatialPointsDataFrame(coords=crds,
                                  data=as.data.frame(raster::extract(env.stack,crds)),
                                  proj4string =crs(rst))
    
    ### ------------------------
    ### Do the geo.strat sampling
    ### ------------------------
  } else if (type=="geo.strat"){
    
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
      sppt=SpatialPoints(regpt,proj4string = crs(rst))
      
      # crude raster::extraction
      xtr1=raster::extract(rst,sppt)
      sppt=sppt[which(!is.na(xtr1)),]
      
    } else {
      Ntarg=round(1.1*n*ncell(rst)/length(which(!is.na(values(rst)))))
      sppt=sampleRegular(rst,Ntarg,sp=T)
      sppt=sppt[-which(is.na(sppt@data[,1])),]
      
    }
    
    # Preprare output
    sp_abs=SpatialPointsDataFrame(coords=sppt@coords,
                                  data=as.data.frame(raster::extract(env.stack,sppt)),
                                  proj4string =crs(rst))
    
  } else if (type=="density"){
    
    ### ------------------------
    ### Prepare point pattern object
    ### ------------------------
    
    xrng=extent(rst)[1:2]
    yrng=extent(rst)[3:4]
    
    # Define Point Pattern object to calculate
    owi=owin(xrange=xrng,yrange=yrng)
    myppp=ppp(x=pres@coords[,1],y=pres@coords[,2],window = owi)
    
    ### ------------------------
    ### Generate 'im' object with density info
    ### ------------------------
    
    lo=dim(rst)[1:2]
    
    x=seq(xrng[1],xrng[2],length.out = lo[2])
    y=seq(yrng[1],yrng[2],length.out = lo[1])
    
    dens=density(myppp,xy=list(x=x,y=y),adjust=geodist_fact/10)
    
    ### ------------------------
    ### Draw locations proportional to point density
    ### ------------------------
    
    drst=raster(dens)
    drst=resample(drst,rst)
    drst=mask(drst,rst)
    
    vls=values(drst)
    
    # Replace NA's with zero probability
    if(any(is.na(vls)) || any(vls<0)){
      vls[which(is.na(vls) | vls<0)]=0
    }
    
    # Sample from density distributions
    pts=sample(1:length(vls),n*(1-add.strat)*1.1,prob=vls)
    
    sppt=SpatialPoints(coordinates(drst)[pts,])
    
    # Preprare output
    sp_abs=SpatialPointsDataFrame(coords=sppt@coords,
                                  data=as.data.frame(raster::extract(env.stack,sppt)),
                                  proj4string =crs(rst))
    
  }
  
  if(exists("sp_abs")){
    # Subsample and remove NAs
    nosna=apply(sp_abs@data,1,function(x){
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
    if(is.na(env.strat_path)){
      
      strpts = create_envstrat(env.stk=env.stack,
                               rAll=rAll,
                               save_it=FALSE,
                               strat_dir=NA,
                               type=tyyp)
      
      # if strat directory is supplied load if file exists
      # otherwise write file
      # if .RData file is supplied read directly
    } else if(grepl(".RData",env.strat_path)){
      load(tmfl)
    } else{
      
      tmfl=list.files(env.strat_path,pattern=tyyp,full.names=TRUE)
      lyn=paste(names(env.stack),collapse="_")
      tmfl=grep(lyn,tmfl,value=TRUE)
      
      if(length(tmfl)>0){
        
        load(tmfl)
        strpts=strpts[sample(1:nrow(strpts)),]
        
      } else {
        
        strpts = create_envstrat(env.stk=env.stack,
                                 rAll=rAll,
                                 save_it=TRUE,
                                 strat_dir=env.strat_path,
                                 type=tyyp)
        
      }
    }
    
    if(force_spat_thin%in%c("absences","both")){
      
      strfy=upsample_thin(strpts,limdist,n*add.strat*1.1)
      
    } else {
      if(type=="env.semi.strat"){
        strfy=strpts[sample(1:nrow(strpts),n*add.strat*1.1),]
      } else {
        strfy=stratify(strpts,tyyp,n*add.strat*1.1)
      }
    }
    
    sp_abse=SpatialPointsDataFrame(coords=coordinates(strfy),
                                   data=as.data.frame(raster::extract(env.stack,strfy)),
                                   proj4string =strpts@proj4string)
    
    nosna=apply(sp_abse@data,1,function(x){
      all(!is.na(x))
    })
    
    sp_abse=sp_abse[which(nosna),]
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
  
  if(length(pres)>0){
    ptout=rbind(as(pres,"SpatialPoints"),as(sp_abs,"SpatialPoints"))
    
    out@pa=c(rep(1,length(pres)),
             rep(0,nrow(sp_abs)))
    
    out@env_vars=as.data.frame(rbind(xt_pres,sp_abs@data))
    out@xy=rbind(coordinates(pres),coordinates(sp_abs))
  } else {
    ptout=as(sp_abs,"SpatialPoints")
    out@pa=rep(0,nrow(sp_abs))
    out@env_vars=sp_abs@data
    out@xy=coordinates(sp_abs)
  }
  
  # return
  return(out)
  
}

wsl.samplePseuAbs.SPD.thin <- function(n=10000,
                                       env.stack,
                                       type="geographic",
                                       add.strat=0,
                                       pres=numeric(),
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
  
  possibtype=c("geographic","random","target.group","geo.strat","density",
               "env.strat","env.semi.strat")
  if(length(type)!=1 | !(type%in%possibtype)){
    stop("Invalid specification of pseudoabsence sampling type!")
  }
  
  possibthin=c("no","presences","absences","both")
  if(length(type)!=1 | !(force_spat_thin%in%possibthin)){
    stop("Invalid specification of spatial thinning method!")
  }
  
  if(grepl("env",type)){
    add.strat=1
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
  ### Prepare template raster
  ### ------------------------
  
  if(type%in%c("geographic","random","geo.strat","density") | force_spat_thin%in%c("presences","both")){
    
    # no template directory is supplied, just
    # calculate from scratch
    if(is.na(template_dir)){
      
      rst=aggregate(env.stack[[1]],
                    fact=geores_fact,
                    fun=function(x,na.rm){
                      ifelse(all(is.na(x)),NA,1)
                    },na.rm=T)
      # if template directory is supplied load if file exists
      # otherwise writeRaster
    } else {
      ptrn=paste0("template",geores_fact,".tif")
      tmfl=list.files(template_dir,pattern=ptrn,full.names=TRUE)
      
      if(length(tmfl)>0){
        rst=raster(tmfl)
      } else {
        tmfl=paste0(template_dir,"/",ptrn)
        rst=aggregate(env.stack[[1]],
                      fact=geores_fact,
                      fun=function(x,na.rm){
                        ifelse(all(is.na(x)),NA,1)
                      },na.rm=T,filename=tmfl,overwrite=TRUE)
      }
    }
    crs(rst)=crs(env.stack)
    
    # Calculate (latitudinal) distance between cells for potential
    # downstream analyses if no minim is provided
    if(is.na(limdist)){
      proje=grepl("longlat",crs(rst))
      dpp=SpatialPoints(coordinates(rst)[c(1,1+dim(rst)[2]),],
                        proj4string = crs(rst))
      limdist=spDists(dpp,longlat = proje)[1,2]
    }
    
    
    # Write path to template file into meta information
    out@meta$template_file=paste0(template_dir,"/template",geores_fact,".tif")
  }
  
  ### ------------------------
  ### raster::raster::extract and refine presences
  ### ------------------------
  
  if(length(pres)>0){
    
    xt_pres=raster::extract(env.stack,pres)
    sna=apply(xt_pres,1,function(x){
      any(is.na(x))
    })
    
    if(length(which(sna))>0){
      pres=pres[-which(sna)]
      xt_pres=xt_pres[-which(sna),]
      cat(paste0(length(which(sna))," non-matching presences removed..\n"))
    }
    
    if(force_spat_thin%in%c("presences","both")){
      
      n_mx=ifelse(n=="auto",50000,n)
      
      pres=SpatialPointsDataFrame(pres,data=data.frame(ID=1:length(pres)))
      if(nrow(pres@coords)<3000){
        tpp=thin_them(pres,limdist)
      } else {
        if(set_max_npres_to_nabs & nrow(pres@coords)<7*n_mx & nrow(pres@coords) >= n_mx){
          tpp=upsample_strategic(pres,limdist,n_mx,warnig=FALSE)
        }else if(set_max_npres_to_nabs & nrow(pres@coords)>=7*n_mx){
          tpp=upsample_thin(pres,limdist,n_mx)
        }else{
          tpp=upsample_strategic(pres,limdist,nrow(pres@coords),warnig=FALSE)
        }
      }
      
      cat(paste0(length(pres)-length(tpp)," presences removed to obtain min distance of ",
                 round(limdist,digits=2),"..\n"))
      pres=tpp
      xt_pres=xt_pres[pres$ID,]
      
      if(n=="auto"){
        if( ( (0.8*sum(!is.na(env.stack[[1]][]))) > max(100,10*length(pres)))|((0.8*sum(!is.na(env.stack[[1]][])))>50000) ){
          n=ifelse(length(pres)<10,100,ifelse(length(pres)>=10 & length(pres)<5000,10*length(pres),50000))
        }else{
          n=0.8*sum(!is.na(env.stack[[1]][]))
        }
      }
      
    } else {
      
      if(n=="auto"){
        if(n=="auto"){
          if( ( (0.8*sum(!is.na(env.stack[[1]][]))) > max(100,10*length(pres)))|((0.8*sum(!is.na(env.stack[[1]][])))>50000) ){
            n=ifelse(length(pres)<10,100,ifelse(length(pres)>=10 & length(pres)<5000,10*length(pres),50000))
          }else{
            n=0.8*sum(!is.na(env.stack[[1]][]))
          }
        }
      }
      
      if(set_max_npres_to_nabs & length(pres)>n){
        pres=pres[sample(1:length(pres),n)]
      }
    }
  }
  
  ### ------------------------
  ### Do the geographic sampling
  ### ------------------------
  
  if(type=="geographic"){
    
    # Sample a regular grid of abence points with
    # n_presences x geodist_fact points
    abs=sampleRegular(rst,round(length(pres)*geodist_fact),sp=T)
    
    # create geo absences from geo_nrep times jittering regular samples
    geo.pts=list()
    for(i in 1:geo_nrep){
      
      pt.abs=abs@coords[,c("x","y")]
      pt.abs=apply(pt.abs,2,jitter,factor=3)
      
      model.idw <- geoIDW(p=as.data.frame(pres@coords),
                          a=as.data.frame(pt.abs))
      
      prd <- predict(rst, model.idw,mask=TRUE)
      
      # Sample cell centers proportional to interpolated presence
      # probability
      nonaval=which(!is.na(values(prd)))
      
      prb=round(values(prd)[nonaval]*10^4)
      
      smp=sample(1:length(prb),size=round(n*(1-add.strat+.1)/geo_nrep),prob=prb,replace=F)
      
      geo.pts[[i]]=coordinates(prd)[nonaval[smp],]
    }
    
    # combine
    df.pseu=SpatialPoints(do.call("rbind",geo.pts),
                          proj4string = crs(rst))
    
    # raster::extract and subsample to match desired number
    sp_abs=as(df.pseu,"SpatialPointsDataFrame")
    sp_abs@data=as.data.frame(raster::extract(env.stack,df.pseu))
    
    ### ------------------------
    ### Do the random sampling
    ### ------------------------
  } else if (type=="random"){
    
    nona=which(!is.na(values(rst)))
    
    rnd.pts=sample(nona,
                   size=n*(1-add.strat)*1.5,
                   prob=values(suppressWarnings(raster::area(rst)))[nona])
    crds=coordinates(rst)[rnd.pts,]
    sp_abs=SpatialPointsDataFrame(coords=crds,
                                  data=as.data.frame(raster::extract(env.stack,crds)),
                                  proj4string =crs(rst))
    
    ### ------------------------
    ### Do the target.group sampling
    ### ------------------------
  } else if (type=="target.group"){
    
    if(is.na("target.group_dir")){
      stop("target.group_dir has to be supplied for sampling type target.group!")
    }
    
    fls=list.files(target.group_dir,full.names=T)
    ltarpt=lapply(fls,read.table,header=TRUE)
    dftarpt=do.call("rbind",ltarpt)
    sptarpt=SpatialPoints(dftarpt,
                          proj4string = crs(env.stack))
    
    if(force_spat_thin%in%c("absences","both")){
      
      if(nrow(dftarpt)>7*n*(1-add.strat)*1.1){
        
        crds=upsample_thin(sptarpt,limdist,n*(1-add.strat)*1.1)
        
      } else {
        
        crds=upsample_strategic(sptarpt,limdist,n*(1-add.strat)*1.1)
      }
    } else {
      if(nrow(dftarpt)>n*(1-add.strat)*1.1){
        crds=sptarpt[sample(1:nrow(dftarpt),n*(1-add.strat)*1.1,replace=FALSE),]
        
      } else {
        crds=sptarpt[sample(1:nrow(dftarpt),n*(1-add.strat)*1.1,replace=TRUE),]
        cat(paste("Less target.group points available than requested. Sampling with replacemnent..."))
        
      }
      
    }
    
    sp_abs=SpatialPointsDataFrame(coords=crds,
                                  data=as.data.frame(raster::extract(env.stack,crds)),
                                  proj4string =crs(rst))
    
    ### ------------------------
    ### Do the geo.strat sampling
    ### ------------------------
  } else if (type=="geo.strat"){
    
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
      sppt=SpatialPoints(regpt,proj4string = crs(rst))
      
      # crude raster::extraction
      xtr1=raster::extract(rst,sppt)
      sppt=sppt[which(!is.na(xtr1)),]
      
    } else {
      Ntarg=round(1.1*n*ncell(rst)/length(which(!is.na(values(rst)))))
      sppt=sampleRegular(rst,Ntarg,sp=T)
      sppt=sppt[-which(is.na(sppt@data[,1])),]
      
    }
    
    # Preprare output
    sp_abs=SpatialPointsDataFrame(coords=sppt@coords,
                                  data=as.data.frame(raster::extract(env.stack,sppt)),
                                  proj4string =crs(rst))
    
  } else if (type=="density"){
    
    ### ------------------------
    ### Prepare point pattern object
    ### ------------------------
    
    xrng=extent(rst)[1:2]
    yrng=extent(rst)[3:4]
    
    # Define Point Pattern object to calculate
    owi=owin(xrange=xrng,yrange=yrng)
    myppp=ppp(x=pres@coords[,1],y=pres@coords[,2],window = owi)
    
    ### ------------------------
    ### Generate 'im' object with density info
    ### ------------------------
    
    lo=dim(rst)[1:2]
    
    x=seq(xrng[1],xrng[2],length.out = lo[2])
    y=seq(yrng[1],yrng[2],length.out = lo[1])
    
    dens=density(myppp,xy=list(x=x,y=y),adjust=geodist_fact/10)
    
    ### ------------------------
    ### Draw locations proportional to point density
    ### ------------------------
    
    drst=raster(dens)
    drst=resample(drst,rst)
    drst=mask(drst,rst)
    
    vls=values(drst)
    
    # Replace NA's with zero probability
    if(any(is.na(vls)) || any(vls<0)){
      vls[which(is.na(vls) | vls<0)]=0
    }
    
    # Sample from density distributions
    pts=sample(1:length(vls),n*(1-add.strat)*1.1,prob=vls)
    
    sppt=SpatialPoints(coordinates(drst)[pts,])
    
    # Preprare output
    sp_abs=SpatialPointsDataFrame(coords=sppt@coords,
                                  data=as.data.frame(raster::extract(env.stack,sppt)),
                                  proj4string =crs(rst))
    
  }
  
  if(exists("sp_abs")){
    # Subsample and remove NAs
    nosna=apply(sp_abs@data,1,function(x){
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
    if(is.na(env.strat_path)){
      
      strpts = create_envstrat(env.stk=env.stack,
                               rAll=rAll,
                               save_it=FALSE,
                               strat_dir=NA,
                               type=tyyp)
      
      # if strat directory is supplied load if file exists
      # otherwise write file
      # if .RData file is supplied read directly
    } else if(grepl(".RData",env.strat_path)){
      load(tmfl)
    } else{
      
      tmfl=list.files(env.strat_path,pattern=tyyp,full.names=TRUE)
      lyn=paste(names(env.stack),collapse="_")
      tmfl=grep(lyn,tmfl,value=TRUE)
      
      if(length(tmfl)>0){
        
        load(tmfl)
        strpts=strpts[sample(1:nrow(strpts)),]
        
      } else {
        
        strpts = create_envstrat(env.stk=env.stack,
                                 rAll=rAll,
                                 save_it=TRUE,
                                 strat_dir=env.strat_path,
                                 type=tyyp)
        
      }
    }
    
    if(force_spat_thin%in%c("absences","both")){
      
      strfy=upsample_thin(strpts,limdist,n*add.strat*1.1)
      
    } else {
      if(type=="env.semi.strat"){
        strfy=strpts[sample(1:nrow(strpts),n*add.strat*1.1),]
      } else {
        strfy=stratify(strpts,tyyp,n*add.strat*1.1)
      }
    }
    
    sp_abse=SpatialPointsDataFrame(coords=coordinates(strfy),
                                   data=as.data.frame(raster::extract(env.stack,strfy)),
                                   proj4string =strpts@proj4string)
    
    nosna=apply(sp_abse@data,1,function(x){
      all(!is.na(x))
    })
    
    sp_abse=sp_abse[which(nosna),]
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
  
  if(length(pres)>0){
    ptout=rbind(as(pres,"SpatialPoints"),as(sp_abs,"SpatialPoints"))
    
    out@pa=c(rep(1,length(pres)),
             rep(0,nrow(sp_abs)))
    
    out@env_vars=as.data.frame(rbind(xt_pres,sp_abs@data))
    out@xy=rbind(coordinates(pres),coordinates(sp_abs))
  } else {
    ptout=as(sp_abs,"SpatialPoints")
    out@pa=rep(0,nrow(sp_abs))
    out@env_vars=sp_abs@data
    out@xy=coordinates(sp_abs)
  }
  
  # return
  return(out)
  
}

# range.fun.SPD <- function (species_name, 
#                            occ_coord, 
#                            gppoly = gppoly,
#                            gprst = gprst,
#                            #Bioreg, 
#                            #Bioreg_name = 'ECO_NAME', 
#                            degrees_outlier=10,
#                            buffer_width=2,
#                            clustered_points_outlier=3
#                            #buffer_width_point=0.5, 
#                            #buffer_increment_point_line=0.5, 
#                            #buffer_width_polygon=0.1, 
#                            #dir_temp=paste0("temp",sample(1:99999999,1))
# ){
#   
#   ### =========================================================================
#   ### remove duplicates
#   ### =========================================================================
#   
#   occ_coord@coords <- round(occ_coord@coords, 4)
#   occ_coord <- remove.duplicates(occ_coord)
#   
#   ### =========================================================================
#   ### Check if sufficient data
#   ### =========================================================================
#   
#   # Check if there sufficient species & if not, make an entry in the log-file and
#   # end the function
#   if (length(occ_coord)<=clustered_points_outlier+1){
#     stop("Too few occurences!")
#   } 
#   
#   cat("## Start of computation for species: ",species_name," ###", "\n") 
#   
#   ### =========================================================================
#   ### Identify outliers
#   ### =========================================================================
#   
#   #create distance matrix...
#   mat_dist <- as.matrix(knn.dist(occ_coord@coords, k=clustered_points_outlier))
#   
#   #mark outliers
#   cond <- apply(mat_dist, 1, function(x) x[clustered_points_outlier])>degrees_outlier
#   rm(mat_dist) 
#   
#   cat(paste0(sum(cond), " outlier's from " ,nrow(occ_coord), " | proportion from total points: ", round((sum(cond)/length(occ_coord))*100,0), "%\n"))
#   
#   occ_coord_mod <- occ_coord[!cond,]
#   
#   # Stop if too many outliers in data set
#   if(length(occ_coord_mod)==0){
#     stop('Too few occurrences within outlier threshold!')
#   } 
#   
#   ### =========================================================================
#   ### Extract geopolitical units
#   ### =========================================================================
#   
#   gpgrids <- unique(extract(gprst,occ_coord_mod))
#   gppoly.sp <- gppoly[gppoly$ADCODE99%in%gpgrids,]
#   gsi=gSimplify(gppoly.sp,tol=0.1)
#   gppoly.sp <- gBuffer(gsi,width=buffer_width)
#   if(length(occ_coord_mod)<3000){
#     ooc_coord_outlierbd <- gBuffer(occ_coord_mod,width=degrees_outlier)
#     gppoly.sp <- gIntersection(gppoly.sp,ooc_coord_outlierbd)
#   }
#   #ooc_coord_outlierbd <- gBuffer(occ_coord_mod,width=degrees_outlier)
#   #gppoly.sp <- gIntersection(gppoly.sp,ooc_coord_outlierbd)
#   
#   ### =========================================================================
#   ### Check and return output
#   ### =========================================================================
#   
#   # if(!dir.exists(dir_temp)){
#   #   unlink(dir_temp, recursive=T)
#   # }
#   
#   # if(length(L)==0){
#   #   stop('No occurrences within Bioregions. Empty raster produced.')
#   # } 
#   
#   # shp_species <- Reduce(rbind, L)
#   # shp_species=gUnaryUnion(shp_species,checkValidity = 2)
#   # shp_species@proj4string=occ_coord@proj4string
#   
#   cat("## End of computation for species: ",species_name," ###", "\n") 
#   
#   #return(shp_species)
#   return(gppoly.sp)
#   
# }

# range.fun.SPD.ptBuffer <- function (species_name, 
#                                     occ_coord, 
#                                     gppoly = gppoly,
#                                     gprst = gprst,
#                                     #Bioreg, 
#                                     #Bioreg_name = 'ECO_NAME', 
#                                     degrees_outlier=10,
#                                     buffer_width=2,
#                                     clustered_points_outlier=3,
#                                     point_buffer=3
#                                     #buffer_width_point=0.5, 
#                                     #buffer_increment_point_line=0.5, 
#                                     #buffer_width_polygon=0.1, 
#                                     #dir_temp=paste0("temp",sample(1:99999999,1))
# ){
#   
#   ### =========================================================================
#   ### remove duplicates
#   ### =========================================================================
#   
#   occ_coord@coords <- round(occ_coord@coords, 4)
#   occ_coord <- remove.duplicates(occ_coord)
#   
#   ### =========================================================================
#   ### Check if sufficient data
#   ### =========================================================================
#   
#   # Check if there sufficient species & if not, make an entry in the log-file and
#   # end the function
#   if (length(occ_coord)<=clustered_points_outlier+1){
#     stop("Too few occurences!")
#   } 
#   
#   cat("## Start of computation for species: ",species_name," ###", "\n") 
#   
#   ### =========================================================================
#   ### Identify outliers
#   ### =========================================================================
#   
#   #create distance matrix...
#   mat_dist <- as.matrix(knn.dist(occ_coord@coords, k=clustered_points_outlier))
#   
#   #mark outliers
#   cond <- apply(mat_dist, 1, function(x) x[clustered_points_outlier])>degrees_outlier
#   rm(mat_dist) 
#   
#   cat(paste0(sum(cond), " outlier's from " ,nrow(occ_coord), " | proportion from total points: ", round((sum(cond)/length(occ_coord))*100,0), "%\n"))
#   
#   occ_coord_mod <- occ_coord[!cond,]
#   
#   # Stop if too many outliers in data set
#   if(length(occ_coord_mod)==0){
#     stop('Too few occurrences within outlier threshold!')
#   } 
#   
#   ### =========================================================================
#   ### Extract geopolitical units
#   ### =========================================================================
#   
#   gpgrids <- unique(extract(gprst,occ_coord_mod))
#   gppoly.sp <- gppoly[gppoly$ADCODE99%in%gpgrids,]
#   gsi=gSimplify(gppoly.sp,tol=0.1)
#   gppoly.sp <- gBuffer(gsi,width=buffer_width)
#   if(length(occ_coord_mod)<3000){
#     ooc_coord_outlierbd <- gBuffer(occ_coord_mod,width=degrees_outlier)
#     gppoly.sp <- gIntersection(gppoly.sp,ooc_coord_outlierbd)
#   }
#   
#   ooc_coord_min <- gBuffer(occ_coord_mod,width=point_buffer)
#   gppoly.sp <- gUnion(gppoly.sp,ooc_coord_min)
#   
#   ### =========================================================================
#   ### Check and return output
#   ### =========================================================================
#   
#   # if(!dir.exists(dir_temp)){
#   #   unlink(dir_temp, recursive=T)
#   # }
#   
#   # if(length(L)==0){
#   #   stop('No occurrences within Bioregions. Empty raster produced.')
#   # } 
#   
#   # shp_species <- Reduce(rbind, L)
#   # shp_species=gUnaryUnion(shp_species,checkValidity = 2)
#   # shp_species@proj4string=occ_coord@proj4string
#   
#   cat("## End of computation for species: ",species_name," ###", "\n") 
#   
#   #return(shp_species)
#   return(gppoly.sp)
#   
# }

# range.fun.SPD.thin <- function (species_name, 
#                                 occ_coord, 
#                                 gppoly = gppoly,
#                                 gprst = gprst,
#                                 #Bioreg, 
#                                 #Bioreg_name = 'ECO_NAME', 
#                                 degrees_outlier=10,
#                                 buffer_width=2,
#                                 clustered_points_outlier=3
#                                 #buffer_width_point=0.5, 
#                                 #buffer_increment_point_line=0.5, 
#                                 #buffer_width_polygon=0.1, 
#                                 #dir_temp=paste0("temp",sample(1:99999999,1))
# ){
#   
#   ### =========================================================================
#   ### remove duplicates
#   ### =========================================================================
#   
#   occ_coord@coords <- round(occ_coord@coords, 4)
#   occ_coord <- remove.duplicates(occ_coord)
#   
#   ### =========================================================================
#   ### Check if sufficient data
#   ### =========================================================================
#   
#   # Check if there sufficient species & if not, make an entry in the log-file and
#   # end the function
#   if (length(occ_coord)<=clustered_points_outlier+1){
#     stop("Too few occurences!")
#   } 
#   
#   cat("## Start of computation for species: ",species_name," ###", "\n") 
#   
#   ### =========================================================================
#   ### Identify outliers
#   ### =========================================================================
#   
#   #create distance matrix...
#   #mat_dist <- as.matrix(knn.dist(occ_coord@coords, k=clustered_points_outlier))
#   
#   #mark outliers
#   #cond <- apply(mat_dist, 1, function(x) x[clustered_points_outlier])>degrees_outlier
#   #rm(mat_dist) 
#   
#   #cat(paste0(sum(cond), " outlier's from " ,nrow(occ_coord), " | proportion from total points: ", round((sum(cond)/length(occ_coord))*100,0), "%\n"))
#   
#   #occ_coord_mod <- occ_coord[!cond,]
#   occ_coord_mod <- occ_coord
#   # Stop if too many outliers in data set
#   if(length(occ_coord_mod)==0){
#     stop('Too few occurrences within outlier threshold!')
#   } 
#   
#   ### =========================================================================
#   ### Extract geopolitical units
#   ### =========================================================================
#   
#   gpgrids <- unique(extract(gprst,occ_coord_mod))
#   gppoly.sp <- gppoly[gppoly$ADCODE99%in%gpgrids,]
#   gsi=gSimplify(gppoly.sp,tol=0.1)
#   gppoly.sp <- gBuffer(gsi,width=buffer_width)
#   if(length(occ_coord_mod)<3000){
#     ooc_coord_outlierbd <- gBuffer(occ_coord_mod,width=degrees_outlier)
#     gppoly.sp <- gIntersection(gppoly.sp,ooc_coord_outlierbd)
#   }
#   #ooc_coord_outlierbd <- gBuffer(occ_coord_mod,width=degrees_outlier)
#   #gppoly.sp <- gIntersection(gppoly.sp,ooc_coord_outlierbd)
#   
#   ### =========================================================================
#   ### Check and return output
#   ### =========================================================================
#   
#   # if(!dir.exists(dir_temp)){
#   #   unlink(dir_temp, recursive=T)
#   # }
#   
#   # if(length(L)==0){
#   #   stop('No occurrences within Bioregions. Empty raster produced.')
#   # } 
#   
#   # shp_species <- Reduce(rbind, L)
#   # shp_species=gUnaryUnion(shp_species,checkValidity = 2)
#   # shp_species@proj4string=occ_coord@proj4string
#   
#   cat("## End of computation for species: ",species_name," ###", "\n") 
#   
#   #return(shp_species)
#   return(gppoly.sp)
#   
# }

#############
#### env.stk@crs ==>>
#############
create_envstrat <- function(env.stk,
                            rAll=TRUE,
                            save_it=TRUE,
                            strat_dir=NA,
                            poolsiz=5*10^6,
                            sampsiz=400000,
                            type){
  
  recli=list()
  # loop over environmental layers
  for(i in 1:nlayers(env.stk)){
    
    # read layer
    if(rAll){
      rsti=readAll(env.stk[[i]])
    } else {
      rsti=raster(env.stk[[i]])
    }
    
    # take a sample to check if raster has less than 10 levels 
    smp=sampleRandom(rsti,min(1000,ncell(rsti)))
    
    if(length(unique(smp))<10){
      
      warning(paste("Less than 10 unique values found for layer",names(env.stk)[i],"no reclassification applied.."))
      values(rsti)=as.numeric(as.factor(values(rsti)))
      recli[[i]]=rsti
      
    } else {
      
      # Define five equidistant bins
      strt_i=seq(from=rsti@data@min,
                 to=rsti@data@max,
                 length.out=6)
      
      # Reclassify raster according to these bins
      rclm=matrix(c(strt_i[1:5],strt_i[2:6],1:5*10^(i-1)),ncol=3)
      recli[[i]]=reclassify(rsti,rclm,include.lowest=TRUE)
      
    }
    
    
    cat(paste0("reclassified layer",i,"...\n"))
  }
  
  # create one raster layer representing combined bins
  recl=sum(stack(recli))
  
  # Create ca 400000 stratfied point samples
  if(rAll){
    sptz=as(recl,"SpatialPixelsDataFrame")
    poolsiz=nrow(sptz)
  } else {
    sptz=sampleRandom(recl,poolsiz,sp=TRUE)
  }
  sptz@proj4string=crs(env.stk)#env.stk@crs
  
  strpts=stratify(sptz,type,sampsiz)
  
  if(save_it){
    lyn=paste(names(env.stk),collapse="_")
    save(strpts,file=paste0(strat_dir,type,"_",lyn,".RData"))
  }
  
  return(strpts)
}
