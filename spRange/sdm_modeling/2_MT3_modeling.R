### =========================================================================
### Prepare data for species id
### =========================================================================

cat('sampling pseudoabsences...\n')

# Create pseudoabsences
pseu.abs_i=wsl.samplePseuAbs.geo.strat(type="geo.strat", #geo.strat
                                       n="auto",
                                       rst=rst.env,
                                       env=env.df,
                                       pred_sdm=pred_sdm,
                                       pres=obs,
                                       add.strat=0.2,
                                       #template_dir=templ_dir,
                                       env.strat_path=enstr_path,
                                       geodist_fact=1,
                                       geores_fact=1,
                                       taxon=spi_name,
                                       force_spat_thin = "both",
                                       limdist=1)

### =========================================================================
### Decide on predictor number depending on available presence observations
### =========================================================================

cat('preparing model formulations...\n')

ssize=floor(length(which(pseu.abs_i@pa==1))/10)

# If there are less than 20 presence observations, do nothing
# if(ssize<2){
#   next
#   # If there are less than 90 observations reduce predictor set so that
#   # at least 10 observations are available per predictor
# } else if(ssize<9){
#   env.stk_i=env.stk_i[[1:ssize_i]] 
#   pseu.abs_i@env_vars=pseu.abs_i@env_vars[, 1:ssize,drop=F]
# }

### =========================================================================
### Prepare cross-validation folds
### =========================================================================

wip=which(pseu.abs_i@pa==1)
blkp=sample(rep(1:3,each=ceiling(length(wip)/3)),ceiling(length(wip)/3)*3)[1:length(wip)]
wia=which(pseu.abs_i@pa==0)
blka=sample(1:3,size = length(wia),replace = T)

blks=rep(NA,length(pseu.abs_i@pa))
blks[wip]=blkp
blks[wia]=blka


### =========================================================================
### Define SDMs
### =========================================================================

#combns<-combinations(6,2)
combns <- combinat::combn(6,2) %>% t()
modinp <- list()
for (combn in 1:nrow(combns)){
  flag <- combns[combn,]
  vnames <- pred_sdm[flag]
  # GLMs
  form.glm.s=as.formula(paste("Presence~",paste(vnames,collapse="+"))) # simple
  
  # GAMs
  form.gam.s=as.formula(paste("Presence~",paste(paste0("s(",vnames,",df=2)"),collapse="+"))) # simple
  
  # Trees
  form.trees=as.formula(paste("Presence~",paste(vnames,collapse="+"))) # simple
  
  modinp.tmp=list(multi("glm",list(formula=form.glm.s,family="binomial"),paste(c("glm-simple",vnames),collapse = '-'),step=FALSE,weight=FALSE),
                  multi("gam",list(formula=form.gam.s,family="binomial"),paste(c("gam-simple",vnames),collapse = '-'),step=FALSE,weight=FALSE),
                  multi("gbm",list(formula=form.trees,
                                   distribution = "bernoulli",
                                   interaction.depth = 5,
                                   shrinkage=.005,
                                   n.minobsinnode = 5,
                                   n.trees = 500),paste(c("gbm-simple",vnames),collapse = '-'),weight=FALSE),
                  multi("randomForest",list(formula=form.trees,ntree=500,nodesize=10),paste(c("rf-simple",vnames),collapse = '-')))
  modinp <- c(modinp,modinp.tmp)
}

### =========================================================================
### Run models
### =========================================================================

cat('fitting cv replicates...\n')

modis=wsl.flex(x=pseu.abs_i,
               replicatetype="block-cv",
               reps=3,
               strata=blks,
               project="tree_map",
               mod_args=modinp)


### =========================================================================
### evaluate
### =========================================================================

cat('evaluating...\n')

evals<-wsl.evaluate(modis,crit="maxTSS",prevalence_correction =T)

smev=summary(evals)
ord=sort(smev["tss",],decreasing=T)
top6=which(colnames(smev)%in%names(ord)[1:10])
modinp_top=modinp[top6]
#modinp_top=modinp


### =========================================================================
### fit models for prediction
### =========================================================================

cat('fitting projection model...\n')

prmod=wsl.flex(x=pseu.abs_i,
               replicatetype="none",
               reps=1,
               project="tree_map",
               mod_args=modinp_top)

### Get thresholds
thrs=get_thres(evals)

save(prmod,modinp_top,thrs,smev,top6,finalpol,file=path_output_mod)
save(pseu.abs_i,file=path_output_mod2)
