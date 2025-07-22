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

### Define formulas
# GLMs
form.glm.s=as.formula(paste("Presence~",paste(pred_sdm,collapse="+"))) # simple
form.glm.i=as.formula(paste("Presence~",paste(paste0("poly(",pred_sdm,",2)"),collapse="+"))) # intermediate

# GLM complex
if(length(pred_sdm)>1){
  cmbs<-combn(pred_sdm,2)
  pst<-apply(cmbs,2,paste,collapse=":")
  int.part=paste(pst,collapse="+")
  form.glm.c=as.formula(paste(paste("Presence~",paste(paste0("poly(",pred_sdm,",3)"),collapse="+")),int.part,sep="+")) # complex
} else {
  form.glm.c=as.formula(paste(paste("Presence~",paste(paste0("poly(",pred_sdm,",3)"),collapse="+")))) # complex
}

# GAMs
form.gam.s=as.formula(paste("Presence~",paste(paste0("s(",pred_sdm,",df=2)"),collapse="+"))) # simple
form.gam.i=as.formula(paste("Presence~",paste(paste0("s(",pred_sdm,",df=3)"),collapse="+"))) # intermediate
form.gam.c=as.formula(paste("Presence~",paste(paste0("s(",pred_sdm,",df=5)"),collapse="+"))) # complex

# Trees
form.trees=Presence ~ .

### define model settings
modinp=list(multi("glm",list(formula=form.glm.s,family="binomial"),"glm-simple",step=FALSE,weight=FALSE),
            multi("glm",list(formula=form.glm.i,family="binomial"),"glm-interm",step=FALSE,weight=FALSE),
            multi("glm",list(formula=form.glm.c,family="binomial"),"glm-complex",step=FALSE,weight=FALSE),
            multi("gam",list(formula=form.gam.s,family="binomial"),"gam-simple",step=FALSE,weight=FALSE),
            multi("gam",list(formula=form.gam.i,family="binomial"),"gam-interm",step=FALSE,weight=FALSE),
            multi("gam",list(formula=form.gam.c,family="binomial"),"gam-complex",step=FALSE,weight=FALSE),
            multi("gbm",list(formula=form.trees,
                             distribution = "bernoulli",
                             interaction.depth = 5,
                             shrinkage=.005,
                             n.trees = 500),"gbm-simple",weight=FALSE),
            multi("gbm",list(formula=form.trees,
                             distribution = "bernoulli",
                             interaction.depth = 5,
                             shrinkage=.005,
                             n.trees = 1000),"gbm-interm",weight=FALSE),
            multi("gbm",list(formula=form.trees,
                             distribution = "bernoulli",
                             interaction.depth = 5,
                             shrinkage=.005,
                             n.trees = 10000),"gbm-complex",weight=FALSE),
            multi("randomForest",list(formula=form.trees,ntree=500,nodesize=10),"rf-simple"),
            multi("randomForest",list(formula=form.trees,ntree=500,nodesize=3),"rf-interm"),
            multi("randomForest",list(formula=form.trees,ntree=500,nodesize=1),"rf-complex"))

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
#Sys.time()
### =========================================================================
### evaluate
### =========================================================================

cat('evaluating...\n')

evals<-wsl.evaluate(modis,crit="maxTSS",prevalence_correction =T)

smev=summary(evals)
ord=sort(smev["tss",],decreasing=T)
top6=which(colnames(smev)%in%names(ord)[1:6])
modinp_top=modinp[top6]
# modinp_top=modinp

### =========================================================================
### fit models for prediction
### =========================================================================

cat('fitting projection model...\n')

prmod=wsl.flex(x=pseu.abs_i,
               replicatetype="none",
               reps=1,
               project="tree_map",
               mod_args=modinp_top)
#Sys.time()

### Get thresholds
thrs=get_thres(evals)

save(prmod,modinp_top,thrs,smev,top6,finalpol,file=path_output_mod)
save(pseu.abs_i,file=path_output_mod2)