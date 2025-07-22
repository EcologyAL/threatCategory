load(path_output_mod)
load(path_output_mod2)

env.df <- filter(rstlist,cell_poll%in%finalpol)
tmp <- matrix(FALSE,nrow=nrow(env.df),ncol=1+length(names(rstlist.f))) %>% as.data.frame()
colnames(tmp) <- paste0('sn_',c('1981-2010',names(rstlist.f)))

prenull=wsl.predict.df(prmod,
                       predat=env.df[,pred_sdm],
                       thres=thrs[top6])
rst_c_pred <- do.call('cbind',prenull@predictions[[1]])
rst_c_pred <- rowSums(rst_c_pred)

tmp[,1] <- rst_c_pred

#for (i.f in 1:9) {
for (i.f in 1:9) {
  print(i.f)
  env.df.tmp <- filter(rstlist.f[[i.f]],cell_poll%in%finalpol)
  prenull=wsl.predict.df(prmod,
                         predat=env.df.tmp[,pred_sdm],
                         thres=thrs[top6])
  rst_c_pred <- do.call('cbind',prenull@predictions[[1]])
  rst_c_pred <- rowSums(rst_c_pred)
  tmp[,i.f+1] <- rst_c_pred
}

#colnames(tmp) <- paste0('sn_',c('1981-2010',names(rstlist.f)))  
rst.c <- cbind(tmp,env.df[,c('cell','cell_poll')]) 
#rst.c <- rst.c[,c('cell','cell_poll',colnames(tmp))]
rst.c <- rst.c[rowSums(rst.c[,colnames(tmp)])>0,]
#write.csv(rst.c,path_output,row.names = F)

if(length(top6)==6){
  write.csv(rst.c,path_output_6,row.names = F)
}else if(length(top6)==10){
  write.csv(rst.c,path_output_10,row.names = F)
}
rm(pseu.abs_i,prmod,modinp_top,thrs,smev,top6,finalpol)