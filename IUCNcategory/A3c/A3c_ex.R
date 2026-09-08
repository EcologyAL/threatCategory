sp.ex <- read.csv(paths["spEX"])
colnames(sp.ex)
for (i in 3:ncol(sp.ex)) {
  sp.ex[,i] <- (sp.ex[,i]<=0)&(sp.ex[,2]>0)
  #sp.ex[,i] <- (sp.ex[,i]<=0)
}
sp.ex[,-(1:2)] %>% apply(2,sum,na.rm=T)
write.csv(sp.ex,paths["A3cEX"],row.names = F)
