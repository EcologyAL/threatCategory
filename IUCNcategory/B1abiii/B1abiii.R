sp.eco <- read.csv(paths["spECO"])
scenarios <- read.csv('data/scenarios_list.csv')
spls <- read.csv(paths["spLS"])

sp.eco <- sp.eco[,c('speciesKey',scenarios$sns)]
sp.eco <- sp.eco[,c('speciesKey',scenarios$sns[str_detect(scenarios$sns,'2071.2100')])]

colnames(sp.eco)[-1] <- colnames(sp.eco)[-1] %>% str_remove('sn_2071.2100_') %>% str_remove('DS_') %>% str_remove('mean_')
colnames(sp.eco)[-1] <- paste0('B_',colnames(sp.eco)[-1])
write.csv(sp.eco,paths["B2"],row.names = F)
