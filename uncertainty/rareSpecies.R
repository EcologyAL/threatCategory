path1 <- 'data/rareSp/BIEN_Random_RareSpecies_MBG_Jørgensen.csv'
path2 <- 'data/rareSp/BIEN_Random_RareSpecies_NYBG_Thiers.csv'

rareSp1 <- read.csv(path1)
rareSp2 <- read.csv(path2)

sp <- c(rareSp1$Latin,rareSp2$Latin) %>% unique()

colnames(rareSp1)
rareSp1$Latin <- rareSp1$nameMatched
rareSp2$Latin <-  rareSp2$nameMatched

rareSp <- rbind(rareSp1[,c('Latin','RecognizedAsRare','NotRare.largeRangeOrHighAbundance.')],
    rareSp2[,c('Latin','RecognizedAsRare','NotRare.largeRangeOrHighAbundance.')])

rareSp$states <- TRUE
rareSp$states[rareSp$RecognizedAsRare%in%c('yes','Y?','y','Yes')] <- TRUE 
rareSp$states[rareSp$NotRare.largeRangeOrHighAbundance.%in%'yes'] <- FALSE
rareSp$states[rareSp$RecognizedAsRare%in%'no'] <- FALSE

spls <- read.csv('data/spinfo/spls_info.csv')
spls$num_coords
spls <- spls[spls$species%in%sp,]
spls <- spls %>% rename(Latin=species) %>% dplyr::select(Latin,num_coords,num_coords_c) %>% right_join(rareSp)
#spls <- spls
spls <- spls[!is.na(spls$num_coords_c),]

table(spls$states[spls$num_coords_c<=3])
table(spls$states[spls$num_coords_c>3])

x <- c(table(spls$states[spls$num_coords_c<=3]),
       table(spls$states[spls$num_coords_c>3]))
names(x) <- c('N≤3, not rare','N≤3, rare',
       'N>3, not rare','N>3, rare')
names(x) <- paste0(names(x),' (sp:',x,')')

pie(x,
    col = c("#2C7FB8", "tomato","#41B6C4","#FEB24C"),
    main = "")



