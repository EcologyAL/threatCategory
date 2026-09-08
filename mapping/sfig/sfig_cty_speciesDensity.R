# read new assessment -----------------------------------------------------

red.list <- read.csv(paths['spAssSim'])

red.list$intCategory <- factor(red.list$intCategory,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()
red.list$intCategory_com <- factor(red.list$intCategory_com,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()
red.list$redlistCategory <- factor(red.list$redlistCategory,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()

spds <- read.csv(paths["spds1d"])
sp_cty <- read.csv(paths['spCty']) 

cty <- read.csv('data/map/economy/cty_cell_5percent.csv')
cty <- cty %>% group_by(cell_poll) %>% 
  slice_max(order_by = p, n = 1)
cty <- cty[,c('cell_poll','cty')] %>% rename(ne_id=cty) %>% 
  left_join(distinct(sp_cty[,c('ne_id','Code','adm0_a3_is','Income.group')]))

rne <- rnaturalearth::ne_countries()
rne <- rne[,'adm0_a3'] %>% rename(adm0_a3_is=adm0_a3)
rne$adm0_a3_is[rne$adm0_a3_is%in%'TWN'] <- 'CHN'
rne <- st_transform(rne, crs = "+proj=moll")
rne$area_km2 <- st_area(rne) / 1e9
cty <- left_join(cty,st_drop_geometry(rne)) %>% 
  dplyr::select(-adm0_a3_is) %>% distinct() %>% na.omit()

sp_cell_cty <- left_join(spds,cty)
#ylab(expression(Species~(per~10^10~m^2)))

dat <- left_join(sp_cell_cty,red.list[,c('speciesKey',
                                  'intCategory','intCategory_com','redlistCategory',
                                  'wt_intC','wt_intC_com','wt_redlistC')])
dat <- dat %>% rename(income_grp=Income.group)

dat$category <- dat$intCategory %>% as.numeric()
dat$wt <- dat$wt_intC

pdat <- dat %>% group_by(cell_poll) %>% 
  mutate(TSR=sum(category>3,na.rm=T),
         SR=sum(category>1,na.rm=T),
         SD=sum(category>1,na.rm=T),
         RLI=mean(wt,na.rm=T)) %>% 
  ungroup() %>% 
  dplyr::select(TSR,SR,SD,RLI,income_grp,Code) %>% 
  distinct() %>% 
  na.omit()
pdat$income_grp <- pdat$income_grp %>% str_remove("[1-5]. ") %>% str_remove(": OECD") %>% str_remove(": nonOECD")

pdat <- pdat[pdat$SR>10,]
pdatb <- pdat
p5b <- ggplot(pdat, aes(x = SR, y = RLI)) +
  geom_point(size = 0.1,color='gray',alpha=0.8) +
  geom_smooth(method = "lm", formula = y ~ x, 
              se = TRUE,
              alpha = 0.3,
              linewidth = 0.2,
              fill = "grey90") +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(#after_stat(rr.label),
                      after_stat(p.value.label),
                      sep = "~~~")),
    parse = TRUE,
    label.x = "right",        
    label.y = 1.1,#label.y = "top",          
    size = 3.5                
  )+
  labs(title = 'b')+
  xlab(expression(Species~per~1^'°'~grid~cell))+
  ylab(expression(RLI[INT]))+
  #scale_color_manual(name = "Income group", values = mycol) +
  #xlab("Evaluated species number per 1-arc-degree grid cell") +
  #geom_hline(yintercept = g.mean, col = 1, lty = 2) +
  theme_bw() +
  scale_x_log10() +
  theme(
    legend.position='none',
    panel.grid.minor = element_blank(),
    legend.key.height = unit(0.1, "inch"),
    legend.key.width  = unit(0.1, "inch"),
    #text = element_text(size = 8),
    axis.text = element_text(size = 6)
  )
p5b

#######
#######
sp_cty <- read.csv(paths['spCty']) 
sp_cty$adm0_a3_is[sp_cty$adm0_a3_is%in%'TWN'] <- "CHN"
sp_cty$Code[sp_cty$adm0_a3_is%in%'TWN'] <- "CHN"
sp_cty$Income.group[sp_cty$Code%in%'CHN'] <- "Upper middle income"

# rne <- rnaturalearth::ne_countries()
# rne <- rne[,'adm0_a3'] %>% rename(adm0_a3_is=adm0_a3)
# rne <- st_transform(rne, crs = "+proj=moll")
# rne$area <- st_area(rne) / 1e9
# rne$area[rne$adm0_a3_is%in%c('CHN','TWN')] <- sum(rne$area[rne$adm0_a3_is%in%c('CHN','TWN')]) 
# rne$area<-as.numeric(rne$area)
#sp_cell_cty <- left_join(spds,cty)
#sp_cty <- left_join(sp_cty,st_drop_geometry(rne))

cty <- read.csv('data/map/economy/cty_cell_5percent.csv')
cty <- cty[cty$p>0.05,]

cty <- cty[,c('cell_poll','cty')] %>% rename(ne_id=cty) %>% 
  distinct() %>% 
  left_join(distinct(sp_cty[,c('ne_id','Code','adm0_a3_is','Income.group')])) %>% 
  dplyr::select(-ne_id) %>% na.omit() %>% 
  group_by(Code) %>% 
  reframe(area=n()) %>%
  na.omit()
sp_cty <- left_join(sp_cty,cty)

dat <- left_join(sp_cty,red.list[,c('speciesKey',
                                         'intCategory','intCategory_com','redlistCategory',
                                         'wt_intC','wt_intC_com','wt_redlistC')])
dat <- dat %>% rename(income_grp=Income.group)

dat$category <- dat$intCategory %>% as.numeric()
dat$wt <- dat$wt_intC

pdat <- dat %>% group_by(Code) %>% 
  mutate(TSR=sum(category>3,na.rm=T),
         SR=sum(category>1,na.rm=T),
         RLI=mean(wt,na.rm=T)) %>% 
  ungroup() %>% 
  dplyr::select(TSR,SR,area,RLI,income_grp,Code) %>% 
  distinct() %>% 
  na.omit()
pdat$income_grp <- pdat$income_grp %>% str_remove("[1-5]. ") %>% str_remove(": OECD") %>% str_remove(": nonOECD")
pdat$SD <- pdat$SR/pdat$area %>% as.numeric()

pdat <- pdat[pdat$SR>100,]
mycol <- c("#078653","#9E4E9A")
pdat$income_grp <- factor(pdat$income_grp,
                          levels = c('High income','Upper middle income','Lower middle income','Low income'),
                          labels = c('High-income','Low- & middle-income','Low- & middle-income','Low- & middle-income'))

pdat <- pdat[pdat$SR>100,]

pdata <- pdat
p5a <- ggplot(pdat, aes(x = SD, y = RLI, color = income_grp)) +
  geom_smooth(aes(group = income_grp), method = "lm", formula = y ~ x, 
              se = TRUE,
              alpha = 0.3,
              linewidth = 0.2,
              fill = "grey90") +
  geom_text(aes(label = Code), size = 2) +
  scale_color_manual(name = "Income group", values = mycol) +
  xlab(expression(Species~per~1^'°'~grid~cell))+
  ylab(expression(RLI[INT]))+
  #xlab(expression(Species~(per~10^3~km^2)))+
  theme_bw() +
  scale_x_log10() +
  theme(
    legend.position='none',
    panel.grid.minor = element_blank(),
    legend.key.height = unit(0.1, "inch"),
    legend.key.width  = unit(0.1, "inch"),
    text = element_text(size = 8),
    axis.text = element_text(size = 6)
  )


pdat <- pdata
p5a <- ggplot(pdat, aes(x = SD, y = RLI)) +
  geom_smooth(method = "lm", formula = y ~ x, 
              se = TRUE,
              alpha = 0.3,
              linewidth = 0.2,
              fill = "grey90") +
  geom_text(aes(label = Code), size = 2) +
  scale_color_manual(name = "Income group", values = mycol) +
  xlab(expression(Species~per~1^'°'~grid~cell))+
  ylab(expression(RLI[INT]))+
  #xlab(expression(Species~(per~10^3~km^2)))+
  theme_bw() +
  scale_x_log10() +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(
                      after_stat(p.value.label),  
                      sep = "~~~")),
    parse = TRUE,
    label.x = "left",        
    label.y = 1.1,#label.y = "top",          
    size = 3.5                
  )+
  labs(title = 'a')+
  geom_smooth(method = "lm", formula = y ~ x, 
              se = TRUE,
              alpha = 0.3,
              linewidth = 0.2,
              fill = "grey90") +
  theme(
    legend.position='none',
    panel.grid.minor = element_blank(),
    legend.key.height = unit(0.1, "inch"),
    legend.key.width  = unit(0.1, "inch"),
    text = element_text(size = 6),
    title = element_text(size = 8),
    axis.text = element_text(size = 8)
  )
pdat <- pdatb
p5b <- ggplot(pdat, aes(x = SR, y = RLI)) +
  geom_point(size = 0.1,color='gray',alpha=0.8) +
  geom_smooth(method = "lm", formula = y ~ x, 
              se = TRUE,
              alpha = 0.3,
              linewidth = 0.2,
              fill = "grey90") +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(#after_stat(rr.label),
                      after_stat(p.value.label),
                      sep = "~~~")),
    parse = TRUE,
    label.x = "left",        
    label.y = 1.1,#label.y = "top",          
    size = 3.5                
  )+
  labs(title = 'b')+
  xlab(expression(Species~per~1^'°'~grid~cell))+
  ylab(expression(RLI[INT]))+
  theme_bw() +
  scale_x_log10() +
  theme(
    legend.position='none',
    panel.grid.minor = element_blank(),
    legend.key.height = unit(0.1, "inch"),
    legend.key.width  = unit(0.1, "inch"),
    title = element_text(size = 8),
    axis.text = element_text(size = 8)
  )
p5b
p <- p5a + p5b + plot_layout(width=c(3,3),height=3.5)
p
ggsave(paths['sfig_sr_rli'],plot=p,
       width=6,height=3)
