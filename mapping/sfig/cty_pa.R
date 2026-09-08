library(ggpmisc)
source('code/tables/country.R') ;pdat <- pdat[!pdat$Code%in%c(NA,'NCL','TWN'),]
rm(list = ls()[!ls() %in% c("paths","pdat")])
colnames(pdat)
pdat <- pdat[];

p1 <- ggplot(pdat,aes(x=`Protected T.S.(%)`,y=RLI,label = Code)) +
  geom_text(size=2)+
  labs(title = '(a)')+
  ylab(expression(RLI[INT])) +
  xlab('Effectively Protected Threatened Species  (%)') +
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = TRUE,              
              color = "firebrick")+
  theme_bw()+
  theme(#legend.position = c(0.17,1.1),
    panel.grid.minor = element_blank(),
    legend.key.height = unit(0.1, "inch"),
    legend.key.width = unit(0.1, "inch"),
    legend.background = element_rect(color='gray'), 
    #legend.background = element_rect(fill='transparent'), 
    text = element_text(size=8),
    axis.text = element_text(size=6)) + 
  stat_poly_eq(
      formula = y ~ x,
      aes(label = paste(#after_stat(rr.label),  
                        after_stat(p.value.label),
                        sep = "~~~")),
      parse = TRUE,
      label.x = "left",        
      label.y = 1.1,#label.y = "top",          
      size = 3.5                
    )
p1


p2 <- ggplot(pdat,aes(x=`Protected T.S.(%)`,y=`Delta RLI`,label = Code)) +
  geom_text(size=2)+
  labs(title = '(b)')+
  #xlab('ΔRLI') +
  ylab(expression(ΔRLI[INT])) +
  xlab('Effectively Protected Threatened Species  (%)') +
  # geom_smooth(method = "lm",
  #             formula = y ~ x,
  #             se = TRUE,              
  #             color = "firebrick")+
  theme_bw()+
  theme(#legend.position = c(0.17,1.1),
    panel.grid.minor = element_blank(),
    legend.key.height = unit(0.1, "inch"),
    legend.key.width = unit(0.1, "inch"),
    legend.background = element_rect(color='gray'), 
    #legend.background = element_rect(fill='transparent'), 
    text = element_text(size=8),
    axis.text = element_text(size=6)) + stat_poly_eq(
      formula = y ~ x,
      aes(label = paste(#after_stat(rr.label),
                        after_stat(p.value.label),
                        sep = "~~~")),
      parse = TRUE,
      label.x = "left",        
      label.y = 1.1,#label.y = "top",          
      size = 3.5                
    )
p2

pdat$`Delta RLI`
p3 <- ggplot(pdat,aes(x=log(`S.`),y=`Delta RLI`,label = Code)) +
  geom_text(size=2)+
  xlab('log(species richness)') +
  ylab(expression(ΔRLI[INT])) +
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = TRUE,              
              color = "firebrick")+
  theme_bw()+
  labs(title = '(c)')+
  theme(panel.grid.minor = element_blank(),
        legend.key.height = unit(0.1, "inch"),
        legend.key.width = unit(0.1, "inch"),
        legend.background = element_rect(color='gray'), 
        #legend.background = element_rect(fill='transparent'), 
        text = element_text(size=8),
        axis.text = element_text(size=6)) + 
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(#after_stat(rr.label),  
                      after_stat(p.value.label),  
                      sep = "~~~")),
    parse = TRUE,
    label.x = "right",        
    label.y = 1.1,#"top",          
    size = 3.5)
p3
p4 <- ggplot(pdat,aes(x=`T.S.(%)`,y=`Delta RLI`,label = Code)) +
  geom_text(size=2)+
  labs(title = '(d)')+
  xlab(expression(PTS["INT"]~"(%)")) +
  ylab(expression(ΔRLI[INT])) +
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = TRUE,              
              color = "firebrick")+
  theme_bw()+
  theme(#legend.position = c(0.17,1.1),
    panel.grid.minor = element_blank(),
    legend.key.height = unit(0.1, "inch"),
    legend.key.width = unit(0.1, "inch"),
    legend.background = element_rect(color='gray'), 
    #legend.background = element_rect(fill='transparent'), 
    text = element_text(size=8),
    axis.text = element_text(size=6)) + stat_poly_eq(
      formula = y ~ x,
      aes(label = paste(#after_stat(rr.label), 
                        after_stat(p.value.label),
                        sep = "~~~")),
      parse = TRUE,
      label.x = "right",        
      label.y = 1.1,#label.y = "top",          
      size = 3.5                
    )
p4

p <- p1+p2+p3+p4+plot_layout(nrow=2)
ggsave(paths['sfig_pa_cty'],plot=p,width = 7,height=6.5,dpi='print')

