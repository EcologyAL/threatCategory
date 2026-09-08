source('code/tables/family.R') 
rm(list = ls()[!ls() %in% c("paths","pdat")])
pdat <- pdat[!pdat$Family%in%"All S.",]

colnames(pdat)
pdat <- pdat[pdat$S.>100,];

p1 <- ggplot(pdat,aes(x=`S.`,y=`RLI`)) +
  geom_point(shape = 21,
             size = 3,
             fill = "#4F79A7",
             colour = "black",
             stroke = 0.4,
             alpha = 0.75)+
  # geom_smooth(method = "lm",
  #             formula = y ~ x,
  #             se = TRUE,
  #             color = "firebrick")+
  theme_bw()+
  #labs(title = '(a)')+
  xlab('Spcies Richness Per Family') +
  ylab(expression(RLI[FCC])) +
  scale_x_log10()+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        legend.key.height = unit(0.1, "inch"),
        legend.key.width = unit(0.1, "inch"),
        legend.background = element_rect(color='gray'), 
        text = element_text(size=8),
        axis.text = element_text(size=6))  +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(#after_stat(rr.label),
                      after_stat(p.value.label),
                      sep = "~~~")),
    parse = TRUE,
    label.x = "right",
    label.y = 1.25,#"top",
    size = 3.5)
p1

m <- summary(lm(pdat$`T.S.(%)` ~ pdat$S.))
r2 <- m$r.squared;p <- m$coefficients[2,4]
print(r2);print(p)

m <- summary(glm(pdat$`T.S.(%)` ~ pdat$S.))
p <- m$coefficients[2,4]
print(r2);print(p)

p2 <- ggplot(pdat,aes(x=`S.`,y=`T.S.(%)`)) +
  geom_point(shape = 21,
             size = 3,
             fill = "#4F79A7",
             colour = "black",
             stroke = 0.4,
             alpha = 0.75)+
  # geom_smooth(method = "lm",
  #             formula = y ~ x,
  #             se = TRUE,              
  #             color = "firebrick")+
  theme_bw()+
  labs(title = '(b)')+
  xlab('Spcies Richness Per Family') +
  ylab(expression(PTS[FCC] ('%'))) +
  scale_x_log10()+
  theme_bw()+
  labs(title = '(b)')+
  theme(panel.grid.minor = element_blank(),
        legend.key.height = unit(0.1, "inch"),
        legend.key.width = unit(0.1, "inch"),
        legend.background = element_rect(color='gray'), 
        text = element_text(size=8),
        axis.text = element_text(size=6)) +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(#after_stat(rr.label),
                      after_stat(p.value.label),
                      sep = "~~~")),
    parse = TRUE,
    label.x = "right",
    label.y = 1.25,#"top",
    size = 3.5)
p2


# p <- p1+p2+plot_layout(ncol=2)
# p
# ggsave(paths['sfig_fa'],plot=p,width = 6,height=3,dpi='print')

ggsave(paths['sfig_fa'],plot=p1,width = 6,height=3,dpi='print')

