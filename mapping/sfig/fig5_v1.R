# read new assessment -----------------------------------------------------

red.list <- read.csv(paths['spAssSim'])

red.list$intCategory <- factor(red.list$intCategory,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()
red.list$intCategory_com <- factor(red.list$intCategory_com,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()
red.list$redlistCategory <- factor(red.list$redlistCategory,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()


# read country species list and join categroy list ------------------------

sp_cty <- read.csv(paths['spCty'])
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
  dplyr::select(TSR,SR,RLI,income_grp,Code) %>% 
  distinct() %>% 
  na.omit()
pdat$income_grp <- pdat$income_grp %>% str_remove("[1-5]. ") %>% str_remove(": OECD") %>% str_remove(": nonOECD")

g.mean <- mean(pdat$RLI,na.rm=T)  

mycol <- c("#078653","#9E4E9A")
pdat$income_grp <- factor(pdat$income_grp,
                          levels = c('High income','Upper middle income','Lower middle income','Low income'),
                          labels = c('High-income','Low- & middle-income','Low- & middle-income','Low- & middle-income'))
#pdat$mycol <- factor(pdat$income_grp,levels = c('High income','Upper middle income','Lower middle income','Low income'),labels=mycol)

pdat <- pdat[pdat$SR>100,]
p5a <- ggplot(pdat, aes(x = SR, y = RLI, color = income_grp)) +
  geom_smooth(aes(group = income_grp), method = "lm", formula = y ~ x, 
              se = TRUE,
              alpha = 0.3,
              linewidth = 0.2,
              fill = "grey90") +
  geom_text(aes(label = Code), size = 2) +
  scale_color_manual(name = "Income group", values = mycol) +
  xlab("Assessed species number") +
  geom_hline(yintercept = g.mean, col = 1, lty = 2) +
  theme_bw() +
  scale_x_log10(breaks = c(300, 1000, 3000, 10000, 30000)) +
  theme(
    legend.position='none',
    #legend.position = c(0.17, 0.165),
    panel.grid.minor = element_blank(),
    legend.key.height = unit(0.1, "inch"),
    legend.key.width  = unit(0.1, "inch"),
    #legend.background = element_rect(color = "gray"),
    text = element_text(size = 8),
    axis.text = element_text(size = 6)
  )
p5a
# P5a
# ggsave('results/figure/figs/figure4a.jpg',width = 5,height = 3,units='in',dpi=300)

####
library(ggbeeswarm)
library(ggdist)
pdat$income_grp2 <- pdat$income_grp
levels(pdat$income_grp2) <- str_remove(levels(pdat$income_grp),' income')

p5b <- ggplot(pdat, aes(income_grp2, RLI)) + 
  stat_slab(fill='gray90',scale = 0.5) +
  stat_dotsinterval(aes(color=income_grp),
                    side="bottom",
                    size=0.5,
                    scale = 0.5,
                    dotsize=0.5)+
  xlab('Income group') + ylab('') +
  geom_hline(yintercept=g.mean,col=1,lty=2)+
  scale_color_manual(name = "Income group", 
                     guide = guide_legend(ncol = 1),
                     values = mycol) +
  theme_bw()+
  theme(
    text = element_text(size=6),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.text.x  = element_blank(),
    axis.text = element_text(size=5),
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.box.spacing = unit(0, "cm"),
    legend.background = element_rect(fill='transparent'), 
    legend.key.size = unit(0.02, "inch"),
    legend.position = 'bottom'
    #legend.position = 'none'
    )
p5b

p <- p5a+
  inset_element(
    p5b,
    left = 0.68, bottom = 0.5,
    right = 0.98, top = 0.98,
    align_to = "panel"
  )
p
ggsave(paths['fig5a'],plot=p,width = 4,height = 3,units='in')


# fig 5 b -----------------------------------------------------------------

sp <-  read.csv(paths["spAssSim"])
sp_cty <- read.csv(paths['spCty'])

dat <- left_join(sp,sp_cty) %>% group_by(Economy) %>% 
  reframe(SR = sum(!is.na(wt_intC)),SR_old = sum(!is.na(wt_redlistC)),
          RLI=mean(wt_intC,na.rm=T),RLI_old=mean(wt_redlistC,na.rm=T),
          Delta.RLI=mean(wt_intC,na.rm=T)-mean(wt_redlistC,na.rm=T))
dat <- dat[!dat$Economy%in%NA,]
#dat <- dat[order(dat$Delta.RLI)[1:20],]
dat <- dat[order(dat$RLI)[1:20],]
dat$Economy <- factor(dat$Economy,levels=dat$Economy)
#dat$Ratio <- round(dat$SR_old/dat$SR*100,1)

cols <- c("gray10","#1f77b4","#ff7f0e")
tmp <- dat
tmp$flag <- 'Int. ass.'
dat2 <- dat

dat2$RLI <- dat2$RLI_old
dat2$flag <- 'IUCN ass.'
dat2 <- rbind(dat2,tmp)

gap <- 0.01
seg_dat <- dat %>%
  mutate(
    dy   = round(RLI - RLI_old,2),
    sgn  = sign(dy),
    y_start = RLI_old + sgn * gap,
    y_end   = RLI - sgn * gap,
    y_mid = (RLI_old + RLI)/2
  )
seg_dat[abs(seg_dat$Delta.RLI) < 0.02,c('y_start','y_end')] <- NA

cols <- c("gray10","#1f77b4","#ff7f0e")
p3 <- ggplot(dat2, aes(x = Economy, y = RLI,colour = flag)) +
  geom_point(size=3)+
  xlab('')+
  labs(title='a')+
  scale_color_manual(values = cols) +
  theme_bw()+
  theme(
    legend.position = c(0.75,1.1),
    legend.direction = "horizontal",
    legend.margin = margin(t = 0.5, r = 1.5, b = 0.5, l = 1),
    legend.title = element_blank(),
    legend.background = element_rect(color = 'gray50'),
    legend.text = element_text(size = 8),
    legend.spacing = unit(1, "pt"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=8)
  )+
  geom_text(data=seg_dat,
            aes(
              x = Economy,
              y = y_mid,
              label = dy
            ), 
            angle=90,
            vjust=-0.3,
            size=3,
            inherit.aes = FALSE)+
  geom_segment(
    data = seg_dat,
    aes(
      x = Economy, xend = Economy,
      y = y_start, yend = y_end
    ),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(1.5, "mm"), type = "closed"),
    linewidth = 0.4,
    colour = "grey40"
  )
p3
