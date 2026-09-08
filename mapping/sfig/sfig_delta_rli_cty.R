# read new assessment -----------------------------------------------------

red.list <- read.csv(paths['spAssSim'])

red.list$intCategory <- factor(red.list$intCategory,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()
red.list$intCategory_com <- factor(red.list$intCategory_com,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()
red.list$redlistCategory <- factor(red.list$redlistCategory,levels=c('DD','LC','NT','VU','EN','CR','EX')) #%>% as.numeric()

# fig 5 b -----------------------------------------------------------------

sp <-  read.csv(paths["spAssSim"])
sp_cty <- read.csv(paths['spCty'])

dat <- left_join(sp,sp_cty) %>% group_by(Economy) %>% 
  reframe(SR = sum(!is.na(wt_intC)),SR_old = sum(!is.na(wt_redlistC)),
          RLI=mean(wt_intC,na.rm=T),RLI_old=mean(wt_redlistC,na.rm=T),
          Income.group = unique(Income.group),
          Delta.RLI=mean(wt_intC,na.rm=T)-mean(wt_redlistC,na.rm=T))
dat <- dat[!dat$Economy%in%NA,]
dat$Income.group <- factor(dat$Income.group,
                           levels = c('High income','Upper middle income','Lower middle income','Low income'),
                           labels = c('High-income','Low- & middle-income','Low- & middle-income','Low- & middle-income'))
dat$lab <- rank(dat$Delta.RLI) %>% scales::ordinal()
#dat <- dat[order(dat$Delta.RLI)[1:20],]
dat <- dat[order(dat$RLI_old)[1:30],]
dat$Economy <- str_remove(dat$Economy,', RB') %>% str_remove('Darussalam') %>% 
  str_replace('Congo, Dem. Rep.','DR Congo')
dat$Economy <- factor(dat$Economy,levels=dat$Economy)
#dat$Ratio <- round(dat$SR_old/dat$SR*100,1)

tmp <- dat
tmp$flag <- 'INT'
dat2 <- dat

dat2$RLI <- dat2$RLI_old
dat2$flag <- 'IRL'
dat2 <- rbind(dat2,tmp)

gap <- 0.01
seg_dat <- dat %>%
  mutate(
    dy   = round(RLI - RLI_old,2),
    lab = paste0(round(RLI - RLI_old,2),' (',lab,')') ,
    Income.group=Income.group,
    sgn  = sign(dy),
    y_start = RLI_old + sgn * gap,
    y_end   = RLI - sgn * gap,
    y_mid = (RLI_old + RLI)/2
  )
seg_dat[abs(seg_dat$Delta.RLI) < 0.02,c('y_start','y_end')] <- NA

mycol <- c("#078653","#9E4E9A")
p <- ggplot(dat2, aes(x = RLI, y = Economy,
                        colour = Income.group, shape = flag)) +
  geom_point(data=dat2[dat2$flag%in%"IRL",],aes(x = RLI, y = Economy,
                 colour = Income.group, shape = flag),size=1.5)+
  geom_point(size=0.75)+
  xlab('')+
  scale_color_manual(values = mycol) +
  theme_bw()+
  coord_flip()+
  theme(
    legend.position = 'top',
    legend.direction = "horizontal",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.key.width = unit(1, "mm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    legend.spacing = unit(1, "pt"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y = element_text(color = 'black',size=6),
    axis.title = element_blank(),
    axis.text.x = element_text(hjust = 1,angle=90, vjust = 1,size=6)
  )
p
p <- p + geom_text(data=seg_dat,
            aes(
              x = y_mid,
              y = Economy,
              label = lab
            ),
            color='gray70',
            angle=90,
            vjust=-0.6,
            hjust=0.2,
            size=1.5,
            inherit.aes = FALSE)+
  geom_segment(
    data = seg_dat,
    aes(
      x = y_start, xend = y_end,
      y = Economy, yend = Economy
    ),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(1.2, "mm"), type = "closed"),
    linewidth = 0.2,
    colour = "grey70"
  )
#quartz(width = 6,height = 3.5)
p
ggsave(paths["sfig_delta_rli_cty"],plot=p,width = 6,height = 2.5,units='in')

