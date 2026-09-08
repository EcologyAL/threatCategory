shp <- read_sf('data/map/data_1d/data_1d_land.shp')
shp <- shp[,c('cell_poll','land')]
shp$land <- shp$land%in%c('1','T','TRUE')
dat <- read.csv(paths['cellInfo_all_sn']) %>% distinct()
shp <- left_join(shp,dat)

#shp$RLI[shp$SR<10] <- NA
########

#mypalette <- colorRampPalette((c('#79171c','#934f24','#ac7e26','#c2b055', '#cbdda1','#a5d7ca','#5cb8c9','#2d8ab6','#185da3','#0f3188')))
mypalette <- colorRampPalette(c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred"))
mycol <- rev(mypalette(100))
mycol <- mycol[c(seq(1,50,2),51:100)]
vals <- seq(0.4, 1, length.out = length(mycol))

library(patchwork)
library(stringr)

# --- 1. Build subplot list (no title, no legend) ---
p.list <- list()
for(i in c(4:6, 1:3, 7:9)){
  x <- colnames(shp)[3 + i]
  shp$value <- shp[[x]]
  shp$value[!shp$land] <- NA
  
  p <- ggplot(shp) +
    geom_sf(aes(fill = value, colour = value)) +
    scale_fill_gradientn(colors = mycol,
                         values = scales::rescale(vals, from = c(0.4, 1)),
                         limits = c(0.4, 1),
                         oob    = scales::squish,
                         na.value = 'gray95',
                         name   = 'RLI') +
    scale_colour_gradientn(colors = mycol,
                           values = scales::rescale(vals, from = c(0.4, 1)),
                           limits = c(0.4, 1),
                           oob    = scales::squish,
                           na.value = 'gray95',
                           guide  = 'none') +
    scale_x_continuous(limits = c(-170, 170)) +
    theme_bw() +
    theme(panel.grid   = element_blank(),
          axis.text    = element_blank(),
          axis.ticks   = element_blank(),
          axis.title   = element_blank(),
          legend.position = 'none',  # hide individual legends
          plot.title   = element_blank())
  
  p.list <- c(p.list, list(p))
}

# --- 2. Column headers (ssp) --- increase plot height via margin
col_headers <- lapply(col_labels, function(lab)
  ggplot() +
    annotate('text', x = 0.5, y = 0.5, label = lab,
             fontface = 'bold', size = 3.5) +
    theme_void() +
    theme(plot.margin = margin(2, 0, 2, 0))  # top/bottom margin
)

# --- 3. Row headers (dispersal) ---
row_headers <- lapply(row_labels, function(lab)
  ggplot() +
    annotate('text', x = 0.5, y = 0.5, label = lab,
             fontface = 'bold', size = 3.5, angle = 90) +
    theme_void() +
    theme(plot.margin = margin(0, 2, 0, 2))
)

# --- 4. Shared legend (extracted from one subplot) ---
p_legend <- ggplot(shp) +
  geom_sf(aes(fill = value)) +
  scale_fill_gradientn(colors = mycol,
                       values = scales::rescale(vals, from = c(0.4, 1)),
                       limits = c(0.4, 1),
                       oob    = scales::squish,
                       na.value = 'gray95',
                       name   = 'RLI',
                       guide  = guide_colorbar(
                         direction      = 'horizontal',
                         barwidth       = unit(3, 'in'),
                         barheight      = unit(0.05, 'in'),
                         title.position = 'left',
                         title.vjust    = 0.8
                       )) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0),
        aspect.ratio = 0.4,
        legend.position = 'bottom')

shared_legend <- cowplot::get_legend(p_legend)

# --- 5. Assemble with patchwork ---
# Top row: empty corner + 3 col headers
# Final assembly — tighten heights
p_final <- header_row / row1 / row2 / row3 /
  wrap_elements(shared_legend) +
  plot_layout(heights = c(0.06, 1, 1, 1, 0.08)) &
  theme(plot.margin = margin(0, 0, 0, 0))


# Each data row: row header + 3 maps
make_row <- function(row_idx, row_header) {
  idx <- ((row_idx - 1) * 3 + 1) : (row_idx * 3)
  row_header +
    p.list[[idx[1]]] + p.list[[idx[2]]] + p.list[[idx[3]]] +
    plot_layout(ncol = 4, widths = c(0.12, 1, 1, 1))
}

row1 <- make_row(1, row_headers[[1]])
row2 <- make_row(2, row_headers[[2]])
row3 <- make_row(3, row_headers[[3]])

# Stack rows + legend
p_final <- header_row / row1 / row2 / row3 /
  wrap_elements(shared_legend) +
  plot_layout(heights = c(0.1, .4, .4, .4, 0.1)) &  # compress header/legend rows
  theme(plot.margin = margin(0, 0, 0, 0))           # remove subplot margins

p_final
ggsave(paths['sfig_RLI_sns'],plot=p_final, width = 7, height = 4)

# p.list <- list()
# colnames(shp)
# for(i in c(4:6,1:3,7:9)){
#   x <- y <- colnames(shp)[3+i]
#   y <- y %>% 
#     str_replace('spp126','ssp1-2.6') %>% 
#     str_replace('spp379','ssp3-7.0') %>% 
#     str_replace('spp585','ssp5-8.5') %>% 
#     str_replace('unlimit','Full dispersal') %>% 
#     str_replace('limit','20 km/decade') %>% 
#     str_replace('no','No dispersal') %>% 
#     str_replace('_',', ')
#   #x <- c('SR','SR_THR','SR_NTHR','SR_NTHR','SR_VU','SR_EN','SR_CREX')[i]
#   #y <- c('ALL','Threatened','LC&NT','TH','VU','EN','CR&EX')[i]
#   shp$value <- shp[[x]]
#   shp$value[!shp$land] <- NA
#   p <- ggplot(shp) +    
#     geom_sf(aes(fill = value,colour=value)) +  
#     scale_fill_gradientn(colors = mycol,
#                          values = scales::rescale(vals, from = c(0.4, 1)),
#                          limits = c(0.4, 1),
#                          oob = scales::squish,
#                          na.value = "gray95",
#                          name = ""
#                          ) +
#     scale_colour_gradientn(colors = mycol,
#                            na.value = 'gray95',
#                            values = scales::rescale(vals, from = c(0.4, 1)),
#                            limits = c(0.4, 1),
#                            oob = scales::squish,
#                            guide='none') +
#     scale_x_continuous(limits = c(-170,170)) +
#     labs(title=y)+
#     theme_void()+
#     theme_bw() + theme(panel.grid=element_blank(),
#                        legend.position = 'bottom',
#                        legend.direction = 'horizontal',
#                        axis.ticks.x = element_blank(),
#                        axis.text.x = element_blank(),
#                        legend.key.height = unit(0.1, "in"),
#                        legend.key.width = unit(0.3, "in"))
#   
#   p.list <- c(p.list,list(p))
# }
# 
# #windows(height = 6.5,width = 6)
# p <- p.list[[1]] + p.list[[2]] + p.list[[3]] + 
#   p.list[[4]] + p.list[[5]] + p.list[[6]] +
#   p.list[[7]] + p.list[[8]] + p.list[[9]] +
#   plot_layout(ncol=3)#+plot_layout(ncol=2,nrow=3,heights = c(1.5,1.5,1.5),widths = c(3,3))
# p
# ggsave(paths['sfig_RLI_sns'],width = 6,height = 6.5)
# ggsave(str_replace(paths['sfig_RLI_sns'],'jpg','eps'),width = 6,height = 6.5)
# 
