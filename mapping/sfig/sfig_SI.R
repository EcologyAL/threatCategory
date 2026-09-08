# ══════════════════════════════════════════════════════════════════════════════
# SI-stratified RLI maps + ΔRLI maps vs IUCN official
# ══════════════════════════════════════════════════════════════════════════════

shp <- read_sf('data/map/data_1d/data_1d_land.shp')
shp <- shp[, c('cell_poll', 'land')]
shp$land <- shp$land %in% c('1', 'T', 'TRUE')
shp.null <- shp

sp.si <- read.csv(paths['SI'])
sp.si$SI_GRID <- sp.si$SI_GRID_MAX
sp.si$SI_kNN  <- 1 / sp.si$SI_knn
sp.si <- sp.si[, c('speciesKey', 'SI_GRID', 'SI_kNN')]
sp.ds.1d <- read.csv(paths["spds1d"])
spAss    <- read.csv(paths["spAssSim"])
spLS     <- read.csv(paths["spLS"])

# IUCN official Red List
iucn.levels <- c('LC', 'NT', 'VU', 'EN', 'CR', 'EX')
red.list.c  <- read.csv(paths["IUCNredList"]) %>%
  filter(redlistCategory %in% iucn.levels) %>%
  dplyr::select(speciesKey, redlistCategory) %>%
  rename(IUCN_official = redlistCategory) %>%
  mutate(num_iucn = factor(IUCN_official, levels = iucn.levels) %>% as.numeric())

# ── Colour palettes ───────────────────────────────────────────────────────────
mypalette       <- colorRampPalette(c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred"))
mycol           <- rev(mypalette(100))

mypalette.delta <- colorRampPalette(c("#FFEE99","#FFCC00","#FF6600","#CC0000"))
mycol.delta     <- rev(mypalette.delta(100))

# ── Loop over strategies and quartiles ───────────────────────────────────────
p.list.rli   <- list()
p.list.delta <- list()

for (strategy in c('SI_GRID', 'SI_kNN')) {
  for (i in c(0, .25, .50, .75)) {
    
    # Species in this SI quartile
    sp <- sp.si[between(
      sp.si[, strategy],
      quantile(sp.si[, strategy], i,        na.rm = TRUE),
      quantile(sp.si[, strategy], i + 0.25, na.rm = TRUE)
    ), 'speciesKey']
    
    # Panel title
    strategy_label <- switch(strategy,
                             'SI_GRID' = bquote(SI[GRID]~'['*.(ceiling(i*100))*'th,'*.(ceiling(i*100)+25)*'th]'),
                             'SI_kNN'  = bquote(SI[kNN]~ '['*.(ceiling(i*100))*'th,'*.(ceiling(i*100)+25)*'th]')
    )
    
    # ── Figure A: RLI map ─────────────────────────────────────────────────────
    rli <- sp.ds.1d[sp.ds.1d[['speciesKey']] %in% sp, ] %>%
      left_join(spAss[, c('speciesKey', 'wt_climC')]) %>%
      na.omit() %>%
      group_by(cell_poll) %>%
      reframe(value = mean(wt_climC, na.rm = TRUE), SR = n())
    
    shp             <- shp.null %>% left_join(rli)
    shp$value[!shp$land]  <- NA
    shp$value[shp$SR < 5] <- NA
    
    v      <- shp$value[!is.na(shp$value)]
    lo     <- quantile(v, 0.01)
    hi     <- quantile(v, 0.99)
    vals_i <- seq(lo, hi, length.out = length(mycol))
    
    p.rli <- ggplot(shp) +
      geom_sf(aes(fill = value, colour = value)) +
      scale_fill_gradientn(
        colors   = mycol,
        values   = scales::rescale(vals_i),
        limits   = c(lo, hi),
        oob      = scales::squish,
        na.value = "gray95",
        name     = "RLI",
        breaks   = c(lo, hi),
        labels   = c('High risk', 'Low risk'),
        guide    = guide_colourbar(
          direction      = "horizontal",
          barwidth       = unit(0.6, "in"),
          barheight      = unit(0.06, "in"),
          title.position = "top",
          title.hjust    = 0.5,
          ticks          = FALSE,
          title.theme    = element_text(size = 6, hjust = 0.5),
          label.theme    = element_text(size = 5, hjust = 0.5)
        )
      ) +
      scale_colour_gradientn(
        colors   = mycol,
        values   = scales::rescale(vals_i),
        limits   = c(lo, hi),
        oob      = scales::squish,
        na.value = "gray95",
        guide    = "none"
      ) +
      scale_x_continuous(limits = c(-170, 170)) +
      labs(title = strategy_label) +
      theme_bw() +
      theme(
        panel.grid         = element_blank(),
        plot.title         = element_text(size = 8),
        legend.position    = 'none',#c(0.12, 0.25),
        legend.background  = element_rect(fill = "transparent", colour = NA),
        axis.text          = element_blank(),
        axis.ticks         = element_blank(),
        axis.title         = element_blank(),
        legend.box.spacing = unit(0.0, "in")
      )
    
    p.list.rli <- c(p.list.rli, list(p.rli))
    
    # ── Figure B: ΔRLI map ────────────────────────────────────────────────────
    
    sp.common.si <- intersect(sp, red.list.c$speciesKey)
    
    rl.model <- spAss %>%
      filter(speciesKey %in% sp.common.si) %>%
      dplyr::select(speciesKey, wt_climC) %>%
      mutate(
        num_model = pmin(pmax(round((1 - wt_climC) * 5 + 1), 1L), 6L)
      ) %>%
      dplyr::select(speciesKey, num_model)
    
    sp.merged <- left_join(
      red.list.c %>% filter(speciesKey %in% sp.common.si) %>%
        dplyr::select(speciesKey, num_iucn),
      rl.model,
      by = "speciesKey"
    ) %>%
      filter(!is.na(num_model)) %>%
      mutate(IUCN = factor(pmax(num_iucn, num_model, na.rm = TRUE),
                           levels = 1:6, labels = iucn.levels) %>% as.character())
    
    rli.iucn.si <- sp.ds.1d %>%
      filter(speciesKey %in% sp.common.si) %>%
      left_join(red.list.c %>% dplyr::select(speciesKey, IUCN_official) %>%
                  rename(IUCN = IUCN_official)) %>%
      filter(!is.na(IUCN)) %>%
      mutate(num = factor(IUCN, levels = iucn.levels) %>% as.numeric(),
             wt  = 1 - (num - 1) / 5) %>%
      group_by(cell_poll) %>%
      reframe(RLI_iucn = mean(wt, na.rm = TRUE), SR = n()) %>%
      filter(SR > 5)
    
    rli.comb.si <- sp.ds.1d %>%
      filter(speciesKey %in% sp.merged$speciesKey) %>%
      left_join(sp.merged %>% dplyr::select(speciesKey, IUCN)) %>%
      filter(!is.na(IUCN)) %>%
      mutate(num = factor(IUCN, levels = iucn.levels) %>% as.numeric(),
             wt  = 1 - (num - 1) / 5) %>%
      group_by(cell_poll) %>%
      reframe(RLI_comb = mean(wt, na.rm = TRUE), SR = n()) %>%
      filter(SR > 5)
    
    rli.delta.si <- left_join(
      rli.iucn.si %>% dplyr::select(cell_poll, RLI_iucn),
      rli.comb.si %>% dplyr::select(cell_poll, RLI_comb),
      by = "cell_poll"
    ) %>% mutate(delta = RLI_comb - RLI_iucn)
    
    shp.d                    <- shp.null %>% left_join(rli.delta.si)
    shp.d$delta[!shp.d$land] <- NA
    
    v.d    <- shp.d$delta[!is.na(shp.d$delta)]
    lo.d   <- quantile(v.d, 0.01)
    hi.d   <- min(quantile(v.d, 0.99), 0)
    vals.d <- seq(lo.d, hi.d, length.out = length(mycol.delta))
    
    shp_ocean       <- shp.d[!shp.d$land, ]
    shp_land_nodata <- shp.d[shp.d$land & is.na(shp.d$delta), ]
    shp_valid       <- shp.d[shp.d$land & !is.na(shp.d$delta), ]
    shp_valid$value <- shp_valid$delta
    
    p.delta <- ggplot() +
      geom_sf(data = shp_ocean,       fill = "gray95", colour = "gray95") +
      geom_sf(data = shp_land_nodata, fill = "gray75", colour = "gray75") +
      geom_sf(data = shp_valid,       aes(fill = value, colour = value)) +
      scale_fill_gradientn(
        colors   = mycol.delta,
        values   = scales::rescale(vals.d),
        limits   = c(lo.d, hi.d),
        oob      = scales::squish,
        na.value = "gray95",
        name     = expression(Delta*"RLI"),
        breaks   = c(lo.d, hi.d),
        labels   = c('Increasing risk', 'No change'),
        guide    = guide_colourbar(
          direction      = "horizontal",
          barwidth       = unit(0.6, "in"),
          barheight      = unit(0.06, "in"),
          title.position = "top",
          title.hjust    = 0.5,
          ticks          = FALSE,
          title.theme    = element_text(size = 6, hjust = 0.5),
          label.theme    = element_text(size = 5, hjust = 0.5)
        )
      ) +
      scale_colour_gradientn(
        colors   = mycol.delta,
        values   = scales::rescale(vals.d),
        limits   = c(lo.d, hi.d),
        oob      = scales::squish,
        na.value = "gray95",
        guide    = "none"
      ) +
      scale_x_continuous(limits = c(-170, 170)) +
      labs(title = strategy_label) +
      theme_bw() +
      theme(
        panel.grid         = element_blank(),
        plot.title         = element_text(size = 8),
        legend.position    = 'none',#c(0.12, 0.25),
        legend.background  = element_rect(fill = "transparent", colour = NA),
        axis.text          = element_blank(),
        axis.ticks         = element_blank(),
        axis.title         = element_blank(),
        legend.box.spacing = unit(0.0, "in")
      )
    
    p.list.delta <- c(p.list.delta, list(p.delta))
  }
}

# ── Extract shared legend from a single plot ──────────────────────────────────
# Build a dummy plot just for the legend
p_legend_rli <- ggplot(shp) +
  geom_sf(aes(fill = value)) +
  scale_fill_gradientn(
    colors   = mycol,
    name     = "RLI",
    breaks   = c(range(shp$value,na.rm = T)[1], range(shp$value,na.rm = T)[2]),
    labels   = c('High risk', 'Low risk'),
    guide    = guide_colourbar(
      direction      = "horizontal",
      barwidth       = unit(1.8, "in"),
      barheight      = unit(0.05, "in"),
      title.position = "top",
      title.hjust    = 0.5,
      ticks          = FALSE,
      title.theme    = element_text(size = 8),
      label.theme    = element_text(size = 7)
    )
  ) +
  theme_void() +
  theme(legend.position = 'bottom')

shared_legend_rli <- cowplot::get_legend(p_legend_rli)

# ── Assemble ──────────────────────────────────────────────────────────────────
fig.rli <- (
  p.list.rli[[1]] + p.list.rli[[2]] +
    p.list.rli[[3]] + p.list.rli[[4]] +
    p.list.rli[[5]] + p.list.rli[[6]] +
    p.list.rli[[7]] + p.list.rli[[8]] +
    plot_layout(ncol = 2, byrow = FALSE)
) /
  wrap_elements(shared_legend_rli) +
  plot_layout(heights = c(1, 0.08))

# Same for delta
p_legend_delta <- ggplot(shp.d) +
  geom_sf(aes(fill = delta)) +
  scale_fill_gradientn(
    colors   = mycol.delta,
    name     = expression(Delta*"RLI"),
    breaks   = c(range(shp.d$delta,na.rm = T)[1], range(shp.d$delta,na.rm = T)[2]),
    labels   = c('Increasing risk', 'No change'),
    guide    = guide_colourbar(
      direction      = "horizontal",
      barwidth       = unit(1.8, "in"),
      barheight      = unit(0.05, "in"),
      title.position = "left",
      title.hjust    = 0.5,
      ticks          = FALSE,
      title.theme    = element_text(size = 8),
      label.theme    = element_text(size = 7)
    )
  ) +
  theme_void() +
  theme(legend.position = 'bottom')

shared_legend_delta <- cowplot::get_legend(p_legend_delta)

fig.delta <- (
  p.list.delta[[1]] + p.list.delta[[2]] +
    p.list.delta[[3]] + p.list.delta[[4]] +
    p.list.delta[[5]] + p.list.delta[[6]] +
    p.list.delta[[7]] + p.list.delta[[8]] +
    plot_layout(ncol = 2, byrow = FALSE)
) /
  wrap_elements(shared_legend_delta) +
  plot_layout(heights = c(1, 0.08))

ggsave(paste0(paths['sfig_SI'], 'map_SI.jpg'),
       plot = fig.rli,   width = 6, height = 6.5)
ggsave(paste0(paths['sfig_SI'], 'map_SI_delta.jpg'),
       plot = fig.delta, width = 6, height = 6.5)
