shp <- read_sf('data/map/data_1d/data_1d_land.shp')
shp <- shp[, c('cell_poll', 'land')]
shp$land <- shp$land %in% c('1', 'T', 'TRUE')
shp.null <- shp

sp.si    <- read.csv(paths['SI'])
sp.si$SI_GRID <- sp.si$SI_GRID_MAX
sp.si$SI_kNN  <- 1 / sp.si$SI_knn
sp.si    <- sp.si[, c('speciesKey', 'SI_GRID', 'SI_kNN')]
sp.ds.1d <- read.csv(paths["spds1d"])
spAss    <- read.csv(paths["spAssSim"])
spLS     <- read.csv(paths["spLS"])
spRange  <- sp.ds.1d %>% group_by(speciesKey) %>% reframe(range = n())

iucn.levels <- c('LC', 'NT', 'VU', 'EN', 'CR', 'EX')
red.list.c  <- read.csv(paths["IUCNredList"]) %>%
  filter(redlistCategory %in% iucn.levels) %>%
  dplyr::select(speciesKey, redlistCategory) %>%
  rename(IUCN_official = redlistCategory) %>%
  mutate(num_iucn = factor(IUCN_official, levels = iucn.levels) %>% as.numeric())

mypalette       <- colorRampPalette(c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred"))
mycol           <- rev(mypalette(100))
mypalette.delta <- colorRampPalette(c("#FFEE99","#FFCC00","#FF6600","#CC0000"))
mycol.delta     <- rev(mypalette.delta(100))

# ── Helper: compute ΔRLI ─────────────────────────────────────────────────────
compute_delta <- function(sp) {
  sp.common <- intersect(sp, red.list.c$speciesKey)
  
  rl.model <- spAss %>%
    filter(speciesKey %in% sp.common) %>%
    dplyr::select(speciesKey, wt_climC) %>%
    mutate(num_model = pmin(pmax(round((1 - wt_climC) * 5 + 1), 1L), 6L)) %>%
    dplyr::select(speciesKey, num_model)
  
  sp.merged <- left_join(
    red.list.c %>% filter(speciesKey %in% sp.common) %>%
      dplyr::select(speciesKey, num_iucn),
    rl.model, by = "speciesKey"
  ) %>%
    filter(!is.na(num_model)) %>%
    mutate(IUCN = factor(pmax(num_iucn, num_model, na.rm = TRUE),
                         levels = 1:6, labels = iucn.levels) %>% as.character())
  
  rli.iucn <- sp.ds.1d %>%
    filter(speciesKey %in% sp.common) %>%
    left_join(red.list.c %>% dplyr::select(speciesKey, IUCN_official) %>%
                rename(IUCN = IUCN_official)) %>%
    filter(!is.na(IUCN)) %>%
    mutate(num = factor(IUCN, levels = iucn.levels) %>% as.numeric(),
           wt  = 1 - (num - 1) / 5) %>%
    group_by(cell_poll) %>%
    reframe(RLI_iucn = mean(wt, na.rm = TRUE), SR = n()) %>%
    filter(SR > 5)
  
  rli.comb <- sp.ds.1d %>%
    filter(speciesKey %in% sp.merged$speciesKey) %>%
    left_join(sp.merged %>% dplyr::select(speciesKey, IUCN)) %>%
    filter(!is.na(IUCN)) %>%
    mutate(num = factor(IUCN, levels = iucn.levels) %>% as.numeric(),
           wt  = 1 - (num - 1) / 5) %>%
    group_by(cell_poll) %>%
    reframe(RLI_comb = mean(wt, na.rm = TRUE), SR = n()) %>%
    filter(SR > 5)
  
  left_join(
    rli.iucn %>% dplyr::select(cell_poll, RLI_iucn),
    rli.comb %>% dplyr::select(cell_poll, RLI_comb),
    by = "cell_poll"
  ) %>% mutate(delta = RLI_comb - RLI_iucn)
}

# ── Species sets ──────────────────────────────────────────────────────────────
sp.wide <- spRange$speciesKey[spRange$range > quantile(spRange$range, 0.75)]

sp.wide.si <- spRange %>%
  filter(range > 100) %>%
  filter(speciesKey %in%
           sp.si$speciesKey[sp.si$SI_GRID > quantile(sp.si$SI_GRID, 0.75, na.rm = TRUE)]) %>%
  pull(speciesKey)

# ── RLI map helper ────────────────────────────────────────────────────────────
draw_rli <- function(sp, title_text) {
  rli <- sp.ds.1d[sp.ds.1d[['speciesKey']] %in% sp, ] %>%
    left_join(spAss[, c('speciesKey', 'wt_climC')]) %>% na.omit() %>%
    group_by(cell_poll) %>%
    reframe(value = mean(wt_climC, na.rm = TRUE), SR = n())
  
  shp           <- shp.null %>% left_join(rli)
  shp$value[!shp$land]  <- NA
  shp$value[shp$SR < 5] <- NA
  
  v      <- shp$value[!is.na(shp$value)]
  lo     <- quantile(v, 0.01)
  hi     <- quantile(v, 0.99)
  vals_i <- seq(lo, hi, length.out = length(mycol))
  
  ggplot(shp) +
    geom_sf(aes(fill = value, colour = value)) +
    scale_fill_gradientn(
      colors   = mycol,
      values   = scales::rescale(vals_i),
      limits   = c(lo, hi),
      oob      = scales::squish,
      na.value = "gray95",
      name     = "RLI",
      guide    = guide_colourbar(
        direction      = "horizontal",
        barwidth       = unit(1.2, "in"),
        barheight      = unit(0.08, "in"),
        title.position = "left",
        title.vjust    = 0.8,
        ticks          = FALSE,
        title.theme    = element_text(size = 6),
        label.theme    = element_text(size = 5)
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
    labs(title = title_text) +
    theme_void() + theme_bw() +
    theme(
      panel.grid        = element_blank(),
      plot.title        = element_text(size = 8),
      legend.position   = "bottom",
      legend.direction  = "horizontal",
      legend.background = element_rect(fill = "transparent", colour = NA),
      axis.text         = element_blank(),
      axis.ticks        = element_blank(),
      axis.title        = element_blank()
    )
}

# ── ΔRLI map helper ───────────────────────────────────────────────────────────
draw_delta <- function(sp, title_text) {
  rli.delta <- compute_delta(sp)
  
  shp.d             <- shp.null %>% left_join(rli.delta)
  shp.d$delta[!shp.d$land] <- NA
  
  v.d    <- shp.d$delta[!is.na(shp.d$delta)]
  lo.d   <- quantile(v.d, 0.01)
  hi.d   <- min(quantile(v.d, 0.99), 0)
  vals.d <- seq(lo.d, hi.d, length.out = length(mycol.delta))
  
  shp_ocean       <- shp.d[!shp.d$land, ]
  shp_land_nodata <- shp.d[shp.d$land & is.na(shp.d$delta), ]
  shp_valid       <- shp.d[shp.d$land & !is.na(shp.d$delta), ]
  shp_valid$value <- shp_valid$delta
  
  ggplot() +
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
      guide    = guide_colourbar(
        direction      = "horizontal",
        barwidth       = unit(1.2, "in"),
        barheight      = unit(0.08, "in"),
        title.position = "left",
        title.vjust    = 0.8,
        ticks          = FALSE,
        title.theme    = element_text(size = 6),
        label.theme    = element_text(size = 5)
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
    labs(title = title_text) +
    theme_void() + theme_bw() +
    theme(
      panel.grid        = element_blank(),
      plot.title        = element_text(size = 8),
      legend.position   = "bottom",
      legend.direction  = "horizontal",
      legend.background = element_rect(fill = "transparent", colour = NA),
      axis.text         = element_blank(),
      axis.ticks        = element_blank(),
      axis.title        = element_blank()
    )
}

# ── Draw 4 panels with legend each ───────────────────────────────────────────
p1 <- draw_rli  (sp.wide,    '(a) Widely distributed species')
p2 <- draw_rli  (sp.wide.si, '(b) Widely distributed species & high SI')
p3 <- draw_delta(sp.wide,    '(c) Widely distributed species')
p4 <- draw_delta(sp.wide.si, '(d) Widely distributed species & high SI')

# ── Assemble 2×2 ─────────────────────────────────────────────────────────────

library(patchwork)
final <- (p1 + p2) / (p3 + p4)

ggsave(paste0(paths['sfig_SI'], 'map_wide_range.png'),
       plot = final, width = 7, height = 4.5)
# ggsave(paste0(paths['sfig_SI'], 'map_wide_range.pdf'),
#        plot = final, width = 8, height = 6.5)