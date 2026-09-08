# ══════════════════════════════════════════════════════════════════════════════
# Subsampling sensitivity analysis: RLI maps + ΔRLI maps vs IUCN official
# ══════════════════════════════════════════════════════════════════════════════

library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)

dir.path <- 'data_modOutput/v4/subSampling/'
set.seed(456)

iucn.levels <- c('LC', 'NT', 'VU', 'EN', 'CR', 'EX')

# ── 1. Build species-level threat categories for each subsampling combination ──

sp.info <- data.frame()

for (n1 in c('GRID', 'kNN')) {
  for (n2 in c('75', '50', '25')) {
    
    sp.range   <- read.csv(paste0(dir.path, n1, '_', n2, '_spInfo.csv'))
    sp.timespan <- read.csv(paths['spMarkAll'])
    
    # Keep baseline (1981-2010) and three future periods
    sp.range <- sp.range[, c("speciesKey", "sn_1981.2010",
                             "sn_2011.2040_mean_ssp370_DS_limit",
                             "sn_2041.2070_mean_ssp370_DS_limit",
                             "sn_2071.2100_mean_ssp370_DS_limit")]
    
    # Range loss rate (%) relative to baseline
    sp.range[, -(1:2)] <- (sp.range[, 2] - sp.range[, -(1:2)]) / sp.range[, 2] * 100
    sp.range <- left_join(sp.range, sp.timespan)
    
    # Classify into IUCN categories by range loss thresholds
    for (i in 3:5) {
      flag4 <- which(sp.range[, i] >= 100); sp.range[flag4, i] <- 99.9
      flag1 <- which(sp.range[, i] <= 0);   sp.range[flag1, i] <- -0.1
      sp.range[, i] <- cut(sp.range[, i], c(-1, 5, 30, 50, 80, 100)) %>% as.numeric()
      sp.range[, i] <- factor(sp.range[, i], levels = 1:5,
                              labels = c('LC','NT','VU','EN','CR')) %>% as.character()
    }
    
    sp.a3c <- sp.range
    
    # Assign each species the category matching its focal time period
    for (ds in c('DS_limit')) {
      for (sn in c('ssp370')) {
        sp.a3c$tmp <- NA
        for (year in c('2011-2040', '2041-2070', '2071-2100')) {
          flag <- which(sp.a3c$timespan %in% year)
          sp.a3c[flag, 'tmp'] <- sp.a3c[flag, paste0('sn_', str_replace(year, '-', '.'),
                                                     '_mean_', sn, '_', ds)]
        }
        colnames(sp.a3c)[colnames(sp.a3c) %in% 'tmp'] <- paste0(sn, '_', ds)
      }
    }
    
    sp.a3c <- sp.a3c[, c('speciesKey', 'ssp370_DS_limit')]
    
    # Mark functionally extinct species (future range = 0, baseline > 0)
    sp.ex <- read.csv(paste0(dir.path, n1, '_', n2, '_spInfo.csv'))
    sp.ex <- sp.ex[, c("speciesKey", "forEX_sn_1981.2010",
                       "forEX_sn_2011.2040_mean_ssp370",
                       "forEX_sn_2041.2070_mean_ssp370",
                       "forEX_sn_2071.2100_mean_ssp370")]
    for (i in 3:5) sp.ex[, i] <- (sp.ex[, i] == 0) & (sp.ex[, 2] != 0)
    
    sp.a3c <- left_join(sp.a3c, sp.timespan[, c('speciesKey', 'timespan')])
    
    for (ds in c('DS_limit')) {
      for (sn in c('ssp370')) {
        for (year in c('2011-2040', '2041-2070', '2071-2100')) {
          col  <- paste0('sn_', str_replace(year, '-', '.'), '_mean_', sn)
          flag <- which((sp.a3c$timespan %in% year) &
                          (!is.na(sp.a3c[, paste0(sn, '_', ds)])) &
                          (sp.a3c$speciesKey %in%
                             sp.ex$speciesKey[sp.ex[, str_replace(col, 'sn_', 'forEX_sn_')]]))
          sp.a3c[flag, paste0(sn, '_', ds)] <- 'EX'
        }
      }
    }
    
    sp.a3c <- sp.a3c[, 1:2]
    colnames(sp.a3c)[2] <- paste(n1, n2, sep = '_')
    
    if (nrow(sp.info) > 0) {
      sp.info <- left_join(sp.info, sp.a3c)
    } else {
      sp.info <- sp.a3c
    }
  }
}

# Subsample to 20,000 species for computational efficiency
sp.info <- sp.info[sample(nrow(sp.info), 2e4), ]

# ── 2. Compute per-cell RLI for each subsampling combination ──────────────────

sp.ds.1d.raw <- read.csv(paths["spds1d"])
cell.info    <- data.frame()

for (n1 in c('GRID', 'kNN')) {
  for (n2 in c('75', '50', '25')) {
    col_name <- paste(n1, n2, sep = '_')
    
    red.list      <- sp.info[, c('speciesKey', col_name)]
    colnames(red.list)[2] <- 'IUCN'
    
    sp.ds.1d      <- left_join(sp.ds.1d.raw, red.list)
    sp.ds.1d      <- sp.ds.1d[!is.na(sp.ds.1d$IUCN), ]
    sp.ds.1d$IUCN <- factor(sp.ds.1d$IUCN, levels = iucn.levels) %>% as.numeric()
    sp.ds.1d$wt   <- 1 - (sp.ds.1d$IUCN - 1) / max(sp.ds.1d$IUCN - 1, na.rm = TRUE)
    
    rli <- sp.ds.1d %>%
      group_by(cell_poll) %>%
      mutate(RLI = mean(wt, na.rm = TRUE), SR = n()) %>%
      ungroup() %>%
      dplyr::select(cell_poll, RLI, SR) %>%
      distinct() %>%
      filter(SR > 10) %>%
      dplyr::select(cell_poll, RLI)
    colnames(rli)[2] <- col_name
    
    if (nrow(cell.info) > 0) {
      cell.info <- left_join(cell.info, rli)
    } else {
      cell.info <- rli
    }
  }
}

# Full model prediction RLI (ssp370, DS_limit, all species)
red.list.full <- read.csv(paths["A3c"])
red.list.full$IUCN <- red.list.full[["ssp370_DS_limit"]]
red.list.full <- red.list.full %>%
  filter(speciesKey %in% sp.info$speciesKey) %>%
  dplyr::select(speciesKey, IUCN)

sp.ds.1d      <- left_join(sp.ds.1d.raw, red.list.full)
sp.ds.1d      <- sp.ds.1d[!is.na(sp.ds.1d$IUCN), ]
sp.ds.1d$IUCN <- factor(sp.ds.1d$IUCN, levels = iucn.levels) %>% as.numeric()
sp.ds.1d$wt   <- 1 - (sp.ds.1d$IUCN - 1) / max(sp.ds.1d$IUCN - 1, na.rm = TRUE)

rli.full <- sp.ds.1d %>%
  group_by(cell_poll) %>%
  mutate(RLI = mean(wt, na.rm = TRUE), SR = n()) %>%
  ungroup() %>%
  dplyr::select(cell_poll, RLI, SR) %>%
  distinct() %>%
  filter(SR > 10) %>%
  dplyr::select(cell_poll, RLI)
colnames(rli.full)[2] <- 'Full'

cell.info <- left_join(rli.full, cell.info)

# R² of each subsampling combination vs Full
cat("R² of subsampling vs Full prediction:\n")
for (i in 3:ncol(cell.info)) {
  m <- lm(cell.info[[i]] ~ cell.info[[2]]) %>% summary()
  cat(colnames(cell.info)[i], ":", round(m$r.squared, 4), "\n")
}

# ── 3. IUCN official Red List — build combined (max) threat category ──────────

red.list.c <- read.csv(paths["IUCNredList"]) %>%
  filter(redlistCategory %in% iucn.levels) %>%
  dplyr::select(speciesKey, redlistCategory) %>%
  rename(IUCN_official = redlistCategory)

# Restrict to species present in both IUCN official list and model predictions
sp.common <- intersect(red.list.c$speciesKey,
                       red.list.full$speciesKey[!is.na(red.list.full$IUCN)])
cat("Species in common for fair comparison:", length(sp.common), "\n")

red.list.c    <- red.list.c    %>% filter(speciesKey %in% sp.common)
red.list.full <- red.list.full %>% filter(speciesKey %in% sp.common)
sp.info.filt  <- sp.info       %>% filter(speciesKey %in% sp.common)

# Numeric rank for IUCN official
red.list.c$num_iucn <- factor(red.list.c$IUCN_official,
                              levels = iucn.levels) %>% as.numeric()

# ── Helper: compute per-cell RLI from a species threat table ──────────────────
compute_rli <- function(sp_threat_df) {
  # sp_threat_df must have columns: speciesKey, IUCN (character)
  tmp      <- left_join(sp.ds.1d.raw, sp_threat_df, by = "speciesKey")
  tmp      <- tmp[!is.na(tmp$IUCN), ]
  tmp$num  <- factor(tmp$IUCN, levels = iucn.levels) %>% as.numeric()
  tmp$wt   <- 1 - (tmp$num - 1) / 5
  rli_out  <- tmp %>%
    group_by(cell_poll) %>%
    mutate(RLI = mean(wt, na.rm = TRUE), SR = n()) %>%
    ungroup() %>%
    dplyr::select(cell_poll, RLI, SR) %>%
    distinct() %>%
    filter(SR > 10) %>%
    dplyr::select(cell_poll, RLI)
  return(rli_out)
}

# ── 4. Compute ΔRLI = RLI(max[IUCN, model]) − RLI(IUCN official) ─────────────

delta.info <- compute_rli(
  red.list.c %>% dplyr::select(speciesKey) %>%
    mutate(IUCN = red.list.c$IUCN_official)
) %>% rename(RLI_iucn = RLI)

# Full model: take more pessimistic category per species
sp.full.merged <- left_join(
  red.list.c    %>% dplyr::select(speciesKey, num_iucn),
  red.list.full %>% mutate(num_model = factor(IUCN, levels = iucn.levels) %>% as.numeric()) %>%
    dplyr::select(speciesKey, num_model),
  by = "speciesKey"
) %>% filter(!is.na(num_model))

sp.full.merged$IUCN <- factor(pmax(sp.full.merged$num_iucn,
                                   sp.full.merged$num_model, na.rm = TRUE),
                              levels = 1:6, labels = iucn.levels) %>% as.character()

rli.combined <- compute_rli(sp.full.merged %>% dplyr::select(speciesKey, IUCN))
colnames(rli.combined)[2] <- 'RLI_combined'
delta.info <- left_join(delta.info, rli.combined)

# Subsampling combinations: take more pessimistic category per species
for (n1 in c('GRID', 'kNN')) {
  for (n2 in c('75', '50', '25')) {
    col_name <- paste(n1, n2, sep = '_')
    
    rl_sub <- sp.info.filt %>%
      dplyr::select(speciesKey, all_of(col_name)) %>%
      rename(IUCN_model = !!col_name) %>%
      filter(!is.na(IUCN_model)) %>%
      mutate(num_model = factor(IUCN_model, levels = iucn.levels) %>% as.numeric())
    
    sp.sub.merged <- left_join(
      red.list.c %>% dplyr::select(speciesKey, num_iucn),
      rl_sub     %>% dplyr::select(speciesKey, num_model),
      by = "speciesKey"
    ) %>% filter(!is.na(num_model)) %>%
      mutate(IUCN = factor(pmax(num_iucn, num_model, na.rm = TRUE),
                           levels = 1:6, labels = iucn.levels) %>% as.character())
    
    rli_tmp <- compute_rli(sp.sub.merged %>% dplyr::select(speciesKey, IUCN))
    colnames(rli_tmp)[2] <- col_name
    delta.info <- left_join(delta.info, rli_tmp)
  }
}

# ΔRLI (always ≤ 0: taking the max can only lower or keep RLI)
delta.cols <- c('RLI_combined', 'GRID_75', 'GRID_50', 'GRID_25',
                'kNN_75', 'kNN_50', 'kNN_25')
for (col in delta.cols) {
  delta.info[[paste0('d_', col)]] <- delta.info[[col]] - delta.info$RLI_iucn
}

cat("ΔRLI range per combination:\n")
print(apply(delta.info[, paste0('d_', delta.cols)], 2,
            function(x) range(x, na.rm = TRUE)))

# ══════════════════════════════════════════════════════════════════════════════
# 5. Load shapefile and attach data
# ══════════════════════════════════════════════════════════════════════════════

shp.base <- read_sf(paths['baseMap'])

# Extract max latitude per cell polygon
tmp <- st_coordinates(shp.base) %>% as.data.frame() %>%
  group_by(L3) %>% mutate(Y = max(Y)) %>% ungroup() %>%
  dplyr::select(L3, Y) %>% distinct()
shp.base$lat  <- tmp$Y
shp.base$land <- shp.base$land %in% c('T', 'TRUE', '1')
shp.base <- shp.base[, c('cell_poll', 'land', 'lat')]

# ══════════════════════════════════════════════════════════════════════════════
# 6. Figure A — RLI maps (original style)
# ══════════════════════════════════════════════════════════════════════════════

mypalette <- colorRampPalette(c("#70BE50","#FFF204","#F89D57","#ED1C2E","darkred"))
mycol     <- rev(mypalette(100))
vals      <- seq(0.4, 1, length.out = length(mycol))

shp <- read_sf(paths['baseMap'])
shp$land <- shp$land %in% c(1, 'T', 'TRUE')
shp <- shp[, c('cell_poll', 'land')]
shp <- left_join(shp, cell.info)
shp$RLI[!shp$land] <- NA
colnames(shp)[4] <- 'Full'

p.list <- list()
for (i in c("GRID_25","GRID_50","GRID_75",'Full',
            "kNN_25","kNN_50","kNN_75")) {
  shp$value <- shp[[i]]
  lab_text  <- i %>%
    str_replace('_', ' (subsampling to ') %>%
    paste0('th)') %>%
    str_replace('llth\\)', 'll')
  
  p <- ggplot(shp) +
    geom_sf(aes(fill = value, colour = value)) +
    scale_fill_gradientn(
      colors = mycol,
      values = scales::rescale(vals, from = c(0.4, 1)),
      limits = c(0.4, 1),
      oob    = scales::squish,
      na.value = "gray95",
      name   = "RLI"
    ) +
    scale_colour_gradientn(
      colors = mycol,
      values = scales::rescale(vals, from = c(0.4, 1)),
      limits = c(0.4, 1),
      oob    = scales::squish,
      na.value = 'gray95',
      guide  = 'none'
    ) +
    scale_x_continuous(limits = c(-170, 170)) +
    labs(title = lab_text) +
    theme_void() +
    theme_bw() +
    theme(
      panel.grid         = element_blank(),
      legend.position    = 'none',
      title              = element_text(size = 6),
      legend.text        = element_text(size = 6),
      axis.ticks.x       = element_blank(),
      axis.text.x        = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.box.spacing = unit(0.0, "in"),
      legend.key.height  = unit(0.05, "in"),
      legend.key.width   = unit(0.3, "in")
    )
  p.list <- c(p.list, list(p))
}

# Add legend to Full panel (index 4)
p.list[[4]] <- p.list[[4]] +
  theme(legend.position = 'bottom')

p0 <- ggplot() + theme_void()

fig.rli <- p.list[[1]] + p.list[[2]] + p.list[[3]] + p.list[[4]] +
  p.list[[5]] + p.list[[6]] + p.list[[7]] + p0 +
  plot_layout(ncol = 2, byrow = FALSE)

print(fig.rli)
ggsave(paste0(paths["sfig_subsampl_map"], 'RLI_subsampling.jpg'),
       plot = fig.rli, width = 6,height = 6.2)
# ggsave(paste0(paths["sfig_subsampl_map"], 'RLI_subsampling.pdf'),
#        plot = fig.rli, ,width = 6,height = 6.2)
# ggsave(paste0(paths["sfig_subsampl_map"], 'RLI_subsampling.jpg'),
#        plot = fig.rli, width = 10, height = 12, dpi = 300)

# ══════════════════════════════════════════════════════════════════════════════
# 7. Figure B — ΔRLI maps (same style as Figure A, red→yellow palette)
# ══════════════════════════════════════════════════════════════════════════════
# Palette: deep red (large negative ΔRLI) → near white (ΔRLI near 0)
mypalette.delta <- colorRampPalette(c("#FFEE99","#FFCC00","#FF6600","#CC0000"))
mycol.delta     <- rev(mypalette.delta(100))

# Value anchor points
delta_lim <- max(abs(delta.info[, paste0('d_', delta.cols)]), na.rm = TRUE)
delta_lim <- ceiling(delta_lim * 20) / 20
cat("ΔRLI colour scale limit:", -delta_lim, "to 0\n")
vals.delta <- seq(-delta_lim, 0, length.out = length(mycol.delta))

# Attach delta values to shapefile
shp.delta <- read_sf(paths['baseMap'])
shp.delta$land <- shp.delta$land %in% c(1, 'T', 'TRUE')
shp.delta <- shp.delta[, c('cell_poll', 'land')]
shp.delta <- left_join(shp.delta, delta.info)
for (dc in paste0('d_', delta.cols)) shp.delta[[dc]][!shp.delta$land] <- NA

label_map.delta <- c(
  'RLI_combined' = 'Full',
  'GRID_75'      = 'GRID (subsampling to 75th)',
  'GRID_50'      = 'GRID (subsampling to 50th)',
  'GRID_25'      = 'GRID (subsampling to 25th)',
  'kNN_75'       = 'kNN (subsampling to 75th)',
  'kNN_50'       = 'kNN (subsampling to 50th)',
  'kNN_25'       = 'kNN (subsampling to 25th)'
)

p.list.delta <- list()
plot.order.delta <- c("GRID_25","GRID_50","GRID_75","RLI_combined",
                      "kNN_25", "kNN_50", "kNN_75")

for (col in plot.order.delta) {
  dc <- paste0('d_', col)
  shp.delta$value <- as.numeric(shp.delta[[dc]])
  
  # Split into three non-overlapping subsets to avoid NA overwriting grey
  shp_ocean       <- shp.delta[!shp.delta$land, ]
  shp_land_nodata <- shp.delta[shp.delta$land & is.na(shp.delta$value), ]
  shp_valid       <- shp.delta[shp.delta$land & !is.na(shp.delta$value), ]
  
  p <- ggplot() +
    # Layer 1: ocean
    geom_sf(data = shp_ocean,
            fill = "gray95", colour = "gray95") +
    # Layer 2: land with no species data → grey, excluded from legend
    geom_sf(data = shp_land_nodata,
            fill = "gray75", colour = "gray75") +
    # Layer 3: land with valid ΔRLI → colour scale
    geom_sf(data = shp_valid,
            aes(fill = value, colour = value)) +
    scale_fill_gradientn(
      colors   = mycol.delta,
      values   = scales::rescale(vals.delta, from = c(-delta_lim, 0)),
      limits   = c(-delta_lim, 0),
      oob      = scales::squish,
      na.value = "gray95",
      name     = expression(Delta*"RLI")
    ) +
    scale_colour_gradientn(
      colors   = mycol.delta,
      values   = scales::rescale(vals.delta, from = c(-delta_lim, 0)),
      limits   = c(-delta_lim, 0),
      oob      = scales::squish,
      na.value = 'gray95',
      guide    = 'none'
    ) +
    scale_x_continuous(limits = c(-170, 170)) +
    labs(title = label_map.delta[col]) +
    theme_void() +
    theme_bw() +
    theme(
      panel.grid         = element_blank(),
      legend.position    = 'none',
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      title              = element_text(size = 6),
      legend.text        = element_text(size = 6),
      axis.ticks.x       = element_blank(),
      axis.text.x        = element_blank(),
      legend.box.spacing = unit(0.0, "in"),
      legend.key.height  = unit(0.05, "in"),
      legend.key.width   = unit(0.3, "in")
    )
  p.list.delta <- c(p.list.delta, list(p))
}

# Add legend to Full panel (index 4)
p.list.delta[[4]] <- p.list.delta[[4]] +
  theme(legend.position = 'bottom')

fig.delta <- p.list.delta[[1]] + p.list.delta[[2]] + p.list.delta[[3]] + p.list.delta[[4]] +
  p.list.delta[[5]] + p.list.delta[[6]] + p.list.delta[[7]] + p0 +
  plot_layout(ncol = 2, byrow = FALSE)

print(fig.delta)
ggsave(paste0(paths["sfig_subsampl_map"], 'deltaRLI_subsampling.jpg'),
       plot = fig.delta, width = 6, height = 6.2)
