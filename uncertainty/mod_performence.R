sp.tss <- read.csv(paths["spTSS"])

bins <- 20 #max(10, 21)
{
  # TSS
  x <- sp.tss$tss %>% as.numeric()
  x <- x[!is.na(x)]
  mu  <- mean(x)
  med <- median(x)
  
  lims <- quantile(x, c(0.001, 0.999), na.rm = TRUE)
  x_plot <- x[x >= lims[1] & x < lims[2]]
  df  <- tibble(value = x_plot)

  h <- hist(df$value, breaks = bins, plot = FALSE)
  y_box_tss <- max(h$density) + max(h$density)/5
  
  p_tss <- ggplot(df, aes(x = value)) +
    geom_histogram(aes(y = ..density..), bins = bins, fill = "#A6CEE3", color = "white", linewidth = .3) +
    geom_vline(xintercept = mu,  linetype = "dashed", linewidth = 0.7, color = "#4C78A8") +
    geom_vline(xintercept = med, linetype = "dotdash", linewidth = 0.7, color = "#72B7B2") +
    geom_rug(linewidth = 0.01,alpha = 0.15) +
    labs(x = "TSS", y = "Density") +
    theme_bw(base_size = 12) +
    theme(
      axis.ticks.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(0, 10, 5, 10)
    )+
    geom_boxplot(aes(x = value, y = y_box_tss),
                 stat = "boxplot",
                 inherit.aes = FALSE,
                 fill = "#D9E3F0", color = "#6B7A90", width = y_box_tss/20,
                 outlier.shape = 21, outlier.size = 2, outlier.alpha = 0.6)+
    annotate("text", x = mu-0.02,  y = y_box_tss+y_box_tss/15, label = paste0("Mean=", round(mu, 2)),
             vjust = -0.7, hjust = 1.05, size = 2.5, color = "#4C78A8") +
    annotate("text", x = med+0.01, y = y_box_tss+y_box_tss/15, label = paste0("Med.=", round(med, 2)),
             vjust = -0.7, hjust = 0, size = 2.5, color = "#72B7B2")+
    labs(title='(a)')+
    scale_x_continuous(breaks = seq(0, 1, by = 0.1),)
    scale_y_continuous(expand = expansion(mult = c(0.035, 0.07)))
  p_tss
  
  # AUC
  x <- sp.tss$auc %>% as.numeric()
  x <- x[!is.na(x)]
  mu  <- mean(x)
  med <- median(x)
  
  lims <- quantile(x, c(0.001, 0.999), na.rm = TRUE)
  x_plot <- x[x >= lims[1] & x < lims[2]]
  df  <- tibble(value = x_plot)
  
  h <- hist(df$value, breaks = bins, plot = FALSE)
  y_box_auv <- max(h$density) + max(h$density)/5
  
  p_auc<- ggplot(df, aes(x = value)) +
    geom_histogram(aes(y = ..density..), bins = bins, fill = "#A6CEE3", color = "white", linewidth = .3) +
    #geom_density(linewidth = 1.0, alpha = 0.9, color = "#1F78B4") +
    geom_vline(xintercept = mu,  linetype = "dashed", linewidth = 0.7, color = "#4C78A8") +
    geom_vline(xintercept = med, linetype = "dotdash", linewidth = 0.7, color = "#72B7B2") +
    geom_rug(linewidth = 0.01,alpha = 0.15) +
    labs(x = "AUC", y = "Density") +
    theme_bw(base_size = 12) +
    theme(
      axis.ticks.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(0, 10, 5, 10)
    )+
    geom_boxplot(aes(x = value, y = y_box_auv),
                 stat = "boxplot",
                 inherit.aes = FALSE,
                 fill = "#D9E3F0", color = "#6B7A90", width = y_box_auv/20,
                 outlier.shape = 21, outlier.size = 2, outlier.alpha = 0.6)+
    annotate("text", x = mu-0.02,  y = y_box_auv+y_box_auv/10, label = paste0("Mean=", round(mu, 2)),
             vjust = -0.6, hjust = 1.05, size = 2.5, color = "#4C78A8") +
    annotate("text", x = med+0.005, y = y_box_auv+y_box_auv/10, label = paste0("Med.=", round(med, 2)),
             vjust = -0.6, hjust = 0, size = 2.5, color = "#72B7B2")+
    labs(title='(b)') +
    scale_y_continuous(expand = expansion(mult = c(0.035, 0.07)))
  p_auc
}
p <- p_tss + p_auc + plot_layout(nrow=1)
p
ggsave(paths['sfig_mod_summary'],plot=p,width = 6.7,height = 3.5,dpi = 300)

