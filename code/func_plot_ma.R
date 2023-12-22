# Install and load necessary packages
# install.packages(c("ggplot2", "metafor"))
library(ggplot2)
source('./code/func_ggsave.R')

# Create a forest plot-style ggplot

plot_effect_size_overall <- function(data) {
  
  x_limit_max <- max(abs(data$es.lower), abs(data$es.upper)) * 1.1
  
  g <- make_gradient(deg = 180, n = 500, cols = brewer.pal(9, "RdBu"))
  
  
  p <- data %>%
    dplyr::mutate(n_lab = paste0('n = ', n_study)) %>%
    ggplot(., 
           aes(x = es.mean, 
               y = reorder(ind_sub, es.mean))) +
    
    annotation_custom(grob = g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    # xlim(-x_limit_max, x_limit_max) +
    scale_x_continuous(n.breaks = 10, limits = c(-x_limit_max, x_limit_max)) + 
  
    geom_point(aes(x = es.mean), size = 3) +
    geom_errorbarh(aes(xmin = es.lower, xmax = es.upper), height = 0.2) +
    geom_text(aes(x = es.mean, label = n_lab), vjust = -1, size = 3) +
    geom_text(aes(x = es.mean, label = p.star), vjust = 1.5, size = 5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    labs(title = "",
         x = "Effect Size",
         y = "") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
  

  ## save plot ---------------------------------------------------------------------------
  #' how many sub-indicators -> decide plot height
  # height_tbd <- 1+3/7*length(unique(data$ind_sub))
  # p
  # fname <- paste0(dir.fig, 'plot_es_', unique(data$tool) ,'.png'); fname
  # func_ggsave(fname, w = 6, h = height_tbd, save_png = T)
  
  
  ## return plot -------------------------------------------------------------------------
  return(p)
  
}


## test
# plot_ma(data = ma_result)
# p1 <- plot_ma(data = ma_result)


