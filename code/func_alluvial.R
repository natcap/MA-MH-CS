
library(ggalluvial)

func_alluvial <- function(data, 
                          sorted = NA,
                          indicator_n_min = 5, 
                          width_my = 1/2.5, 
                          w_p = 6,
                          labele_small = 15,
                          n_ctr = '',
                          show_y_ticks = T, 
                          filename.prefix = 'mini-review_',
                          filename.postfix = '') {
  p <- data %>%
    dplyr::filter(freq >= indicator_n_min) %>%
    ggplot(
      .,
      aes(
        x = dimension,
        y = freq,
        alluvium = id_within_layers,
        stratum = layers,
        fill = layers
      )
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    # scale_fill_manual(values = getPalette(colourCount)) +
    # geom_flow(width = width_my) +
    geom_flow(
      width = width_my,
      # aes.bind = TRUE,
      decreasing = sorted
    ) +
    geom_stratum(
      alpha = .6, width = width_my,
      decreasing = sorted
    ) +
    stat_stratum(
      geom = "text",
      aes(label = ifelse(as.numeric(total) >= labele_small, layers, NA)),
      decreasing = sorted
    ) +
    ggrepel::geom_text_repel(
      decreasing = sorted,
      aes(label = ifelse(as.numeric(total) < labele_small, as.character(layers), NA)),
      # segment.square  = F,
      # segment.inflect = T,
      segment.size = 0.3,
      segment.alpha = .7,
      segment.curvature = 0.2, ## negative for left-hand and positive for right-hand curves, 0 for straight lines
      # segment.curvature = -1e-20,
      segment.ncp = 3,
      segment.angle = 20, ## values greater than 90 would skew toward the end
      
      nudge_x = .4,
      nudge_y = .5,
      force_pull = 1,
      direction = "both",
      stat = "stratum",
      size = 3
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      plot.margin = margin(0, 0, 0, 0, "in"),
      panel.spacing = unit(0, "in"),
      axis.title = element_blank(),
      axis.line = element_blank(),
      
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(
        size = rel(1.4),
        vjust = 10,
        margin = margin(t = 0, r = 0, b = 0, l = 0)
      )
    )
  
  if (show_y_ticks == F) {
    p <- p + 
      theme(axis.ticks.y = element_blank(), axis.text.y  = element_blank())
  } else (
    p <- p + 
      theme(axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))
  )
  
  
  f <- paste0(filename.prefix, filename.postfix, '_min', indicator_n_min, '_ctr', n_ctr, '_', today, '.png')
  fname <- paste0(dir.fig, f); print(fname)
  ggsave(filename = fname, plot = p, width = w_p, height = 8, units = 'in', dpi = 300)
  
  return(p)
}
