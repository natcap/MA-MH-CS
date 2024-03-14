# Install and load necessary packages
# install.packages(c("ggplot2", "metafor"))
library(ggplot2)
library(ggpubr)
source('./code/func_ggsave.R')
source('./code/func_make_gradient_bg.R')

# Create a forest plot-style ggplot

plot_effect_size_overall <- function(
    data, 
    color_var = NULL, ## e.g.,  color_var = "tool"
    xlab_name = "Effect Size", 
    subgroup = NULL, 
    dodge_value = 0.9,
    facet_bygroup = F,
    add_gradient_bg = T,
    show_legend = F) {
  
  x_limit_max <- max(abs(data$es.lower), abs(data$es.upper)) * 1.1
  
  gradient_bg <- make_gradient_bg(deg = 180, n = 500, cols = brewer.pal(9, "RdBu"))
  
  p <- data %>%
    dplyr::mutate(n_lab = paste0('n = ', n_study),
                  # I2_lab= paste0("I^{2} ==", I2*100, '~', "\"%\""),
                  I2_lab=  sprintf("italic(I)^2 == %.2f", I2), 
                  # I2_lab= paste0("I^{2} == ", I2)
                  )
  
  ## if to include subgroup analysis
  if (is.null(subgroup)) { 
    p <- p %>%
      ggplot(., 
             aes(x = es.mean, 
                 # y = reorder(ind_sub, desc(abs(es.mean))), # the largest effect on the top 
                 y = reorder(ind_sub, desc(es.mean)), # the largest effect on the top 
                 group = .data[[color_var]],
                 color = .data[[color_var]]))
  } else {
    p <- p %>%
      ggplot(., 
             aes(x = es.mean, 
                 y = reorder(ind_sub, desc(es.mean)), # the largest effect on the top 
                 color = !!sym(subgroup),
                 )) 
  }
  
  ## if to add gradient background color
  if(add_gradient_bg == T){
    p <- p + 
      annotation_custom(grob = gradient_bg, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
  } else {
    p <- p
  }
  
  p <- p + 
    geom_vline(xintercept = 0, linewidth = 0.4, color = "red", alpha = 0.2) + # linetype = "dashed", 
    ### SMD effect threshold values: small - moderate - large
    geom_vline(xintercept = -0.2, linewidth = 0.4, linetype = "dotted", color = "grey70") + ## , linewidth = 0.5
    geom_vline(xintercept = -0.5, linewidth = 0.4, linetype = "dotted", color = "grey70") +
    geom_vline(xintercept = -0.8, linewidth = 0.4, linetype = "dotted", color = "grey70") +
    geom_vline(xintercept =  0.2, linewidth = 0.4, linetype = "dotted", color = "grey70") +
    geom_vline(xintercept =  0.5, linewidth = 0.4, linetype = "dotted", color = "grey70") +
    geom_vline(xintercept =  0.8, linewidth = 0.4, linetype = "dotted", color = "grey70") +
    
    scale_x_continuous(#n.breaks = 10, 
                       # breaks = c(-0.8, -0.5, -0.2, 0, 0.2,  0.5,  0.8), 
                       limits = c(-x_limit_max, x_limit_max)) + 
  
    geom_point(aes(x = es.mean), size = 2, position = position_dodge(dodge_value), show.legend = show_legend) +
    geom_errorbarh(aes(xmin = es.lower, xmax = es.upper), height = 0.2, position=position_dodge(width = dodge_value), show.legend = F) +
    # geom_text(aes(x = es.mean, label = paste0(round(es.mean, digits = 2), ' ', p.star)), 
    #           vjust = 1.7, size = 2.5, position=position_dodge(dodge_value), show.legend = F) +
    geom_text(aes(x = es.mean, label = round(es.mean, digits = 2)), vjust = 1.7, size = 2.5, position=position_dodge(dodge_value), show.legend = F) +
    geom_text(aes(x = es.mean, label = p.star), vjust = 0, size = 4, position=position_dodge(dodge_value), show.legend = F) +
    labs(title = "", x = xlab_name, y = "") +
    guides(color = guide_legend(reverse=TRUE)) +
    scale_color_brewer(palette = "Dark2", direction = -1)
  
  ## for non-subgroup analysis
  if (is.null(subgroup)) { 
    p <- p +
      geom_text(aes(x = es.upper, label = n_lab), vjust = 0.5-0.1,  hjust = -0.2, size = 2.5, color='gray40', fontface = "italic",
                position=position_dodge(dodge_value), show.legend = F) +
      geom_text(aes(x = es.upper, label = I2_lab), vjust = 1.7, hjust = -0.1, size = 2.5, color='gray40', fontface = "italic",
                position=position_dodge(dodge_value), show.legend = F, parse = T)
    ## remove sample size for subgroups
  } else {
    p <- p
  }
  
  
  p <- p +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())
  
  ## when the number of subgroup is too large, it is better to facet the plot
  if (facet_bygroup == T) {
    p <- p +
      facet_wrap(~ind_sub, scales = 'free') +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            legend.spacing.y = unit(0, 'cm'),
            legend.spacing.x = unit(0, 'cm'),
            legend.key.size = unit(0.15, 'cm'),
            )
  }
  

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


