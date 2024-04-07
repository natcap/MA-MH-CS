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
    facet_scales = 'free',
    add_gradient_bg = T,
    text_size = 12,
    show_legend = F) {
  
  theme_set(theme_minimal() + 
              theme(text = element_text(size = text_size), 
                    axis.text = element_text(size = text_size),
                    strip.text = element_text(size = text_size)
                    )) # Applies a base size
  
  # x_limit_max <- max(abs(data$es.lower), abs(data$es.upper), na.rm = T) * 1.1
  
  gradient_bg <- make_gradient_bg(deg = 180, n = 500, cols = brewer.pal(9, "RdBu"))
  
  data <- data %>%
    dplyr::mutate(n_lab = paste0('n = ', n_study),
                  # I2_lab= paste0("I^{2} ==", I2*100, '~', "\"%\""),
                  I2_lab=  sprintf("italic(I)^2 == %.2f", I2), 
                  # I2_lab= paste0("I^{2} == ", I2)
                  ind_sub = as.factor(ind_sub)
                  )
  
  
  ## 1. if to include subgroup analysis --------------------------------------------------
  if (is.null(subgroup)) { 
    
    if (!is.null(color_var)) {
      p <- data %>%
        ggplot(., 
               aes(x = es.mean, 
                   # y = ind_sub, 
                   # y = reorder(ind_sub, desc(abs(es.mean))), # the largest effect on the top
                   y = reorder(ind_sub, desc(es.mean)),      # the largest effect on the top
                   group = .data[[color_var]],
                   color = .data[[color_var]]
                   ))
    } else {
      p <- data %>%
        ggplot(., 
               aes(x = es.mean, 
                   # y = ind_sub, 
                   # y = reorder(ind_sub, desc(abs(es.mean))),
                   y = reorder(ind_sub, desc(es.mean)),
               ))
    }
    
    
  } else {
    p <- data %>%
      ggplot(., 
             aes(x = es.mean, 
                 y = reorder(ind_sub, desc(es.mean)), # the largest effect on the top 
                 color = !!sym(subgroup),
                 )) 
  }
  
  
  
  ## 2. if to add gradient background color ----------------------------------------------
  if(add_gradient_bg == T){
    p <- p + 
      annotation_custom(grob = gradient_bg, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
    vline_0_color = "gray70"
    vline_width = 0.6
  } else {
    p <- p
    vline_0_color = "grey50"
    vline_width = 0.4
  }
  

  
  
  p <- p + 
    geom_vline(xintercept = 0, linewidth = vline_width, color = vline_0_color, alpha = 0.5) + # linetype = "dashed", 
    ### SMD effect threshold values: small - moderate - large
    geom_vline(xintercept = -0.2, linewidth = 0.4, linetype = "dotted", color = "grey70") + ## , linewidth = 0.5
    geom_vline(xintercept = -0.5, linewidth = 0.4, linetype = "dotted", color = "grey70") +
    geom_vline(xintercept = -0.8, linewidth = 0.4, linetype = "dotted", color = "grey70") +
    geom_vline(xintercept =  0.2, linewidth = 0.4, linetype = "dotted", color = "grey70") +
    geom_vline(xintercept =  0.5, linewidth = 0.4, linetype = "dotted", color = "grey70") +
    geom_vline(xintercept =  0.8, linewidth = 0.4, linetype = "dotted", color = "grey70") +
    
    ## 
    # scale_x_continuous(limits = c(-x_limit_max, x_limit_max)) + 
  
    geom_point(aes(x = es.mean), size = 2, position = position_dodge(dodge_value), show.legend = show_legend) +
    geom_errorbarh(aes(xmin = es.lower, xmax = es.upper), 
                   height = 0.15, position=position_dodge(width = dodge_value), show.legend = F) +
    # geom_text(aes(x = es.mean, label = paste0(round(es.mean, digits = 2), ' ', p.star)), 
    #           vjust = 1.7, size = 2.5, position=position_dodge(dodge_value), show.legend = F) +
    
    ## p value label
    geom_text(aes(x = es.mean, label = p.star), 
              vjust = 0.21, size = text_size/4, position=position_dodge(dodge_value), show.legend = F) +
    labs(title = "", x = xlab_name, y = "") +
    guides(color = guide_legend(reverse=F)) 
  
  
  ## add annotated arrows and text
  p <- 
    p + 
    annotate("segment", x = 0, xend =  1.5, y = 0, yend = 0, colour = "#175E54", linewidth = .8, arrow = arrow(length = unit(0.1, "inches"), type = "closed")) +
    annotate("segment", x = 0, xend = -1.5, y = 0, yend = 0, colour = "#8C1515", linewidth = .8, arrow = arrow(length = unit(0.1, "inches"), type = "closed")) +
    annotate("text", x = 1,  y = 0, label = "Increase positive", colour = "#175E54", angle = 0, vjust = -1) +
    annotate("text", x = -1, y = 0, label = "Reduce negative",   colour = "#8C1515", angle = 0, vjust = -1) +
    #' Adjust the limits and aspect of the plot as needed
    #' clip off can ensure the full arrows can be shown on the axis
    coord_cartesian(clip = "off", ylim = c(NA, NA))
  
  ## 3. for non-subgroup analysis --------------------------------------------------------
  if (is.null(subgroup)) { 
    p <- p +
      ## effect size label
      geom_text(aes(x = es.mean, label = round(es.mean, digits = 2)), 
                vjust = 1.7, size = text_size/4, 
                position=position_dodge(dodge_value), show.legend = F) +
      geom_text(aes(x = es.upper, label = n_lab), 
                vjust = 0,  hjust = -0.2, 
                size = text_size/4, color='gray40', fontface = "italic",
                position=position_dodge(dodge_value), show.legend = F) +
      geom_text(aes(x = es.upper, label = I2_lab), 
                vjust = 1, hjust = -0.15, 
                size = text_size/4, color='gray40', fontface = "italic",
                position=position_dodge(dodge_value), show.legend = F, parse = T)
    ## remove sample size for subgroups
  } else {
    p <- p +
      ## effect size label
      geom_text(aes(#x = es.mean/abs(es.mean)*(-2), 
                    # x = es.mean*1.1,
                    x = es.mean,
                    vjust = 1.7, 
                    label = round(es.mean, digits = 1)), 
                size = text_size/5, 
                position=position_dodge(dodge_value), show.legend = F) 
  }
  
  
  if (!is.null(color_var)) {
    p <- p + scale_color_brewer(palette = "Dark2", direction = -1) 
  } else {
    p <- p
  }
  
  p <- p +
    # theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())
  
  ## when the number of subgroup is too large, it is better to facet the plot
  if (facet_bygroup == T) {
    p <- p +
      facet_wrap(~ind_sub, scales = facet_scales) +
      theme_bw() +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            # plot.margin = margin(t = 5, r = 5, b = 5, l = 5, "points"),
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


