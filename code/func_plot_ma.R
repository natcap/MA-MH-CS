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
    
    ##' To keep the order of each Category unchanged in the legend while sorting the y-axis values within each facet based on x values, 
    ##' you can use a separate factor for the sorting within the facets and maintain the original factor levels for the legend. 
    ##' This approach involves creating two different variables:
    ##'     Original Category Variable: Used for the legend and keeps the original order of categories.
    ##'     Sorted Category Variable: Used for plotting within the facets, where the order is determined based on the x values within each facet.
    ##'     
    # Original order for the legend
    data$subgroup_original <- factor(data$subgroup, levels = unique(data$subgroup))
    
    # Step 1: Determine ordering within each facet based on mean Value
    ordering_info <- data %>%
      group_by(ind_sub) %>%
      arrange(es.mean) %>%
      dplyr::mutate(subgroup_sorted = factor(subgroup, levels = unique(subgroup)))
    
    # Step 2: Merge sorted category information back into original data frame
    data <- data %>%
      left_join(ordering_info %>% select(ind_sub, subgroup, subgroup_sorted), by = c("ind_sub", "subgroup"))
    
    p <- data %>%
      ##' plot
      ggplot(., 
             aes(x = es.mean, 
                 # y = reorder(ind_sub, desc(es.mean)), # the largest effect on the top 
                 y = subgroup_sorted, # the largest effect on the top
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
  

  
  ## the main plot function 
  p <- p + 
    geom_vline(xintercept = 0, linewidth = vline_width, color = vline_0_color, alpha = 0.5) + # linetype = "dashed", 
    
    ## 
    # scale_x_continuous(limits = c(-x_limit_max, x_limit_max)) + 
  
    geom_point(aes(x = es.mean), size = 2, position = position_dodge(dodge_value), show.legend = show_legend) +
    geom_errorbarh(aes(xmin = es.lower, xmax = es.upper), 
                   height = 0.15, position=position_dodge(width = dodge_value), show.legend = F) +
    # geom_text(aes(x = es.mean, label = paste0(round(es.mean, digits = 2), ' ', p.star)), 
    #           vjust = 1.7, size = 2.5, position=position_dodge(dodge_value), show.legend = F) +
    
    ## p value label
    geom_text(aes(x = es.mean, label = p.star), vjust = 0.1, size = text_size/4, position=position_dodge(dodge_value), show.legend = F) +
    labs(title = "", x = xlab_name, y = "") +
    scale_color_brewer(type = 'qual', palette = 'Dark2') +  ## Colorblind Friendly
    guides(color = guide_legend(reverse=F)) 
  
  
  
  
  ## 3. for non-subgroup analysis --------------------------------------------------------
  if (is.null(subgroup)) { 
    p <- p +
      ### SMD effect threshold values: small - moderate - large
      geom_vline(xintercept = -0.2, linewidth = 0.4, linetype = "dotted", color = "grey70") + ## , linewidth = 0.5
      geom_vline(xintercept = -0.5, linewidth = 0.4, linetype = "dotted", color = "grey70") +
      geom_vline(xintercept = -0.8, linewidth = 0.4, linetype = "dotted", color = "grey70") +
      geom_vline(xintercept =  0.2, linewidth = 0.4, linetype = "dotted", color = "grey70") +
      geom_vline(xintercept =  0.5, linewidth = 0.4, linetype = "dotted", color = "grey70") +
      geom_vline(xintercept =  0.8, linewidth = 0.4, linetype = "dotted", color = "grey70") +
      
      ### effect size label
      geom_text(aes(x = es.mean, label = round(es.mean, digits = 2)), 
                vjust = 1.7, size = text_size/4, 
                position=position_dodge(dodge_value), show.legend = F) +
      geom_text(aes(x = es.upper, label = n_lab), 
                vjust = 0,  hjust = -0.2, 
                size = text_size/4, color='gray30', fontface = "italic",
                position=position_dodge(dodge_value), show.legend = F) +
      geom_text(aes(x = es.upper, label = I2_lab), 
                vjust = 1, hjust = -0.15, 
                size = text_size/4, color='gray30', fontface = "italic",
                position=position_dodge(dodge_value), show.legend = F, parse = T)
    
    
    ## add annotated arrows and text
    p <- 
      p + 
      annotate("segment", x = 0, xend =  1.5, y = 0, yend = 0, colour = "#175E54", linewidth = .8, arrow = arrow(length = unit(0.1, "inches"), type = "closed")) +
      annotate("segment", x = 0, xend = -1.5, y = 0, yend = 0, colour = "#8C1515", linewidth = .8, arrow = arrow(length = unit(0.1, "inches"), type = "closed")) +
      annotate("text", x = 1.2,  y = 0, label = "Increase positive", colour = "#175E54", angle = 0, vjust = -1) +
      annotate("text", x = -1.2, y = 0, label = "Reduce negative",   colour = "#8C1515", angle = 0, vjust = -1) +
      #' Adjust the limits and aspect of the plot as needed
      #' clip off can ensure the full arrows can be shown on the axis
      coord_cartesian(clip = "off", ylim = c(NA, NA))
    
    
    
    ## remove sample size for subgroups
  } else {
    p <- p +
      
      ## effect size label
      # geom_text(aes(#x = es.mean/abs(es.mean)*(-2),
      #               # x = es.mean*1.1,
      #               x = es.mean,
      #               label = round(es.mean, digits = 1)),
      #           vjust = 1.7,
      #           size = text_size/5,
      #           position=position_dodge(dodge_value), show.legend = F) +

      geom_text(aes(x = ifelse(es.mean<0, 
                               es.lower,
                               es.upper
                               # ifelse((es.upper-es.lower)>3, 2.5*es.mean/abs(es.mean), es.lower), 
                               # ifelse((es.upper-es.lower)>3, 2.5*es.mean/abs(es.mean), es.upper)
                               ), 
                    hjust = ifelse((es.upper-es.lower)>2, 
                                   ifelse(es.mean<0, -(es.mean+es.lower)/20, -(es.mean)/30), 
                                   ifelse(abs(es.lower-es.mean)<0.5, -0.8*es.mean/abs(es.mean), -0.3*es.mean/abs(es.mean))),
                    label = n_lab),  
                vjust = -0.5, size = text_size/6, position=position_dodge(dodge_value), show.legend = F) +
      geom_text(aes(x = ifelse(es.mean<0, 
                               es.lower,
                               es.upper
                               # ifelse((es.upper-es.lower)>3, 2.5*es.mean/abs(es.mean), es.lower), 
                               # ifelse(es.upper>3, 2.5*es.mean/abs(es.mean), es.upper)
                               ), 
                    hjust = ifelse((es.upper-es.lower)>2, 
                                   ifelse(es.mean<0, -(es.mean+es.lower)/20, -(es.mean)/30), 
                                   ifelse(abs(es.lower-es.mean)<0.5, -0.8*es.mean/abs(es.mean), -0.3*es.mean/abs(es.mean))),
                    label = ifelse(n_study>1, I2_lab, NA)), 
                vjust = 1, size = text_size/6, position=position_dodge(dodge_value), show.legend = F, parse = T) +
      scale_x_continuous(expand = expansion(mult = c(0.2, 0.25))) 
  }
  
  
  if (!is.null(color_var)) {
    p <- p + scale_color_brewer(palette = "Dark2", direction = 1) ## Colorblind Friendly
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
      facet_wrap(~ind_sub, scales = facet_scales, ncol = 4) +
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
            legend.key.size = unit(0.1, 'cm'),
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


