

##' subgroup analysis
##'   as a source code script prepared for the main script
##'   

p_all <- ma_result_all %>%
  dplyr::filter(group_name == subgroup_select) %>%
  dplyr::group_by(tool, ind_sub) %>%
  dplyr::mutate(ind_sub = factor(ind_sub, levels = ind_sub_levels)) %>%
  # dplyr::filter(ind_sub %in% ind_for_presentation) %>%
  # dplyr::filter(!subgroup %in% nature_type_remove_for_presentation) %>%
  as.data.frame()

p_all$subgroup <- factor(p_all$subgroup, levels = group_list)
# colors_group <- func_color_bygroup(df = p_all, column_name = "subgroup", color_pal = color_bygroup)
# color_bygroup <- colors_group


p_all %>%
  plot_effect_size_overall(data = .,
                           subgroup = 'subgroup', 
                           facet_bygroup = facet_decide, 
                           facet_scales = 'free',
                           text_size = 11,
                           add_gradient_bg = F,
                           show_legend = T) +
  scale_colour_manual(values = color_bygroup) +
  scale_x_continuous(expand = expansion(mult = c(0.2, 0.25))) +
  # scale_x_continuous(expand = expansion(add = c(0.25, 0.25))) +
  # this will allow the text outside of the plot panel
  coord_cartesian(clip = 'off', xlim = c(NA, NA), expand = TRUE) +
  theme(legend.position = c(0.75, 0.15),
        legend.spacing.y = unit(0, 'cm'),
        legend.spacing.x = unit(0, 'cm'),
        legend.key.size = unit(0.01, 'cm'), 
        legend.key.width = unit(0.01, 'cm'),
        legend.justification = "left", 
        legend.background = element_rect(fill='transparent'),
        plot.margin = margin(t = 0, r = 0.05, b = 0, l = 0, "in"),
        # panel.background = element_rect(colour = "gray50", linewidth=0.1, fill=NA), 
        panel.border = element_rect(linewidth=0.2), 
        strip.background = element_rect(linewidth=0.2), 
        legend.title=element_text(size=7, face = 'bold'),
        legend.text=element_text(size=6),
        panel.grid = element_blank(),
  ) +
  guides(color = guide_legend(title=group_title))

# p_all

fname <- paste0(dir.fig, 'es_comb_subgroup_', subgroup_select, '_', today, vv, '.png'); fname
func_ggsave(fname, w = 6, h = 6, save_png = T)