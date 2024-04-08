

##' subgroup analysis
##'   as a source code script prepared for the main script
##'   

##' plot tool - 1 ------------------------------------------------------------------------
p1_tool <- ma_result_all_sub %>%
  # data.table::as.data.table() %>%
  dplyr::filter(tool == mh_tools[1]); 


##' the number of categories of a subgroup
##' use this to decide whether to facet a plot 
len <- length(unique(p1_tool$subgroup)); len
facet_decide <- ifelse(len >1, T, F)

##' add named colors to each category to keep the color consistent 
element_list1 <- unique(p1_tool$subgroup); element_list1
colors_group1 <- func_color_bygroup(df = p1_tool, column_name = "subgroup", color_pal = color_bygroup)
colored_group <- color_bygroup

p1 <- p1_tool %>%
  # dplyr::mutate(subgroup = factor(x = subgroup, levels = element_list_named)) %>%
  plot_effect_size_overall(data = .,
                           subgroup = 'subgroup', 
                           facet_bygroup = facet_decide, 
                           add_gradient_bg = F,
                           show_legend = F) +
  scale_colour_manual(values = colored_group)



##' plot tool - 2 ------------------------------------------------------------------------

p2_tool <- ma_result_all_sub %>%
  dplyr::filter(subgroup != 'Overall', 
                group_name == subgroup_select,
                tool == mh_tools[2]) 

len <- length(unique(p2_tool$subgroup)); len
facet_decide <- ifelse(len >1, T, F)

## add named colors
element_list2 <- unique(p2_tool$subgroup); 
colors_group2 <- func_color_bygroup(df = p2_tool, column_name = "subgroup", color_pal = color_bygroup)

p2 <- p2_tool %>%
  # dplyr::mutate(subgroup = factor(x = subgroup, levels = element_list_named)) %>%
  plot_effect_size_overall(data = ., 
                           subgroup = 'subgroup', 
                           facet_bygroup = facet_decide, 
                           add_gradient_bg = F,
                           show_legend = F) + 
  theme(legend.position = c(0.8, 0.1)) +
  # It's recommended to use a named vector
  scale_colour_manual(values = colored_group)
# p2



##' create a legend that includes elements from both figures -----------------------------
set.seed(1) # for reproducibility
### - if choose to include available categories
ls_element <- unique(c(element_list1, element_list2))
ls <- unique(c(colors_group1, colors_group2))
### - if choose to include all categories 
ls_element <- unique(ma_result_all_sub$subgroup)
colors_group <- func_color_bygroup(df = ma_result_all_sub, column_name = "subgroup", color_pal = color_bygroup)
ls <- unique(colors_group)

n  <- length(ls); n

# generate 5 random points
tbl <- tibble(x = runif(n),
              y = runif(n),
              Groups = factor(ls_element, levels = group_list), 
              class = factor( LETTERS[1:n] ))

# print if x > 0.5
legend_plot <- 
  ggplot(data = tbl,
         aes(x = x, y = y, color = Groups)) +
  geom_point(size = 2) +
  # scale_colour_discrete(drop = FALSE) +
  scale_colour_manual(values = colored_group) +
  theme_bw() +
  theme(legend.spacing.y = unit(0, 'cm'),
        legend.spacing.x = unit(0, 'cm'),
        legend.key.size = unit(0.05, 'cm'),
        legend.justification = "left",
        plot.margin = margin(0, 0, 0, 0, "pt"),
        legend.title=element_text(size=7.5, face = 'bold'),
        legend.text=element_text(size=7),
  ) +
  guides(color = guide_legend(reverse=F, title=group_title))
# legend_plot  
legend_made <- cowplot::get_legend(legend_plot)



##' combine plots ------------------------------------------------------------------------ 
p_comb <- 
  ggarrange(p1 + rremove("x.title"), 
            p2 + theme(legend.position = 'none'), 
            labels = c("A", "B"), ncol = 1, nrow = 2,
            heights = c(0.4, 1), 
            common.legend = F, 
            align = "v")
p_comb

fname <- paste0(dir.fig, 'es_comb', postfix_group, '_', today, vv, '.png'); 
print(fname)
# func_ggsave(fname, w = 6, h = 5.5+1, save_png = T)


##' add legend 
library(grid)
library(gridExtra)

fname <- gsub('.png', '_legend.png', fname); fname
png(filename = fname, width = 6, height = 5.5+1, units = 'in', res = 600)

grid.draw(p_comb)
sample_vp <- viewport(x = 2/3, y = 0, # legend position 
                      width = 1/3+0.2, height = 0.3,
                      just = c("left", "bottom")
)
pushViewport(sample_vp)
# grid.draw(roundrectGrob())
grid.draw(legend_made)
popViewport()

dev.off()