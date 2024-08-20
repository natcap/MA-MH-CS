library(agricolae)
library(ggplot2)


##' The use of letters to indicate significant differences in pairwise comparisons is called 
##'   compact letter display, and can simplify the visualization and discussion of 
##'   significant differences among means. We are going to use the `multcompLetters4` function 
##'   from the `multcompView` package. 
##'   The arguments are the object from an aov function and the object from the `TukeyHSD` function.
##'   
##'   
## Example non-parametric data
# data <- data.frame(
#   Group = factor(rep(c("Control", "Experiment 1", "Experiment 2"), each = 10)),
#   Value = c(runif(10, min = 4, max = 6), runif(10, min = 5, max = 7), runif(10, min = 6, max = 8))
# )





func_test_dif <- function(df, 
                          value = 'value', 
                          group = 'group',
                          facet_by = 'ind_sub') {
  # df <- ma_result_each_i
  unique(df$ind_sub)
  
  data_comb <- data.frame()
  
  for (ind in unique(df$ind_sub)) {
    data <- df %>%
      dplyr::rename('Value' = value, 
                    'Group' = group) %>%
      dplyr::filter(ind_sub == ind)
    
    # Perform Kruskal-Wallis test and obtain CLD
    kruskal_result <- kruskal(data$Value, data$Group, group = TRUE)
    cld <- kruskal_result$groups
    
    # Merge CLD with original data
    data$Group <- factor(data$Group, levels = rownames(cld))
    cld$Group <- rownames(cld)
    
    # Combine the original data with CLD information
    data_merged <- merge(data, cld, by = "Group")
    
    data_comb <- rbind(data_comb, data_merged)
  }
  
  
  # Create the plot with ggplot2
  p <- ggplot(data_comb, aes(x = Group, y = Value, fill = Group)) +
    geom_boxplot(show.legend = F) +
    geom_text(aes(label = groups, 
                  # Remove outliers and calculate the 75th percentile in one step
                  y = quantile(
                    Value[Value >= quantile(Value, 0.25, na.rm = T) - 1.5 * IQR(Value, na.rm = T) & 
                            Value <= quantile(Value, 0.75, na.rm = T) + 1.5 * IQR(Value, na.rm = T)], 
                    probs = 75/100, na.rm = T)), 
              vjust=0, hjust = -0.5, 
              color = 'gray20'
              ) +
    labs(
      # title = "Boxplot with Compact Letter Display (Non-Parametric)", 
      caption = '*Means not sharing any letter are significantly different by the Tukey-test at the 5% level of significance.',
      x = "", y = "Means") +
    # theme_minimal() +
    # coord_flip() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  
  if (!is.null(facet_by)) {
    p <- p +
      facet_wrap(~ get(facet_by), 
                 scales = 'free_y',
                 ncol = 5)
  }
  
  return(p)
}

