

plot_freq <- function(data, var = "Mental health indicators") {
  p <- ggplot(
    data = data,
    aes(
      x = reorder(eval(parse(text = var)), n),
      y = n,
      # fill = n > 10
    )) +
    geom_col(show.legend = F) +
    theme_bw() +
    # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    coord_flip() +
    # scale_fill_distiller(name = '', palette = "Blues", guide = "colorbar", direction = 1) +
    xlab("")
  
  return(p)
}
