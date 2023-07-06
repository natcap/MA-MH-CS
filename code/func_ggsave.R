
func_ggsave <- function(fname, w = 7, h = 4, save_png = save_plot) {
  if (save_png == T) {
    ggsave(filename = fname, plot = last_plot(), width = w, height = h, units = "in", dpi = 300)
  } else {
    print('The plot will not be saved.')
  }
}