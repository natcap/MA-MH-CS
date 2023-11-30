load('./data/YY/data_debug_yingjie.RData')


San_nat_tool  <- San_nat  %>%  
  dplyr::rename("nature_type" = "nature_type.1") %>%
  pivot_longer(names_to  = 'dimension', 
               values_to = 'layers', 
               cols = c( "nature_type", "MH_tool")) %>%
  group_by(dimension) %>%
  dplyr::mutate(id_within_layers = row_number(dimension)) %>%
  arrange(dimension) %>%
  dplyr::rename(freq = n) %>%
  dplyr::mutate(
    dimension = factor(dimension, levels = c("nature_type", "MH_tool")),
  ) %>%
  group_by(layers) %>%
  dplyr::mutate(total = sum(freq, na.rm = T)) %>%
  as.data.frame()

func_alluvial(data = San_nat_tool, indicator_n_min = 1, labele_small = 1, filename.postfix = 'YY_San_nat_tool')
