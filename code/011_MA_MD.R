
# unique(exp_sub_mods_md$Tools)


# Data ----

## data extracted in Table option 1 ----
exp_sub_mods_md1 <- exp_sub_mods_md %>%
  dplyr::select(1:Notes_o1)


## data extracted in Table option 2 ----
exp_sub_mods_md2 <- exp_sub_mods_md %>%
  dplyr::select(1:buffers_unit, Other_covariates_o2:Notes_o2) %>%
  
  ## check sample size data
  dplyr::select(1:n_participants, Control_N, Treatment_N, everything()) %>%
  dplyr::mutate(
    
    n_participants = str_squish(n_participants) %>% trimws(.) %>% as.numeric(.),
    Control_N   = str_squish(Control_N)   %>% trimws(.) %>% as.numeric(.),
    Treatment_N = str_squish(Treatment_N) %>% trimws(.) %>% as.numeric(.),
    
    c_n = case_when(
      !is.na(c_mean) & is.na(Control_N) & !is.na(Treatment_N) ~ Treatment_N,
      !is.na(c_mean) & is.na(Control_N) &  is.na(Treatment_N) & !is.na(n_participants) ~ n_participants,
      TRUE ~ Control_N
    ),
    
    e_n = case_when(
      !is.na(e_mean) & is.na(Treatment_N) & !is.na(Control_N) ~ Control_N,
      !is.na(e_mean) & is.na(Treatment_N) &  is.na(Control_N) & !is.na(n_participants) ~ n_participants,
      TRUE ~ Treatment_N
    ),
  ) %>%
  dplyr::select(1:n_participants, Control_N, c_n, Treatment_N, e_n, everything()) %>%
  as.data.frame()



# MA =====================================================================================

## SMD ----

# Make sure meta and dmetar are already loaded
library(meta)

# Use metcont to pool results.
ma_smd <- metacont(data = exp_sub_mods_md2,
                   n.e = e_n,
                   n.c = c_n,
                   mean.e = e_mean,
                   mean.c = c_mean,
                   sd.e = e_sd,
                   sd.c = c_sd,
                   studlab = id,
                   
                   sm = "SMD",
                   method.smd = "Hedges",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",
                   hakn = TRUE,
                   title = "POMS")
ma_smd



## forest ----
f <- paste0('./figures/', 'plot_forest_POMS_', today, '.png'); f
png(filename = f, 
    # width = 1000, height = 1000, units = "px", pointsize = 22,
    width = 25, height = 100, units = "cm",
    res = 500)
forest(ma_smd,
       layout = "RevMan5",
       lab.e = "Post-intervention",
       lab.c = "Pre-intervention",
       label.left = "loss",
       label.right = "gain",
       digits.sd = 2,
       digits.tau2 = 2,
       # colgap = "0.5cm",
       # colgap.forest = "1cm",
       col.by = "black",
       col.square = "black",
       col.inside = "black",
       col.square.lines = "black",
       test.effect.subgroup.random = TRUE,
       overall = FALSE,
       overall.hetstat = FALSE)
dev.off()