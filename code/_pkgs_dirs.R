Sys.setenv(LANG = "en")

## Most used packages
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(cowplot)

today <- format(Sys.time(), "%Y%m%d"); today ## "%Y%m%d%H%M"

## Data dirs
dir.fig <- "./figures/"

## load functions
source('./code/func_expand_col_to_long.R')
source('./code/func_clean_geo.R')
source('./code/func_clean_indicators.R')
source('./code/func_clean_indicatorsPro.R')
source('./code/func_clean_tools.R')
source('./code/func_plot_freq.R')
source('./code/func_alluvial.R')
source('./code/func_ggsave.R')
