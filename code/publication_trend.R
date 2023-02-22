# To clear your environment 
remove(list = ls())

dir_fig <- './figures/'

library(SDGdetector)
packageVersion('SDGdetector')

library(dplyr)
library(stringr)
library(ggplot2)
library(cowplot)


codelist.supp <- data.frame(
  country.name.en = c('USA'),
  iso3c = c('USA')
)
codelist <- SDGdetector::codelist_panel %>%
  dplyr::distinct(country.name.en, iso3c) %>%
  rbind(., codelist.supp) %>%
  dplyr::mutate(
    country.name.en = case_when(
      str_detect(string = iso3c, pattern = 'HKG') ~ 'Hong Kong',
      T ~ country.name.en)
  ) %>%
  dplyr::distinct(country.name.en, iso3c)
  
shp <- SDGdetector::shp


df <- readxl::read_excel(path = './data/analyze_publication by county.xlsx') 
  
names(df) <- c('names', 'count', 'percent')

df <- df %>%
  dplyr::mutate(
    regionname = tolower(names),
    regionname = case_when(
      str_detect(string = regionname, pattern = 'scotland') ~ 'United Kingdom',
      str_detect(string = regionname, pattern = 'england|wales') ~ 'United Kingdom',
      str_detect(string = regionname, pattern = 'united states') ~ 'United States',
      T ~ regionname)
    )

dt <- detect_region(x = df, col = regionname) %>%
  dplyr::mutate(
    region = case_when(
      str_detect(string = names, pattern = 'USA') ~ 'United States',
      str_detect(string = names, pattern = 'SOUTH KOREA') ~ 'South Korea',
      str_detect(string = names, pattern = 'CZECH REPUBLIC') ~ 'Czechia',
      str_detect(string = names, pattern = 'ROMANIA') ~ str_to_title('ROMANIA'),
      str_detect(string = names, pattern = 'SOUTH AFRICA') ~ str_to_title('SOUTH AFRICA'),
      str_detect(string = names, pattern = 'NIGERIA') ~ str_to_title('NIGERIA'),
      T ~ region)
  ) %>%
  merge(x = ., y = codelist, by.x = 'region', by.y = 'country.name.en', all.x = T) %>%
  arrange(!is.na(iso3c))

dt1 <- dt %>%
  group_by(iso3c) %>%
  dplyr::summarise_at(c('count', 'percent'), sum, rm.na = T) 


dt_sf <- dt1 %>%
  merge(x= shp, y = ., by.x = 'iso_a3', by.y  = 'iso3c', all.x = T) 


dt_sf %>%
  dplyr::filter(name != 'Antarctica') %>%
  ggplot(.) +
  geom_sf(aes(fill = count), size = 0.1, color = 'gray80') +
  # geom_sf_text(aes(label = iso_a3), colour = "gray", size =1) + 
  scale_fill_distiller(palette = 'YlGnBu', direction = 1, na.value = "gray90") +
  # theme_bw() +
  theme_nothing() +
  theme(legend.position = c(0.06, 0.4))

fname <- paste0(dir_fig, 'pub_count_map.png'); fname
ggsave(filename = fname, plot = last_plot(), width = 6.4*2, height = 3.2*2, units = 'in', dpi = 300)
