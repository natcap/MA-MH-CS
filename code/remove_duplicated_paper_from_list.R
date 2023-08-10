
#' remove duplicated papers in the extraction list 
#' 
#' 
#' read list from Google Sheet

library(googlesheets4)
library(dplyr)

link <- 'https://docs.google.com/spreadsheets/d/1MZlUT5USAyydOE4CJQma3ZrmsYTwryUV3DbW5nZxL84/edit?usp=sharing'
tab  <- googlesheets4::read_sheet(link, sheet = "paperID_byTopTools", skip = 1) %>%
  as.data.frame() 

tab_ <- tab %>%
  ##' remove exclued papers
  ##'   but, we no need to remove these. because the newly added ones can duplicated with 
  ##'   these 'excluded' ones as well
  # dplyr::filter(!exclude %in% c('yes')) %>%
  dplyr::select(id:exclude, added_id) %>%
  dplyr::mutate(id = gsub('##', '#', id)) %>%
  ## remove duplicated ones in 2
  arrange(id, Tool, -added_id) %>%
  # dplyr::distinct(id, Tool, .keep_all = T) %>%
  group_by(id, Tool) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  ## keep the newly added ones
  dplyr::filter(added_id == 2) %>%
  dplyr::filter(count < 2) %>%
  arrange(Tool, id) %>%
  as.data.frame()
