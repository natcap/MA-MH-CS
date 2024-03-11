
#' Clean messy data in `Country` and `City`
#' @description Clean messy data
#'
#' @usage func_clean_country(data, column_name = "Country")
#' 
#' @param data          Data frame as the input
#' @param column_name   The target column for cleaning, in a string format
#' 

func_clean_country <- function(data, column_name = "Country") {
  d <- data %>%
    dplyr::mutate(
      ## clean and correct country names
      !!column_name := case_when(
        str_detect(string = !!sym(column_name), pattern = 'Scotland|UK|England|United kingdom|The United Kingdom') ~ 'United Kingdom',
        str_detect(string = !!sym(column_name), pattern = 'Indonasia') ~ 'Indonesia',
        str_detect(string = !!sym(column_name), pattern = 'Malasia') ~ 'Malaysia',
        str_detect(string = !!sym(column_name), pattern = 'The Netherlands') ~ 'Netherlands',
        str_detect(string = !!sym(column_name), pattern = 'Danmark') ~ 'Denmark',
        str_detect(string = !!sym(column_name), pattern = 'Beigium') ~ 'Belgium',
        str_detect(string = !!sym(column_name), pattern = 'Brasil') ~ 'Brazil',
        str_detect(string = !!sym(column_name), pattern = 'Chili') ~ 'Chile',
        str_detect(string = !!sym(column_name), pattern = 'Czech') ~ 'Czechia',
        str_detect(string = !!sym(column_name), pattern = 'Virginia') ~ 'USA',
        # 
        T ~ !!sym(column_name))
    ) %>%
    dplyr::mutate(
      !!column_name := str_squish(!!sym(column_name)),
      !!column_name := trimws(!!sym(column_name))) %>%
    as.data.frame()
  return(d)
}



func_clean_city <- function(data, column_name = "City") {
  d <- data %>%
    dplyr::mutate(!!column_name := case_when(
      !!sym(column_name) %in% c('m', NA) ~ NA,
      T ~ !!sym(column_name))
      ) %>%
    dplyr::mutate(
      !!column_name := str_squish(!!sym(column_name)),
      !!column_name := trimws(!!sym(column_name))) %>%
    as.data.frame()
  return(d)
}
