
##' fill blanks in one column using data from the other column `from_column`, 
##'     but only if there is only one entry in `from_column`
func_fill_c2_from_c1 <- function(data, to_column, from_column) {
  d <- data %>%
    dplyr::mutate(
      !!to_column := ifelse(
        is.na(!!sym(to_column)) &         ## 1. the column is blank
          !is.na(!!sym(from_column)) &    ## 2. the intend-to-use column is not blank
          str_detect(pattern = ";",       ## 3. the intend-to-use column has only one entry
                     string = !!sym(from_column), negate = T), 
        !!sym(from_column), !!sym(to_column)),
      
      !!to_column := str_squish(!!sym(to_column)),
      !!to_column := trimws(!!sym(to_column))
    ) %>%
    as.data.frame()
  
  return(d)
}
