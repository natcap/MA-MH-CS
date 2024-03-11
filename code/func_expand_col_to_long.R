
library(splitstackshape) ## `cSplit()`

expand_col_to_long <- function(
    data, 
    target_col = "Mental health indicators",
    sep_list = c(';', ',')
    ) {
  
  
  ## if using more than one separators
  if (length(sep_list) > 1) {
    pat <- paste(sep_list, collapse = "|")
    sep_use <- ';'
    
    data_ <- data %>%
      dplyr::rename("col_split" = target_col) %>%
      ## unify the separator as ";"
      dplyr::mutate(col_split = gsub(pat, sep_use, col_split))
  } else {
    
    ## if using only one separator
    sep_use = sep_list
    data_ <- data %>%
      dplyr::rename("col_split" = target_col)
  }
    
  ## split the column using `sep_use` 
  data_long <- data_ %>%
    splitstackshape::cSplit(
      indt = .,
      splitCols = c("col_split"),
      sep = sep_use,
      drop = F, # drop the original col or not
      direction = "long", # this is better than "wide"
      stripWhite = T,
      type.convert=FALSE
    ) %>% # clean white space
    dplyr::mutate(across(where(is.factor), as.character)) %>%
    dplyr::mutate(col_split = trimws(col_split)) %>%
    
    ## - convert long input in sentence format -----------------------
    # dplyr::mutate(col_split = ifelse(
    #   nchar(col_split) > 10,
    #   str_to_sentence(col_split),
    #   col_split
    # )) %>%
  
  ## - capitalizes first word but not subsequent words -------------
  dplyr::mutate(col_split = Hmisc::capitalize(col_split)) %>%
    as.data.frame() %>%
    # group_by(col_split) %>%
    # dplyr::summarise_at(c("n"), sum, na.rm = T) %>%
    as.data.frame()
  
  ## change back to the original column name
  names(data_long)[names(data_long) == "col_split"] <- target_col
  
  return(data_long)
}
