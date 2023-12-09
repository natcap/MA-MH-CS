

## Use SE, CI, to fill NA in SD ----------------------------------------------------------
func_for_sd <- function(data){
  
  data <- data %>%

    ## for control group ##
    dplyr::mutate(c_sd_r = case_when(
      ## se to sd
      is.na(c_sd) & !is.na(c_se)        ~ c_se * sqrt(c_n),
      ## ci to sd
      is.na(c_sd) & !is.na(c_ci95lower) ~ 2*abs(c_ci95lower) * sqrt(c_n)/3.92,
      TRUE ~ c_sd
    )) %>%
    dplyr::select(1:c_sd, c_sd_r, everything()) %>%
    
    ## for experimental group ##
    dplyr::mutate(e_sd_r = case_when(
      ## se to sd
      is.na(e_sd) & !is.na(e_se)        ~ e_se * sqrt(e_n),
      ## ci to sd
      is.na(e_sd) & !is.na(e_ci95lower) ~ 2*abs(e_ci95lower) * sqrt(e_n)/3.92,
      TRUE ~ e_sd
    )) %>%
    dplyr::select(1:e_sd, e_sd_r, everything()) %>%
    as.data.frame()
      
  # Return the modified data frame
  return(data)
}




## convert SE, CI, to SD -----------------------------------------------------------------
func_to_sd <- function(data, column_name){
  
  # Check if the specified column exists in the data frame
  if (!(column_name %in% colnames(data))) {
    stop("Column not found in the data frame.")
  }
  
  ## se to sd
  if (grepl('_se', column_name, fixed = TRUE)) {
    nn <- gsub('_se', '_n', column_name); nn
    sd_new_name <- gsub('_se', '_sd_r', column_name)
    
    data <- data %>%
      dplyr::mutate(
        !!sd_new_name := !!sym(column_name) * sqrt(!!sym(nn)) ) %>%
      dplyr::select(1:!!sym(column_name), !!sd_new_name, everything())
    
  ## ci to sd
  } else if (grepl('_ci95lower', column_name, fixed = TRUE)) {
    nn <- gsub('_ci95lower', '_n', column_name); nn
    sd_new_name <- gsub('_ci95lower', '_sd_r', column_name)
    
    data <- data %>%
      dplyr::mutate(
        !!sd_new_name := 2*abs(!!sym(column_name)) * sqrt(!!sym(nn)) / 3.92 ) %>%
      dplyr::select(1:!!sym(column_name), !!sd_new_name, everything())
    
  } else {
    data <- data
  }
  
  # Return the modified data frame
  return(data)
}

