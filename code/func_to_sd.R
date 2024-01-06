

## Use SE, CI, to fill NA in SD ----------------------------------------------------------

#' @description
#' This function can convert all of the variation indicators (i.e., `se`, `ci95lower`) 
#'    to `sd` for both of the control and experiment groups, respectively. 
#'    
#' @param table_option description: which table option is going to use for applying this 
#'    function. The argument can be either 'o1' (i.e., table option 1, with correlation or 
#'    regression data) or 'o2' (i.e., table option 2, with control vs experiment groups). 
#'    Default argument is 'o2'
#' 
func_all_to_sd <- function(data, table_option = 'o2') {
  
  ## Check if the specified argument exists in the data frame
    if (!(table_option %in% c('o1', 'o2') )) {
      stop("The table_option can only be either 'o1' or 'o2'.")
    }
  
  ## which table to apply the function 
  ## - for table option 2 ...
  if (table_option == 'o2') {
    data <- data %>%
  
      ## for control group ##
      dplyr::mutate(
        c_sd_r = case_when(
          ## se to sd
          is.na(c_sd) & !is.na(c_se)        ~ c_se * sqrt(c_n),
          ## ci to sd
          is.na(c_sd) & !is.na(c_ci95lower) ~ 2*abs(c_ci95lower) * sqrt(c_n)/3.92,
          TRUE ~ c_sd
      )) %>%
      dplyr::select(1:c_sd, c_sd_r, everything()) %>%
      
      ## for experimental group ##
      dplyr::mutate(
        e_sd_r = case_when(
          ## se to sd
          is.na(e_sd) & !is.na(e_se)        ~ e_se * sqrt(e_n),
          ## ci to sd
          is.na(e_sd) & !is.na(e_ci95lower) ~ 2*abs(e_ci95lower) * sqrt(e_n)/3.92,
          TRUE ~ e_sd
      )) %>%
      dplyr::select(1:e_sd, e_sd_r, everything()) %>%
      as.data.frame()
    
    ## - for table option 2 ...  
  } else {
    data <- data %>%
      dplyr::mutate(
        sd_r = case_when(
          ## se to sd
          is.na(sd) & !is.na(se)        ~ se * sqrt(n),
          ## ci to sd
          is.na(sd) & !is.na(ci95lower) ~ 2*abs(ci95lower) * sqrt(n)/3.92,
          TRUE ~ sd
        )) %>%
      dplyr::select(1:sd, sd_r, everything()) %>%
      as.data.frame()
    
  }
      
  ## Return the modified data frame
  return(data)
}




## convert SE, CI, to SD -----------------------------------------------------------------

#' @description
#' This function can only convert one of the variation indicators to `sd`
#' 
#' @param column_name can be 'c_se', 'c_ci95lower', 'e_se', or 'e_ci95lower'
#' 
#' @note Not use for now ...
#' 
# func_to_sd <- function(data, column_name){
#   
#   # Check if the specified column exists in the data frame
#   if (!(column_name %in% colnames(data))) {
#     stop("Column not found in the data frame.")
#   }
#   
#   ## se to sd
#   if (grepl('_se', column_name, fixed = TRUE)) {
#     nn <- gsub('_se', '_n', column_name); nn
#     sd_new_name <- gsub('_se', '_sd_r', column_name)
#     
#     data <- data %>%
#       dplyr::mutate(
#         !!sd_new_name := !!sym(column_name) * sqrt(!!sym(nn)) ) %>%
#       dplyr::select(1:!!sym(column_name), !!sd_new_name, everything())
#     
#   ## ci to sd
#   } else if (grepl('_ci95lower', column_name, fixed = TRUE)) {
#     nn <- gsub('_ci95lower', '_n', column_name); nn
#     sd_new_name <- gsub('_ci95lower', '_sd_r', column_name)
#     
#     data <- data %>%
#       dplyr::mutate(
#         !!sd_new_name := 2*abs(!!sym(column_name)) * sqrt(!!sym(nn)) / 3.92 ) %>%
#       dplyr::select(1:!!sym(column_name), !!sd_new_name, everything())
#     
#   } else {
#     data <- data
#   }
#   
#   # Return the modified data frame
#   return(data)
# }

