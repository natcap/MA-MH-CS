
##' This function is extremely useful when there are multiple number inputs 
##'   within a single cell in a dataframe. 
##'   
##' The function will create a new column name and calculate the mean value
##' 
##' Users can specify other separators used in the actual dataset, such as ",". 



library(dplyr) 

func_cell_mean <- function(df, column_name, sep = ";") {
  
  column_name_new = paste0(column_name, '_mean')

  d <- df %>%
    dplyr::mutate(
      !!column_name_new := sapply(strsplit(!!sym(column_name), sep), function(x) {
        num_values <- tryCatch(as.numeric(x), error = function(e) return(NA))
        if (all(!is.na(num_values))) {
          mean(num_values, na.rm = TRUE) # Handle potential NA values if errors occurred
        } else {
          NA_real_  # Return NA if the entire string couldn't be converted
        } 
    })) %>%
    dplyr::select(1:any_of(c(column_name)), any_of(c(column_name_new)), everything())
  
  return(d)

}


##' example test
# tt <- data.frame(column_name = c("5,15", "1;2;100", "NA"))
# func_cell_mean(df = tt, 'column_name', sep = ',')
# df_f <- func_cell_mean(df, 'duration_value', sep = ';')
