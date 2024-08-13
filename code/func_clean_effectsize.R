

func_clean_effectsize <- function(
    data, 
    column_name = 'effect_size_indices') {
  
  column_name_raw = paste(column_name, 'raw', sep = '_')
  
  d <- data %>%
    dplyr::mutate(
      
      ## keep a copy of the original value
      !!column_name_raw := !!sym(column_name),
      
      ## clean the column
      !!column_name := gsub("Other: ", "", !!sym(column_name)),
      !!column_name := gsub("\\s*\\([^\\)]+\\)", "", !!sym(column_name)), # remove text within parenthesis 
      
      ## text cleaning 
      # !!column_name := gsub("=.*", "", !!sym(column_name)), ## remove everything after "="
      !!column_name := gsub(" = correlation using linear regression", "", !!sym(column_name)),
      
      !!column_name := gsub(" = odds ratio = \\(group1 odds of outcome\\)", "", !!sym(column_name)),
      !!column_name := gsub("\\/\\(group2 odds of outcome\\)", "", !!sym(column_name)),
      !!column_name := gsub(" = odds ratio =\\/", "", !!sym(column_name)),
      
      !!column_name := gsub(" = Pearson's r correlation \\(range\\: -1 to 1\\)", "", !!sym(column_name)),
      !!column_name := gsub(" = Pearson's r correlation", "", !!sym(column_name)),
      
      
      !!column_name := gsub(" = Cohen\\’s d = \\(mean1 - mean2\\)\\/sd", "", !!sym(column_name)),
      !!column_name := gsub(" = Cohen’s d =\\/sd", "", !!sym(column_name)),
      
      !!column_name := gsub("Measure values before and after intervention", "Mean_pre_post", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("raw values|raw value|raw scores|raw score", "Raw values", !!sym(column_name), ignore.case = T),
      !!column_name := gsub("Incidence Rate Ratios|Incidence Rate Ratio", "IRR", !!sym(column_name), ignore.case = T),
      
      
      !!column_name := gsub(" = correlation using logistic regression", "", !!sym(column_name)),
      !!column_name := gsub("corr_linear|corr_logi|regression coefficient|Regression coefficient", "coefficient", !!sym(column_name)),
      

      ## final formatting 
      !!column_name := gsub(",+", ",", !!sym(column_name)), ## Removing repeated punctuation character from a string
      !!column_name := gsub(";+", ";", !!sym(column_name)), ## Removing repeated punctuation character from a string
      !!column_name := trimws(!!sym(column_name)),
      !!column_name := str_squish(!!sym(column_name))) %>%
    dplyr::filter(!!sym(column_name) != 'NA') %>%
    dplyr::select(1:!!sym(column_name), !!sym(column_name_raw), everything()) %>%
    as.data.frame()
  
  return(d)
}
