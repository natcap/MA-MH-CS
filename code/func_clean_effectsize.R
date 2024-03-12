

func_clean_effectsize <- function(data) {
  d <- data %>%
    dplyr::mutate(
      effect_size_indices = gsub("Other: ", "", effect_size_indices),
      effect_size_indices = gsub("\\s*\\([^\\)]+\\)", "", effect_size_indices), # remove text within parenthesis 
      
      ## text cleaning 
      # effect_size_indices = gsub("=.*", "", effect_size_indices), ## remove everything after "="
      effect_size_indices = gsub(" = correlation using linear regression", "", effect_size_indices),
      
      effect_size_indices = gsub(" = odds ratio = \\(group1 odds of outcome\\)", "", effect_size_indices),
      effect_size_indices = gsub("\\/\\(group2 odds of outcome\\)", "", effect_size_indices),
      effect_size_indices = gsub(" = odds ratio =\\/", "", effect_size_indices),
      
      effect_size_indices = gsub(" = Pearson's r correlation \\(range\\: -1 to 1\\)", "", effect_size_indices),
      effect_size_indices = gsub(" = Pearson's r correlation", "", effect_size_indices),
      
      
      effect_size_indices = gsub(" = Cohen\\’s d = \\(mean1 - mean2\\)\\/sd", "", effect_size_indices),
      effect_size_indices = gsub(" = Cohen’s d =\\/sd", "", effect_size_indices),
      
      effect_size_indices = gsub("Measure values before and after intervention", "Mean_pre_post", effect_size_indices),
      effect_size_indices = gsub("raw values|raw value|raw scores|raw score", "Raw values", effect_size_indices, ignore.case = T),
      effect_size_indices = gsub("Incidence Rate Ratios|Incidence Rate Ratio", "IRR", effect_size_indices, ignore.case = T),
      
      
      effect_size_indices = gsub(" = correlation using logistic regression", "", effect_size_indices),
      effect_size_indices = gsub("corr_linear|corr_logi|regression coefficient|Regression coefficient", "coefficients", effect_size_indices),
      

      ## final formatting 
      effect_size_indices = gsub(",+", ",", effect_size_indices), ## Removing repeated punctuation character from a string
      effect_size_indices = gsub(";+", ";", effect_size_indices), ## Removing repeated punctuation character from a string
      effect_size_indices = trimws(effect_size_indices),
      effect_size_indices = str_squish(effect_size_indices)) %>%
    dplyr::filter(effect_size_indices != 'NA') %>%
    as.data.frame()
  
  return(d)
}
