
### format data
## - wide to long format}

##' Test code for formatting one model
# exp_sub_mod1 <- exp_sub_df %>%
#   dplyr::select(id, starts_with("Model 1 ") & !contains("measurement", ignore.case = TRUE))
# 
# exp_sub_mod11 <- exp_sub_df %>%
#   dplyr::select(id, starts_with("Model 11 "))


##' loop all 20 models (in the Covidence form, we have 20 rows for data extraction)
exp_sub_l <- data.frame()
for (i in 1:20) {
  # print(i)
  mod_id <- paste0("Model ", i, " ")
  exp_sub_dfi <- exp_sub_df %>%
    dplyr::select(id, `Study ID`,
                  all_of(cols_keep),
                  contains("effect_size_indices"), 
                  `Health outcome direction`, 
                  n_participants, buffers, buffers_unit,
                  ##' loop a model by selecting the column names 
                  ##' but need to exclude data from table 3 (with column names that contain key word 'measurement')
                  starts_with(mod_id) & !contains("measurement", ignore.case = TRUE)) %>%
    dplyr::mutate(model_id = i) %>%
    dplyr::select(id, effect_size_indices, model_id, `Health outcome direction`, everything())
  
  ##' remove model id so that they can be rbind (require the same column names)
  colnames(exp_sub_dfi) <- sub(mod_id, "", colnames(exp_sub_dfi))
  ##' Remove part of string after "."
  colnames(exp_sub_dfi) <- gsub("\\..*","",colnames(exp_sub_dfi))
  
  ##' Repair duplicate names - specify your own name repair function
  exp_sub_dfi <- exp_sub_dfi %>%
    as_tibble(., .name_repair = make.unique) %>%
    as.data.frame()
  
  exp_sub_l <- rbind(exp_sub_l, exp_sub_dfi)
}

rm(exp_sub_dfi)




## - clean names from table 1/2, include=FALSE}
##' _o1:  table option 1
##' _o2:  table option 2
##' _cov: extracted data stored in Covidence 
##' _gs:  extracted data stored in gsheet (the extended tables)

## for data extracted in table option 1 --------------------------------------------------
exp_sub_l_o1_cov <- exp_sub_l %>%
  dplyr::select(id:t_value) %>%
  as.data.frame()
name1 <- names(exp_sub_l_o1_cov) %>%
  gsub('\\.1|\\.2', '', .) %>%
  gsub('Other covariates that are beyond the baseline', 'Other_covariates_o1', .)
name1.1 <- name1[1: (which(name1 == 'MH_indicator')-1)]
name1.2 <- name1[which(name1 == 'MH_indicator'):length(name1)] %>% paste0(., '_o1')
names(exp_sub_l_o1_cov) <- c(name1.1, name1.2) 
names(exp_sub_l_o1_cov)



## - bind Covidence and gsheet data}

exp_sub_l_o1_gs <- df_2o1 %>%
  dplyr::select(-c("group_id", "Study ID", "Reviewer", "Reviewer_id")) %>%
  dplyr::mutate(id = gsub('#', '', id))

# exp_sub_l_o1_cov_ <- exp_sub_l_o1_cov %>%
#   dplyr::select(id, model_id, Other_covariates_o1:ncol(.))


unique(exp_sub_l_o1_gs$id)

exp_sub_l_o1_gs_add <- exp_sub_l_o1_cov %>%
  ##' get the paper list that stored in gsheet
  ##' use the id list to get general data (section 1-3) in the 
  ##'   Covidence-based data, which has also been subset based on specific MH tools
  dplyr::filter(id %in% unique(exp_sub_l_o1_gs$id)) %>%
  dplyr::distinct(id, .keep_all = T) %>%
  dplyr::select(1:exp_mean, -model_id) %>%
  
  ##' join the *Covidence data* to *gsheet-based table*
  right_join(., exp_sub_l_o1_gs, by = 'id') %>%
  dplyr::select(1:effect_size_indices, model_id, everything()) %>%
  ##' if effect_size_indices is NA, this means these papers are not related to 
  ##'   the MH tool we are working on 
  dplyr::filter(!is.na(effect_size_indices)) %>%
  as.data.frame()


names(exp_sub_l_o1_cov)
names(exp_sub_l_o1_gs_add)
names(exp_sub_l_o1_gs_add) <- names(exp_sub_l_o1_cov)

## bind covidence and gsheet data
exp_sub_l_o1 <- rbind(exp_sub_l_o1_cov, exp_sub_l_o1_gs_add) %>%
  dplyr::mutate(model_id = as.character(model_id))



## for data extracted in table option 2 --------------------------------------------------
exp_sub_l_o2_cov <- exp_sub_l %>%
  dplyr::select(id, model_id, Notes:Notes.1) %>%
  dplyr::select(-c(Notes:t_value))
name2 <- names(exp_sub_l_o2_cov) %>%
  gsub('\\.1|\\.2', '_o2', .) %>%
  gsub('Other covariates that are beyond the baseline', 'Other_covariates', .)
names(exp_sub_l_o2_cov) <- name2  
names(exp_sub_l_o2_cov)


exp_sub_l_o2_gs <- df_2o2 %>%
  dplyr::select(-c("group_id", "Study ID", "Reviewer", "Reviewer_id")) %>%
  ## only include the selected `mh_tool`
  dplyr::filter(id %in% unique(exp_sub_df$id)) %>%
  as.data.frame()
names(exp_sub_l_o2_cov)
names(exp_sub_l_o2_gs)
names(exp_sub_l_o2_gs) <- names(exp_sub_l_o2_cov)

## bind covidence and gsheet data
exp_sub_l_o2 <- rbind(exp_sub_l_o2_cov, exp_sub_l_o2_gs) %>%
  dplyr::mutate(model_id = as.character(model_id))



##' further combine data from the combined table option 1 and option 2 -------------------
##' 
##' 1. get the shared/common info, and unique columns for tables o1 and o2
tab_o1 <- exp_sub_l_o1 %>%
  dplyr::select(id, model_id, exp:ncol(.))
tab_o2 <- exp_sub_l_o2

non_shared_cols <- setdiff(names(tab_o1), 'id')

tab_share <- exp_sub_l_o1 %>%
  dplyr::select(-any_of(non_shared_cols)) %>%
  dplyr::distinct_all()

##' 
##' 2. merge table option 1 and table option 2 as a combined table
##'     given the number of models for option 1 could be different from the number of 
##'     models for option 2, we need to use `full_join()`, 
##'     instead of `left_join()`, in order to keep all the data
##'     
tab_comb <- full_join(tab_o1, tab_o2, 
                      by = c("id", "model_id")) %>%
  ##' 3. merge shared info to the combined table
  left_join(., tab_share, 
            by = c('id')) %>%
  ## move the common data columns to the most left side 
  dplyr::select(any_of(names(tab_share)), everything()) %>%
  dplyr::select(id, model_id, everything())


exp_sub_comb <- tab_comb %>%
  func_clean_indicatorsPro(data = ., column_name = 'Indicator') %>%
  func_clean_indicatorsPro(data = ., column_name = 'MH_indicator_o1') %>%
  func_clean_indicatorsPro(data = ., column_name = 'MH_indicator_o2') %>%
  func_clean_buffer(data = ., column_name = 'buffer_o1') %>%
  func_clean_buffer(data = ., column_name = 'buffer_o2') %>%
  
  func_clean_tools(data = ., column_name = 'MH_tool_o1') %>%
  func_clean_tools(data = ., column_name = 'MH_tool_o2') %>%
  
  func_clean_exposure(data = ., column_name = 'exposure_o1') %>%
  func_clean_exposure(data = ., column_name = 'exposure_o2') %>%
  
  as.data.frame()

## remove these two df after merging
# rm(exp_sub_l_o1, exp_sub_l_o2)




## - add geodata}
f <- paste0('./data/0301-MA-input/id_region_match.rds'); f
id_region <- readRDS(f) %>%
  dplyr::select(id, Region) %>%
  dplyr::mutate(id = as.character(id))

exp_sub_comb_update <- exp_sub_comb %>%
  left_join(., id_region, by = 'id') %>%
  dplyr::select(id, Region, everything())




## - clean numeric values}
### clean up the negative sign "-" in data
test <- exp_sub_comb_update %>%
  dplyr::filter(id %in% c(387)) ## take the character from this paper

m <- gsub(' ', '', test$Mean)
m <- "−0.23"
sign <- m[1] %>% substr(., 1, 1)
sign
# mm <- gsub(sign, '-', m)
# mm
# as.numeric(mm)
rm(test)
sign <- paste(sign, '−', sep = '|')


func_clean_number <- function(data, column_name){
  
  # Check if the specified column exists in the data frame
  if (!(column_name %in% colnames(data))) {
    stop("Column not found in the data frame.")
  }
  
  column_name_new <- column_name %>%
    gsub("Control_",   "c_", .) %>%
    gsub("Treatment_", "e_", .) %>%
    as.character() %>%
    str_to_lower()
  
  
  # Manipulate the specified column using multiple pipes
  d <- data %>%
    dplyr::mutate(
      !!column_name_new := !!sym(column_name),
      !!column_name_new := as.character(!!sym(column_name_new)),
      !!column_name_new := gsub(sign, "-", !!sym(column_name_new)),
      !!column_name_new := gsub(" ", "", !!sym(column_name_new)), 
      !!column_name_new := str_squish(!!sym(column_name_new)),
      !!column_name_new := trimws(!!sym(column_name_new)),
      !!column_name_new := as.numeric(!!sym(column_name_new))
    ) %>%
    dplyr::select(1:!!sym(column_name), !!column_name_new, everything()) %>%
    as.data.frame()
  return(d)
}

### test code
# exp_sub_comb_clean1 <- exp_sub_comb_update %>%
#   func_clean_number(column_name = 'Mean')

exp_sub_comb_clean <- exp_sub_comb_update %>%
  dplyr::filter(!is.na(Mean) | !is.na(Treatment_Mean) | !is.na(Treatment_Mean)) %>%
  
  ##' clean the special character "-" in text
  func_clean_number(data = ., column_name = 'Mean') %>%
  func_clean_number(data = ., column_name = 'SD') %>%
  func_clean_number(data = ., column_name = 'SE') %>%
  func_clean_number(data = ., column_name = 'CI_95_lower') %>%
  dplyr::rename('ci95lower' = 'ci_95_lower') %>%
  
  func_clean_number(data = ., column_name = 'Control_Mean') %>%
  func_clean_number(data = ., column_name = 'Control_SD') %>%
  func_clean_number(data = ., column_name = 'Control_SE') %>%
  func_clean_number(data = ., column_name = 'Control_CI95lower') %>%
  
  func_clean_number(data = ., column_name = 'Treatment_Mean') %>%
  func_clean_number(data = ., column_name = 'Treatment_SD') %>%
  func_clean_number(data = ., column_name = 'Treatment_SE') %>%
  func_clean_number(data = ., column_name = 'Treatment_CI95lower') %>%
  
  dplyr::mutate(
    study_label = paste(id, model_id, sep = '_'), # `Study ID`, 
  ) %>%
  dplyr::select(study_label, everything()) %>%
  
  ## check and tidy sample size (N) ------------------------------------------------------
  dplyr::mutate(
    n_participants = str_squish(n_participants) %>% trimws(.) %>% as.numeric(.),
    
    ## for o1
    N = str_squish(N) %>% trimws(.),
    N = gsub(" ", "", N), 
    ## use "n_participants" to fill data gaps of N in data extraction tables
    n = ifelse(is.na(N) & !is.na(n_participants), n_participants, N),
    n = as.numeric(n),
  ) %>%
  dplyr::select(1:N, n, everything()) %>%
  
  dplyr::mutate(
    ## for o2
    Control_N   = str_squish(Control_N)   %>% trimws(.) %>% as.numeric(.),
    Treatment_N = str_squish(Treatment_N) %>% trimws(.) %>% as.numeric(.),
    c_n = case_when(
      !is.na(c_mean) & is.na(Control_N) & !is.na(Treatment_N) ~ Treatment_N,
      !is.na(c_mean) & is.na(Control_N) &  is.na(Treatment_N) & !is.na(n_participants) ~ n_participants,
      TRUE ~ Control_N
    ),
    e_n = case_when(
      !is.na(e_mean) & is.na(Treatment_N) & !is.na(Control_N) ~ Control_N,
      !is.na(e_mean) & is.na(Treatment_N) &  is.na(Control_N) & !is.na(n_participants) ~ n_participants,
      TRUE ~ Treatment_N
    )
  ) %>%
  ## keep new created variables next to its similar ones
  dplyr::select(1:Control_N, c_n, Treatment_N, e_n, study_label, everything()) %>%
  
  ## convert SE, and CI to `SD` ----------------------------------------------------------
  func_all_to_sd(data = ., table_option = 'o1') %>%
  func_all_to_sd(data = ., table_option = 'o2') %>%
  
  func_clean_nature_quant(data = ., column_name = 'nature_quantity_o1') %>%
  func_clean_nature_quant(data = ., column_name = 'nature_quantity_o2') %>%
  
  ## fill blanks using data from the column `Tools`, but only if there is only one tool in `Tools`
  func_fill_c2_from_c1(data = ., to_column = "MH_tool_o1", from_column = 'Tools') %>%
  func_fill_c2_from_c1(data = ., to_column = "MH_tool_o2", from_column = 'Tools') %>%
  dplyr::mutate(MH_tool_o1 = case_when(MH_tool_o1 == "GHQ" ~ "GHQ-12", TRUE ~ MH_tool_o1),
                MH_tool_o2 = case_when(MH_tool_o2 == "GHQ" ~ "GHQ-12", TRUE ~ MH_tool_o2)) %>%
  
  ## `func_fill_c2_from_c1()` is adapted from `func_fill_tool` (deleted) but can replace the latter
  func_fill_c2_from_c1(data = ., to_column = 'exposure_o1', from_column = "exposure_type") %>%
  func_fill_c2_from_c1(data = ., to_column = 'exposure_o2', from_column = "exposure_type") %>%
  func_fill_c2_from_c1(data = ., to_column = 'nature_type_o1', from_column = "nature_type") %>%
  func_fill_c2_from_c1(data = ., to_column = 'nature_type_o2', from_column = "nature_type") %>%
  
  ## clean text in nature-related columns 
  func_clean_nature_type(data = ., column_name = 'nature_type_o1', aggregate = F) %>%
  func_clean_nature_type(data = ., column_name = 'nature_type_o2', aggregate = F) %>%
  ## merge selected nature types
  func_clean_nature_type_customize(data = ., column_name = 'nature_type_o1') %>%
  func_clean_nature_type_customize(data = ., column_name = 'nature_type_o2') %>%
  
  dplyr::select(id, model_id, study_label, Region,
                all_of(cols_keep), 
                effect_size_indices:Mean, mean, 
                # CI_95_lower, ci95lower, 
                everything()) %>%
  arrange(effect_size_indices, id, model_id) %>%
  as.data.frame()


# names(exp_sub_comb_clean)
unique(exp_sub_comb_clean$MH_tool_o1) %>% cat('\nUnique MH_tool_o1: ', ., '\n')
unique(exp_sub_comb_clean$MH_tool_o2) %>% cat('\nUnique MH_tool_o2: ', ., '\n')

unique(exp_sub_comb_clean$id) %>% length() %>% cat('\n', ., 'unique papers for', mh_tool, '\n')





## - unify outcome direction}
unique(exp_sub_comb_clean$effect_size_indices)


##' Unify the direction of MH outcomes, mainly apply to the following tools --------------
##'   - GHQ-12 
##'   - SF
##'   - WHO-5
##'   - WEMWBS
exp_sub_mods <- exp_sub_comb_clean %>%
  dplyr::mutate(
    MH_direction_o1 = as.numeric(MH_direction_o1),
    MH_direction_o2 = as.numeric(MH_direction_o2),
    
    ##' fix missing data in `MH_direction_o`
    MH_direction_o1 = case_when(
      is.na(MH_direction_o1) & `Health outcome direction` == "Lower is better"  ~ -1, 
      is.na(MH_direction_o1) & `Health outcome direction` == "Higher is better" ~ 1, 
      TRUE ~ MH_direction_o1),
    
    MH_direction_o2 = case_when(
      is.na(MH_direction_o2) & `Health outcome direction` == "Lower is better"  ~ -1, 
      is.na(MH_direction_o2) & `Health outcome direction` == "Higher is better" ~ 1, 
      TRUE ~ MH_direction_o2),
    
    ## change coefficient-related outcomes to positive direction 
    mean = case_when(
      effect_size_indices %in% c("Standardized coefficients", "Unstandardized coefficients", "coefficient") &
        MH_direction_o1 == -1 ~ -mean,
      effect_size_indices == 'or' &
        MH_direction_o1 == -1 ~ 1-(mean-1),
      TRUE ~ mean),
    
    ci95lower = ifelse(MH_direction_o1 == 1, -ci95lower, ci95lower),
  ) %>%
  # dplyr::select(1:`Health outcome direction`, MH_direction_o1, everything()) %>%
  as.data.frame()




### subset `effect_size_indice`

##' further filter data based on MH tools ------------------------------------------------
source('./code/func_clean_indicatorsPro.R')

cat('\n\n')
unique(exp_sub_comb_clean$MH_tool_o1) %>% cat('unique MH_tool_o1:', ., '\n')
unique(exp_sub_comb_clean$MH_tool_o2) %>% cat('unique MH_tool_o2:', ., '\n')

if ( all(mh_tool %in% c('GHQ-12', "SF-12", "SF-36", 'WEMWBS', 'WHO-5')) ) {
  exp_sub_mods_coef <- exp_sub_mods %>%
    dplyr::filter((str_detect(pattern = paste(effect_size_ind_i, collapse = "|"), string = effect_size_indices))) %>%
    dplyr::filter(MH_tool_o1 %in% mh_tool | MH_tool_o2 %in% mh_tool) %>%
    func_clean_indicator_level2(data = ., column_name = 'MH_indicator_o1') %>%
    func_clean_indicator_level2(data = ., column_name = 'MH_indicator_o2') %>%
    as.data.frame() 
  
} else if ( all(mh_tool %in% c('PANAS', 'POMS', 'ROS', 'PSS')) ) {
  exp_sub_mods_md <- exp_sub_mods %>%
    dplyr::filter((str_detect(pattern = paste(effect_size_ind_i, collapse = "|"), string = effect_size_indices))) %>%
    dplyr::filter(MH_tool_o1 %in% mh_tool | MH_tool_o2 %in% mh_tool) %>%
    func_clean_indicator_level2(data = ., column_name = 'MH_indicator_o1') %>%
    func_clean_indicator_level2(data = ., column_name = 'MH_indicator_o2') %>%
    as.data.frame()
  ## make a copy of the data
  # exp_sub_mods_panas <- exp_sub_mods_md
  
} else {
  print('...')
}




## - print & check
### print and double-check results =======================================================
if ( any(mh_tool %in% c('GHQ-12', "SF-12", "SF-36", 'WEMWBS', 'WHO-5')) ) {
  exp_sub_mods_print <- exp_sub_mods_coef
} else {
  exp_sub_mods_print <- exp_sub_mods_md
}

## how many unique papers?
length(unique(exp_sub_mods_print$id)) %>% cat('\nFor', mh_tool, '------ \n In total, there are', ., 'unique papers.')
nrow(exp_sub_mods_print) %>% cat('\n In total, there are', ., 'studies. \n\n')

##' unique tools 
unique(exp_sub_mods_print$MH_tool_o1) %>% cat('unique MH_tool_o1:', ., '\n')
unique(exp_sub_mods_print$MH_tool_o2) %>% cat('unique MH_tool_o2:', ., '\n\n')

##' inspect data for further cleaning --------------------------------------------------
unique(exp_sub_mods_print$MH_indicator_o1) %>% sort() %>% 
  paste(., collapse = '\n\t', sep = '') %>%
  cat('MH_indicator_o1:\n\t', ., '\n\n');
unique(exp_sub_mods_print$MH_indicator_o2) %>% sort() %>% 
  paste(., collapse = '\n\t', sep = '') %>%
  cat('MH_indicator_o2:\n\t', ., '\n\n');


unique(exp_sub_mods_print$nature_type_o1) %>% sort()
unique(exp_sub_mods_print$nature_type_o2) %>% sort()
