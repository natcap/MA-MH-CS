
func_add_reviewer_id <- function(data) {
  d <- data %>%
    dplyr::mutate(Reviewer_id = case_when(
      Reviewer == 'Yingjie Li'    ~ 1, 
      Reviewer == 'Yuanyuan Mao'  ~ 2, 
      Reviewer == 'Zander Galli'  ~ 3, 
      Reviewer == 'Xin Lan'       ~ 4, 
      Reviewer == 'Anders Rydstrom' ~ 4.5,
      Reviewer == 'Renee Bertrand'  ~ 4.6,
      Reviewer == 'Emily Brieant'   ~ 5, 
      Reviewer == 'Carl Purisima' ~ 5.2,
      Reviewer == 'Sehrish Gohar' ~ 5.3,
      Reviewer == 'Bryan Diaz'    ~ 5.4,
      Reviewer == 'Katherine Wu'  ~ 6,
      Reviewer == 'test person'   ~ 7,
      Reviewer == 'Mary Lee'      ~ 8,
      Reviewer == 'Consensus'     ~ 99,
      TRUE                        ~ 999 ## other input
    )) %>%
    dplyr::select(id:Reviewer, Reviewer_id, everything()) %>%
    as.data.frame()
}



#' For quality assessment 
func_add_reviewer_id_qa <- function(data) {
  d <- data %>%
    dplyr::mutate(Reviewer_id = case_when(
      Reviewer == 'Consensus'       ~ 0,
      Reviewer == 'Yingjie Li'      ~ 1, 
      Reviewer == 'Anders Rydstrom' ~ 1.5,
      Reviewer == 'Yuanyuan Mao'    ~ 2, 
      Reviewer == 'Carl Purisima'   ~ 2.5,
      Reviewer == 'Zander Galli'    ~ 3, 
      
      Reviewer == 'Xin Lan'         ~ 4, 
      Reviewer == 'Renee Bertrand'  ~ 4.6,
      Reviewer == 'Emily Brieant'   ~ 5, 
      
      Reviewer == 'Sehrish Gohar'   ~ 5.3,
      Reviewer == 'Bryan Diaz'      ~ 5.4,

      TRUE                          ~ 999 ## other input
    )) %>%
    dplyr::select(id:Reviewer, everything()) %>%
    as.data.frame()
}
