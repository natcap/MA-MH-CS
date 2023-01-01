
getwd()

library(dplyr)
library(readr)

list.files(pattern = 'csv$')


s1 <- readr::read_csv("scopus.csv")
s2 <- readr::read_csv("scopus (2).csv")


ss_dif1Y2N <- anti_join(x = s1, y = s2, by = 'EID')
ss_dif2Y1N <- anti_join(x = s2, y = s1, by = 'EID')

readr::write_csv(x = ss_dif1Y2N, file = 'ss_dif1Y2N.csv')
readr::write_csv(x = ss_dif2Y1N, file = 'ss_dif2Y1N.csv')
# writexl::write_xlsx(x = ss_dif, path = 'ss_dif.xlsx')
