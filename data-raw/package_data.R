library(haven)
library(dplyr)
library(spdep)

# dataset
firmbirth <- read_sas("data-raw/data2.sas7bdat") %>% tbl_df()
devtools::use_data(firmbirth, overwrite = TRUE)

# spatial weights
wqueen <- read_sas("data-raw/wqueen.sas7bdat") %>%
  as.matrix() %>%
  as(., "CsparseMatrix")
devtools::use_data(wqueen, overwrite = TRUE)
