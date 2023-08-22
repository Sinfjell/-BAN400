library(dplyr)
library(tidyr)
library(foreign)

df <- 
  read.dta("data-ESS8NO.dta") %>%
  transmute(
    party = prtvtbno, 
    age = agea, 
    religiosity = rlgdgr, 
    income_decile = hinctnta)