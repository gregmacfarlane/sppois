# from ?lagsarlm
data(oldcol)
mydata <- COL.OLD %>% tbl_df() %>%
  transmute(
    # convert crime rate to integer index
    crime_i = as.integer(round(CRIME / 10, 0)),
    income = INC, home_value = HOVAL
  )
nblist <- nb2listw(COL.nb, style = "W")



