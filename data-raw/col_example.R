# from ?lagsarlm
data(oldcol)
columbus_crime <- COL.OLD %>% tbl_df() %>%
  transmute(
    # convert crime rate to integer index
    crime_i = as.integer(round(CRIME / 10, 0)),
    income = INC, home_value = HOVAL
  )

devtools::use_data(columbus_crime)

columbus_neighbors <- nb2listw(COL.nb, style = "W")
devtools::use_data(columbus_neighbors)



