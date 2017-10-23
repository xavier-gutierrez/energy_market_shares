
# Data --------------------------------------------------------------------
cap.eia <- read.csv(paste(base, 'data-proc/eia860_CAsupplement.csv', sep='')) %>%
  select(year, in_service, overnight_category, fuel_1_general)


# Summary -----------------------------------------------------------------

summary <- cap.eia %>% # SIZE 3528709
  filter(in_service == year) %>% # SIZE 7581
  # Prevents double counting and only counts a generator in the year that it came online 
  filter(overnight_category != 'undefined') %>% #7563
  filter(! is.na(fuel_1_general)) %>% #7508
  filter(! is.na(in_service)) %>% #7508
  filter(in_service >= 1990) %>% # FINAL SIZE 7508
  # Nuclear capacity has not been added since the early 1990s. This is why the capacity factor isn't showing up for nuclear (i.e., the above constraint in_service >= 1990)
  distinct(overnight_category, fuel_1_general, in_service) %>%
  group_by(overnight_category, fuel_1_general) %>%
  complete(in_service=seq(from=1990, to=2014, by=1)) %>%
  ungroup() %>%
  rename(year=in_service)


# Save --------------------------------------------------------------------

write.csv(summary, paste(base, 'data-proc/summary.csv',sep=''), row.names=FALSE)

