# Data --------------------------------------------------------------------
# Generator-level data
cap.eia <- read.csv(paste(base, 'data-proc/eia860_CAsupplement.csv', sep='')) %>%
  select(year, in_service, plant_code, overnight_category, fuel_1_general, heat_rate, summer_capacity)

#Plant-level data
plant.generation.raw <- read.table(paste(base, "data-raw/annual_plant_generation_eia.tsv",sep=''), sep='\t', header=TRUE) %>%
  select(year, plant_code, net.generation.annual)

# summary
summary <- read.csv(paste(base, 'data-proc/summary.csv',sep=''))

# total capacity: sum over all plant's generators -------------------------
plant.capacity <- cap.eia %>%
  group_by(year, plant_code) %>%
  summarize(total.capacity = sum(summer_capacity))

# calculate each plant's capacity factor ----------------------------------
cf <- plant.generation.raw %>%
  group_by(plant_code, year) %>%
  summarize(total.generation = sum(net.generation.annual)) %>%
  ungroup() %>%
  left_join(plant.capacity, by = c('plant_code', 'year')) %>% 
  # filters out some plants we have 860/CA heat_rate/capacity data for but not generation data
  mutate(capacity.factor = total.generation/(8760*total.capacity)) %>%
  # energy actually produced / (1 yr * capacity)
  mutate(capacity.factor = replace(capacity.factor, capacity.factor>1, NA)) %>%
  ### THE CAPACITY FACTOR WORK NEEDS TO BE CHECKED (SOME VALUES GREATER THAN ONE)
  # NEED TO ACCOUNT FOR THE NUMBER OF GENERATORS AT THE PLANT
  select(plant_code, year, capacity.factor)

# average for (overnight, fuel, year) -------------------------------------
# We get weird numbers if we calculate the capacity factors only from the new units so we calculate them using the entire population
cf.avg <- cap.eia %>%
  left_join(cap.fac, by=c('plant_code', 'year')) %>% # assign 
  group_by(overnight_category, fuel_1_general, year) %>%
  summarise(capacity.factor.avg = mean(capacity.factor, na.rm=TRUE)) %>%
  ungroup() %>%
  # if no data for an entire year, avg == NaN
  mutate(capacity.factor.avg = ifelse(is.nan(capacity.factor.avg), NA, capacity.factor.avg))

# summary -----------------------------------------------------------------
summary.cf <- summary %>%
  left_join(cf.avg, by=c('overnight_category','fuel_1_general','year')) %>% 
  # attach heat.rate.avg and capacity.factor.avg to overnight-fuel capacity
  arrange(overnight_category, fuel_1_general, year) 

# plot & save -------------------------------------------------------------
ggplot(summary.cf) +
  geom_point(aes(x = year, y = capacity.factor.avg)) +
  facet_wrap(~overnight_category + fuel_1_general) + 
  labs(x='year', y='Fraction', title='Average Capacity Factor ')

write.csv(summary.cf, paste(base, 'data-proc/capacity_factors.csv',sep=''), row.names=FALSE)




