
# plant_capacity_mw (form860) ---------------------------------------------


cap.eia = read.table(paste0(path_data, "/heat rate/ca_almanac_R.tsv"), header=TRUE, sep ="\t")
cap.eia = cap.eia %>%
  select(year, in_service, plant_code, overnight_category, fuel_1_general, heat_rate, summer_capacity)
# source: ca_almanac_R.r

plant.capacity = cap.eia %>%
  group_by(year, plant_code) %>%
  summarize(plant_capacity_mw = sum(summer_capacity))


# net.generation.annual.total/plant_capacity_mw = capacity.factor ----------


plant.generation.raw = read.table(file = paste0(path_data, "annual_plant_generation_eia.tsv"), header=TRUE, sep ="\t")
plant.generation.raw = plant.generation.raw %>% 
  select(year, plant_code, net.generation.annual)
# source unknown
plant.generation = plant.generation.raw %>%
  group_by(plant_code, year) %>%
  summarize(net.generation.annual.total = sum(net.generation.annual)) %>%
  left_join(plant.capacity, by = c('plant_code', 'year')) %>% 
  # filters out some plants we have 860/CA heat_rate/capacity data for but not generation data
  mutate(capacity.factor = net.generation.annual.total/(8760*plant_capacity_mw)) %>%
  # energy actually produced / (1 yr * capacity)
  mutate(capacity.factor = replace(capacity.factor, capacity.factor>1, NA)) %>%
  ### THE CAPACITY FACTOR WORK NEEDS TO BE CHECKED (SOME VALUES GREATER THAN ONE)
  # NEED TO ACCOUNT FOR THE NUMBER OF GENERATORS AT THE PLANT
  select(plant_code, year, capacity.factor)
  # Calculate the plant level capacity factor (we don't have enough information for generator level so we assign the plant CF to each generator)



# average (capacity factor, heat rate) for (overnight, fuel, year) --------


# We get weird numbers if we calculate the capacity factors only from the new units so we calculate them using the entire population
cf.hr = cap.eia %>%
  left_join(plant.generation, by = c('plant_code','year')) %>% 
  #augment w/ capacity.factor info, but not for all cap.eia generators  
  # assigns PLANT capacity.factor to each generator within
  group_by(overnight_category, fuel_1_general, year) %>%
  summarize(heat_rate_avg = mean(heat_rate, na.rm=TRUE),
            capacity.factor.avg = mean(capacity.factor, na.rm=TRUE)) %>%
  ungroup() 
# averaging over a year w/o any data at all yields NaN (even with na.rm=TRUE). replace NaN with NA
cf.hr[is.nan(cf.hr$heat_rate_avg), "heat_rate_avg"] <- NA
cf.hr[is.nan(cf.hr$capacity.factor.avg), "capacity.factor.avg"] <- NA

# plot/save capacity.factor.avg figures 
p = ggplot(cf.hr) +
  geom_point(aes(x = year, y = capacity.factor.avg)) +
  facet_wrap(~overnight_category + fuel_1_general)
dev.off()
pdf(file = paste0(path_figure, 'calculated.capacity.factor.pdf'))
dev.off()

## test changes to match summary.cap.eia.year final size 
cf.hr1  = cap.eia %>% 
  filter(in_service == year) %>%
  filter(overnight_category != 'undefined') %>%
  filter(is.na(fuel_1_general) == FALSE) %>%
  filter(is.na(in_service) == FALSE) %>%
  # Remove entries that don't have the inservice date
  filter(in_service >= 1990) %>% #SIZE 7508
  left_join(plant.generation, by = c('plant_code','year')) %>% # assigns PLANT capacity.factor to each generator within
  group_by(overnight_category, fuel_1_general, in_service) %>%
  summarize(heat_rate_avg = mean(heat_rate, na.rm=TRUE),
            capacity.factor.avg = mean(capacity.factor, na.rm=TRUE)) %>% # SIZE 239
  arrange(overnight_category, fuel_1_general, in_service) %>%
  ungroup() %>% # SIZE 239
  group_by(overnight_category, fuel_1_general) %>%
  complete(in_service = seq(from = 1990, to = 2014, by = 1))   # creates row for each year, fills w/ NA if no data 
  rename(year = in_service)

# averaging over a year w/o any data at all yields NaN (even with na.rm=TRUE). replace NaN with NA
cf.hr1[is.nan(cf.hr1$heat_rate_avg), "heat_rate_avg"] <- NA
cf.hr1[is.nan(cf.hr1$capacity.factor.avg), "capacity.factor.avg"] <- NA


# plot/save capacity.factor.avg figures 
p = ggplot(cf.hr1) +
  geom_point(aes(x = year, y = capacity.factor.avg)) +
  facet_wrap(~overnight_category + fuel_1_general)
dev.off()
pdf(file = paste0(path_figure, 'calculated.capacity.factor.new.pdf'))
p
dev.off()



# summary -----------------------------------------------------------------


summary.cap.eia.year = cap.eia %>% # SIZE 3528709
  filter(in_service == year) %>% # SIZE 7581
  # Prevents double counting and only counts a generator in the year that it came online 
  #### SHOULD ALSO CONSIDER STATUS_CODE??? probably should be implemented further upstream
  filter(overnight_category != 'undefined') %>%
  filter(is.na(fuel_1_general) == FALSE) %>%
  filter(is.na(in_service) == FALSE) %>%
  # Remove entries that don't have the inservice date
  filter(in_service >= 1990) %>% # FINAL SIZE 7508
  # Nuclear capacity has not been added since the early 1990s. This is why the capacity factor isn't showing up for nuclear (i.e., the above constraint in_service >= 1990)
  group_by(overnight_category, fuel_1_general, in_service) %>%
  summarize(capacity_mw = sum(summer_capacity, na.rm=TRUE)) %>% # SIZE 239
  arrange(overnight_category, fuel_1_general, in_service) %>%
  ungroup() %>%
  group_by(overnight_category, fuel_1_general) %>%
  complete(in_service = seq(from = 1990, to = 2014, by = 1)) %>% # 
  # creates row for each year, fills w/ NA if no data 
  # SIZE 500
  ungroup() %>%
  mutate(capacity_mw = replace(capacity_mw, is.na(capacity_mw)==TRUE, 0)) %>%
  rename(year = in_service) %>%
  left_join(cf.hr, by=c('overnight_category','fuel_1_general','year')) %>%
  arrange(overnight_category, fuel_1_general, year) 

# plot/save capacity.factor.avg figures 
p = ggplot(summary.cap.eia.year) +
  geom_point(aes(x = year, y = capacity.factor.avg)) +
  facet_wrap(~overnight_category + fuel_1_general)
dev.off()
pdf(file = paste0(path_figure, 'calculated.capacity.factor.new.pdf'))
p
dev.off()



# theoretical efficiency limits -> heat rates -----------------------------

theoretical.limits = data.frame(overnight_category = c("biomass", "coal", "conventional combined cycle", "conventional combined cycle", 
                                                       "conventional combustion turbine", "conventional combustion turbine", "igcc"),
                                fuel_1_general = c("biomass", "coal", "natural gas", "oil", "natural gas", "oil", "coal"), 
                                year.2050 = c(0.31,0.45,0.57,0.59,0.38,0.39,0.48),
                                year.2100 = c(0.36,0.51,0.62,0.64,0.42,0.44,0.54))
# Source: The data is from Table 5 of 'Cost of power or power of cost: a US modeling perspective' by Muratori, Ledna, McJeon, et al.
# We can use this data to put some upper bounds on the heat rate gains by power plant type

theoretical.limits = melt(theoretical.limits, id.vars = c("overnight_category", "fuel_1_general"))
colnames(theoretical.limits) = c("overnight_category", "fuel_1_general", "year", "efficiency")
theoretical.limits = theoretical.limits %>%
  mutate(year = as.character(year)) %>%
  mutate(year = replace(year, year=="year.2050", 2050)) %>%
  mutate(year = replace(year, year=="year.2100", 2100)) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(heat_rate = 3412/efficiency)
  # We can convert the efficiency back to a heat rate using the equivalent btu content of a kWh (~3,412 btu). This is also in the documentation. 

summary.cap.eia.year = summary.cap.eia.year %>%
  plyr::rbind.fill(theoretical.limits)




# heat rate linear regression ---------------------------------------------


h.r.model = summary.cap.eia.year %>%
  filter(heat_rate > 0) %>% # size 595
  group_by(overnight_category, fuel_1_general) %>%
  complete(year = seq(from = 1990, to = 2100, by = 1)) %>% 
  ungroup() %>%
  mutate(time = year - 1989) %>%
  mutate(time_sq = time^2) %>%
  group_by(overnight_category, fuel_1_general) %>%
  do({mod <- lm(heat_rate ~ time + time_sq, data = .)
  # The model is the same as heat_rate ~ time + time_sq + time*overnight_category + time_sq*overnight_category because of the group_by
  pred.heat.rate <- predict(mod, newdata = .[c("time", "time_sq")])
  data.frame(., pred.heat.rate)}) %>%
  select(c(overnight_category, fuel_1_general, year, pred.heat.rate)) %>%
  mutate(pred.heat.rate = replace(pred.heat.rate, fuel_1_general=="solar", NA)) %>%
  mutate(pred.heat.rate = replace(pred.heat.rate, fuel_1_general=="wind", NA))
  # We still have overnight category, general fuel combinations with low sample sizes (even after implementing the aggregation measures). 

summary.cap.eia.year = summary.cap.eia.year %>%
  left_join(h.r.model, by = c("overnight_category", "fuel_1_general", "year")) %>%
  arrange(overnight_category, fuel_1_general, year)

p = ggplot(summary.cap.eia.year) +
  geom_line(aes(x = year, y = pred.heat.rate))+
  geom_point(aes(x = year, y = heat_rate)) +
  facet_wrap(~overnight_category + fuel_1_general)

dev.off()
pdf(file = paste0(path_figure, 'heat.rate.regression.pdf'))
p
dev.off()



# adj.fuel.price ----------------------------------------------------------



energy.prices = read.table(paste0(path_data, "energy.prices.txt.gz"), header=TRUE, sep ="\t")

uranium.prices = read.table(paste0(path_data, "uranium.prices.txt.gz"), header=TRUE, sep ="\t")
uranium.prices = uranium.prices[,1:2] %>%
  # grab the year column and weighted average price across all sources
  mutate(fuel_1_general = "uranium")
  # add the necessary identifier
colnames(uranium.prices)[names(uranium.prices) == "weighted.avg.price.nominal"] = "fuel.price"
energy.prices = rbind(energy.prices, uranium.prices)

summary.cap.eia.year = summary.cap.eia.year %>%
  left_join(energy.prices, by = c('fuel_1_general','year')) %>%
  mutate(adj.fuel.price = (fuel.price/10^6)*pred.heat.rate) %>%
  # Fuel price is in $ per 10^6 btu, which we convert to $ per kWh using the heat rate and fuel price.
  mutate(adj.fuel.price = adj.fuel.price*1000)
  # Convert units from dollars per kWh to dollars per MWh (done this way for readability)

gz1 = gzfile(paste(path_data,"summary_files.txt.gz", sep=""), "w")
write.table(summary.cap.eia.year, file = gz1, sep="\t",col.names = TRUE, row.names = FALSE)
close(gz1)
# Output the cleaned up data to the data folder as a .txt.gz file

