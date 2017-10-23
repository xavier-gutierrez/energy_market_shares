
# Data --------------------------------------------------------------------
# Generator-level data
cap.eia <- read.csv(paste(base, 'data-proc/eia860_CAsupplement.csv', sep='')) %>%
  select(year, plant_code, overnight_category, fuel_1_general, heat_rate)

# summary
summary <- read.csv(paste(base, 'data-proc/summary.csv',sep=''))

# average heat rate for (overnight, fuel, year) ---------------------------
hr <- cap.eia %>%
  group_by(overnight_category, fuel_1_general, year) %>%
  summarize(heat.rate.avg = mean(heat_rate, na.rm=TRUE)) %>%
  ungroup() %>%
  # if no data for an entire year, avg == NaN
  mutate(heat.rate.avg = ifelse(is.nan(heat.rate.avg), NA, heat.rate.avg)) 

# summary -----------------------------------------------------------------
summary.hr <- summary %>%
  left_join(hr, by=c('overnight_category','fuel_1_general','year')) %>% 
  # attach heat.rate.avg and capacity.factor.avg to overnight-fuel capacity
  arrange(overnight_category, fuel_1_general, year) 
  
ggplot(summary.hr) +
  geom_point(aes(x = year, y = heat.rate.avg)) +
  facet_wrap(~overnight_category + fuel_1_general) +
  labs(x='year', y='Btu/MWh', title='Average Heat Rate ')


# theoretical efficiency limits -> heat rates -----------------------------

theoretical.limits <- data.frame(overnight_category = c("biomass", "coal", "conventional combined cycle", "conventional combined cycle", 
                                                       "conventional combustion turbine", "conventional combustion turbine", "igcc"),
                                fuel_1_general = c("biomass", "coal", "natural gas", "oil", "natural gas", "oil", "coal"), 
                                year.2050 = c(0.31,0.45,0.57,0.59,0.38,0.39,0.48),
                                year.2100 = c(0.36,0.51,0.62,0.64,0.42,0.44,0.54))
# Source: The data is from Table 5 of 'Cost of power or power of cost: a US modeling perspective' by Muratori, Ledna, McJeon, et al.
# We can use this data to put some upper bounds on the heat rate gains by power plant type

theoretical.limits <- melt(theoretical.limits, id.vars = c("overnight_category", "fuel_1_general"))
colnames(theoretical.limits) <- c("overnight_category", "fuel_1_general", "year", "efficiency")
theoretical.limits <- theoretical.limits %>%
  mutate(year = as.character(year)) %>%
  mutate(year = replace(year, year=="year.2050", 2050)) %>%
  mutate(year = replace(year, year=="year.2100", 2100)) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(heat.rate.avg = 3412/efficiency) %>% # btu/kWh = 3412
  select(-efficiency)

summary.hr <- summary.hr %>%
  plyr::rbind.fill(theoretical.limits)

# heat rate linear regression ---------------------------------------------

h.r.model <- summary.hr %>% #514
  filter(heat.rate.avg > 0) %>% #230
  group_by(overnight_category, fuel_1_general) %>%
  complete(year = seq(from = 1990, to = 2100, by = 1)) %>% #1998
  ungroup() %>%
  mutate(time = year - 1989) %>%
  mutate(time_sq = time^2) %>%
  group_by(overnight_category, fuel_1_general) %>%
  do({mod <- lm(heat.rate.avg ~ time + time_sq, data = .)
  # The model is the same as heat_rate ~ time + time_sq + time*overnight_category + time_sq*overnight_category because of the group_by
  pred.heat.rate <- predict(mod, newdata = .[c("time", "time_sq")])
  data.frame(., pred.heat.rate)}) %>%
  select(overnight_category, fuel_1_general, year, pred.heat.rate) %>%
  mutate(pred.heat.rate = replace(pred.heat.rate, fuel_1_general=="solar", NA)) %>%
  mutate(pred.heat.rate = replace(pred.heat.rate, fuel_1_general=="wind", NA))
  # We still have overnight category, general fuel combinations with low sample sizes (even after implementing the aggregation measures). 

summary.hr <- summary.hr %>%
  left_join(h.r.model, by = c("overnight_category", "fuel_1_general", "year")) %>%
  arrange(overnight_category, fuel_1_general, year)

ggplot(summary.hr) +
  geom_line(aes(x = year, y = pred.heat.rate)) +
  geom_point(aes(x = year, y = heat.rate.avg)) +
  facet_wrap(~overnight_category + fuel_1_general) + 
  labs(x='year', y='Btu/MWh', title='Average/Predicted Heat Rate ')

# adj.fuel.price ----------------------------------------------------------

energy.prices <- read.table(paste0(base, "data-raw/energy.prices.txt.gz"), header=TRUE, sep ="\t")
uranium.prices <- read.table(paste0(base, "data-raw/uranium.prices.txt.gz"), header=TRUE, sep ="\t")
uranium.prices <- uranium.prices[,1:2] %>%
  # grab the year column and weighted average price across all sources
  mutate(fuel_1_general = "uranium")
  # add the necessary identifier
colnames(uranium.prices)[names(uranium.prices) == "weighted.avg.price.nominal"] <- "fuel.price"
energy.prices <- rbind(energy.prices, uranium.prices)

summary.hr <- summary.hr %>%
  left_join(energy.prices, by = c('fuel_1_general','year')) %>%
  mutate(adj.fuel.price = (fuel.price/10^6)*pred.heat.rate) %>% # $/MBtu * Btu/Kwh = $/Kwh
  mutate(adj.fuel.price = adj.fuel.price*1000) # $/kwh -> $/Mwh

# save files --------------------------------------------------------------
write.csv(summary.hr, paste(base, 'data-proc/summary.csv',sep=''), row.names=FALSE)

