######## match CA plant codes w/ EIA plant codes ######## 
ca.mapping = read.table(paste0(path_data, "/heat rate/ca.mapping.tsv"), header=TRUE, sep="\t")
# Source: http://www.energy.ca.gov/almanac/electricity_data/web_qfer/, 'Power Plant ID Cross Reference Table.xls'
# Dataset that maps the unique identifier in the CA dataset to the EIA identification system
# Note: Also source of heat rate data in next section
ca.mapping = ca.mapping %>%
  filter(is.na(plant_code)==FALSE) %>%
  mutate(plant.id = tolower(plant.id)) %>%
  mutate(plant.id = gsub(" ", "", plant.id)) %>%
  distinct(plant.id, plant_code, .keep_all = TRUE) %>%
  select(plant.id, plant_code) #CA Energy Commision ID && EIA Plant Code #'s


######## store plant heat rates ######## 
ca.elec.almanac = read.table(paste0(path_data, "/heat rate/ca.elec.almanac.tsv"), header=TRUE, sep="\t")
# source? http://www.energy.ca.gov/almanac/electricity_data/web_qfer/Heat_Rates.php
# contains heat rate, fuel consumption, and energy production data
ca.elec.almanac = ca.elec.almanac %>%
  filter(heat.rate != 0) %>%
  mutate(plant.id = gsub(" ", "", plant.id)) %>%
  mutate(plant.id = tolower(plant.id)) %>%
  left_join(ca.mapping, by = "plant.id") %>% # join ca.mapping (which contains EIA ID's) to ca.elec.almanac 
  mutate(heat.rate = 1000*heat.rate) %>%
  # Converts units from MMbtu/MWh to btu/kWh (units favored by the EIA)
  select(plant_code, year, heat.rate) %>% #EIA Plant Code #'s, Year, & Heat Rate
  filter(year != 2015)


######## update EIA plant information w/ CA heat rate data ######## 
cap.eia = read.table(paste0(path_data, "/heat rate/mapping_860_overnight_R.tsv"), header=TRUE, sep ="\t")
# source unknown
# contains fuel and overnight_category information, as well as heat rate data
cap.eia = cap.eia %>%
  mutate(heat_rate = replace(heat_rate, heat_rate == 0, NA)) %>%
  left_join(ca.elec.almanac, by=c("plant_code", "year")) %>% 
  # join CA almanac heat rates data to mapping_860_overnight_R.tsv data
  # multiple plants w/in same year ? 5005 original heat.rates, yet pasted 11243 times in final cap.eia dataframe
  mutate(heat_rate = ifelse(is.na(heat_rate)==FALSE, heat_rate, heat.rate))
  # where 860 heat_rate data exists, leave. where NA, replace with CA heat.rate data
  # select(year, plant_code, summer_capacity)
imp.cols = names(cap.eia)[match(c("year", "plant_code", "overnight_category", "heat_rate", "summer_capacity"), names(cap.eia))]
unimp.cols = names(cap.eia)[!(names(cap.eia) %in% imp.cols)]
cap.eia = arrange(cap.eia[ , c(imp.cols, unimp.cols)])


######## save file ######## 
gz1 = gzfile(paste(path_data,"/heat rate/ca_almanac_R.tsv", sep=""), "w")
write.table(cap.eia, file = gz1, sep="\t",col.names = TRUE, row.names = FALSE)
close(gz1)