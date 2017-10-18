overnight <- read.table(paste0(path_data, "/overnight/overnight.cost.tsv"), header=TRUE, sep ="\t", as.is = TRUE)
# Although the source has been identified (see below), how this data was compiled is unknown 
# variable.o.m was orginally handled as if it was reported in Mills/kWh (1 mill = 1/10 cent). This conclusion was made based off source 1.
# Upon inspection of Table 8.2 'Cost and Performance Characteristics...' (source 2) of the AEO, the following units are used.
  # variable.o.m in $/MWh
  # fixed.o.m in $/kW-yr
  # base.overnight in $/kW
# These should all be in units of MegaWatts (10^6) rather than kW (10^3).
# Also note that the USD$ year is indicated by cost.year. These values will be converted to 1975 values using GDP deflator data given by source 3.

  # Source 1 : http://www.eia.gov/tools/faqs/faq.cfm?id=19&t=5
  # Source 2 : https://www.eia.gov/outlooks/archive/aeo15/
  # Source 3 : https://fred.stlouisfed.org/series/GDPDEF

# relevant data -----------------------------------------------------------
overnight <- overnight %>%
  select( "year", "cost.year", "overnight_category", "base.overnight", "variable.o.m", "fixed.o.m") %>%
  arrange( year, cost.year, overnight_category)

# unit conversions --------------------------------------------------------
overnight$fixed.o.m <- overnight$fixed.o.m / 1000 # /kw -> /MW
overnight$base.overnight <- overnight$base.overnight / 1000 # /kw -> /MW

# dollar value conversions ------------------------------------------------
gdpdef <- read.csv(paste0(path_data,"/overnight/GDPDEF.csv"), header=TRUE, as.is=TRUE)
gdpdef$year <- ymd(gdpdef$DATE) %>% year()
gdpdef <- gdpdef %>%
  select(year, GDPDEF) %>%
  group_by(year) %>%
  summarise(GDPDEF = mean(GDPDEF)) %>%
  data.frame()
row.names(gdpdef) <- gdpdef$year
reference_deflation <- gdpdef['1975','GDPDEF']
gdpdef <- gdpdef %>%
  mutate(deflator75 = reference_deflation / GDPDEF) %>%
  select(-GDPDEF) %>%
  rename(cost.year=year)

overnight <- overnight %>%
  inner_join(gdpdef) %>%
  mutate(base.overnight = deflator75 * base.overnight,
         variable.o.m = deflator75 * variable.o.m,
         fixed.o.m = deflator75 * fixed.o.m) %>%
  select(-cost.year, -deflator75)


# save file    ------------------------------------------------------------
tsv1 <- file(paste0(path_data,"/overnight/val.adj.overnight.tsv"), "w")
write.table(val.adj.overnight, file = tsv1, sep="\t",col.names = TRUE, row.names = FALSE)
close(tsv1)



# summary files?? ---------------------------------------------------------
# this file combines overnight costs and summary, is called n by multinomial.r 
# need to val.adj 'adj.fuel.price' ?????

summary.cap.eia.year <- read.delim(paste0(path_data, "summary_files.tsv"), header=TRUE, as.is = TRUE)
summary.cap.eia.year <- summary.cap.eia.year %>%
  select("year", "overnight_category", "fuel_1_general", "adj.fuel.price", "N", "capacity_mw", "capacity.factor.avg", "pred.heat.rate")
  filter(year != 2100 & year != 2050) %>% # data for 1990-2015 + 2050 & 2100
  left_join(val.adj.overnight, by = c("overnight_category", "year")) %>%
  arrange(overnight_category, fuel_1_general, year)

tsv1 <- file(paste0(path_data,"processed.overnight.tsv"), "w")
write.table(summary.cap.eia.year, file = gz1, sep="\t",col.names = TRUE, row.names = FALSE)
close(tsv1)
