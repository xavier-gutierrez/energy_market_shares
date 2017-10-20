overnight <- read.table(paste0(base, "data-raw/overnight.cost.tsv"), header=TRUE, sep ="\t", as.is = TRUE)
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
overnight <- overnight %>% 
  mutate(fixed.o.m = fixed.o.m/1000/8760,# /kWyr -> /MWh
         base.overnight = base.overnight/1000) # /kW -> MW

# dollar value conversions ------------------------------------------------
gdpdef <- read.csv(paste(base,"data-raw/GDPDEF.csv",sep=''))
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

adj.overnight <- overnight %>%
  inner_join(gdpdef) %>%
  mutate(base.overnight = deflator75 * base.overnight,
         variable.o.m = deflator75 * variable.o.m,
         fixed.o.m = deflator75 * fixed.o.m) %>%
  select(-cost.year, -deflator75)

o.m <- adj.overnight %>%
  select(-base.overnight) %>%
  melt( id.vars=c('year', 'overnight_category'), 
        measure.vars=c('variable.o.m', 'fixed.o.m'),
        variable.name='cost')

ggplot(o.m) +
  geom_point(aes(x=year, y=value, colour=cost)) +
  facet_wrap(~overnight_category) +
  labs(x='year', y='1975$/MWh', title='O&M Costs (1997-2015)')

select(adj.overnight, year, overnight_category,base.overnight) %>%
  ggplot() +
  geom_point(aes(x=year,y=base.overnight)) +
  facet_wrap(~overnight_category) +
  labs(x='year', y='1975$/MW', title='Overnight Costs (1997-2015)')

# summary files?? ---------------------------------------------------------
# need to val.adj 'adj.fuel.price' ?????
summary.cap.eia.year <- read.csv(paste(base, "data-proc/summary.csv", sep=''))
summary.cap.eia.year <- summary.cap.eia.year %>%
  select("year", "overnight_category", "fuel_1_general", "adj.fuel.price", "N", "capacity_mw", "capacity.factor.avg", "pred.heat.rate")
  filter(year != 2100 & year != 2050) %>% # data for 1990-2015 + 2050 & 2100
  left_join(adj.overnight, by = c("overnight_category", "year")) %>%
  arrange(overnight_category, fuel_1_general, year)

# save file    ------------------------------------------------------------
write.csv(val.adj.overnight, paste(base, 'data-proc/adj.overnight.csv'), row.names=FALSE)
write.csv(summary.cap.eia.year, paste(base, 'data-proc/final.csv', sep=''), row.names=FALSE)
          