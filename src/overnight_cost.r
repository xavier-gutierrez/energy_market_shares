overnight = read.table(paste0(path_data, "/overnight/overnight.cost.tsv"), header=TRUE, sep ="\t", as.is = TRUE)
# How this data was compiled is unknown, although the source is (see below)
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


########  relevant data         ########
overnight = overnight %>%
  select( "year", "cost.year", "overnight_category", "base.overnight", "variable.o.m", "fixed.o.m") %>%
  arrange( year, cost.year, overnight_category)

########  unit conversions      ########
overnight$fixed.o.m = overnight$fixed.o.m / 1000 
overnight$base.overnight = overnight$base.overnight / 1000

######## average over each year ########
gdpdef = read.table(paste0(path_data,"/overnight/GDPDEF.csv"), header=TRUE, sep=",", as.is=TRUE)
gdpdef$DATE = as.POSIXlt(gdpdef$DATE, format = "%Y-%m-%d") #convert to POSIXct class
gdpdef$year = year(gdpdef$DATE) # add year column
gdpdef.grp.year = gdpdef[c("GDPDEF", "year")] %>% 
  group_by(year) %>%
  summarize(avg=mean(GDPDEF))
  #summarize(range = max(GDPDEF) - min(GDPDEF),
  #         avg = mean(GDPDEF))
gdpdef.grp.year = data.frame(gdpdef.grp.year) #return to data.frame (tibble after pipe operations)

# inspect values 
a = ggplot(gdpdef.grp.year, aes(x=year, y=range)) + geom_point() 

# inspect variance within years
gdpdef$month = factor(month(gdpdef$DATE)) # add month column
b = ggplot(gdpdef, aes(x=year, y=GDPDEF)) + geom_point(aes(color=month)) # inspect variance within years

######## dollar value conversions ######## 
val.conversion = overnight[c("cost.year", "year")]  
rows = match(val.conversion$cost.year, gdpdef.grp.year$year) 
# positions in gdpdef.grp.year that give the GDPDEF for the cost.year
val.conversion = val.conversion %>% 
  mutate( deflator= gdpdef.grp.year[rows,"avg"], #populate column w/ GDPDEF values
          sevenfive.value.factor = gdpdef.grp.year[gdpdef.grp.year$year==1975, "avg"] / deflator ) #multiply by cost to get in 1975$

######## save file                ######## 
val.adj.overnight = overnight %>%
  mutate(val.adj.base.overnight = base.overnight * val.conversion$sevenfive.value.factor,
         val.adj.variable.o.m = variable.o.m * val.conversion$sevenfive.value.factor,
         val.adj.fixed.o.m = fixed.o.m * val.conversion$sevenfive.value.factor) %>%
  select("overnight_category", "year", starts_with("val.adj."))

  
tsv1 = file(paste0(path_data,"/overnight/val.adj.overnight.tsv"), "w")
write.table(val.adj.overnight, file = tsv1, sep="\t",col.names = TRUE, row.names = FALSE)
close(tsv1) 


######## this file combines overnight costs and summary, is called n by multinomial.r ######## 
## need to val.adj 'adj.fuel.price' ?????

summary.cap.eia.year = read.table(paste0(path_data, "summary_files.tsv"), header=TRUE, sep ="\t", as.is = TRUE)
summary.cap.eia.year = summary.cap.eia.year %>%
  select("year", "overnight_category", "fuel_1_general", "adj.fuel.price", "N", "capacity_mw", "capacity.factor.avg", "pred.heat.rate")
  filter(year != 2100 & year != 2050) %>% # data for 1990-2015 + 2050 & 2100
  left_join(val.adj.overnight, by = c("overnight_category", "year")) %>%
  arrange(overnight_category, fuel_1_general, year)

tsv1 = file(paste0(path_data,"processed.overnight.tsv"), "w")
write.table(summary.cap.eia.year, file = gz1, sep="\t",col.names = TRUE, row.names = FALSE)
close(tsv1) 