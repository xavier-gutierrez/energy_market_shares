
# Data --------------------------------------------------------------------
cap.raw <- read.table(paste(base, "data-raw/capacity_eia.tsv", sep=""), sep="\t", header=TRUE, comment.char="") %>%
  select(-fuel_2, -fuel_3) %>% 
  rename(fuel = fuel_1) %>%
  mutate(generator_code = tolower(generator_code)) %>%
  mutate(generator_code = gsub(" ","", generator_code)) %>%
  mutate(generator_code = gsub("[^a-zA-Z0-9]","", generator_code)) %>%
  mutate(summer_capacity = as.numeric(summer_capacity)) 

retirement <- read.table(paste(base, "data-raw/retirement.eia860.txt", sep=""), sep="\t", header=TRUE, comment.char="", as.is = TRUE) %>%
  mutate(generator_code = tolower(generator_code)) %>%
  mutate(generator_code = gsub(" ","", generator_code)) %>%
  mutate(generator_code = gsub("[^a-zA-Z0-9]","", generator_code)) %>%
  mutate(retirement = as.integer(retirement)) 
  
movers <- read.csv(paste(base,'860_processing/prime_movers.csv',sep=''))
fuels <- read.csv(paste(base,'860_processing/fuels.csv',sep='')) 
eia.mapping <- read.csv(paste(base,'860_processing/overnight_categories.csv',sep='')) %>%
  select(-prime_mover_text, -fuel_text)

# Data errors --------------------------------------------------------------

cap.eia <- cap.raw %>%
  left_join(retirement, by = c('utility_code', 'plant_code', 'generator_code', 'year')) %>%
  mutate(retirement = replace(retirement, retirement == 9999, NA)) %>%
  # Missing value indicator in some years
  mutate(retirement = replace(retirement, retirement == 1, NA)) %>%
  # No idea what the one means
  mutate(retirement = replace(retirement, which(retirement > year), NA)) %>%
  # If the retirement year is greater than the actual year then this is a planned retirement year. If it actually retires then we'll care otherwise not.
  mutate(retirement = replace(retirement, retirement == 95, 1995)) %>%
  # The mistake seems to be an abbreviated date, it only applies to 21 entries across 95, 96, 97 so we can undo the correction if we want.
  mutate(retirement = replace(retirement, retirement == 96, 1996)) %>%
  mutate(retirement = replace(retirement, retirement == 97, 1997)) %>%
  mutate(retirement = replace(retirement, retirement == 0, NA)) %>%
  mutate(retirement = ifelse(is.na(retirement) & status_code_1=='RE', 1989, retirement)) %>%
  mutate(retirement = ifelse(is.na(retirement) & status_code_2 == 'RE', 1989, retirement)) %>%
  mutate(fuel = replace(fuel, fuel=="BL", "BLQ")) %>%
  # We are assuming that BL was incorrectly entered BLQ
  filter(summer_capacity != 0) %>%
  # Remove plants with no summer time capacity (not sure why they exist in the data anyway)
  mutate(in_service = replace(in_service, in_service==0, NA)) %>%
  # recodes an inservice date of '0' as missing
  mutate(summer_capacity = ifelse(summer_capacity < 0, summer_capacity*-1, summer_capacity)) %>%
  # It appears that some capacity values were mistakenly entered as negative, which we correct
  mutate(summer_capacity = ifelse(year >= 1990 & year <= 2000, summer_capacity/1000, summer_capacity)) %>%
  # For years 1990 to 2000 summer capacity is in kW, which we change to MW
  mutate(prime_mover = toupper(prime_mover)) # one case of ic (instead of IC)
  
# Filter status codes -----------------------------------------------------

cap.eia <- cap.eia %>%
  filter(status_code_1 != "RE" & status_code_2 != "RE") %>%
  filter(status_code_2 != "RA") %>%
  filter(status_code_2 != "CN") %>%
  filter(status_code_2 != "PL" & status_code_2 != "P" & status_code_2 != "IP") %>%
  filter(status_code_2 != "CO" & status_code_2 != "L" & status_code_2 != "T") %>%
  filter(status_code_2 != "U" & status_code_2 != "V" & status_code_2 != "TS" & status_code_1 != "TS") %>%
  filter(status_code_2 != "LE") 

# Map mover & fuel onto overnight cost categories -------------------------
# unique combinations of primary fuel and production technology 

cap.eia <- cap.eia %>%
  left_join(eia.mapping, by = c("prime_mover", "fuel"))

# Save --------------------------------------------------------------------
write.csv(cap.eia, file=paste(base, "data-proc/eia860_processed.csv", sep=''), row.names=FALSE)
