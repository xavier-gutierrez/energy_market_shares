eia.mapping <- cap.eia %>%
  left_join(movers, by = 'prime_mover') %>%
  left_join(fuels, by = c('fuel'='fuel')) %>%
  select(prime_mover, fuel, prime_mover_text, fuel_text) %>%
  distinct() %>%
  mutate(overnight_category = "") %>%
  mutate(overnight_category = ifelse(prime_mover == "CG" | prime_mover == "OT" | fuel == "WOC" | fuel == "MF" | 
                                       fuel == "OTH" | fuel == "" | fuel == "BL", "undefined", overnight_category)) %>%
  # CG, BL, and WOC not defined anywhere in the supporting documentation (that I can find)
  # OT, OTH, "", and MF do not provide enough information to assign either the fuel or production technology to these units
  mutate(overnight_category = ifelse(fuel == "WAT", "hydro", overnight_category)) %>%
  # hydropower assignment, which moves two sets of units from undefined to hydro (HC, "") and (HR, "")
  mutate(overnight_category = ifelse(fuel == "", 
                                     ifelse(prime_mover == "HC" | prime_mover == "HR", "hydro", 
                                            overnight_category), overnight_category)) %>%
  # some hydropower are missing a fuel so assign these to hydro overnight cost
  mutate(overnight_category = ifelse(fuel == "GEO" | fuel == "GST", "geothermal", overnight_category)) %>%
  # geothermal assignment
  mutate(overnight_category = ifelse(prime_mover == "PV" | prime_mover == "SP", "photovoltaic", overnight_category)) %>%
  # photovoltaic assignment
  mutate(overnight_category = ifelse(prime_mover != "PV" & prime_mover != "SP" & fuel == "SUN", 
                                     "solar thermal", overnight_category)) %>%
  # solar thermal assignment
  mutate(overnight_category = ifelse(fuel == "WND", 'wind', overnight_category)) %>%
  # onshore wind assignment
  mutate(overnight_category = ifelse(fuel == "UR" | fuel == "TH" | fuel == "NUC", 'nuclear', overnight_category)) %>%
  # nuclear assignment
  mutate(overnight_category = ifelse(prime_mover == "FC", 'fuel cell', overnight_category)) %>%
  mutate(overnight_category = ifelse(fuel == "MSW", 'municipal solid waste', overnight_category)) %>%
  mutate(overnight_category = ifelse(fuel == "MSW", 'municipal solid waste', overnight_category)) %>%
  mutate(overnight_category = ifelse(prime_mover == "ST", 
                                     ifelse(fuel == "LFG" | fuel == "SNG" |  fuel == "OG" | 
                                              fuel == "NG" | fuel == "LPG" | fuel == "BFG" |
                                              fuel == "OBG" | fuel == "BLQ",'steam turbine', 
                                            overnight_category), overnight_category)) %>%
  # steam turbine running on natural gas assignment
  mutate(overnight_category = ifelse(prime_mover == "ST", 
                                     ifelse(fuel == "BLQ" | fuel == "DFO" |  fuel == "FO1" | 
                                              fuel == "FO2" | fuel == "FO4" | fuel == "FO6" |
                                              fuel == "RFO" | fuel == "WO" | fuel == "SC" |
                                              fuel == "RFO" | fuel == 'SLW','steam turbine', 
                                            overnight_category), overnight_category)) %>%
  # steam turbine running on oil assignment
  mutate(overnight_category = ifelse(prime_mover == "IG", "igcc", overnight_category)) %>%
  # integrated gasification combined cycle assignment
  mutate(overnight_category = ifelse(fuel != "PC" | fuel != "OTH", ifelse(prime_mover == "JE" | prime_mover == "IC" | 
                                                                            prime_mover == "GT" | prime_mover == "ic",
                                                                          "conventional combustion turbine", overnight_category), overnight_category)) %>%
  # conventional combustion turbine assignment but don't assign the petroleum coke units (those are more similar to coal fired)
  mutate(overnight_category = ifelse(prime_mover == "CH" & fuel == "NG", "conventional combustion turbine", overnight_category)) %>%
  mutate(overnight_category = ifelse(fuel == "BIT" | fuel == "SUB" | fuel == "LIG" | fuel == "ANT" |
                                       fuel == "COL" | fuel == "RC" | fuel=="WC" | fuel == "PC" | fuel == "TDF", 
                                     ifelse(prime_mover != "IG" & prime_mover != "CC" & prime_mover != "CA" &
                                              prime_mover != "CT" & prime_mover != "IC", 'coal', overnight_category), 
                                     overnight_category)) %>%
  # coal fired power plants not igcc, combined cycle, or internal combustion (conventional combustion)
  mutate(overnight_category = ifelse(fuel != "OTH", ifelse(prime_mover == "CA" | prime_mover == "CC" | prime_mover == "CT" | 
                                                             prime_mover == "CS" | prime_mover == "CW", 'conventional combined cycle', overnight_category), 
                                     overnight_category)) %>%
  # the combined cycle power plants assignment for plants with a known fuel type 
  mutate(overnight_category = ifelse(prime_mover != "CA", ifelse(fuel== "WD" | fuel== "REF" | fuel== "AB" | fuel == "WDS" | 
                                                                   fuel == "BIO" | fuel == "OBS", 'biomass', overnight_category), overnight_category)) %>%
  # biomass assignment
  mutate(overnight_category = ifelse(fuel == "MWH", "distributed peak", overnight_category)) %>%
  mutate(overnight_category = ifelse(prime_mover == "CE", "distributed base", overnight_category)) %>%
  # distibuted generation assignment
  mutate(overnight_category = ifelse(overnight_category=="", "undefined", overnight_category))
