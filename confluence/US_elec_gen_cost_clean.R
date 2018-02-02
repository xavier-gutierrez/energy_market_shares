## Script to process and clean EIA data on US electricity generation and costs
## Stephanie, February 2016

# -----------------------------------------------------------------------------
# Define functions and load libraries
# -----------------------------------------------------------------------------
## Load libraries
library( ggplot2 )
library( reshape2 )
library( dplyr )
library( scales )
library( extrafont )
library( ggthemes )
library( Hmisc )
library( grid )

setwd( "/Users/wald405/Documents/GCAM-R data processing/Stephanie-Rfiles" )
source( "functions.R" )

# -----------------------------------------------------------------------------
# Read in data
# -----------------------------------------------------------------------------
d.gen <- inputData( d, "/Users/wald405/Documents/SFA/EC1_tech_choice/US_elec_gen_cost_price", "elec_gen_usa_MER_T07_02A.csv", 0 )
    # Electricity Net Generation Total, All Sector: Monthly Energy Review, January 2016 - Table 7.2a
    # Coverage years: 1949-2015
    # Fuels: Fossil, nuclear, renewables
    # Units: Mil kWh
d.cost <- inputData( d, "/Users/wald405/Documents/SFA/EC1_tech_choice/US_elec_gen_cost_price", "elec_OMF_costs_plant_type_2003_2012.csv", 8 )
    # Sources: Federal Energy Regulatory Commission, FERC Form 1, "Annual Report of Major Electric Utilities, Licensees and Others via Ventyx Global Energy Velocity Suites"
    # Coverage years: 2003-2013
    # Sectors: fossil steam, nuclear, gas turbine/small scale (see notes)
    # Units: Mils/kWh

# -----------------------------------------------------------------------------
  ## Additional data - not using these data right now, but can add if helpful 
# -----------------------------------------------------------------------------
## Electricity prices by demand sector
# d.price <- inputData( d, "/Users/wald405/Documents/SFA/EC1_tech_choice/US_elec_gen_cost_price", "MER_T09_08_avg_retail_price_elec.csv", 0 )
#     # Average Retail Prices of Electricity (incl taxes): Monthly Energy Review, January 2016 - Table 9.8
#     # Coverage years: 1960-2015
#     # Sectors: Res, com, ind, trans, other
#     # Units: cents/kWh

## Average OM costs, detailed plant technology
# d.cost.tech <- inputData( d, "/Users/wald405/Documents/SFA/EC1_tech_choice/US_elec_gen_cost_price", "elec_OMF_costs_plant_tech_2010_2011.csv", 0 )
#     # Estimates of Power Plant Capital and Operating Costs, 2010 & 2011 (update)
#     # Coverage years: 2010-2011
#     # Technologies: FF (PC, single unit, double unit, NGCC, IGCC, ..., w/ & w/o CCS), renewables (PV, thermal, onshore, offshore, ...), nuclear 
#     # Units: multiple

# -----------------------------------------------------------------------------
# Clean generation data
# -----------------------------------------------------------------------------
  # Replace fuel names
fuelName <- function ( d )
{
  d$fuel <- gsub( "CLETPUS", "coal", d$fuel ) 
  d$fuel <- gsub( "PAETPUS", "petroleum", d$fuel ) 
  d$fuel <- gsub( "NGETPUS", "natural_gas", d$fuel ) 
  d$fuel <- gsub( "OJETPUS", "other_gas", d$fuel ) 
  d$fuel <- gsub( "NUETPUS", "nuclear", d$fuel ) 
  d$fuel <- gsub( "HPETPUS", "hydro_pump", d$fuel ) 
  d$fuel <- gsub( "HVETPUS", "hydro_conv", d$fuel ) 
  d$fuel <- gsub( "WDETPUS", "wood", d$fuel ) 
  d$fuel <- gsub( "WSETPUS", "waste", d$fuel ) 
  d$fuel <- gsub( "GEETPUS", "geothermal", d$fuel ) 
  d$fuel <- gsub( "SOETPUS", "solar_pv", d$fuel ) 
  d$fuel <- gsub( "WYETPUS", "wind", d$fuel ) 
  d$fuel <- gsub( "ELETPUS", "total", d$fuel )
  return( d )
}
  # Create annual values for generation data
yearMonth <- function( d ) 
{
  d$year <- as.numeric( substr( d$year_month, 1,4 ) )
  d$month <- substr( d$year_month, 5,6 )
  d <- subset( d, month == "13" & year != 2015 )
  d <- colnameReplace( d, "13", "value" )
  d <- subset( d, select = c( "variable", "unit", "fuel", "year", "value" ) )
  return( d )
}

## Clean generation data
d.gen.clean <- subset( d.gen, select = c( "variable", "unit", "fuel", "year_month", "value" ) )
  d.gen.clean <- fuelName( d.gen.clean )
  d.gen.clean <- yearMonth( d.gen.clean )
    d.gen.clean$unit <- "mil_kwh"
    d.gen.clean$variable <- "elec_net_generation"
    d.gen.clean$iso <- "usa"
    d.gen.clean$value <- as.numeric( d.gen.clean$value )
      d.gen.clean$value <- as.numeric( ifelse( is.na( d.gen.clean$value ), 0, d.gen.clean$value ) )

# -----------------------------------------------------------------------------
# Clean cost data
# -----------------------------------------------------------------------------
  # Create columns to map to generation data by fuel
fuelType <- function( d )
{
  d$coal <- ifelse( d$plant_type == "fossil_steam", d$value, "" )
  d$petroleum <- ifelse( d$plant_type == "fossil_steam", d$value, "" )
  d$natural_gas <- ifelse( d$plant_type == "fossil_steam", d$value, "" )
    ## Gas Turbine and Small Scale category consists of gas turbine, internal combustion, photovoltaic, and wind plants.
  d$other_gas <- ifelse( d$plant_type == "gas_turbine_other_small", d$value, "" )# Is this the right category?
  d$nuclear <- ifelse( d$plant_type == "nuclear", d$value, "" )
    # Hydroelectric category consists of both conventional hydroelectric and pumped storage.
  d$hydro_conv <- ifelse( d$plant_type == "hydroelectric", d$value, "" )
  d$hydro_pump <- ifelse( d$plant_type == "hydroelectric", d$value, "" )
#   d$wood <- ifelse( d$plant_type == "", d$value, "" )
#   d$waste <- ifelse( d$plant_type == "", d$value, "" )
#   d$geothermal <- ifelse( d$plant_type == "", d$value, "" )
  d$solar_pv <- ifelse( d$plant_type == "gas_turbine_other_small", d$value, "" )
  d$wind <- ifelse( d$plant_type == "gas_turbine_other_small", d$value, "" )
  d$value <- NULL
  return( d )
}
# Constant 2010 USD, use deflator from EIA, file MER_TC1_pop_gdp_deflator.csv (convert from 2009$ to 2010$)
defl_2003_2010 <- 0.86735  / 1.01221
defl_2004_2010 <-	0.8912 / 1.01221
defl_2005_2010 <-	0.91988 / 1.01221
defl_2006_2010 <-	0.94814 / 1.01221
defl_2007_2010 <-	0.97337 / 1.01221
defl_2008_2010 <-	0.99246 / 1.01221
defl_2009_2010 <-	1 / 1.01221
defl_2010_2010 <-	1.01221 / 1.01221
defl_2011_2010 <-	1.03311	/ 1.01221
defl_2012_2010 <-	1.05214 / 1.01221
defl_2013_2010 <-	1.06929 / 1.01221
convDollar <- function( d, x, y )
{
  d$value <- as.numeric( d$value )
  d$value <- ifelse( d$year == as.numeric( x ), ( d$value * y ), d$value )
  return( d )
}

d.cost.clean <- d.cost
  d.cost.clean <- fuelType( d.cost.clean )
  d.cost.clean <- melt( d.cost.clean, id.vars = 1:4 )
    d.cost.clean <- colnameReplace( d.cost.clean, "variable", "fuel" )
  d.cost.clean <- subset( d.cost.clean, d.cost.clean$value != "" )
    d.cost.clean <- convDollar( d.cost.clean, "2003", defl_2003_2010 )
    d.cost.clean <- convDollar( d.cost.clean, "2004", defl_2004_2010 )
    d.cost.clean <- convDollar( d.cost.clean, "2005", defl_2005_2010 )
    d.cost.clean <- convDollar( d.cost.clean, "2006", defl_2006_2010 )
    d.cost.clean <- convDollar( d.cost.clean, "2007", defl_2007_2010 )
    d.cost.clean <- convDollar( d.cost.clean, "2008", defl_2008_2010 )
    d.cost.clean <- convDollar( d.cost.clean, "2009", defl_2009_2010 )
    d.cost.clean <- convDollar( d.cost.clean, "2010", defl_2010_2010 )
    d.cost.clean <- convDollar( d.cost.clean, "2011", defl_2011_2010 )
    d.cost.clean <- convDollar( d.cost.clean, "2012", defl_2012_2010 )
    d.cost.clean <- convDollar( d.cost.clean, "2013", defl_2013_2010 )
    d.cost.clean$iso <- "usa"
    d.cost.clean$variable <- paste( d.cost.clean$plant_type, d.cost.clean$cost_type, sep = "_" )
    d.cost.clean$plant_type <- NULL
    d.cost.clean$cost_type <- NULL

# -----------------------------------------------------------------------------
# Merge and save data
# -----------------------------------------------------------------------------
d.elec <- rbind( d.gen.clean, d.cost.clean )
write.csv( d.elec, "us_elec_gen_cost.csv")
