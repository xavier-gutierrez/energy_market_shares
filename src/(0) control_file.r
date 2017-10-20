options(scipen=999)
library(R.utils)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(reshape2)
library(lubridate)
library(mlogit)
base <- 'C:/Users/xavie/Desktop/energy_market_shares/'
path_code = "C:/Users/xavie/Desktop/energy_market_shares/src/"
path_data = "C:/Users/xavier/Desktop/energy_market_shares/data/"
path_figure = "C:/Users/guti220/Desktop/energy_market_shares/figures/"
# Local path to the cloned data and code folders in the repository

source(paste0(path_code,"mapping_860_overnight_R.r"))
# Warning messages occur because we are matching factors where not all of the factors are in both datasets (Ignore the warning)
source(paste0(path_code, "ca_almanac_R.r"))
source(paste0(path_code, "summary_files.r"))
source(paste0(path_code, "overnight_cost.r"))
source(paste0(path_code, 'multinomial.r')) 
# warning a dplyr function call will take ~ 6 minutes (instead just run the sections of code after it, unless changes have been made to data tables)
source(paste0(path_code, "figure.r"))

