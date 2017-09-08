install.packages("R.utils")
install.packages("tidyr")
install.packages("dplyr")
install.packages('data.table')
install.packages('mlogit')
install.packages('ggplot2')
install.packages('reshape2')
install.packages('lubridate')
options(scipen=999)
require(R.utils)
require(dplyr)
require(tidyr)
require(data.table)
require(ggplot2)
require(reshape2)
require(lubridate)
require(mlogit)

path_code = "C:/Users/guti220/Desktop/energy_market_shares/src/"
path_data = "C:/Users/guti220/Desktop/energy_market_shares/data/"
path_data = "C:/Users/guti220/Desktop/fuzzy-waffle-download/data/"
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

