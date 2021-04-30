# -------------------------------------- #
### --- Remove non-reference sites --- ### 
# -------------------------------------- #

# --------------- #
# date:  20.04.21
# files in 
        #-> 02_data_close.rds   | Combined diatom data. From sites within 500m for BRT river 
# files out
        #<- 03_data_reference_condition.rds
# Project:
#       Evaluating European Broad River Types for Diatoms  
# Purpose:
#       Remove sites that not in a reference state
# --------------- #

# Setup ----------------------------------------------------------------
source("R/setup.R")


# load data ------------------------------------------------------------------------
dt_data  = readRDS("data/02_data_close.rds")

# remove non-reference sites  -------------------------------------------------------
dt_data = dt_data[least_impact == 1]


# Save to File  -----------------------------------------------------------
saveRDS(object = dt_data,
        file = "data/03_data_reference_condition.rds")
