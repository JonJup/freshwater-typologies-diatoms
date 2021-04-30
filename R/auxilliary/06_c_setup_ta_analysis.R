# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### ------ macroinvertebrates -------- ###
### ------------ Setup   ------------- ###
# -------------------------------------- #

# --------------- #
# date:         17.03.21
# files out:
#               f_ABplotsTA.rds            | data for plots of A against B   
#               08_typical_assemblages.rds | list with taxa for typical assemblages   
# called by:
#               07_derive_typical_assemblages.R
# Project:
#               Evaluating European Broad River Types for Macroinvertebrates 
# Purpose:
#               Derive typical assemblages from indicator_list.rds 
# --------------- #

# load data ---------------------------------------------------------------
data = readRDS("data/06_indicator_list.rds")

# Deriving typical assemblages ---------------------------------------------------
data = data[B >= x_ls_thesholds$b | (A >= x_ls_thesholds$a & B >= x_ls_thesholds$b2)]

# save to file ------------------------------------------------------------
#saveRDS(plot_new, "data/f_ABplotsTA.rds")
saveRDS(data, "data/07_typical_assemblages.rds")

