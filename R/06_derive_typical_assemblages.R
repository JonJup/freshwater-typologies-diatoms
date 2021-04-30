# ---------------------------------------- #
### --- Derive typical assemblages   --- ### 
# ---------------------------------------- #

# --------------- #
# date:  17.03.21
# files out:
#               <- 07_indicator_list.rds | list of indicators 
# calls scripts: 
#               -> 07_b_compute_indvals.R
#               -> 07_c_setup_ta_analysis.R
#               -> 07_d_redundancy.R
#               -> 07_f_make_ta_lists.R
#               -> 07_g_ta_table.R
# Project:
#               Evaluating European Broad River Types for Macroinvertebrates
# Purpose: 
#               Derive typical assemblages  
# --------------- #


# setup -------------------------------------------------------------------
source("R/setup.R")

# settings ----------------------------------------------------------------
x_ls_thesholds =  list(a = 2, b = .25, b2 = 0.05)
#x_ls_combine = list(c(2,3), c(10,11), c(6,12))
x_ls_combine = list(c(10,11,15,18), c(2,3), c(6,12))


# compute indvals ---------------------------------------------------------
source("R/auxilliary/06_b_compute_indvals.R")

# setup ta analysis -------------------------------------------------------
source("R/auxilliary/06_c_setup_ta_analysis.R")

# redundancy analysis -----------------------------------------------------
source("R/auxilliary/06_d_redundancy.R")

# ## -- save plot to file 
# setEPS()                                             # Set postscript arguments
# postscript("figures/ta_redundancies/round6.eps")                           # Start graphics device driver
# corrplot::corrplot(ma_redundnat, 
#                    method = "number", 
#                    is.corr = FALSE, 
#                    #order = "FPC", 
#                    #diag = F, 
#                    #type = "lower", 
#                    tl.cex = .7,
#                    number.cex = 0.6)                                  # Create plot
# dev.off()   
# beepr::beep()
# stop()
# # # sensitivity analysis -----------------------------------------------------
# #source(file.path(dir$rs, "07_e_sensitivity_analysis.R"))
# 
# # # make lists -----------------------------------------------------
# source(file.path(dir$hlp, "07_f_make_ta_lists.R"))
# 
# #make table for paper----------------------------------------------------------------------------------------------------------------------------------------------
# source(file.path(dir$hlp, "07_g_ta_table.R"))
# 
# #make table for sharing ----------------------------------------------------------------------------------------------------------------------------------------------
# source(file.path(dir$hlp, "07_g_ta_table.R"))
# 
