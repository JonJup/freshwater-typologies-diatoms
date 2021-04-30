# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### ------- Macroinvertebrates ------- ###
### ---------- Redundancy   ---------- ###
# -------------------------------------- #

# --------------- #
# date:         17.03.21
#
# called by:
#               07_derive_typical_assemblages.R
# Project:
#               Evaluating European Broad River Types for Macroinvertebrates 
# Purpose:
#               Evaluate redundancy between typical assemblages 
# --------------- #


#function
source("R/auxilliary/f_01_redundant.R")

names(data)[2] = "group"

ma_redundnat = redundant(data)
#print(ma_redundnat)
# x11()
ma_redundnat[which(ma_redundnat < 0.8)] = 0
x11()
corrplot(ma_redundnat, 
                   method = "number", 
                   is.corr = FALSE, 
                   diag = F, 
                   tl.cex = .7,
                   number.cex = 0.6) 


rm(redundant)

print("#--------------------------------------------------------#")