### ------------------------------------------------- ###
### --- typical assemblages as separate rds files --- ### 
### ------------------------------------------------- ###

# --------------- #
# date: 
#               18.03.21
# files in:
#               08_typical_assemblages.rds
# files out:
#               individual_TAs/ *.rds 
# Project:
#               Evaluating European Broad River Types for Macroinvertebrates
# Purpose:   
#               Create separate .rds file for each typical macroinvertebrate community. 
#               Each holds the name, river type and taxonomic level. 
# --------------- #

# TEST 
# DRY 
# AUTOMATE 
# DOCUMENT 
# NO BORKEN WINDOWS 
# ORTHAGONAL
# UMKEHRBARKEIT 

# load data -------------------------------------------
data = readRDS("data/08_typical_assemblages.rds")

# split data ----------------------------------------
data %<>%
        rename(taxon_level = level) %>% 
        mutate(rt = factor(rt),
               taxon = str_replace(taxon, "\\.", "\\ "),
               taxon_level = replace(taxon_level, taxon_level == "fol", "family_or_lower")
               ) %>%
        select(!c("A", "B", "mechanism")) %>% 
        split.data.frame(f = .$rt) 

# save data -------------------------------------------
saveRDS(data, "data/08_split_ta.rds")
