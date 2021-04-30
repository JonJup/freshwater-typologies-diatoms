# ----------------------------------------- #
### --- Create species X sites tables --- ### 
# ----------------------------------------- #

# --------------- #
# date:  22.04.21
# files in 
#               -> 04_harmonized_diatoms.rds | harmonized diatoms from reference sites 
# files out
#               <- 06_sxs_list.RDS          | taxa  X sites table    
#               <- 06_sxs_genus.RDS          | genus  X sites table    
# Project:
#               Evaluating European Broad River Types for Diatoms 
# Purpose:
#               Create species X sites table 
# --------------- #

# 01. Setup  --------------------------------------------------------------
source("R/setup.R")

# read in and prepare data ------------------------------------------------
## --  load data 
data  = readRDS("data/04_harmonized_diatoms.rds")
## -- trim names 
data[, species := str_trim(species, side = "both")]

## --  remove samples from before 2000
if (any(year(data$date) < 2000, na.rm = T)){
        data$gr_sample_id %<>% as.character()
        data[, year := lubridate::year(date)]
        data = data[(is.na(year) | year >= 2000)]
}

# 04. Turn to site X species matrix --------------------------------------------------------
data2 = copy(data)

sites.col = which(names(data2) == "gr_sample_id")
sp.col = which(names(data2) == "species")

data2 %<>% 
        splist2presabs(sites.col, sp.col) %>% 
        setDT 

## -- add river type 
join_data = data[, c("gr_sample_id", "ls_bd_20")]
join_data = unique(join_data, by = "gr_sample_id")
data3 = data2[join_data, on = "gr_sample_id"]

# 05. remove rare species/ sites --------------------------------------------------------
# -- low richness sites -- #

# Compute number of taxa across levels for each site and output a vector of gr_sample_ids with empty sites
for (i in seq_along(unique(data3$gr_sample_id))){
        if (i == 1) out = c()
        ls_loop        = list()
        ls_loop$id     = unique(data3$gr_sample_id)[i]
        ls_loop$data    = data3[gr_sample_id == ls_loop$id]
        ls_loop$sum     = sum(rowSums(ls_loop$data[,-c(1,ncol(ls_loop$data)), with = F]))
        out[i] = ls_loop$sum 
        #if (ls_loop$sum == 1) ch_1     = append(ch_1, ls_loop$id)
        #if (ls_loop$sum < 5)  ch_5     = append(ch_5, ls_loop$id)
        print(i)
        rm(ls_loop)
}

hist(out, breaks = 100)
out %>% sort()
remove_id = which(out <=5)

data3 = data3[!gr_sample_id %in% unique(data3$gr_sample_id)[remove_id]]

# 08. Save data to file ---------------------------------------------------
saveRDS(data3, "data/05_sxs_list.RDS")

