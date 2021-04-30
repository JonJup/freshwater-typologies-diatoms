# ---------------------------------------------------------- #
### --- Remove sites with large distance to next river --- ### 
# ---------------------------------------------------------- #

# --------------- #
# date:  19.04.21
# files in 
        #-> m_river_fec_broad_type.shp | Broad River Types Shape file 
        #-> 01_all_mzb_combined.rds    | Combined Invertebrate data sets 
# files out
        #<- 02_data_close.rds
# Evaluating European Broad River Types for Diatoms  
# Remove sites that are to far away from closest river and assign river 
# types to remaining sites.
# --------------- #

# Setup ----------------------------------------------------------------
source("R/setup.R")

## --  maximal distance to next river (in meters)
cut_off = 500 

# load data  -----------------------------------------------------------
sf_river = st_read(file.path(dir$ls, "m_river_fec_broad_type.shp"))
dt_data  = readRDS("data/01_all_dia_combined.rds")

# clean data -----------------------------------------------------------
dt_sites = unique(dt_data, by = "gr_sample_id")
sf_sites = st_as_sf(dt_sites)
sf_sites = sf_sites[,c(1)]
sf_sites %<>% st_transform(crs = 3035)
sf_river = sf_river[, c(2)]
sf_river %<>% st_transform(crs = 3035)

# add catchment info to bio data ------------------------------------------

nn = st_nearest_feature(sf_sites, sf_river); beep()
sum(is.na(nn))
rivers_resorted <- sf_river[nn,]

# the code that is commented out below is time consmuing to run and the resulting object (distance_list)
# is provided as .rds file in the data directory. 

# distance_list <-
#         map(.x = 1:nrow(sf_sites),
#             .f = ~ as.numeric(st_distance(x = sf_sites[.x, ],
#                                           y = rivers_resorted[.x, ])))
# 
# saveRDS(distance_list, "data/02_distance_list.rds")

# load distance list 
distance_list = readRDS("data/02_distance_list.rds")

distance_table <- data.table("gr_sample_id" = sf_sites$gr_sample_id,
                              "nn_distance" = unlist(distance_list),
                              "ls_bd_20"    = rivers_resorted$m_btype20c)

hist(distance_table$nn_distance, breaks = 100)

# investigate far away sites
# distance_table %>% 
#         filter(nn_distance > cut_off) %>% 
#         pull(gr_sample_id) -> 
#         far_away_sites

## -- code without tidyverse (temporary problems with packages)
far_away_sites = distance_table[which(distance_table$nn_distance > cut_off), gr_sample_id]

## --  subset to close sites 
distance_table2 <- distance_table[nn_distance <= cut_off]
hist(distance_table2$nn_distance)


# Filter data -------------------------------------------------------------
# filter site data to close sites 
sf_sites2 = sf_sites[which(sf_sites$gr_sample_id %in% distance_table2$gr_sample_id), ]
# join site data with distance which holds the river type 

sf_sites3 = distance_table2[sf_sites2, on = "gr_sample_id"]
# sf_sites3 <- left_join(
#         sf_sites2,
#         distance_table2,
#         by = "gr_sample_id"
#         )

# remove distance column 
sf_sites3[, nn_distance := NULL]

# subset observations to close sites
dt_data2 <- dt_data[gr_sample_id %in% sf_sites3$gr_sample_id]
# join river type and observation 
dt_data3 = distance_table2[dt_data2, on = "gr_sample_id"]
# remove distance column 
dt_data3[, nn_distance := NULL]

# Save to File  -----------------------------------------------------------
saveRDS(object = dt_data3,
        file = "data/02_data_close.rds")
