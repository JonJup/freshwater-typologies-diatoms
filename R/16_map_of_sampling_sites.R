# ------------------------------------------ #
### --- Map of Sampling sites          --- ### 
# ------------------------------------------ # 

# --------------- #
# date:  12.04.21
# files in 
#       -> 01_all_mzb_combined.rds |
#       -> 02_data_close.rds       |
#       -> 03_data_low_impact.rds  |
#       -> 06_sxs_list.RDS         |
#       -> 
# files out
#       <- 
# Project: 
#       Evaluating European Broad River Types for Macroinvertebrates 
# Purpose: 
#       1. create a data set with all sampling sites and a factor that indicates at which 
#          level the sites was removed from the analysis if at all.
#       2. create map of these data
# --------------- #

# setup -----------------------------------------------------------------------------
source("R/setup.R")

# load data  ------------------------------------------------------------------------
all    = readRDS("data/01_all_dia_combined.rds")
close  = readRDS("data/02_data_close.rds")
impact = readRDS("data/03_data_reference_condition.rds")
time   = readRDS("data/05_sxs_list.RDS")
europe = st_read("D://Arbeit/Data/natural_earth/2020_06_29_europe.gpkg")
# combine data  ---------------------------------------------------------------------

sites = unique(all, by = "gr_sample_id")
sites[, used := "NA"]
sites[!gr_sample_id %in% close$gr_sample_id, used := "distance"]
sites[gr_sample_id %in% close$gr_sample_id, used := "impact"]
sites[gr_sample_id %in% impact$gr_sample_id, used := "used"]
sites[, used := factor(used)]
sites = sites[,c("gr_sample_id", "used","geometry")]
sites %<>% st_as_sf
   
## -- remove several countries from Europe 
europe %<>%
        filter(! GEOUNIT %in% c("Turkey", "Russia"))

    
# create map  -----------------------------------------------------------------------
tmap_mode("plot")

make_map = function(filter_arg, title_arg, col_arg){
        out = tm_shape(europe) + 
                tm_polygons() + 
                tm_shape(filter(sites, used == filter_arg)) + 
                tm_dots(col = col_arg, alpha = 0.2, size = .1) + 
                #tm_compass(type = "arrow") + 
                tm_layout(bg.color = "lightblue", frame = TRUE, title = title_arg) 
        out
}

## -- make maps 
map1 = make_map("distance", "too far", "blue")
map2 = make_map("impact", "impacted", "orange")
#map3 = make_map("time",  "before 2000", "brown")
map4 = make_map("used", "final", "green")

## -- combine maps 
map_combine = tmap_arrange(map1, map2, map4)

# save to file  ---------------------------------------------------------------------
tmap_save(
        tm = map_combine, 
        filename = "figures/map_for_paper.png"
)



