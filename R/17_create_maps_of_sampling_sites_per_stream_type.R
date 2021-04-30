# ---------------------------------------------- #
### --- create maps of sampling sites from --- ### 
# ---------------------------------------------- #

# --------------- #
# date:
#       16.04.21
# files in: 
#       
# files out:
#
# Project:
#       Evaluating European Broad River Types for Macroinvertebrates 
# Purpose:
#       Plot all instances of a river type together with all available sampling sites 
# --------------- #

# TEST 
# Dont Repeat Yourself  
# AUTOMATE 
# DOCUMENT 
# NO BORKEN WINDOWS 
# Design by Contract 
# Fail fast/ defensive programming 


# setup -------------------------------------------------------------------
source("R/setup.R")
setwd(here())


# load data -------------------------------------------------------------------------
europe <- st_read("D://Arbeit/Data/natural_earth/2020_06_29_europe.gpkg")
ls20   <- st_read(file.path(dir$ls, "m_river_fec_broad_type.shp"))
sites  <- readRDS("data/03_data_reference_condition.rds")

# carpeting --------------------------------------------------------------

## -- remove sites from before 2000 
sites %<>%
        #sites[year(date) >= 2000] %>%
        unique(by = "gr_sample_id") %>%
        st_as_sf

## -- transform river types 
ls20 %<>% st_transform(crs = 4326)


# Map  --------------------------------------------------------------------
tmap_mode("plot")
for (i in 1:20) {
        
        # declare variables 
        river     <- paste0("RT", i)
        map_title <- paste("Rivertype", i) 
        save_name <- paste0("figures/sampling_sites_per_river_type/", river, ".pdf")
        
        loop_sub_rivers <- filter(ls20, m_btype20c == river)
        loop_sub_sites  <- filter(sites, ls_bd_20 == river) 
        
        if (nrow(loop_sub_sites) == 0) next()
        
        loop_map <- tm_shape(europe) + tm_polygons() + 
                tm_shape(loop_sub_rivers) + tm_lines(col = "blue") + 
                tm_shape(loop_sub_sites) + tm_dots(col = "red", size = .1) + 
                tm_layout(title = map_title) + 
                tm_compass(position = c("left", "top")) + 
                tm_scale_bar(position = c("center", "bottom"))
        
        tmap_save(tm = loop_map, 
                  filename = save_name, 
                  )
        print(paste(i))
}


       
