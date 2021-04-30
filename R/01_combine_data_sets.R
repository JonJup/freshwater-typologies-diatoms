# ----------------------------- #
### --- Combine data sets --- ### 
# ----------------------------- #

# --------------- #
# date:  19.04.21
# files in 
        #-> individual diatom data sets 
# files out
        #<- 01_all_dia_combined.rds
# Project:
#       Evaluating European Broad River Types for Diatoms
# Purpose:
#       Combine individual diatom data sets into one harmonized data set.   
# --------------- #

# Setup -------------------------------------------------------------------
source("R/setup.R")
library(data.table)
library(sf)
library(magrittr)
library(stringr)

# load data ---------------------------------------------------------------

set01 <- readRDS(file.path(dir$dia, "cf_stars_diatoms.RDS"))
set02 <- readRDS(file.path(dir$dia, "cf_wiser_diatoms.RDS"))
set03 <- readRDS(file.path(dir$dia, "ep_diatoms.rds"))
set04 <- readRDS(file.path(dir$dia, "hd_diatoms.rds"))
set05 <- readRDS(file.path(dir$dia, "ir_diatoms.rds"))
set06 <- readRDS(file.path(dir$dia, "jg_diatoms.rds"))
set07 <- readRDS(file.path(dir$dia, "jj_diatoms.rds"))
set08 <- readRDS(file.path(dir$dia, "js_diatoms.RDS"))
set09 <- readRDS(file.path(dir$dia, "ld_diatoms.rds"))
set10 <- readRDS(file.path(dir$dia, "mc_diatoms.rds"))
set11 <- readRDS(file.path(dir$dia, "mi_daitoms.RDS"))
set12 <- readRDS(file.path(dir$dia, "na_diatoms.RDS"))
set13 <- readRDS(file.path(dir$dia, "sb_diatoms.RDS"))
set14 <- readRDS(file.path(dir$dia, "sk_diatoms.rds"))

# prepare data ------------------------------------------------------------

## -- vector with all names 
ch_files <- ls()[grepl("^set", ls())]
## -- all to sf class
for (i in 1:length(ch_files)){
        if (i == 1) 
                all_files = list()
        y = get(ch_files[i])
        x = copy(y)
        ## -- only one observation per taxon and site
        x = unique(x, by = c("gr_sample_id", "species"))
        ## -- make spatial 
        x = st_as_sf(x, coords = c("x.coord", "y.coord"), crs = x$EPSG[1])
        ## -- add date column if missing 
        if (!"date" %in% names(x))
                x$date = as.Date(NA)
        ## -- transform to common CRS 
        if (x$EPSG[1] != 4326)
                x %<>% st_transform(crs = 4326)
        ## -- wirte to list 
        all_files[[i]] = x
        ## -- message progress
        print(i)
        ## -- clean up 
        rm(x,y,i);gc()
}
# give common structure
all_files %<>%
        # keep only relevant columns
        map(.f = ~ .x[, c(
                "gr_sample_id",
                "date",
                "species",
                "abundance",
                "least_impact",
                "data.set",
                "geometry"
        )])  %>%
        map(.f = setDT) %>% 
        # Data sets where date = NA have a different class in that column than those with actual dates.
        # To avoid having to check each one separately I made sure each date is of class date.
        map(.f = ~ .x[, date := as.Date(date)])
        
## -- combine data sets 
set_all <- rbindlist(all_files)


## -- visual checks 
# visual check - writes to file 
# set_all %>%
#         unique(by = "gr_sample_id") %>%
#         st_as_sf() %>%
#         st_write("test.gpkg")
        
# list(all_files[[2]], all_files[[3]]) %>%
#         rbindlist() %>% 
#         st_as_sf() %>% 
#         tm_shape() + tm_dots(col = "data.set")

rm(list = setdiff(ls(), c("set_all", "dir")))

# Create Lists  -----------------------------------------------------------
## -- Manual pre checks -- ## 


## -- Remove entries -- ## 

entries_to_remove = c(
        "Abnormal diatom",
        "Diatomee anormale"
)

set_all = set_all[!species %in% entries_to_remove]

## -- Small fixes -- ## 
set_all[, species := str_replace_all(species, "\\ sp\\.", "")]
set_all[, species := str_replace_all(species, "\\ sp\\ ", "")]
set_all[, species := str_replace_all(species, "\\ sp$", "")]
set_all[, species := str_replace_all(species, "\\ \\[[0-9]\\]", "")]

set_all[, species := str_replace_all(species, "\\ var\\ "   , "\\ var\\.")]
set_all[, species := str_replace_all(species, "\\ v\\."     , "\\ var\\.")]
set_all[, species := str_replace_all(species, "\\ ssp\\."   , "\\ var\\.")]
set_all[, species := str_replace_all(species, "\\ subsp\\." , "\\ var\\.")]
set_all[, species := str_replace_all(species, "\\ var\\.\\ ", "\\ var\\.")]

set_all[, species := str_replace_all(species, "\\ fo\\.", "\\ f\\.")]
set_all[, species := str_replace_all(species, "\\ f\\.\\ ", "\\ f\\.")]
set_all[, species := str_replace_all(species, "\\  for\\.\\ ", "\\ f\\.")]

set_all[, species := str_replace_all(species, "\\ species$", "")]

set_all[str_detect(pattern = "\\ ", string = species, negate = T), species := str_to_title(species)]

set_all[, species := str_to_sentence(species)]

#set_all$species  %>% unique %>% sort %>%  data.frame(x = .) %>% View()

## -- individual fixes 
set_all[species == "Achnanthes lanceolata rostrata", species := "Achnanthes lanceolata var.rostrata"]
set_all[species == "Achnanthidium linearioides", species := "Achnanthidium linearoides"]
set_all[species == "Adlafia aquaductae", species := "Adlafia aquaeductae"]
set_all[species == "Adlafia lange-bertalotii", species := "Adlafia"]
set_all[species == "Aneumastus tuscula", species := "Aneumastus tusculus"]
set_all[species == "Chamaepinnularia lange-bertalot", species := "Chamaepinnularia"]
set_all[species == "Denticula sundayensis", species := "Denticula sundaysensis"]
set_all[species == "Diploneis peterseni", species := "Diploneis petersenii"]
set_all[species == "Encyonemaventricosum", species := "Encyonema ventricosum"]
set_all[species == "Encyonopsis krammer", species := "Encyonopsis"]
set_all[species == "Eolimna lange-bertalot", species := "Eolimna"]
set_all[species == "Eunotia naegeli", species := "Eunotia naegelii"]
set_all[species == "Eunotia pectinalis var.undulatafo.triodon", species := "Eunotia pectinalis var.undulata"]
set_all[species == "Eunotiaboreotenuis", species := "Eunotia boreotenuis"]
set_all[species == "Eunotiameisteri", species := "Eunotia meisteri"]
set_all[species == "Fallacia lenzi", species := "Fallacia lenzii"]
set_all[species == "Fogedia finnmarchica", species := "Fogedia finmarchica"]
set_all[species == "Fragilariadelicatissima", species := "Fragilaria delicatissima"]
set_all[species == "Fragilariforma virenscens var.subsalina", species := "Fragilariforma virescens var.subsalina"]
set_all[species == "Geissleria lange-bertalot", species := "Geissleria"]
set_all[species == "Gomphonema minutum for. minutum",  species := "Gomphonema minutum f.minutum"]
set_all[species == "Gomphonema minutum for. syriacum", species := "Gomphonema minutum f.syriacum"]
set_all[species == "Gomphonema paralellistriatum", species := "Gomphonema parallelistriatum"]
set_all[species == "Gomphonema parvulum var.parvulumfo.parvulum", species := "Gomphonema parvulum var.parvulum"]
set_all[species == "Surirella angusta.kutzing", species := "Surirella angusta"]
set_all[species == "Stephanodiscus hantzschii for. tenuis", species := "Stephanodiscus hantzschii f.tenuis"]
set_all[species == "Staurosira construens for. venter", species := "Staurosira construens f.venter"]
set_all[species == "Stauroneis kriegeri", species := "Stauroneis kriegerii"]
set_all[species == "Rossithidium petersennii", species := "Rossithidium petersenii"]
set_all[species == "Psammothidium grishunum" , species := "Psammothidium grischunum"]
set_all[species == "Proschkinia poretzkajae" , species := "Proschkinia poretzkiae"]
set_all[species == "Pleurosira laevis for. laevis", species := "Pleurosira laevis f.laevis"]
set_all[species == "Planothidium septentrionalis" , species := "Planothidium septentrionale"]
set_all[species == "Planothidium round" , species := "Planothidium"]
set_all[species == "Planothidium peragalloi" , species := "Planothidium peragallii"]
set_all[species == "Planothidium peragalli" ,  species := "Planothidium peragallii"]
set_all[species == "Pinnularia maior" ,  species := "Pinnularia major"]
set_all[species == "Pinnularia acrospheria" ,  species := "Pinnularia acrosphaeria"]
set_all[species == "Nitzschiabavarica" ,  species := "Nitzschia bavarica"]
set_all[species == "Nitzschiacapitellata" ,  species := "Nitzschia capitellata"]
set_all[species == "Nitzschiafonticola" ,  species := "Nitzschia fonticola"]
set_all[species == "Nitzschiapalea" ,  species := "Nitzschia palea"]
set_all[species == "Nitzschiaperminuta" ,  species := "Nitzschia perminuta"]
set_all[species == "Nitzschia wuellerstorfii" ,  species := "Nitzschia wuellerstorffii"]
set_all[species == "Nitzschia palea f.palea"  ,  species := "Nitzschia palea var.palea"]
set_all[species == "Nitzschia homburgensis"   ,  species := "Nitzschia homburgiensis"]
set_all[species == "Nitzschia angustiforaminata",  species := "Nitzschia angusteforaminata"]
set_all[species == "Neidium septentrionalis"   ,  species := "Neidium septentrionale"]
set_all[species == "Neidium longiseps"         ,  species := "Neidium longiceps"]
set_all[species == "Neidiopsis lange-bertalot" ,  species := "Neidiopsis"]
set_all[species == "Naviculatenelloides"  ,  species := "Navicula tenelloides"]
set_all[species == "Naviculadicta spec.1" ,  species := "Naviculadicta"]
set_all[species == "Naviculadicta"        ,  species := "Naviculadicta"]
set_all[species == "Navicula(dicta) seminulum"        ,  species := "Naviculadicta seminulum"]
set_all[species == "Navicula viridula var.viridula f.linearis"        ,  species := "Navicula viridula var.viridula"]
set_all[species == "Navicula stankovici"        ,  species := "Navicula stankovicii"]
set_all[species == "Navicula slevicensis"        ,  species := "Navicula slesvicensis"]
set_all[species == "Navicula salinarum f.minima"        ,  species := "Navicula salinarum var.minima"]
set_all[species == "Navicula rhyncocephala"        ,  species := "Navicula rhynchocephala"]
set_all[species == "Navicula meninsculus"        ,  species := "Navicula menisculus"]
set_all[species == "Navicula jo3"        ,  species := "Navicula"]
set_all[species == "Navicula dificillima"        ,  species := "Navicula difficillima"]
set_all[species == "Navicula denselineolata"        ,  species := "Navicula densilineolata"]
set_all[species == "Karayevia oblongellum"        ,  species := "Karayevia oblongella"]
set_all[species == "Hippodonta lange-bertalot."        ,  species := "Hippodonta"]
set_all[species == "Halamphora normannii"        ,  species := "Halamphora normanii"]
set_all[species == "Gyrosigma sciotoense"        ,  species := "Gyrosigma sciotense"]
set_all[species == "Gyrosigma parkeri"        ,  species := "Gyrosigma parkerii"]
set_all[species == "Gomphospenia tackei"        ,  species := "Gomphosphenia tackei"]
set_all[species == "Gomphospenia grovei var.lingulata"        ,  species := "Gomphosphenia grovei var.lingulata"]
set_all[species == "Gomphonemapumilum"        ,  species := "Gomphonema pumilum"]
set_all[species == "Gomphonemainterpositum"        ,  species := "Gomphonema interpositum"]
set_all[species == "Gomphonemahebridense"        ,  species := "Gomphonema hebridense"]
set_all[species == "Gomphonemagracile"        ,  species := "Gomphonema gracile"]
set_all[species == "Gomphonemaclavatum"        ,  species := "Gomphonema clavatum"]
set_all[species == "Gomphonemabozenae"        ,  species := "Gomphonema bozenae"]

## -- synonyms -- ## 
set_all[species %in% c("Encyonema prostratrum", "Encyonema prostratum"), species := "Encyonema leibleinii"]


## --  new data set without duplicates 
set_all <- unique(set_all , by = c("gr_sample_id", "species"))

# save to file ------------------------------------------------------------
saveRDS(set_all, "data/01_all_dia_combined.rds")

