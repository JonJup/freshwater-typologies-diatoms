# ----------------------------------------- #
### --- Clean Diatoms with Marias table -- ## 
# ----------------------------------------- #

# --------------- #
# date:  20.04.21
# files in 
#           -> 02_data_close.rds   | Combined diatom data. From sites within 500m for BRT river 
# files out
#           <- 03_data_reference_condition.rds
# Project:
#           Evaluating European Broad River Types for Diatoms  
# Purpose:
#           Harmonize diatom names 
# --------------- #


# setup -------------------------------------------------------------------
pacman::p_load(
               data.table, 
               magrittr,
               tidyverse,
               readxl
               )

# load data ---------------------------------------------------------------
data = readRDS("data/03_data_reference_condition.rds")
fwb  = read_excel("data/auxi/fwb13490-sup-0003-TableS2.xlsx")
omnida = read_excel("data/auxi/OMNIDIAFILE.xls")

# prepare taxon names  ----------------------------------------------------
data = data[!species %in% c(
   "Abnormal diatom", 
   "Pennate",
   "Coscinodiscophyceae", 
   "Centric Diatoms", 
   "Centric diatoms", 
   "Diatomee anormale"
)]

## -- remove subspecies if the ssp. name is equal to the species name

taxa_names = sort(unique(data$species))
for (i in 1:length(taxa_names)){
   
   lpi_name = taxa_names[i]
   if (str_detect(lpi_name, "\\ ", negate = T)) next()
       
   if (str_detect(lpi_name, pattern = "\\ var\\.", negate = F)){
      lpi_name = str_split(lpi_name, pattern = "\\ ")
      lpi_name[[1]][3] = str_remove(lpi_name[[1]][3], "var\\.")
      if (lpi_name[[1]][2] == lpi_name[[1]][3]) {
         data[species == taxa_names[i], species := str_remove(taxa_names[i], "\\ var\\..*")]
         }
   }
   if (str_detect(lpi_name, pattern = "\\ f\\.", negate = F)){
      lpi_name = str_split(lpi_name, pattern = "\\ ")
      lpi_name[[1]][3] = str_remove(lpi_name[[1]][3], "f\\.")
      if (lpi_name[[1]][2] == lpi_name[[1]][3]){
         data[species == taxa_names[i], species := str_remove(taxa_names[i], "\\ f\\..*")]
      }
   }
   if (i %% 100 == 0) print(i)
   rm(lpi_name, i)
}
gc()


# prepare FWB -------------------------------------------------------------
setDT(fwb)
fwb = fwb[-1,]
fwb[,c("...3", "...4", "...5") := NULL]
names(fwb)[c(1,2)] <- c("new_name", "old_name")
fwb %<>% unique(by = c("new_name", "old_name"))

# prepare omnida --------------------------------------------------------------------

setDT(omnida)
names(omnida)[1:5] <- c("code", "id", "taxon", "references", "synonym")
omnida <- omnida[,c(1,3,5)]
omnida <- omnida[-1]
omnida$synonym %<>% str_remove(pattern = "\\(.*")
omnida$synonym %<>% str_remove(pattern = "=")
omnida$synonym %<>% str_remove(pattern = "\\=.*")
omnida$synonym %<>% str_remove(pattern = "\\ .*")
omnida$synonym %<>% str_remove(pattern = "\\?")
omnida$synonym %<>% str_remove(pattern = "\\)")
omnida[synonym == "1894EPSU", synonym := "EPSU"]
omnida[synonym == "1960FTAU", synonym := "FTAU"]
omnida[str_detect(synonym, "^[0-9]"), synonym := NA]
omnida[synonym == "PRADpp.", synonym := "PRADpp"]
omnida[synonym == "SUTEpp.", synonym := "SUTEpp"]
omnida[synonym == "PLTVss.Rumrich", synonym := "PLTVss"]
omnida[str_detect(synonym, "\\."), synonym := NA]
omnida[str_detect(synonym, "\\/"), synonym := NA]
omnida[synonym %in% c("Cocquyt94", "Cleve-Euler", "CE52p84f1491a-cWit00ID7p389f194:1-5", "CE53p128f747a-b", "CE53p70f631", 
                      "British", "B91f48:1-3", "Alaska", "Amerique", "fig48-50", "fig68-75", "Foged", "fossil", "Fossil", 
                      "Genkal93DR8", "Ghana", "Gran04p38f1:13-14Wit00ID7p310f128:9"), synonym := NA]
omnida$synonym %<>% str_remove(pattern = "pp")
omnida[synonym == "", synonym := NA]
omnida[,clean_name := taxon]

omnida2 = copy(omnida)

## -- call clean_omnida auxiliary script 

# check data against fwb ------------------------------------------------------------
for (i in seq_along(taxa_names)){
   
   lp_data = fwb[str_to_lower(old_name) == str_to_lower(taxa_names[i])]
   
   if(nrow(lp_data) == 0) {
      rm(list = ls()[grepl(x = ls(), pattern = "lp_")])
      rm(i)
      next()
   }
      
   lp_new_entry = lp_data$new_name

   data[species == taxa_names[i], species_fwb := lp_new_entry]

   rm(list = ls()[grepl(x = ls(), pattern = "lp_")])
   rm(i)
}
data[!is.na(species_fwb), species_update := species_fwb]
taxa_names = data[is.na(species_update), sort(unique(species))]

# check against omnida --------------------------------------------------------------
for (i in seq_along(taxa_names)){
   
   lp_data = omnida2[str_to_lower(clean_name) == str_to_lower(taxa_names[i])]
   
   if(nrow(lp_data) == 0) {
      rm(list = ls()[grepl(x = ls(), pattern = "lp_")])
      rm(i)
      next()
   }
   
   ## -- more than one entry? 
   if(nrow(lp_data) > 1) {
      
      lp_is_syno = is.na(lp_data$synonym)
      ## -- all synonyms? 
      if (all(lp_is_syno)) {
         lp_data = lp_data[1,]
      } else if (any(lp_is_syno)){
         lp_data = lp_data[is.na(synonym)]
      }
      ## -- are all synonyms and different
      if ((!lp_is_syno) & uniqueN(lp_data$synonym) == length(lp_data$synonym)){
         ## -- select a random one 
         lp_id = sample(x = 1:nrow(lp_data), size = 1)
         lp_data = lp_data[lp_id, ]
      }
   }
   
   ## -- is the name a synonym? 
   lp_is_syno = !is.na(lp_data$synonym)
   if (lp_is_syno){
      lp_accepted_name = omnida2[code == lp_data$synonym]
      if (nrow(lp_accepted_name) == 1) {
         lp_new_entry = lp_accepted_name$clean_name
      } else {
         lp_new_entry = lp_data$clean_name
      }
   } else {
      lp_new_entry = lp_data$clean_name
   }
   
   data[species == taxa_names[i], species_omnida1 := lp_new_entry]
   
   rm(list = ls()[grepl(x = ls(), pattern = "lp_")])
   rm(i)
}
data[!is.na(species_omnida1), species_update := species_omnida1]
taxa_names = data[is.na(species_update), sort(unique(species))]

# Low taxonomic levels --------------------------------------------------------------
for (i in 1:length(taxa_names)){
   lp_data = taxa_names[i]
   lp_data %<>% str_trim
   lp_one_word = str_detect(lp_data, pattern = "\\ ", negate = F)
   if (lp_one_word) next()
   print(lp_data)
   data[species == taxa_names[i], species_low_level := lp_data]
   rm(list = ls()[grepl(x = ls(), pattern = "lp_")])
   rm(i)
}
## -- some minor fixes 
data[!is.na(species_low_level), species_update := species_low_level]
taxa_names = data[is.na(species_update), sort(unique(species))]
data[species == "Diploneispetersenii", species_update := "Diploneis petersenii"]
data[species == "Pinnulariapisciculus", species_update := "Pinnularia pisciculus"]

# visual check against Omnida  ------------------------------------------------------

## -- call manual_fixes_harmonizations.R auxiliary script 

taxa_names = data[is.na(species_update), sort(unique(species))]
data[, species_update2 := species_update]

# check data against fwb - again------------------------------------------------------------

fwb2 = copy(fwb)

fwb2[, new_name2 := str_replace(new_name, pattern = "var\\.\\ ", replacement = "var\\.")]
fwb2[, old_name2 := str_replace(old_name, pattern = "var\\.\\ ", replacement = "var\\.")]
fwb2[, old_name2 := str_remove_all(old_name2, "\\ cf\\.")]

taxa_names = unique(data$species_update2) %>% sort

for (i in seq_along(taxa_names)){
   
   lp_data = fwb2[str_to_lower(old_name2) == str_to_lower(taxa_names[i])]
   
   if(nrow(lp_data) == 0) {
      rm(list = ls()[grepl(x = ls(), pattern = "lp_")])
      rm(i)
      next()
   }
   
   lp_new_entry = lp_data$new_name2 %>% unique()
   if (length(lp_new_entry) != 1){
      print(lp_new_entry)
      lp_id = readline("Which?")
      lp_new_entry = lp_new_entry[lp_id]
   }
   
   data[species_update2 == taxa_names[i], species_update2 := lp_new_entry]
   
   rm(list = ls()[grepl(x = ls(), pattern = "lp_")])
   rm(i)
}
unique(data$species_update2) %>% sort

View(fwb2[, c("old_name2", "new_name2")])

data[species_update2 == "Achnanthidium exiguum var.constrictum", species_update2 := "Achnanthidium exigua/ziegleri/subexigua"]
data[species_update2 == "Adlafia Complex", species_update2 := "Adlafia"]
data[species_update2 == "Aulacoseira distans var.tenella", species_update2 := "Aulacoseira distans complex"]
data[species_update2 == "Brachysira vitrea var.lanceolata", species_update2 := "Brachysira vitrea Complex"]
data[species_update2 == "Caloneis bacillum var.densestriata", species_update2 := "Caloneis bacillum Complex"]
data[species_update2 == "Craticula Complex", species_update2 := "Craticula"]
data[species_update2 == "Cyclostephanos Complex", species_update2 := "Cyclostephanos"]
data[species_update2 == "Cymbella cistula", species_update2 := "Cymbella cistula group"]
data[species_update2 == "Diploneis ovalis", species_update2 := "Diploneis elliptica/oblongella/ovalis/marginestrata/minuta/modica"]
data[species_update2 == "Diploneis petersenii", species_update2 := "Diploneis"]
data[species_update2 == "Discostella  complex", species_update2 := "Discostella complex"]
data[species_update2 == "Encyonema neogracile", species_update2 := "Encyonema gracile-luna"]
data[species_update2 == "Encyonema silesiacum", species_update2 := "Encyonema silesicacum/minutum/lange-bertalotii"]
data[species_update2 == "Encyonema silesiacum var.distigmata", species_update2 := "Encyonema silesicacum/minutum/lange-bertalotii"]
data[species_update2 == "Encyonema ventricosum", species_update2 := "Encyonema silesicacum/minutum/lange-bertalotii"]
data[species_update2 == "Encyonopsis microcephala var.robusta", species_update2 := "Encyonopsis descripta/falaisensis/microcephala"]
data[species_update2 == "Eunotia  exigua/elegans Complex", species_update2 := "Eunotia exigua/elegans Complex"]
data[species_update2 == "Eunotia arcus", species_update2 := "Eunotia arcus/mucophila/bilunaris Complex"]
data[species_update2 == "Eunotia arcus var.attenuatum", species_update2 := "Eunotia arcus/mucophila/bilunaris Complex"]
data[species_update2 == "Eunotia Complex", species_update2 := "Eunotia"]
data[species_update2 == "Eunotia exigua", species_update2 := "Eunotia exigua/elegans Complex"]
data[species_update2 == "Eunotia exigua var.gibba", species_update2 := "Eunotia exigua/elegans Complex"]
data[species_update2 == "Eunotia formica", species_update2 := "Eunotia"]
data[species_update2 == "Eunotia incisa", species_update2 := "Eunotia incisa Complex"]
data[species_update2 == "Eunotia pectinalis", species_update2 := "Eunotia pectinalis Complex"]
data[species_update2 == "Eunotia pectinalis var.undulata", species_update2 := "Eunotia pectinalis Complex"]
data[species_update2 == "Eunotia pectinalis var.ventralis", species_update2 := "Eunotia pectinalis Complex"]
data[species_update2 == "Eunotia serra", species_update2 := "Eunotia serra Complex"]
data[species_update2 == "Eunotia steineckii", species_update2 := "Eunotia exigua/elegans Complex"]
data[species_update2 == "Fragilaria tenera var.nanana", species_update2 := "Eunotia exigua/elegans Complex"]
data[species_update2 == "Fragilaria virescens var.acuminata", species_update2 := "Fragilaria virescens complex"]
data[species_update2 == "Gomphonema minutum var.curtum", species_update2 := "Gomphonema"]
data[species_update2 == "Gomphonema minutum var.syriacum", species_update2 := "Gomphonema"]
data[species_update2 == "Gomphonema olivaceum var.pusilla"     , species_update2 := "Gomphonema olivaceum/olivaceoides"]                            
data[species_update2 == "Gomphonema olivaceum var.staurophorum", species_update2 := "Gomphonema olivaceum/olivaceoides"] 
data[species_update2 == "Gomphonema parvulum var.aequalis", species_update2 := "Gomphonema parvulum Complex"]                          
data[species_update2 == "Gomphonema parvulum var.exilis", species_update2 := "Gomphonema parvulum Complex"]
data[species_update2 == "Gomphonema pumilum var.rigidum", species_update2 := "Gomphonema pumilum complex"]
data[species_update2 == "Gomphonema truncatum var.elongata", species_update2 := "Gomphonema constrictum complex"]
data[species_update2 == "Gyrosigma acuminatum var.gallica", species_update2 := "Gyrosigma"]
data[species_update2 == "Luticola Complex", species_update2 := "Luticola"]
data[species_update2 == "Mastogloia Complex", species_update2 := "Mastogloia"]
data[species_update2 == "Mastogloia smithii", species_update2 := "Mastogloia"]
data[species_update2 == "Mayamaea atomus", species_update2 := "Mayamaea"]
data[species_update2 == "Mayamaea Complex", species_update2 := "Mayamaea"]
data[species_update2 == "Meridion circulare", species_update2 := "Meridion circulare Complex"]
data[species_update2 == "Navicula complex", species_update2 := "Navicula"]
data[species_update2 == "Navicula cari var.linearis", species_update2 := "Navicula cari-recens-erifuga++"]
data[species_update2 == "Navicula tripunctata var.schizomenoides", species_update2 := "Navicula margalithii/tripunctata"]
data[species_update2 == "Navicula vandamii var.mertensiae", species_update2 := "Navicula reichardtiana-caterva"]
data[species_update2 == "Navicula viridula", species_update2 := "Navicula viridula complex"]
data[species_update2 == "Neidium affine", species_update2 := "Neidium affine Complex"]
data[species_update2 == "Nitzschia angustata var.producta", species_update2 := "Nitzschia"]
data[species_update2 == "Nitzschia capitellata var.tenuirostris", species_update2 := "Nitzschia palea complex"]
data[species_update2 == "Nitzschia fonticola var.pelagica", species_update2 := "Nitzschia fonticola Complex"]
data[species_update2 == "Nitzschia frustulum", species_update2 := "Nitzschia frustulum Complex"]
data[species_update2 == "Nitzschia liebetruthii", species_update2 := "Nitzschia lacuum-alpina-bryophila++"]
data[species_update2 == "Nitzschia linearis", species_update2 := "Nitzschia pura-linearis Complex"]
data[species_update2 == "Nitzschia linearis var.tenuis", species_update2 := "Nitzschia pura-linearis Complex"]
data[species_update2 == "Nitzschia palea", species_update2 := "Nitzschia palea-paleacea"]
data[species_update2 == "Nitzschia palea complex", species_update2 := "Nitzschia palea-paleacea"]
data[species_update2 == "Nitzschia palea var.perminuta", species_update2 := "Nitzschia palea-paleacea"]
data[species_update2 == "Parlibellus protracta", species_update2 := "Parlibellus"]
data[species_update2 == "Pinnularia borealis", species_update2 := "Parlibellus"]
data[species_update2 == "Pinnularia borealis var.scalaris", species_update2 := "Pinnularia alpina-lata-borealis complex"]
data[species_update2 == "Pinnularia divergens", species_update2 := "Pinnularia divergens complex"]
data[species_update2 == "Pinnularia divergentissima", species_update2 := "Pinnularia divergens complex"]
data[species_update2 == "Pinnularia lundii", species_update2 := "Pinnularia lundii Complex"]
data[species_update2 == "Pinnularia major", species_update2 := "Pinnularia maior Complex"]
data[species_update2 == "Pinnularia microstauron", species_update2 := "Pinnularia maior Complex"]
data[species_update2 == "Pinnularia microstauron var.nonfasciata", species_update2 := "Pinnularia microstauron Complex"]
data[species_update2 == "Pinnularia nodosa", species_update2 := "Pinnularia nodosa Complex"]
data[species_update2 == "Pinnularia polyonca", species_update2 := "Pinnularia"]
data[species_update2 == "Pinnularia subcapitata", species_update2 := "Pinnularia subcapitata Complex"]
data[species_update2 == "Pinnularia subcapitata var.paucistriata", species_update2 := "Pinnularia subcapitata Complex"]
data[species_update2 == "Pinnularia viridis", species_update2 := "Pinnularia viridis Complex"]
data[species_update2 == "Placoneis Complex", species_update2 := "Placoneis"]
data[species_update2 == "Planothidium frequentissimum var.magnum", species_update2 := "Planothidium lanceolatum"]
data[species_update2 == "Rhopalodia gibba var.parallela", species_update2 := "Rhopalodia gibba Complex"]
data[species_update2 == "Stauroneis kriegerii", species_update2 := "Stauroneis complex small capitate"]
data[species_update2 == "Stauroneis phonicenteron", species_update2 := "Stauroneis phoenicenteron Complex"]
data[species_update2 == "Stauroneis smithii", species_update2 := "Stauroneis smithii Complex"]
data[species_update2 == "Surirella linearis", species_update2 := "Surirella linearis Complex"]
data[species_update2 == "Surirella linearis var.elliptica", species_update2 := "Surirella linearis Complex"]
data[species_update2 == "Surirella robusta var.bispinosa", species_update2 := "Surirella splendida-tenera"]
data[species_update2 == "Tabellaria flocculosa var.andina", species_update2 := "Tabellaria flocculosa Complex"]
data[species_update2 == "Thalassiosira Complex", species_update2 := "Thalassiosira"]
data[species_update2 == "Ulnaria ulna", species_update2 := "Ulnaria ulna complex"]
data[species == "Achnanthes linearis", species_update2 :=  "Achnanthidium lineare"]
data[species_update2 == "Achnantidium lineare", species_update2 :=  "Achnanthidium lineare"]

unique(data$species_update2) %>% sort

data[,c("species", "species_omnida1", "species_update", "species_fwb", "species_low_level") := NULL]
names(data)[which(names(data) == "species_update2")] = "species"

saveRDS(data, "data/04_harmonized_diatoms.rds")
