# -------------------------------------- #
### --- Analyze Indicator Matrices --- ###
### ------- Macroinvertebrates ------- ###
### ---------- Make Lists   ---------- ###
# -------------------------------------- #

# 16.11.20
# GetReal
# Working Package 2 
# Macroinvertebrates

#source(file.path(DIR$rs, "08_c_setup_ta_analysis.R"))

ch_river_types = unique(dt_mzb$group)
copy_list = list()
for (i in seq_along(ch_river_types)){
        var = ch_river_types[i]
        
        taxa_b = dt_mzb[group == var & mechanism == "B", taxon] %>% 
                str_replace_all(pattern = "\\.", "\\ ") 
        taxa_a = dt_mzb[group == var & mechanism == "A", taxon] %>% 
                str_replace_all(pattern = "\\.", "\\ ") 
        if (length(taxa_a) == 0){
               taxa =  append("**B**", taxa_b)
        } else {
                taxa =  append("**A**", append(taxa_a, append("**B**", taxa_b)))
        }
        ch_temp = paste(taxa, sep = ",", collapse = ", ")
        # dt_bty[group == var, taxon] %>%
        #         str_replace_all(pattern = "\\.", "\\ ") %>%
        #         paste(sep = ",", collapse = ", ") -> ch_temp
        copy_list[[i]] = paste(paste0("## ",ch_river_types[i],"   "), ch_temp, "   ") 
}

copy_list %>% 
        unlist %>% 
        writeClipboard()

        

