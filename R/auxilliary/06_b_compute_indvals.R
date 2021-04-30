# ------------------------------------- #
### --- Compute Indicator Values  --- ### 
# ------------------------------------- #

# --------------- #
# date:         17.03.21
# files out:
#               06_sxs_list.RDS | species X sites table   
# called by:
#               07_derive_typical_assemblages.R
# Project:
#               Evaluating European Broad River Types for Macroinvertebrates 
# Purpose:
#               Compute A and B values of IndVal  
# --------------- #

# load data  --------------------------------------------------------------
data = readRDS("data/05_sxs_list.RDS")

# clean data --------------------------------------------------------------
# How well a stream type is represented was decided visually based on sample site /stream type maps
# Following lines are referred to in "08_seasonal_typical_assemblages.R" as 47:50
ch_acc    = paste0("RT", c(2,3,6, 8,9, 10, 11, 12, 14, 15, 16, 18, 19))

# subset data sets according to the visual categorization of stream type representation
data <- data[ls_bd_20 %in% ch_acc]

rm(ch_acc)
# combined streame types 
for (i in seq_along(x_ls_combine)){
        loop_var = as.character(x_ls_combine[[i]])
        ch_new_name = paste0(loop_var, collapse  = "_")
        ch_new_name = paste0("RT",ch_new_name)
        loop_var = paste0("RT", loop_var)
        data[ls_bd_20 %in% loop_var, ls_bd_20 := ch_new_name]
        rm(ch_new_name, loop_var, i)
}

# Turn river type into factor. 
data$ls_bd_20 %<>%
        factor %>%
        droplevels

ch_rt = unique(data$ls_bd_20)

site_col_id = which(names(data) == "gr_sample_id")
type_col_id = which(names(data) == "ls_bd_20")

taxa = colnames(data)[-c(site_col_id,type_col_id)]

new_indval = data.table(
        taxon = rep(taxa, times = length(ch_rt)),
        rt = rep(ch_rt, each = length(taxa)),
        A  = 0,
        B = 0
)
for (i in seq_along(taxa)){
        
        loop_var = taxa[i]
        loop_dt = data[,c("ls_bd_20", loop_var), with = F]
        id = as.vector(loop_dt[,2] == 1)
        loop_dt = loop_dt[id]
        loop_tbl = table(loop_dt)
        loop_div = nrow(loop_dt)
        loop_vec = as.vector(table(loop_dt)/nrow(loop_dt) )
        loop_rt = rownames(loop_tbl)
        
        for(k in seq_along(loop_vec)){
                new_indval[taxon == loop_var & rt == loop_rt[k], A := loop_vec[k]]   
        }
        rm(list = ls()[grepl("^loop", ls())])
        rm(i)
}
for (i in seq_along(ch_rt)){
        
        loop_var = ch_rt[i]
        loop_dt = data[ls_bd_20 == loop_var]
        n_sites = nrow(loop_dt)
        loop_cs = colSums(loop_dt[,-c(site_col_id,type_col_id), with = F])
        loop_cs2 = loop_cs/n_sites
        names(loop_cs2)[1]
        for(k in seq_along(loop_cs2)){
                new_indval[taxon == names(loop_cs2)[k] & rt == loop_var, B := loop_cs2[k]]   
        }
        rm(i)
}

new_indval
# save to file ------------------------------------------------------------
saveRDS(object = new_indval,   file = "data/06_indicator_list.rds")


# clean environment -------------------------------------------------------

rm_files = ls()[grepl(pattern = "^dt_", x = ls())]
rm(list = rm_files)
rm_files = ls()[grepl(pattern = "^ch_", x = ls())]
rm(list = rm_files)
rm_files = ls()[grepl(pattern = "^ls_", x = ls())]
rm(list = rm_files)
rm(rm_files)

rm(k, id, n_sites, new_indval, taxa)

print("#--------------------------------------------------------#")
# if (readline("delete dt_all? ") == "yes") rm(list = ls())
