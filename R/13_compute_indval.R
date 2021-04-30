### --- Indicator genera Macroinvertebrates --- ### 

# --------------- #
# date:  05.02.21
# files in 
# files out
# macroinvertebrates ins lyche-solheim paper  
# determine Indicator Genera for the stream types of Lyche-Solheim et al. (2020)
# --------------- #

# setup -----------------------------------------------
source(here::here("R/setup.R"))

# load data -------------------------------------------
data = readRDS("data/05_sxs_list.RDS")


# prepare data ------------------------------------------------------------
setDT(data)
data2 = copy(data)
data2[,c("gr_sample_id", "ls_bd_20") := NULL]
accepted_rivers = paste0("RT", c(2,3,6, 8,9, 10, 11, 12, 14, 15, 16, 18, 19))

for (i in seq_along(accepted_rivers)) {
        
        if (i == 1){
                ls_indval = list()
        }
        
        group_var = accepted_rivers[i]
        print(paste(group_var, "start @", format(Sys.time(), "%H:%M:%S")))

        if (nrow(data[ls_bd_20 == group_var]) == 0)
                next()

        ls_indval[[i]] = indicators(
                X = data2,
                cluster = data$ls_bd_20,
                group = group_var,
                max.order = 1,
                verbose = FALSE,
                At = 0,
                Bt = 0,
                func = "IndVal.g",
                control = how(nperm = 15000)
        )
        
        saveRDS(ls_indval, paste0("data/temp/indval_",i,".rds"))
        print(paste(group_var, "ended @", format(Sys.time(), "%H:%M:%S")))
        rm(group_var, i)
}

saveRDS(ls_indval, "data/14_indval_list.rds")

# Find Indicator Taxa  ----------------------------------------------------

ls_indval = readRDS("data/14_indval_list.rds")

# number of tests = number of taxa. Family-wise error only corrected for each river type
n_tests =  length(ls_indval[[1]]$A)
ls_indval2 = lapply(ls_indval, 
                    function(x) data.table(taxon  = x$candidates, 
                                           pvalue = x$p.value,
                                           indval = x$sqrtIV))
ls_indval2 %<>% 
        lapply(setorderv, "pvalue") %>% 
        lapply(function(x) x[, holms_p := 0.05 / n_tests:(n_tests-(nrow(x)-1))]) %>% 
        lapply(function(x) x[, indicator := pvalue <= holms_p])
ls_indval3 = lapply(ls_indval2, function(x) x[indicator == TRUE])
names(ls_indval3) = accepted_rivers
# save data -------------------------------------------
saveRDS(ls_indval3, "data/15_indicator_list.rds")

