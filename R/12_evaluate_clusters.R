### --------------------------------------------- ###
### --- Evaluate bio-clusters from typologies --- ### 
### --------------------------------------------- ###

# --------------- #
# date:   
#               22.03.21
# files in:  
#               -> 11_sxs_genus_W_bio_typology.rds
#               -> 12_sxs_genus_typology_wo_bio.rds
# files out:  
#               <- 13_class_eval_mzb.rds
# Project: 
#               Evaluating European Broad River Types for Macroinvertebrates 
# Purpose: 
#               Compute cluster metrics for the different typologies.   
# --------------- #

# setup ---------------------------------------------------------------------------------------------------------------------------------------------------
source("R/setup.R")
## -- functions 
# - for generalized silhouette width
source("~/03_R/functions/genmean.R")
source("~/03_R/functions/mdist.R")
source("~/03_R/functions/silgen.R")
source("~/03_R/functions/call_gensil.R")

# load data  ----------------------------------------------------------------------------------------------------------------------------------------------
data = readRDS("data/12_sxs_genus_typology_wo_bio.rds")
data2 = readRDS("data/11_sxs_W_bio_typology.rds")

# join typology with bioclusters  ---------------------------------------------------
data2 %<>% dplyr::select(gr_sample_id, bio)
data = left_join(x = data, 
                  y = data2, 
                  by = "gr_sample_id"
)
rm(data2);gc()

## -- remove rare types 
table(data$ls20)
# #x_ls_combine = list(c(2,3), c(10,11), c(6,12))
# x_ls_combine = list(c(10,11,15,18), c(2,3), c(6,12))
# ch_acc    = paste0("RT", c(2,3,6, 8,9, 10, 11, 12, 14, 15, 16, 18, 19))
data %<>%
        mutate(
                ls6 =
                        case_when(
                                data$ls20 %in% c("RT2", "RT3") ~ "RT2_3",
                                data$ls20 %in% c("RT6" , "RT12")  ~ "RT6_12",
                                data$ls20 %in% paste0("RT", c(10,11,15,18)) ~ "RT10_11_15_18",
                                data$ls20 %in% c("RT14") ~ "RT14",
                                data$ls20 %in% c("RT16") ~ "RT16",
                                data$ls20 %in% c("RT19") ~ "RT19",
                                data$ls20 %in% c("RT9") ~ "RT9",
                                data$ls20 %in% c("RT17") ~ "RT8",
                                data$ls20 %in% c("RT8") ~ "RT17"
                        )
        )
# prepare data  ----------------------------------------------------------------
## -- sites x species table as matrix and without typology columns

setDT(data)
ma_data = 
        copy(data) %>% 
        .[, c("gr_sample_id", "ls20", "ls12","ls6", "illies", "eea", "gloric", "bio") := NULL] %>% 
         as.matrix

## -- list of classifications - those that are strings are transformed to numbers 
ls_class = list(
        ls6    = as.numeric(factor(data$ls6)),
        ls12   = as.numeric(factor(data$ls12)),
        ls20   = as.numeric(factor(data$ls20)),
        gloric = data$gloric,
        illies = as.numeric(factor(data$illies)),
        eea    = as.numeric(factor(data$eea)),
        bio    = data$bio
)
## -- compute Jaccard distance matrix of data 
dt_distance = parallelDist(ma_data, method = "binary")

# create null classification --------------------------------------------------------
group_sizes = data[, lapply(.SD, uniqueN), .SDcols = c("ls6","ls12", "ls20", "gloric", "eea", "illies", "bio")]

number_of_typologyies = length(ls_class)

## -- loop to fill ls_class with null typologies 
for (i in 1:100){
        set.seed(i)
        n_types = sample(min(group_sizes):max(group_sizes), 1)
        set.seed(i)
        ls_class[[i + number_of_typologyies]] = sample(1:n_types, 
                                   size = nrow(ma_data), 
                                   replace = TRUE)
        names(ls_class)[i+6] = paste0("null",i)
}
rm(group_sizes, i, n_types, number_of_typologyies)
gc()

# compute cluster metrics ---------------------------------------------------------------------------------------------------------------------------------

## -- This was stable when evaluating all 106 typologies together. 
## -- Hence it is not run in separate chunks. 

## -- cluster.stats function 
eval = lapply(
        1:length(ls_class),
        function (x) cluster.stats(
                d = dt_distance,
                clustering = ls_class[[x]],
                silhouette = FALSE,
                wgap = FALSE,
                sepindex = FALSE,
                sepprob = FALSE,
                sepwithnoise = FALSE
                )
        ) %>% 
        flatten(); beep()

## -- reshape to create results table 
eval = eval[ - which(lapply(eval, length) != 1)]
name_var = names(eval)
eval2 = data.table(eval)
eval2[, c("name", "clus") := .(name_var, rep(1:length(ls_class), 
                                                each = nrow(eval2)/length(ls_class)))]
eval2 = pivot_wider(
        eval2,
        id_cols = clus,
        names_from = name,
        values_from = eval) %>%
        apply(2, unlist) %>%
        as.data.frame() %>%
        setDT()

# generalized silhouette width  -------------------------------------------

## -- This was stable when evaluating all 106 typologies together. 
## -- Hence it is not run in separate chunks. 
gensil_test_output = map(.x = 1:length(ls_class),
                         .f = ~ call_gensil(ls_class[[.x]]))

gensil_dt = rbindlist(gensil_test_output)
typo_names = append (c("brt6", "brt12", "brt20", "gloric", "illies", "eea", "bio"), paste0("null",1:100))
gensil_dt[, typology := rep(typo_names, each = 6)]
gensil_dt %<>% 
        mutate(p2 = case_when(p == -Inf ~"min",
                              p == -2 ~ "m2",
                              p == -1 ~ "harmonic",
                              p == 1 ~ "arithmetic",
                              p == 2 ~ "quadratic",
                              p == Inf ~ "max")) %>%  
        pivot_wider(id_cols = typology, names_from = p2, values_from = silhouette) 

## -- save temporary before computing indval in chunks. 
# saveRDS(list(gensil_dt, eval2), "data/temp/eval_clust_pre_indval.rds")
# read_list = readRDS("data/temp/eval_clust_pre_indval.rds")
# ## -- clean up 
# rm(call_gensil, typo_names, cc, data, eval, eval_cluster, extr_trait, gen_mean, gensil_test_output,
#    i, internal_cluster_fun, mdist, n_types, name_var, plot_pcoa,silgen, proj_pcoa, my_sil, genmean,
#    eval_cluster_fun)


# IndVal ------------------------------------------------------------------
## -- this crashed R once and will hence be run in chunks of ten. 
## -- Outputs are to be safed as temporary files after each run. 
## -- After the second run, check that results can be combined. 
## -- CHECKED: works 
## -- Commented code below runs loop - takes a long time (days)! 

for (i in 0:10){

        lp_name = paste0("results_indval",i+1)
        from = paste0(i,"1") %>% as.numeric()
        to   = paste0(i+1,"0") %>% as.numeric()
        if (i == 10)
                to = 107
        assign(x = lp_name,
               value = lapply(ls_class[from:to],
                              function(x)
                                      multipatt(
                                              ma_data,
                                              cluster = x,
                                              control = how(nperm = 999),
                                              max.order = 1,
                                              print.perm = T
                                      ))
                       )
        xx = get(lp_name)
        save_name = paste0("data/temp/eval_clust_indval_",i+1,".rds")
        saveRDS(xx, save_name)
}

## -- loop for loading results from file. 
# files = dir("data/temp/", pattern = "eval_clust_indval")
# for (i in seq_along(files)){
#         file_number = gsub(pattern = "eval_clust_indval_", replacement = "", x = files[i])
#         file_number = gsub(pattern = ".rds", replacement = "", x = file_number)
#         file_number = gsub(pattern = "_", replacement = "", x = file_number)
#         assign(
#                 x = paste0("results_indval",file_number),
#                 value = 
#                         readRDS(file.path("data/temp", files[i]))
#         )
#         rm(i, file_number);gc()
# }
all_results_indval = c(
        results_indval1, 
        results_indval2,
        results_indval3,
        results_indval4,
        results_indval5,
        results_indval6,
        results_indval7,
        results_indval8,
        results_indval9,
        results_indval10,
        results_indval11
         )
rm(files,results_indval1, results_indval2,results_indval3,results_indval4,results_indval5,results_indval6,results_indval7,results_indval8,results_indval9,results_indval10,results_indval11)
# Now there are many p-values and associated NHSTs. To control for multiple testing
# I will use  Holm's step-down multiple testing procedure.
# How many tests?
# extract p-values and save as data.table
results_indval2 = all_results_indval
results_indval2 %<>% lapply(function(x) setDT(x$sign))
# remove NA p-values and add taxon names
results_indval2 %<>% lapply(function(x) x[, c("p.value") := nafill(p.value,fill=1)])
results_indval2 %<>% lapply(function(x) x[, c("taxon") := rownames(all_results_indval$ls20$B)])
# subset table
results_indval2 %<>% lapply(function(x) x[, c("p.value", "index", "stat", "taxon")])
# order matrix
results_indval2 %<>% lapply(function(x)  setorderv(x, "p.value"))
# number of NHSTs
n_test = nrow(results_indval2$ls20)
# Set the desired significance level
alpha = 0.05
# add Holm's p-value thresholds and boolean that indicates whether the permutation p-value is below the new threshold.
# results_indval2 %<>% lapply(function(x) x[, holms_p := alpha / n_test:1])
# results_indval2 %<>% lapply(function(x) x[, significant := holms_p > p.value])
# first_no         = lapply(results_indval2, function(x) which(x$significant == FALSE)[1])
# significant2     = lapply(first_no, function(x) rep(TRUE, times = x - 1))
# significant2_2   = lapply(significant2, function(x) rep(FALSE, times = nrow(results_indval2$ls20) - length(x)))
# significant2     = lapply(1:length(ls_class), function(a) append_list(x = significant2, y = significant2_2, i = a))
# results_indval2  = lapply(1:length(ls_class), function(x) results_indval2[[x]][, significant := significant2[[x]]])
# indval_statistic = lapply(results_indval2, function(x) x[significant == TRUE, sum(stat)/ncol(ma_data)])

## -- version without accounting for multiple testing 
results_indval2 %<>% lapply(function(x) x[, significant := 0.01 >= p.value])
first_no         = lapply(results_indval2, function(x) which(x$significant == FALSE)[1])
significant2     = lapply(first_no, function(x) rep(TRUE, times = x - 1))
significant2_2   = lapply(significant2, function(x) rep(FALSE, times = nrow(results_indval2$ls20) - length(x)))
significant2     = lapply(1:length(ls_class), function(a) append_list(x = significant2, y = significant2_2, i = a))
results_indval2  = lapply(1:length(ls_class), function(x) results_indval2[[x]][, significant := significant2[[x]]])
indval_statistic = lapply(results_indval2, function(x) x[significant == TRUE, sum(stat)/ncol(ma_data)])

# # ISAMIC ------------------------------------------------------------------
# result_isamic = lapply(ls_class,
#                        function(x) isamic(comm = ma_data,
#                                           clustering = x)
#                        )
# mean_isamic = lapply(result_isamic, mean)
# 
# # Partana  ----------------------------------------------------------------
# result_partna = lapply(ls_class, 
#                        function(x) partana(c = x, 
#                                            dist = dt_distance)$ratio
#                        )

# combine data  -----------------------------------------------------------
#eval2 = read_list[[2]]
eval2[, c(
       # "partana", 
       # "isamic",
        "indval") := .(
       # unlist(result_partna) ,
       # unlist(mean_isamic),
        unlist(indval_statistic)
)]

eval2[, typology := c("brt6","brt12", "brt20", "gloric", "illies", "eea", "bio", paste0("null",1:100))]
eval2[, max.diameter := NULL]
eval3 = copy(eval2)
eval3 %<>% left_join(gensil_dt, 
                     by = "typology")



# compute classification strength ---------------------------------------------------

dt_similartiy = 1 - dt_distance
eval4 = lapply(
        1:length(ls_class),
        function (x) cluster.stats(
                d = dt_similartiy,
                clustering = ls_class[[x]],
                silhouette = FALSE,
                wgap = FALSE,
                sepindex = FALSE,
                sepprob = FALSE,
                sepwithnoise = FALSE
        )
) %>% 
        flatten();beep()
eval4 = eval4[ - which(lapply(eval4, length) != 1)]
name_var = names(eval4)
eval5 = data.table(eval4)
eval5[, c("name", "clus") := .(name_var, rep(1:length(ls_class), 
                                             each = nrow(eval5)/length(ls_class)))]
eval5 = pivot_wider(
        eval5,
        id_cols = clus,
        names_from = name,
        values_from = eval4) %>%
        apply(2, unlist) %>%
        as.data.frame() %>%
        setDT()

eval5[, classification_strength := average.within - average.between]

eval3$classification_strength = eval5$classification_strength

# save to file ----------------------------------------------------------------------
saveRDS(eval3, "data/13_class_eval_dia.rds")

