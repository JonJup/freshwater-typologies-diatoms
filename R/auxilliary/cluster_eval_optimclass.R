# OptimClass ----------------------------------------------------------------------------------------------------------------------------------------------
# Here I write my own version of OptimClass.
# This gives me more control.


        if (i.class == ch_class[1]) ls_optinclass = list()
        
        print(i.class)
        print(paste("Start computing OptimClass",
                    i.class,
                    "@",
                    format(Sys.time(), "%H:%M:%S")))
        
        loop_class = ls_classifications[[i.class]]
        # number of species
        rows = ncol(pa_data)
        # number of clusters
        cols = length(levels(loop_class))
        # matrix to hold p-values of Fisher's Exact Test
        res  = matrix(NA,
                      nrow = rows,
                      ncol = cols)
        
        # loop over all combinations of species and clusters to evaluate how faithful the
        # species is to the cluster. Faithfulness is expressed as the p-value of Fisher's
        # Exact Test.
        for (oc.i in 1:rows) {
                # Species
                for (oc.j in 1:cols) {
                        # Clusters
                        # total number of occurrences of species i
                        n   = colSums(pa_data)[oc.i]
                        # number of occurrences of species i in cluster j
                        n_p = sum(pa_data[loop_class == loop_class[oc.j], oc.i])
                        # total number of instances of cluster j
                        N_p = sum(loop_class == loop_class[oc.j])
                        # total number of sites over all clusters
                        N   = nrow(pa_data)
                        # put these values into a matrix to compute Fisher's Exact Test
                        fisher_matrix = matrix(
                                data = c(n_p, n - n_p, N_p - n_p, N - N_p - n  + n_p),
                                ncol = 2,
                                byrow = TRUE
                        )
                        # compute Fisher's Exact Test and assign p-value to element of the res matrix
                        res[oc.i, oc.j] = fisher.test(fisher_matrix)$p.val
                } # clusters
        } # species
        
        # evaluate how many species are faithful to any cluster for oc.i different p-values
        for (oc.i in 1:100) {
                # different p-values
                # setup p-values to evaluate in the first loop
                if (oc.i == 1) {
                        res2 = matrix(ncol = 2,
                                      nrow = 100)
                        numbers = c(0.05)
                        # The first p-value is 0.05 the second is 0.85 * 0.05, the third
                        # 0.85^2 * 0.05 and so on
                        for (num.i in 1:99)
                                numbers[num.i + 1] = numbers[num.i] * 0.85
                        res2[, 1] = numbers
                }
                
                # logical: which p-value is smaller than the value we test agianst in this
                # loop iteration
                lp_oc_lgl = res < res2[oc.i, 1]
                # how many in each column (cluster)
                lp_oc_cs = colSums(lp_oc_lgl)
                # in total that is
                lp_oc_s = sum(lp_oc_cs)
                # the theoretical maximum is the number of species 'M' times the number of
                # clusters 'cols'. This doesn't need to be computed each time but also
                # doesn't take long and logically fits better here.
                lp_oc_tm = M * cols
                # finally, what is the fraction of faithful species X cluster relations?
                lp_oc_fr = lp_oc_s / lp_oc_tm
                # assign to output matrix 'res2'
                res2[oc.i, 2] = lp_oc_fr
                
                # remove loop objects
                rm(list = ls()[grepl(pattern = "lp_oc", x = ls())])
        }
        res2 %<>% as.data.frame() %>% setDT
        ls_optinclass[[length(ls_optinclass) + 1]] = res2
        
        # Now I want to know how sharply the number of faithful species declines as we
        # decrease the p-value threshold. This means we are interested in the slop of the
        # relationship between number of faithful species and p-value threshold. This can
        # be determined with a linear regression. To met assumptions I log-transformed both
        # variables.For the interpretation this means that one percent change in the p-value
        # threshold elicits a beta % change in the number of faithful species.
        # optimclass_model = lm(log(res2[, 2]) ~ log(res2[, 1]))
        # The coefficient is positive (higher p-value threshold = more faithful species).
        # Higher values imply a worse clustering as the slop is steeper.
        # out_optim_class = unname(optimclass_model$coefficients[2])
        # out_optim_prop_005 = res2[1, 2]
        # out_optim_prop_0m6 = res2[67, 2]


# optimclass_data = rbindlist(ls_optinclass)
# optimclass_data[, classification := rep(ch_class, each = nrow(res2))]
# names(optimclass_data) = c("pvalue", "faithful", "classification")
# saveRDS(optimclass_data, "03_pd/cluster_eval_mzb_optimclass.RDS")

