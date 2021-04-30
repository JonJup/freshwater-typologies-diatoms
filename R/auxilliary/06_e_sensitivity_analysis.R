### ------ Sensitivity Analysis   ---- ###
# -------------------------------------- #

# 06.11.20
# GetReal
# Working Package 2 
# Diatoms

# output: 07_2020-11-06_sensitivity_parameter_a_50steps.RDS
# output: 07_2020-11-06_sensitivity_parameter_b_50steps.RDS

save=TRUE

if(!require(pacman))install.packages("pacman")
p_load(dplyr, 
       magrittr)

ls_mzb  = readRDS(file.path(DIR$pd, "07_indicator_list.RDS"))

# number of points to evaluate 
in_n   = 50
# create sequence of points 
in_v   = round(seq(from = 0.01, to = 1.00, length.out = in_n), 2)

ch_river_types = unique(ls_mzb$gen$rt)

for (i in 1:in_n) {
        # setup in first round 
        if (i == 1){
                dt_a_out = data.table(
                        as.data.frame(
                                matrix(0, 
                                       nrow = length(ch_river_types) * in_n, 
                                       ncol = 4)
                        )
                )
                
                names(dt_a_out) = c("river_type", "threshold", "n_spe", "n_gen")
                dt_a_out[,c("river_type", "threshold") := .(rep(ch_river_types, each = in_n), 
                                                            rep(in_v,           times = length(ch_river_types)))]
                dt_b_out = copy(dt_a_out)
        }
        
        # create TAs for loop parameter values    
        dt_spe_a = ls_mzb$spe[A > in_v[i]]
        dt_spe_b = ls_mzb$spe[B > in_v[i]]
        dt_gen_a = ls_mzb$gen[A > in_v[i] * 0.77]
        dt_gen_b = ls_mzb$gen[B > in_v[i] * 1.65]
        dt_fol_a = ls_mzb$fol[A > in_v[i] * 0.77^2]
        dt_fol_b = ls_mzb$fol[B > in_v[i] * 1.65^2]
        
        ch_data_set <- c()
        for (dt in c("dt_spe_a", "dt_spe_b", "dt_gen_a", "dt_gen_b", "dt_fol_a", "dt_fol_b")){
                dt_loop = get(dt)
                if(nrow(dt_loop)>0)
                        ch_data_set = append(ch_data_set, dt)
        }
        
        # assign uniqueness score to each taxon 
        for (dt in ch_data_set){
                dt_loop = get(dt)
                ch_loop = unique(dt_loop$taxon)
                for (ch in seq_along(ch_loop)){
                        dt_loop[taxon == ch_loop[ch], score := 1/dt_loop[taxon == ch_loop[ch], .N]]
                }
                assign(x = dt,
                       value = dt_loop)
                #print(dt)
        }
        # compute group uniqueness scores 
        for (dt in ch_data_set){
                dt_loop = get(dt)
                dt_loop %<>% 
                        group_by(rt) %>% 
                        dplyr::summarize(m = mean(score)) %>% 
                        setDT
                if (!all(ch_river_types %in% dt_loop$rt)) {
                        ch_missing_type = ch_river_types[!ch_river_types %in% dt_loop$rt]
                        dt_loop = rbindlist(list(dt_loop, data.table(rt = ch_missing_type, m = NA)))
                }
                assign(x = paste0(dt, "_us"), 
                       value = dt_loop)
                
        }
        ch_all_ds = c("dt_spe_a", "dt_gen_a", "dt_spe_b", "dt_gen_b", "dt_fol_a", "dt_fol_b",
                      "dt_spe_a_us", "dt_gen_a_us", "dt_fol_a_us", 
                      "dt_spe_b_us", "dt_gen_b_us", "dt_fol_b_us")
        
        while (!all(ch_all_ds %in% ls())){
                missing = ch_all_ds[!ch_all_ds %in% ls()]
                missing = missing[1]
                if (stringr::str_detect(pattern = "_us$", string = missing)) {
                        assign(x = missing,
                               value = data.table(rt = ch_river_types,
                                                  m = NA))
                } else {
                        print("missing not us")
                        break()
                }
        }
        # loop over river types and assign values to out data tables
        for (j in 1:length(ch_river_types)) {
                
                dt_a_out[river_type  == ch_river_types[j] & threshold    == in_v[i],
                         c("n_spe", "n_gen", "n_fol", "u_spe", "u_gen", "u_fol") :=
                                 .(dt_spe_a[rt == ch_river_types[j], .N],
                                   dt_gen_a[rt == ch_river_types[j], .N],
                                   dt_fol_a[rt == ch_river_types[j], .N],
                                   dt_spe_a_us[rt == ch_river_types[j], m],
                                   dt_gen_a_us[rt == ch_river_types[j], m],
                                   dt_fol_a_us[rt == ch_river_types[j], m])
                ]
                dt_b_out[river_type  == ch_river_types[j] & threshold    == in_v[i],
                         c("n_spe", "n_gen", "n_fol", "u_spe", "u_gen", "u_fol") :=
                                 .(dt_spe_b[rt == ch_river_types[j], .N],
                                   dt_gen_b[rt == ch_river_types[j], .N],
                                   dt_fol_b[rt == ch_river_types[j], .N],
                                   dt_spe_b_us[rt == ch_river_types[j], m],
                                   dt_gen_b_us[rt == ch_river_types[j], m],
                                   dt_fol_b_us[rt == ch_river_types[j], m])
                ]
                dt_a_out[river_type == ch_river_types[j] & threshold == in_v[i], 
                         c("n_all", "u_all") := 
                                 .(
                                         n_spe + n_gen + n_fol,
                                         sum(u_spe, u_gen, u_fol, na.rm=T)/3
                                 )
                         
                ]
                dt_b_out[river_type == ch_river_types[j] & threshold == in_v[i], 
                         c(
                                 "n_all",
                                 "u_all"
                         ) := 
                                 .(
                                         n_spe + n_gen + n_fol,
                                         sum(u_spe, u_gen, u_fol, na.rm=T)/3
                                 )
                ]
        }
        rm(list = ch_all_ds)
        rm(missing, i, j, dt_loop,dt,ch_missing_type,ch_all_ds,ch_data_set, ch)
        gc()
        ## -- debug zone -- ##
        # sen_out[river_type == rt_vector[4] &
        #             B_threshold == b_loop[B_ind] &
        #             A_threshold == a_loop[A_ind],
        #         n_species := ]
        ## --               -- ##
        
        # # add to list 
        # out_a_list[[i]] <- dt_a_out, gen_typ))
        # out_b_list[[i]] <- rbindlist(list(spe_typ, gen_typ))
        # rm(spe_typ, utm);gc()
        # print(i)
}
rm(in_n, in_v)

dt_a_out$river_type %<>% factor()  
dt_b_out$river_type %<>% factor() 

if (save) {
        saveRDS(dt_a_out, file.path(DIR$pd, paste0("07_sensitivity_parameter_a_50steps.RDS")))
        saveRDS(dt_b_out, file.path(DIR$pd, paste0("07_sensitivity_parameter_b_50steps.RDS")))   
}
