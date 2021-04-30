## -- make table -- ## 


data = readRDS("002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/07_final_taxon.RDS")
out_list = list()

dt_mzb$rt %<>% factor(levels = c("RT1", "RT2_3_5", "RT4", "RT8", "RT9", "RT10_11_18", "RT14", "RT15_16"))

for (i in 1:nrow(dt_mzb)) {
        taxon =     dt_mzb$taxon[i]    
        taxon2 = str_replace(taxon, "\\.", "\\ ")
        
        mech = str_remove(string = dt_mzb$rt[i], pattern = "RT")
        
        loop_data = data.table(taxon = taxon2, 
                   family = data[final_taxon == taxon2, unique(family)], 
                   order  = data[final_taxon == taxon2, unique(order)], 
                   class = data[final_taxon == taxon2, unique(class)])
        
        if (dt_mzb$mechanism[i] == "A"){
                loop_data[, c("A", "B") := .(mech, NA)]   
        } else {
                loop_data[, c("A", "B") := .(NA, mech)]   
        }
        out_list[[i]] = loop_data
        rm(taxon, taxon2, mech, loop_data)
        
}

out_dt = rbindlist(out_list)

for (i in 1:uniqueN(out_dt$taxon)) {
        if (i == 1) out_list = list()
        ch_taxon =  unique(out_dt$taxon)[i]
        loop_dt = out_dt[taxon == ch_taxon]
        loop_dt_a = unique(loop_dt$A)
        loop_dt_b = unique(loop_dt$B)
        if (!is.na(loop_dt_a)) {
                loop_dt_a %<>% paste(collapse = ",")
        }
        if (!any(is.na(loop_dt_b))) {
                loop_dt_b %<>% paste(collapse = ", ")
        }
        new_dt = loop_dt[1,]
        new_dt$A = loop_dt_a
        new_dt$B = loop_dt_b
        out_list[[i]] = new_dt
        if (i == uniqueN(out_dt$taxon)) out_dt = rbindlist(out_list)
}
setorderv(out_dt, c("class", "order", "family", "taxon"))
xlsx::write.xlsx(x = out_dt, 
                file = "005_documents/2020_12_report_coauthors/ta_mzb.xlsx")

