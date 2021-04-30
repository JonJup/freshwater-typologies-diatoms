# 
pacman::p_load(
        beepr,
        cluster,
        clusterCrit,
        coseq,
        DataExplorer,
        data.table,
        diathor,
        dplyr,
        FactoMineR,
        FD,
        fpc,
        fuzzySim,
        geosphere,
        ggplot2,
        gridExtra,
        here,
        indicspecies,
        magrittr,
        optpart,
        parallelDist,
        purrr,
        riojaExtra,
        twinspanR,
        sf,
        spaa,
        stringr,
        tmap,
        tmaptools,
        treeClust,
        readxl,
        corrplot
        )
library(here)

# Directories -------------------------------------------------------------
dir = list()
dir$dt     = here("data/")
dir$ls     = "~/01_Uni/my_papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/02_Lyche_Solheim_et_al/"
dir$gloric   = "~/01_Uni/my_papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/03_GloRiC_v10/"
dir$ecoregions = "~/01_Uni/my_papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/04_wfd_ecoregions/"
dir$eea        = "~/01_Uni/my_papers/2020_02_TypologyPaper/TypologyPaper/001_stream_network/05_bfn_eea_bioregions/"
dir$dia  = "~/01_Uni/02_getreal/02_wp2/data/originals_processed/"

# Functions  --------------------------------------------------------------

source("~/03_R/functions/cc.R")
source("~/03_R/functions/internal_cluster_fun.R")
source("~/03_R/functions/cluster_eval_fun.R")
source("~/03_R/functions/extr_traits.r")

append_list = function(x,y,i){
        x[[i]] = append(x[[i]], y[[i]])
}
getmode <- function(v) {
        uniqv <- unique(v)
        if (any(is.na(uniqv))) uniqv <- uniqv[-which(is.na(uniqv))]
        uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Options -----------------------------------------------------------------



# ## keep?
# dir$dia = here("../../02_getreal/002_working_package_02/001_community_data/002_combined/001_diatoms/003_processed_data/")
# dir$mzb = here("../../02_getreal/002_working_package_02/001_community_data/002_combined/002_invertebrates/003_processed_data/")

