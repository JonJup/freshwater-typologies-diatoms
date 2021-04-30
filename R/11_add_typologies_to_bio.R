# ---------------------------------------------------- #
### --- Add typologies to macroinvertebrate data --- ### 
# ---------------------------------------------------- #

# --------------- #
# date:  10.02.21
# files in m_river_fec_broad_type.shp, gloric_v10.RDS, Ecoregions.shp, BiogeoRegions2016.shp, sxs_genus.rds
# files out sxs_genus_typology_wo_bio.rds
# GetReal WP2 
# assign typologies to macroinvertebrate sampling sites
# --------------- #

# setup ---------------------------------------------------------------------------------------------------------------------------------------------------
source("R/setup.R")

# load data -----------------------------------------------------------------------------------------------------------------------------------------------
# typologies 
sf_ls     = st_read(file.path(dir$ls    , "m_river_fec_broad_type.shp"))
sf_gloric = readRDS(file.path(dir$gloric, "gloric_v10.RDS"))
sf_illies = st_read(file.path(dir$ecoregions, "Ecoregions.shp"))
sf_eea    = st_read(file.path(dir$eea, "BiogeoRegions2016.shp"))
# macroinvertebrates
data =  readRDS("data/05_sxs_list.RDS")
data2 =  readRDS("data/04_harmonized_diatoms.rds")

# prepare spatial analysis --------------------------------------------------------------------------------------------------------------------------------
# first we need to check, that there are no empty sites or sites no more than 3 different genera 
# number of genera per site -> rowSums 
# number of sites per genus -> colSums
##  we have 
nrow(data)
## ... 1285 sites.
## 1% of which are ... 
1285/100
## 13 
gps = 1
spg = 1
site_col = which(names(data) == "gr_sample_id")
type_col = which(names(data) == "ls_bd_20")
data_mod = data
data_ot = data_mod[,-c(site_col,type_col), with = F]
## -- all sites have at least five taxa 
any(rowSums(data_ot)<4)
## -- rare taxa? 
any(colSums(data_ot)<13)
sum(colSums(data_ot)<13)
## -- of the 702 taxa, 477 occur do occur at less than 1% of sites 
## -- remove those taxa 
rare_taxa = names(which(colSums(data_ot)<13))
non_rare = dplyr::setdiff(rare_taxa, colnames(data_mod))
non_rare = names(data_mod)[which(!names(data_mod) %in% rare_taxa)]
length(names(data_mod)) == length(rare_taxa) + length(non_rare)
data_mod = data_mod[,.SD, .SDcols = non_rare]

# this removed 704 - 227 = 477 genera ...
ncol(data_mod)
ncol(data)
# ... and 8466 - 8368 = 98 sites
nrow(data_mod)
nrow(data)

# subset data to these sites and genera. 
data2 = data2[species %in% names(data_mod) & gr_sample_id %in% data_mod$gr_sample_id]

# create sites data
sites = unique(data2, by = "gr_sample_id")
# drop columns 
sites = sites[, c("gr_sample_id", "geometry")]
# create spatial object 
sites %<>% st_as_sf()

# transform gloric and lyche solheim to ETRS: Lambert equal area azimuthal  
sf_gloric %<>% st_transform(crs = 3035)
sf_ls     %<>% st_transform(crs = 3035)
sites %<>% st_transform(st_crs(sf_gloric))

# check bbox 
tm_shape(sites, bbox = st_bbox(sites)) + tm_dots()

# crop typologies to data 
crp_gl = st_crop(x = sf_gloric, y = st_bbox(sites))
crp_ls = st_crop(x = sf_ls, y = st_bbox(sites))

buf_gl = st_buffer(x = crp_gl, dist = 250)
buf_ls = st_buffer(x = crp_ls, dist = 250)

# intersect
sgbp_gl = st_intersects(x = sites, y = buf_gl)
sgbp_ls = st_intersects(x = sites, y = buf_ls);beep()

sgbp_gl = lengths(sgbp_gl) > 0 
sgbp_ls = lengths(sgbp_ls) > 0 

sites_gl = sites[sgbp_gl, ]
sites_ls = sites[sgbp_ls, ]

# add types -----------------------------------------------------------------------------------------------------------------------------------------------
nn_gl = st_nearest_feature(sites_gl, buf_gl)
nn_ls = st_nearest_feature(sites_ls, buf_ls);beep()

sites_gl$gl = buf_gl$Kmeans_30[nn_gl]
sites_ls$ls20 = buf_ls$m_btype20c[nn_ls]
sites_ls$ls12 = buf_ls$m_btype12[nn_ls]

# reduce to gr_sample_id and classes 
sites_ls.dt = copy(sites_ls)
sites_gl.dt = copy(sites_gl)

sites_ls.dt %>% setDT 
sites_gl.dt %>% setDT 

sites_ls.dt = sites_ls.dt[, c("gr_sample_id", "ls20", "ls12")] 
sites_gl.dt = sites_gl.dt[, c("gr_sample_id", "gl")] 

common_sites = intersect(sites_ls.dt$gr_sample_id, 
                                 sites_gl.dt$gr_sample_id);beep()

sites_ls.dt = sites_ls.dt[gr_sample_id %in% common_sites]
sites_gl.dt = sites_gl.dt[gr_sample_id %in% common_sites]
sites2 = sites_gl.dt[sites_ls.dt, on = "gr_sample_id"]

# observations per type 
tgl = table(sites2$gl)
tgl = names(which(tgl < 11))
sites2 = sites2[!gl %in% tgl]
tsl = table(sites2$ls12)
tsl = names(which(tsl < 11))
sites2 = sites2[!ls12 %in% tsl]
tsl = table(sites2$ls20)
tsl = names(which(tsl < 11))
sites2 = sites2[!ls20 %in% tsl]

sites3 = left_join(sites2, sites,  by = "gr_sample_id") %>% st_as_sf()
plt_gl = filter(crp_gl, Kmeans_30 %in% sites3$gl)
plt_ls = filter(crp_ls, m_btype20c %in% sites3$ls)
## -- visual checking of the results -- ## 
st_write(plt_gl, "data/temp/gl.gpkg")
st_write(plt_ls, "data/temp/ls.gpkg")
st_write(sites3, "data/temp/sites.gpkg")
## -> ok 
file.remove(fs::dir_ls("data/temp/"))
## -------------------------------------- ## 
### --- bioregions 
sf_illies %<>% st_transform(crs = st_crs(sites))
sf_eea %<>% st_transform(crs = st_crs(sites))
jn_ill = st_join(x = filter(sites, gr_sample_id %in% sites3$gr_sample_id), y = sf_illies) %>% 
        dplyr::select(c("gr_sample_id", "NAME")) %>% 
        st_drop_geometry() %>% 
        setDT
jn_eea = st_join(x = filter(sites, gr_sample_id %in% sites3$gr_sample_id), y = sf_eea) %>% 
        dplyr::select(c("gr_sample_id", "name")) %>% 
        st_drop_geometry() %>% 
        setDT

sites4 = left_join(sites3, 
                   jn_ill)

sites4 %<>% left_join(jn_eea)

names(sites4) = c("gr_sample_id", "gloric", "ls20", "ls12",  "illies", "eea", "geometry")

setDT(sites4)
teea = table(sites4$eea)
teea = names(which(teea < 11))
sites4 = sites4[!eea %in% teea]
till = table(sites4$illies)
till = names(which(till < 11))
sites4 = sites4[!illies %in% till]
tgl = table(sites4$gloric)
tgl = names(which(tgl < 11))
sites4 = sites4[!gloric %in% tgl]
tsl = table(sites4$ls20)
tsl = names(which(tsl < 11))
sites4 = sites4[!ls20 %in% tsl]
tsl = table(sites4$ls12)
tsl = names(which(tsl < 11))
sites4 = sites4[!ls12 %in% tsl]

data3 = data2[gr_sample_id %in% sites4$gr_sample_id]
data3 %<>% left_join(sites4,
                     by = "gr_sample_id")
# site X species matrices  --------------------------------------------------------------------------------------------------------------------------------
sxs = splist2presabs(data3, 
                     sp.col = which(names(data3) == "species"), 
                     sites.col = which(names(data3) == "gr_sample_id"), )

sxs2 = left_join(sxs, sites4, by = "gr_sample_id")
sxs2 %<>% dplyr::select(!geometry)

# save to file ------------------------------------------------------------
saveRDS(sxs2, "data/12_sxs_genus_typology_wo_bio.rds")

