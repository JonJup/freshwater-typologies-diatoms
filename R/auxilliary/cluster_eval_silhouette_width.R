sil = silhouette(dist = loop_dist, x = as.numeric(loop_class))
sil = sil[, 3]
ls_sil[[(i.class - 1) * 4 + i.dist]] = sil


