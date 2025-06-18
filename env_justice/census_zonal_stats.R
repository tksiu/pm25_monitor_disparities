require(exactextractr)


"""  objects inherited from previous scripts """
# source("/kde/adaptive_kde.R")
# source("/env_justice/census_CIMD.R")



"""
Download URLs of Canada Census 2021 boundary files are from here:
https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21
"""


###  Calling provincial boundary  ###

prov = read_sf(dsn = "./data/lpr_000b21a_e.shp", layer = "lpr_000b21a_e")
prov = subset(prov, PRUID %in% c("10","11","12","13","35","24"))
prov = st_transform(prov, 4326)
prov = st_make_valid(prov)


###  Calling DA boundary  ###

da = read_sf(dsn = "./data/lda_000b21a_e.shp", layer = "lda_000b21a_e")
da = subset(da, PRUID %in% c("10","11","12","13","35","24"))
da = st_transform(da, crs = 4326)



###  Quintile function  ###

quintiles = function(x) {
   q = ifelse(x <= 0.2, 1, ifelse(x <= 0.4, 2, ifelse(x <= 0.6, 3, ifelse(x <= 0.8, 4, 5))))
   return(q)
}



###  Extract Zonal Statistics ###
if (sys.nframe() == 0) {

    sp_tw_kde_zonal = exact_extract(raster(pa_sp_den_tw), da$geometry, "mean")
    fem_sp_tw_kde_zonal = exact_extract(raster(fem_sp_den_tw), da$geometry, "mean")
    pm25_zonal = exact_extract(pm25_crop, da$geometry, "mean")

    da$sp_tw_kde = sp_tw_kde_zonal
    da$fem_sp_tw_kde = fem_sp_tw_kde_zonal

    da$sp_tw_kde[is.na(da$sp_tw_kde)] = 0
    da$fem_sp_tw_kde[is.na(da$fem_sp_tw_kde)] = 0

    da$total_sp_tw_kde = da$sp_tw_kde + da$fem_sp_tw_kde
    da$pm25_3yr_mean = pm25_zonal



    ###  Merge/Fusion of Datasets ###

    da = merge(da, can_cimd, by="DAUID", all.x=T)
    da = merge(da, prov_cimd[, c(1, 4:length(can_cimd))], by="DAUID", all.x=T)

    da$Region = ifelse(da$PRUID %in% c("10","11","12","13"), "Atlantic", ifelse(da$PRUID == "35", "Ontario", "Quebec"))


    ###  Extract Quintiles ###

    da$total_sp_tw_kde.ecdf = ecdf(da$total_sp_tw_kde)(da$total_sp_tw_kde)
    da$total_sp_tw_kde.Quintiles = sapply(da$total_sp_tw_kde.ecdf, quintiles)

    da = da %>% group_by(Region) %>% mutate(total_sp_tw_kde.group_ecdf = ecdf(total_sp_tw_kde)(total_sp_tw_kde))
    da$total_sp_tw_kde.group_Quintiles = sapply(da$total_sp_tw_kde.group_ecdf, quintiles)

    da$pm25_3yr_mean.ecdf = ecdf(da$pm25_3yr_mean)(da$pm25_3yr_mean)
    da$pm25_3yr_mean.Quintiles = sapply(da$pm25_3yr_mean.ecdf, quintiles)

    da = da %>% group_by(Region) %>% mutate(pm25_3yr_mean.group_ecdf = ecdf(pm25_3yr_mean)(pm25_3yr_mean))
    da$pm25_3yr_mean.group_Quintiles = sapply(da$pm25_3yr_mean.group_ecdf, quintiles)


    da$can_Deprivation.Quintiles = apply(cbind(da$can_Residential.instability.Quintiles, 
                                            da$can_Economic.dependency.Quintiles, 
                                            da$can_Situational.vulnerability.Quintiles, 
                                            da$`can_Ethno-cultural.composition.Quintiles`), 1, max, na.rm=T)

    da$prov_Deprivation.Quintiles = apply(cbind(da$prov_Residential.instability.Quintiles, 
                                                da$prov_Economic.dependency.Quintiles, 
                                                da$prov_Situational.vulnerability.Quintiles, 
                                                da$`prov_Ethno-cultural.composition.Quintiles`), 1, max, na.rm=T)

    da$can_Deprivation.Quintiles[da$can_Deprivation.Quintiles == -Inf] = NA
    da$prov_Deprivation.Quintiles[da$prov_Deprivation.Quintiles == -Inf] = NA


    da$can_Deprivation_top_quin_count = apply(cbind(da$can_Residential.instability.Quintiles, 
                                                    da$can_Economic.dependency.Quintiles, 
                                                    da$can_Situational.vulnerability.Quintiles, 
                                                    da$`can_Ethno-cultural.composition.Quintiles`), 1, function(x) length(x[x == 5]))

    da$prov_Deprivation_top_quin_count = apply(cbind(da$prov_Residential.instability.Quintiles, 
                                                    da$prov_Economic.dependency.Quintiles, 
                                                    da$prov_Situational.vulnerability.Quintiles, 
                                                    da$`prov_Ethno-cultural.composition.Quintiles`), 1, function(x) length(x[x == 5]))

    da$can_Deprivation_category = ifelse(da$can_Deprivation_top_quin_count > 1, "multiple", 
                                        ifelse(da$can_Deprivation_top_quin_count == 1, "single", "none"))
                                        
    da$prov_Deprivation_category = ifelse(da$prov_Deprivation_top_quin_count > 1, "multiple", 
                                        ifelse(da$prov_Deprivation_top_quin_count == 1, "single", "none"))

    da$coverage_status = ifelse(da$fem_sp_tw_kde > 0, "Covered by FEM", 
                            ifelse(da$fem_sp_tw_kde == 0 & da$total_sp_tw_kde > 0, "Enhanced by PurpleAir", "Not covered"))

    da$percent_enhanced_by_pa_sensor = da$total_sp_tw_kde / (da$fem_sp_tw_kde + 0.0001) * 100
    da$percent_enhanced_by_pa_sensor = ifelse(log(da$percent_enhance_pa_sensor) == -Inf, NA, log(da$percent_enhance_pa_sensor))

} else {

}
