require(exactextractr)


"""  objects inherited from previous scripts """

# source("../kde/adaptive_kde.R")
# source("../multilevel/census_CIMD.R")
# source("../multilevel/census_PwD.R")


"""
Download URLs of Canada Census 2021 boundary files are from here:
https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21
"""


###  Calling provincial boundary  ###

prov = read_sf(dsn = "./lpr_000b21a_e.shp", layer = "lpr_000b21a_e")
prov = subset(prov, PRUID %in% c("10","11","12","13","35","24"))
prov = st_transform(prov, 4326)
prov = st_make_valid(prov)


###  Calling DA boundary  ###

da = read_sf(dsn = "./lda_000b21a_e.shp", layer = "lda_000b21a_e")
da = subset(da, PRUID %in% c("10","11","12","13","35","24"))
da = st_transform(da, crs = 4326)


###  Calling CT boundary  ###
ct = read_sf(dsn = "./lct_000b21a_e.shp", layer = "lct_000b21a_e")
ct = subset(ct, PRUID %in% c("10","11","12","13","35","24"))
ct = st_transform(ct, crs = 4326)


###  Calling CMA boundary  ###
cma = read_sf(dsn = "./lcma000b21a_e.shp", layer = "lcma000b21a_e")
cma = subset(cma, PRUID %in% c("10","11","12","13","35","24"))
cma = st_transform(cma, crs = 4326)

map_ct_cma = read.xlsx("./2021_98260004_DGUID_relationship.xlsx")
map_ct_cma = unique(map_ct_cma[,c("CMADGUID_RMRIDUGD","CTDGUID_SRIDUGD")])
map_ct_cma = subset(map_ct_cma, !is.na(CMADGUID_RMRIDUGD) & !is.na(CTDGUID_SRIDUGD))


###  Quintile function  ###
quintiles = function(x) {
   q = ifelse(x <= 0.2, 1, ifelse(x <= 0.4, 2, ifelse(x <= 0.6, 3, ifelse(x <= 0.8, 4, 5))))
   return(q)
}


###  Extract Zonal Statistics (Dissemination Areas)  ###

if (sys.nframe() == 0) {

   sp_tw_kde_zonal = exact_extract(raster(pa_sp_den_tw), da$geometry, "mean")
   fem_sp_tw_kde_zonal = exact_extract(raster(fem_sp_den_tw), da$geometry, "mean")
   pm25_zonal = exact_extract(pm25_crop, da$geometry, "mean")
   npri_den_zonal = exact_extract(raster(npri_den), da$geometry, "mean")
   
   da$sp_tw_kde = sp_tw_kde_zonal
   da$fem_sp_tw_kde = fem_sp_tw_kde_zonal

   da$sp_tw_kde[is.na(da$sp_tw_kde)] = 0
   da$fem_sp_tw_kde[is.na(da$fem_sp_tw_kde)] = 0
   
   da$total_sp_tw_kde = da$sp_tw_kde + da$fem_sp_tw_kde

   da$pop_density = da$DA_pop / da$LANDAREA
   da$adj_total_sp_tw_kde = da$total_sp_tw_kde / da$pop_density

   da$pm25_3yr_mean = pm25_zonal
   da$npri_pm25_density = npri_den_zonal

   
   ###  Merge/Fusion of Datasets ###
   
   da = merge(da, can_cimd, by="DAUID", all.x=T)
   da = merge(da, prov_cimd[, c(1, 4:length(can_cimd))], by="DAUID", all.x=T)

   da$Region = ifelse(da$PRUID %in% c("10","11","12","13"), "Atlantic", ifelse(da$PRUID == "35", "Ontario", "Quebec"))

   
   ###  Extract Quintiles ###
   
   da$total_sp_tw_kde.ecdf = ecdf(da$total_sp_tw_kde)(da$total_sp_tw_kde)
   da$total_sp_tw_kde.Quintiles = sapply(da$total_sp_tw_kde.ecdf, quintiles)
   
   da = da %>% group_by(Region) %>% mutate(total_sp_tw_kde.group_ecdf = ecdf(total_sp_tw_kde)(total_sp_tw_kde))
   da$total_sp_tw_kde.group_Quintiles = sapply(da$total_sp_tw_kde.group_ecdf, quintiles)


   da$adj_total_sp_tw_kde.ecdf = ecdf(da$adj_total_sp_tw_kde)(da$adj_total_sp_tw_kde)
   da$adj_total_sp_tw_kde.Quintiles = sapply(da$adj_total_sp_tw_kde.ecdf, function(x) ifelse(x <= 0.2, 1, ifelse(x <= 0.4, 2, ifelse(x <= 0.6, 3, ifelse(x <= 0.8, 4, 5)))))
   
   da$pm25_3yr_mean.ecdf = ecdf(da$pm25_3yr_mean)(da$pm25_3yr_mean)
   da$pm25_3yr_mean.Quintiles = sapply(da$pm25_3yr_mean.ecdf, quintiles)
   
   da = da %>% group_by(Region) %>% mutate(pm25_3yr_mean.group_ecdf = ecdf(pm25_3yr_mean)(pm25_3yr_mean))
   da$pm25_3yr_mean.group_Quintiles = sapply(da$pm25_3yr_mean.group_ecdf, quintiles)

   da$npri_pm25_kde.ecdf = ecdf(da$npri_pm25_density)(da$npri_pm25_density)
   da$npri_pm25_kde.Quintiles = sapply(da$npri_pm25_kde.ecdf, function(x) ifelse(x <= 0.2, 1, ifelse(x <= 0.4, 2, ifelse(x <= 0.6, 3, ifelse(x <= 0.8, 4, 5)))))
   
   
   ###  Largest quintile among the four CIMD dimensions  ###
   
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
   
   
   ###  Any of the four CIMD dimensions falling in the top quintile  ###
   
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
   

} else {

}


###  Extract Zonal Statistics (Census Tracts)  ###

if (sys.nframe() == 0) {

   sp_tw_kde_zonal = exact_extract(raster(pa_sp_den_tw), ct$geometry, "mean")
   fem_sp_tw_kde_zonal = exact_extract(raster(fem_sp_den_tw), ct$geometry, "mean")
   pm25_zonal = exact_extract(pm25_crop, ct$geometry, "mean")
   npri_den_zonal = exact_extract(raster(npri_den), ct$geometry, "mean")
   
   ct$sp_tw_kde = sp_tw_kde_zonal
   ct$fem_sp_tw_kde = fem_sp_tw_kde_zonal

   ct$sp_tw_kde[is.na(ct$sp_tw_kde)] = 0
   ct$fem_sp_tw_kde[is.na(ct$fem_sp_tw_kde)] = 0
   
   ct$total_sp_tw_kde = ct$sp_tw_kde + ct$fem_sp_tw_kde

   ct$pop_density = ct$total_population / ct$LANDAREA
   ct$adj_total_sp_tw_kde = ct$total_sp_tw_kde / ct$pop_density

   ct$pm25_3yr_mean = pm25_zonal
   ct$npri_pm25_density = npri_den_zonal

   
   ###  Merge/Fusion of Datasets ###

   disability = read_sf("../data/tract_disability_prop.shp")
   disability$geometry = NULL
   disability = subset(disability, PRUID %in% c("35","10","12","13","24"))

   colnames(disability) = c(
   "CTUID","DGUID","CTNAME","LANDAREA","PRUID","C1_COUNT_T",
   "total_population",'total_psyc_disability_count', 'total_phys_disability_count',
   'child_population', 'child_psyc_disability_count', 'child_phys_disability_count', 
   'elderly_population', 'elderly_psyc_disability_count', 'elderly_phys_disability_count',
   "total_disability_count", 'child_disability_count', 'elderly_disability_count', 
   "tot_perc","psy_perc", "phy_perc", "chd_perc", "chd_psy_perc", "chd_phy_perc", "eld_perc", "eld_psy_perc", "eld_phy_perc"
   )

   # disability$tot_perc = disability$total_disability_count / disability$total_population * 100
   disability$psy_perc = disability$total_psyc_disability_count / disability$total_population * 100
   disability$phy_perc = disability$total_phys_disability_count / disability$total_population * 100

   # disability$chd_perc = disability$child_disability_count / disability$child_population * 100
   disability$chd_psy_perc = disability$child_psyc_disability_count / disability$child_population * 100
   disability$chd_phy_perc = disability$child_phys_disability_count / disability$child_population * 100

   # disability$eld_perc = disability$elderly_disability_count / disability$elderly_population * 100
   disability$eld_psy_perc = disability$elderly_psyc_disability_count / disability$elderly_population * 100
   disability$eld_phy_perc = disability$elderly_phys_disability_count / disability$elderly_population * 100

   disability = disability[, !(names(disability) %in% c("total_disability_count", 'child_disability_count', 'elderly_disability_count',"tot_perc", "chd_perc", "eld_perc"))]
   
   ct = merge(ct, disability, by=c("CTUID","DGUID","CTNAME","LANDAREA","PRUID"), all.x=T)
   ct = merge(
         merge(
            ct, map_ct_cma, 
            by.x=c("DGUID"), by.y=c("CTDGUID_SRIDUGD"), all.x=T
         ),
      cma,
      by.x=c("CMADGUID_RMRIDUGD"), by.y=c("DGUID"), all.x=T
   )

   ct$Region = ifelse(ct$PRUID %in% c("10","11","12","13"), "Atlantic", ifelse(ct$PRUID == "35", "Ontario", "Quebec"))

   
   ###  Extract Quintiles ###
   
   ct$total_sp_tw_kde.ecdf = ecdf(ct$total_sp_tw_kde)(ct$total_sp_tw_kde)
   ct$total_sp_tw_kde.Quintiles = sapply(ct$total_sp_tw_kde.ecdf, quintiles)
   
   ct = ct %>% group_by(Region) %>% mutate(total_sp_tw_kde.group_ecdf = ecdf(total_sp_tw_kde)(total_sp_tw_kde))
   ct$total_sp_tw_kde.group_Quintiles = sapply(ct$total_sp_tw_kde.group_ecdf, quintiles)


   ct$adj_total_sp_tw_kde.ecdf = ecdf(ct$adj_total_sp_tw_kde)(ct$adj_total_sp_tw_kde)
   ct$adj_total_sp_tw_kde.Quintiles = sapply(ct$adj_total_sp_tw_kde.ecdf, function(x) ifelse(x <= 0.2, 1, ifelse(x <= 0.4, 2, ifelse(x <= 0.6, 3, ifelse(x <= 0.8, 4, 5)))))
   
   ct$pm25_3yr_mean.ecdf = ecdf(ct$pm25_3yr_mean)(ct$pm25_3yr_mean)
   ct$pm25_3yr_mean.Quintiles = sapply(ct$pm25_3yr_mean.ecdf, quintiles)
   
   ct = ct %>% group_by(Region) %>% mutate(pm25_3yr_mean.group_ecdf = ecdf(pm25_3yr_mean)(pm25_3yr_mean))
   ct$pm25_3yr_mean.group_Quintiles = sapply(ct$pm25_3yr_mean.group_ecdf, quintiles)

   ct$npri_pm25_kde.ecdf = ecdf(ct$npri_pm25_density)(ct$npri_pm25_density)
   ct$npri_pm25_kde.Quintiles = sapply(ct$npri_pm25_kde.ecdf, function(x) ifelse(x <= 0.2, 1, ifelse(x <= 0.4, 2, ifelse(x <= 0.6, 3, ifelse(x <= 0.8, 4, 5)))))
   

} else {

}
