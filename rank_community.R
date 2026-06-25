require(dplyr)
require(openxlsx)


"""  objects inherited from previous scripts """

# source("../multilevel/census_zonal_stats.R")


"""
Download URLs of Canada Census 2021 boundary files are from here:
https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21
"""

###  Population Centre boundary  ###

pc = read_sf(dsn = "lpc_000b21a_e.shp", layer = "lpc_000b21a_e")
pc = subset(pc, PRUID %in% c("10","11","12","13","35","24"))
pc = st_transform(pc, crs = 4326)

map_ct_pc = read.xlsx("2021_98260004_DGUID_relationship.xlsx")
map_ct_pc = merge(
                unique(map_ct_pc[,c("CTDGUID_SRIDUGD","POPCTRDGUID_CTRPOPIDUGD")]), 
                pc[,c("DGUID","PCNAME")], 
                by.x="POPCTRDGUID_CTRPOPIDUGD", by.y="DGUID"
            )
map_ct_pc$geometry = NULL
map_ct_pc = unique(subset(map_ct_pc, CTDGUID_SRIDUGD %in% ct$DGUID))

###  CSD boundary  ###

csd = read_sf(dsn = "lcsd000b21a_e.shp", layer = "lcsd000b21a_e")
csd = subset(csd, PRUID %in% c("10","11","12","13","35","24"))
csd = st_transform(csd, crs = 4326)

map_da_csd = read.xlsx("2021_98260004_DGUID_relationship.xlsx")
map_da_csd = merge(
                map_da_csd[,c("DADGUID_ADIDUGD","CSDDGUID_SDRIDUGD")], 
                csd[,c("DGUID","CSDNAME")], 
                by.x="CSDDGUID_SDRIDUGD", by.y="DGUID"
            )
map_da_csd$geometry = NULL
map_da_csd = unique(subset(map_da_csd, DADGUID_ADIDUGD %in% da$DGUID))


###  1)  Ranking priority DA falling within the bottom-quintile monitor density and with exceedance or at the margin of exceedance per 2030 CAAQS

q1_da = da[, c(
  "DAUID","DGUID","LANDAREA","PRUID","Province","pm25_3yr_mean","DA_pop",
  "can_Residential.instability.Scores","can_Economic.dependency.Scores","can_Ethnocultural.composition.Scores","can_Situational.vulnerability.Scores",
  "prov_Residential.instability.Scores","prov_Economic.dependency.Scores","prov_Ethnocultural.composition.Scores","prov_Situational.vulnerability.Scores",
  "adj_total_sp_tw_kde.Quintiles_binary"
)]

q1_da = merge(q1_da, map_da_csd, by.x="DGUID", by.y="DADGUID_ADIDUGD")

q1_da$priority_high = ifelse(q1_da$adj_total_sp_tw_kde.Quintiles_binary == 1 & q1_da$pm25_3yr_mean > 8.8, 1, 0)
q1_da$priority_ctr = ifelse(q1_da$adj_total_sp_tw_kde.Quintiles_binary == 1 & q1_da$pm25_3yr_mean < 8.8 & q1_da$pm25_3yr_mean > 7.2, 1, 0)

q1_da = q1_da %>% dplyr::mutate(
  
  RI_high = ifelse(priority_high == 1, can_Residential.instability.Scores, NA),
  EC_high = ifelse(priority_high == 1, can_Economic.dependency.Scores, NA),
  EN_high = ifelse(priority_high == 1, can_Ethnocultural.composition.Scores, NA),
  SV_high = ifelse(priority_high == 1, can_Situational.vulnerability.Scores, NA),
  
  RI_ctr = ifelse(priority_ctr == 1, can_Residential.instability.Scores, NA),
  EC_ctr = ifelse(priority_ctr == 1, can_Economic.dependency.Scores, NA),
  EN_ctr = ifelse(priority_ctr == 1, can_Ethnocultural.composition.Scores, NA),
  SV_ctr = ifelse(priority_ctr == 1, can_Situational.vulnerability.Scores, NA),
)

q1_da_summary = q1_da %>% 
  dplyr::group_by(Province, CSDNAME) %>% 
  dplyr::summarise(n = n(), 
                   
                   n_priority_high = sum(priority_high), 
                   perc_priority_high = sum(priority_high) / n() * 100,
                   n_priority_ctr = sum(priority_ctr), 
                   perc_priority_ctr = sum(priority_ctr) / n() * 100,
                   
                   pop = sum(DA_pop), 
                   pop_high = sum(DA_pop[priority_high == 1]),
                   pop_high_perc = sum(DA_pop[priority_high == 1]) / sum(DA_pop) * 100,
                   pop_ctr = sum(DA_pop[priority_ctr == 1]),
                   pop_ctr_perc = sum(DA_pop[priority_ctr == 1]) / sum(DA_pop) * 100,
                   
                   area = sum(LANDAREA), 
                   area_high = sum(LANDAREA[priority_high == 1]),
                   area_high_perc = sum(LANDAREA[priority_high == 1]) / sum(LANDAREA) * 100,
                   area_ctr = sum(LANDAREA[priority_ctr == 1]),
                   area_ctr_perc = sum(LANDAREA[priority_ctr == 1]) / sum(LANDAREA) * 100,
                   
                   median_RI_high = median(RI_high, na.rm=T), 
                   median_EC_high = median(EC_high, na.rm=T), 
                   median_EN_high = median(EN_high, na.rm=T), 
                   median_SV_high = median(SV_high, na.rm=T), 
                   
                   median_RI_ctr = median(RI_ctr, na.rm=T), 
                   median_EC_ctr = median(EC_ctr, na.rm=T), 
                   median_EN_ctr = median(EN_ctr, na.rm=T), 
                   median_SV_ctr = median(SV_ctr, na.rm=T), 
)

q1_da_summary_high = subset(q1_da_summary, n_priority_high > 0) %>% dplyr::select(-contains("_ctr"))

q1_da_summary_high$rank_RI = rank(-q1_da_summary_high$median_RI_high, ties.method= "average")
q1_da_summary_high$rank_EC = rank(-q1_da_summary_high$median_EC_high, ties.method= "average")
q1_da_summary_high$rank_EN = rank(-q1_da_summary_high$median_EN_high, ties.method= "average")
q1_da_summary_high$rank_SV = rank(-q1_da_summary_high$median_SV_high, ties.method= "average")

q1_da_summary_high$median_rank_deprivation = apply(q1_da_summary_high[,c("rank_RI","rank_EC","rank_EN","rank_SV")], 1, median, na.rm = TRUE)
q1_da_summary_high$rank_deprivation = rank(q1_da_summary_high$median_rank_deprivation, ties.method= "average")

q1_da_summary_ctr = subset(q1_da_summary, n_priority_ctr > 0) %>% dplyr::select(-contains("_high"))

q1_da_summary_ctr$rank_RI = rank(-q1_da_summary_ctr$median_RI_ctr, ties.method= "average")
q1_da_summary_ctr$rank_EC = rank(-q1_da_summary_ctr$median_EC_ctr, ties.method= "average")
q1_da_summary_ctr$rank_EN = rank(-q1_da_summary_ctr$median_EN_ctr, ties.method= "average")
q1_da_summary_ctr$rank_SV = rank(-q1_da_summary_ctr$median_SV_ctr, ties.method= "average")

q1_da_summary_ctr$median_rank_deprivation = apply(q1_da_summary_ctr[,c("rank_RI","rank_EC","rank_EN","rank_SV")], 1, median, na.rm = TRUE)
q1_da_summary_ctr$rank_deprivation = rank(q1_da_summary_ctr$median_rank_deprivation, ties.method= "average")


write.xlsx(q1_da_summary_high, "../community/da_summary_high.xlsx")
write.xlsx(q1_da_summary_ctr, "../community/da_summary_ctr.xlsx")


###  2)  Ranking priority Census Tracts falling within the bottom-quintile monitor density and with exceedance or at the margin of exceedance per 2030 CAAQS

q1_ct = as.data.frame(ct[, c(
  "DGUID","CTUID","CTNAME","CMANAME","PRUID","LANDAREA","pm25_3yr_mean","CT_pop_density",
  "psy_perc","phy_perc","chd_psy_perc","chd_phy_perc","eld_psy_perc","eld_phy_perc",
  "adj_total_sp_tw_kde.Quintiles_binary"
)])
q1_ct$CT_pop = q1_ct$CT_pop_density * q1_ct$LANDAREA
q1_ct = merge(q1_ct, map_ct_pc, by.x="DGUID", by.y="CTDGUID_SRIDUGD", all.x=T)


# q1_ct$priority_high = ifelse(q1_ct$adj_total_sp_tw_kde.Quintiles_binary == 1 & q1_ct$pm25_3yr_mean > 8.8, 1, 0)
# q1_ct$priority_ctr = ifelse(q1_ct$adj_total_sp_tw_kde.Quintiles_binary == 1 & q1_ct$pm25_3yr_mean < 8.8 & q1_ct$pm25_3yr_mean > 7.2, 1, 0)
q1_ct$priority = ifelse(q1_ct$adj_total_sp_tw_kde.Quintiles_binary == 1 & q1_ct$pm25_3yr_mean > 7.2, 1, 0)

q1_ct = q1_ct %>% dplyr::mutate(
  # phy_perc_high = ifelse(priority_high == 1, phy_perc, NA),
  # psy_perc_high = ifelse(priority_high == 1, psy_perc, NA),
  # phy_perc_ctr = ifelse(priority_ctr == 1, phy_perc, NA),
  # psy_perc_ctr = ifelse(priority_ctr == 1, psy_perc, NA),
  phy_perc_priority = ifelse(priority == 1, phy_perc, NA),
  psy_perc_priority = ifelse(priority == 1, psy_perc, NA),
  chd_phy_perc_priority = ifelse(priority == 1, chd_phy_perc, NA),
  chd_psy_perc_priority = ifelse(priority == 1, chd_psy_perc, NA),
  eld_phy_perc_priority = ifelse(priority == 1, eld_phy_perc, NA),
  eld_psy_perc_priority = ifelse(priority == 1, eld_psy_perc, NA),
)

q1_ct_summary = q1_ct %>% 
  dplyr::group_by(PRUID, PCNAME) %>% 
  dplyr::summarise(n = n(), 
                   
                   # n_priority_high = sum(priority_high), 
                   # perc_priority_high = sum(priority_high) / n() * 100,
                   # n_priority_ctr = sum(priority_ctr), 
                   # perc_priority_ctr = sum(priority_ctr) / n() * 100,
                   n_priority = sum(priority), 
                   perc_priority = sum(priority) / n() * 100,
                   
                   pop = sum(CT_pop, na.rm=T), 
                   # pop_high = sum(CT_pop[priority_high == 1]),
                   # pop_high_perc = sum(CT_pop[priority_high == 1]) / sum(CT_pop) * 100,
                   # pop_ctr = sum(CT_pop[priority_ctr == 1]),
                   # pop_ctr_perc = sum(CT_pop[priority_ctr == 1]) / sum(CT_pop) * 100,
                   pop_priority = sum(CT_pop[priority == 1], na.rm=T),
                   pop_priority_perc = sum(CT_pop[priority == 1], na.rm=T) / sum(CT_pop, na.rm=T) * 100,
                   
                   # area = sum(LANDAREA), 
                   # area_high = sum(LANDAREA[priority_high == 1]),
                   # area_high_perc = sum(LANDAREA[priority_high == 1]) / sum(LANDAREA) * 100,
                   # area_ctr = sum(LANDAREA[priority_ctr == 1]),
                   # area_ctr_perc = sum(LANDAREA[priority_ctr == 1]) / sum(LANDAREA) * 100,
                   
                   # median_phy_high = median(phy_perc_high, na.rm=T), 
                   # median_psy_high = median(psy_perc_high, na.rm=T), 
                   
                   # median_phy_ctr = median(phy_perc_ctr, na.rm=T), 
                   # median_psy_ctr = median(psy_perc_ctr, na.rm=T), 
                   
                   median_phy_priority = median(phy_perc_priority, na.rm=T), 
                   median_psy_priority = median(psy_perc_priority, na.rm=T), 
  )

# q1_ct_summary_high = subset(q1_ct_summary, n_priority_high > 0) %>% dplyr::select(-contains("_ctr"))
# 
# q1_ct_summary_high$rank_phy = rank(-q1_ct_summary_high$median_phy_high, ties.method= "average")
# q1_ct_summary_high$rank_psy = rank(-q1_ct_summary_high$median_psy_high, ties.method= "average")
# 
# q1_ct_summary_high$median_rank = apply(q1_ct_summary_high[,c("rank_phy","rank_psy")], 1, median, na.rm = TRUE)
# q1_ct_summary_high$rank_prevalence = rank(q1_ct_summary_high$median_rank, ties.method= "average")

# q1_ct_summary_ctr = subset(q1_ct_summary, n_priority_ctr > 0) %>% dplyr::select(-contains("_high"))
# 
# q1_ct_summary_ctr$rank_phy = rank(-q1_ct_summary_ctr$median_phy_ctr, ties.method= "average")
# q1_ct_summary_ctr$rank_psy = rank(-q1_ct_summary_ctr$median_psy_ctr, ties.method= "average")
# 
# q1_ct_summary_ctr$median_rank = apply(q1_ct_summary_ctr[,c("rank_phy","rank_psy")], 1, median, na.rm = TRUE)
# q1_ct_summary_ctr$rank_prevalence = rank(q1_ct_summary_ctr$median_rank, ties.method= "average")

q1_ct_summary_all = subset(q1_ct_summary, n_priority > 0) %>% dplyr::select(-contains("_high"))

q1_ct_summary_all$rank_phy = rank(-q1_ct_summary_all$median_phy_priority, ties.method= "average")
q1_ct_summary_all$rank_psy = rank(-q1_ct_summary_all$median_psy_priority, ties.method= "average")

q1_ct_summary_all$median_rank = apply(q1_ct_summary_all[,c("rank_phy","rank_psy")], 1, median, na.rm = TRUE)
q1_ct_summary_all$rank_prevalence = rank(q1_ct_summary_all$median_rank, ties.method= "average")


# write.xlsx(q1_ct_summary_high, "../community/ct_summary_high.xlsx")
# write.xlsx(q1_ct_summary_ctr, "../community/ct_summary_ctr.xlsx")
write.xlsx(q1_ct_summary_all, "../community/ct_summary_all.xlsx")
