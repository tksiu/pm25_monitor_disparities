require(lme4)
require(spdep)


"""  objects inherited from previous scripts """

# source("../multilevel/census_zonal_stats.R")


#  outcome variable at DA level
da$adj_total_sp_tw_kde.Quintiles_binary = ifelse(da$adj_total_sp_tw_kde.Quintiles == 1, 1, 0)
#  outcome variable at CT level
ct$adj_total_sp_tw_kde.Quintiles_binary = ifelse(ct$adj_total_sp_tw_kde.Quintiles == 1, 1, 0)


##  mixed-effects model without spatial terms

cimd_logit = lme4::glmer(adj_total_sp_tw_kde.Quintiles_binary ~ 
                           pm25_3yr_mean + 
                           sqrt(sqrt(sqrt(npri_pm25_density))) +
                           can_Residential.instability.Scores + 
                           can_Economic.dependency.Scores + 
                           can_Ethnocultural.composition.Scores + 
                           can_Situational.vulnerability.Scores + 
                           (1 + 
                              pm25_3yr_mean + 
                              sqrt(sqrt(sqrt(npri_pm25_density))) +
                              can_Residential.instability.Scores + 
                              can_Economic.dependency.Scores + 
                              can_Ethnocultural.composition.Scores + 
                              can_Situational.vulnerability.Scores | Province
                           ),
                        data = subset(da, !is.na(adj_total_sp_tw_kde.Quintiles)),
                        family = binomial(),
                        control = lme4::glmerControl(optCtrl=list(maxfun=1e6), calc.derivs = FALSE), nAGQ=0)

pwd_logit = lme4::glmer(adj_total_sp_tw_kde.Quintiles_binary ~ 
                           pm25_3yr_mean + 
                           sqrt(sqrt(sqrt(npri_pm25_density))) +
                           phy_perc +
                           psy_perc +
                           (1 + 
                              pm25_3yr_mean + 
                              sqrt(sqrt(sqrt(npri_pm25_density))) +
                              phy_perc +
                              psy_perc | PRUID / CMANAME
                           ),
                        data = subset(ct, !is.na(adj_total_sp_tw_kde.Quintiles)),
                        family = binomial(),
                        control = lme4::glmerControl(optCtrl=list(maxfun=1e6), calc.derivs = FALSE), nAGQ=0)


pwd_logit_a = lme4::glmer(adj_total_sp_tw_kde.Quintiles_binary ~ 
                            pm25_3yr_mean + 
                            sqrt(sqrt(sqrt(npri_pm25_density))) +
                            chd_phy_perc +
                            chd_psy_perc +
                            (1 + 
                               pm25_3yr_mean + 
                               sqrt(sqrt(sqrt(npri_pm25_density))) +
                               chd_phy_perc +
                               chd_psy_perc | PRUID / CMANAME
                            ),
                          data = subset(ct, !is.na(adj_total_sp_tw_kde.Quintiles)),
                          family = binomial(),
                          control = lme4::glmerControl(optCtrl=list(maxfun=1e6), calc.derivs = FALSE), nAGQ=0)

pwd_logit_b = lme4::glmer(adj_total_sp_tw_kde.Quintiles_binary ~ 
                             pm25_3yr_mean + 
                             sqrt(sqrt(sqrt(npri_pm25_density))) +
                             eld_phy_perc +
                             eld_psy_perc +
                             (1 + 
                                pm25_3yr_mean + 
                                sqrt(sqrt(sqrt(npri_pm25_density))) +
                                eld_phy_perc +
                                eld_psy_perc | PRUID / CMANAME
                             ),
                           data = subset(ct, !is.na(adj_total_sp_tw_kde.Quintiles)),
                           family = binomial(),
                           control = lme4::glmerControl(optCtrl=list(maxfun=1e6), calc.derivs = FALSE), nAGQ=0)



#  create spatial lag term at DA level to account for the spatial autocorrelation in modelling the disparities
#  weighted by Queen Contiguity

geom_da = st_make_valid(subset(da, rownames(da) %in% rownames(cimd_logit@frame))$geometry)

##  1) Queen contiguity
queen_da = poly2nb(geom_da, queen = TRUE)
queen_da_w = nb2listw(queen_da, style = "W", zero.policy = TRUE)

##  2) Buffer distance (5 km)
distnb_da = dnearneigh(st_coordinates(st_centroid(geom_da)), d1 = 0, d2 = 0.05)
distnb_da_w = nb2listwdist(distnb_da, as(st_centroid(geom_da), "Spatial"), type = "idw", style = "B", zero.policy = TRUE)

##  3) k-NN neighbours (k = 5)
knn_da = knearneigh(st_coordinates(st_centroid(geom_da)), k = 5)
knn_da = knn2nb(knn_da)
knn_da_w = nb2listw(knn_da, style = "W", zero.policy = TRUE)


names(da) <- gsub("-", "", names(da))

##  generate spatially lagged variables with Queen contiguity weighting matrix

da_processed = da %>% ungroup()
for (f in c("adj_total_sp_tw_kde.Quintiles_binary",
            "pm25_3yr_mean", 
            "npri_pm25_density",
            "can_Residential.instability.Scores",
            "can_Economic.dependency.Scores",
            "can_Ethnocultural.composition.Scores",
            "can_Situational.vulnerability.Scores")) {

  da_processed[paste0("splag.", f)] <- lag.listw(queen_da_w, da_processed[[f]], zero.policy = TRUE, NAOK=TRUE)
}



#  create spatial lag term at CT level to account for the spatial autocorrelation in modelling the disparities
#  weighted by Queen Contiguity

geom_ct = st_make_valid(subset(ct, rownames(ct) %in% rownames(pwd_logit@frame))$geometry)
geom_ct_a = st_make_valid(subset(ct, rownames(ct) %in% rownames(pwd_logit_a@frame))$geometry)
geom_ct_b = st_make_valid(subset(ct, rownames(ct) %in% rownames(pwd_logit_b@frame))$geometry)

##  1) Queen's contiguity
queen_ct = poly2nb(geom_ct, queen = TRUE)
queen_ct_w = nb2listw(queen_ct, style = "W", zero.policy = TRUE)

queen_ct_a = poly2nb(geom_ct_a, queen = TRUE)
queen_ct_a_w = nb2listw(queen_ct_a, style = "W", zero.policy = TRUE)

queen_ct_b = poly2nb(geom_ct_b, queen = TRUE)
queen_ct_b_w = nb2listw(queen_ct_b, style = "W", zero.policy = TRUE)

##  2) Buffer distance (5 km)
distnb_ct = dnearneigh(st_coordinates(st_centroid(geom_ct)), d1 = 0, d2 = 0.05)
distnb_ct_w = nb2listwdist(distnb_ct, as(st_centroid(geom_ct), "Spatial"), type = "idw", style = "B", zero.policy = TRUE)

distnb_ct_a = dnearneigh(st_coordinates(st_centroid(geom_ct_a)), d1 = 0, d2 = 0.05)
distnb_ct_a_w = nb2listwdist(distnb_ct_a, as(st_centroid(geom_ct_a), "Spatial"), type = "idw", style = "B", zero.policy = TRUE)

distnb_ct_b = dnearneigh(st_coordinates(st_centroid(geom_ct_b)), d1 = 0, d2 = 0.05)
distnb_ct_b_w = nb2listwdist(distnb_ct_b, as(st_centroid(geom_ct_b), "Spatial"), type = "idw", style = "B", zero.policy = TRUE)

##  3) k-NN neighbours (k = 5, setting a max. buffer = 25 km, because CMAs are disconnected, tracts at the edges of each CMA should be separated from other CMA if neighbours < 5)
filter_nb = function(nb, dist, cutoff) {
  valid_nb = nb[dist < cutoff]
  if (length(valid_nb) == 0) {
    return(as.integer(0))
  } else {
    return(as.integer(valid_nb))
  }
}

knn_ct = knearneigh(st_coordinates(st_centroid(geom_ct)), k = 5)
knn_ct = knn2nb(knn_ct)
knn_ct_dist = nbdists(knn_ct, st_coordinates(st_centroid(geom_ct)))
knn_ct_adj = mapply(filter_nb, knn_ct, knn_ct_dist, 0.05)
class(knn_ct_adj) <- "nb"
knn_ct_w = nb2listw(knn_ct_adj, style = "W", zero.policy = TRUE)

knn_ct_a = knearneigh(st_coordinates(st_centroid(geom_ct_a)), k = 5)
knn_ct_a = knn2nb(knn_ct_a)
knn_ct_a_dist = nbdists(knn_ct_a, st_coordinates(st_centroid(geom_ct_a)))
knn_ct_a_adj = mapply(filter_nb, knn_ct_a, knn_ct_a_dist, 0.05)
class(knn_ct_a_adj) <- "nb"
knn_ct_a_w = nb2listw(knn_ct_a_adj, style = "W", zero.policy = TRUE)

knn_ct_b = knearneigh(st_coordinates(st_centroid(geom_ct_b)), k = 5)
knn_ct_b = knn2nb(knn_ct_b)
knn_ct_b_dist = nbdists(knn_ct_b, st_coordinates(st_centroid(geom_ct_b)))
knn_ct_b_adj = mapply(filter_nb, knn_ct_b, knn_ct_b_dist, 0.05)
class(knn_ct_b_adj) <- "nb"
knn_ct_b_w = nb2listw(knn_ct_b_adj, style = "W", zero.policy = TRUE)


##  generate spatially lagged variables with Queen contiguity weighting matrix

ct_processed_pwd = ct %>% ungroup()
for (f in c("adj_total_sp_tw_kde.Quintiles_binary",
            "pm25_3yr_mean", 
            "npri_pm25_density",
            "psy_perc","phy_perc")) {
  
  ct_processed_pwd[paste0("splag.", f)] <- lag.listw(queen_ct_w, ct_processed_pwd[[f]], zero.policy = TRUE, NAOK=TRUE)
}
ct_processed_pwd$splag.adj_total_sp_tw_kde.Quintiles_binary = ct_processed_pwd$splag.adj_total_sp_tw_kde.Quintiles_binary * 100

ct_processed_pwd_a = ct %>% ungroup()
for (f in c("adj_total_sp_tw_kde.Quintiles_binary",
            "pm25_3yr_mean", 
            "npri_pm25_density",
            "chd_psy_perc","chd_phy_perc")) {
  
  ct_processed_pwd_a[paste0("splag.", f)] <- lag.listw(queen_ct_a_w, ct_processed_pwd_a[[f]], zero.policy = TRUE, NAOK=TRUE)
}
ct_processed_pwd_a$splag.adj_total_sp_tw_kde.Quintiles_binary = ct_processed_pwd_a$splag.adj_total_sp_tw_kde.Quintiles_binary * 100

ct_processed_pwd_b = ct %>% ungroup()
for (f in c("adj_total_sp_tw_kde.Quintiles_binary",
            "pm25_3yr_mean", 
            "npri_pm25_density",
            "eld_psy_perc","eld_phy_perc")) {
  
  ct_processed_pwd_b[paste0("splag.", f)] <- lag.listw(queen_ct_b_w, ct_processed_pwd_b[[f]], zero.policy = TRUE, NAOK=TRUE)
}
ct_processed_pwd_b$splag.adj_total_sp_tw_kde.Quintiles_binary = ct_processed_pwd_b$splag.adj_total_sp_tw_kde.Quintiles_binary * 100


###  spatial mixed-effects model (spMM)

cimd_logit = lme4::glmer(adj_total_sp_tw_kde.Quintiles_binary ~ 
                        pm25_3yr_mean + 
                        sqrt(sqrt(sqrt(npri_pm25_density))) +
                        can_Residential.instability.Scores + 
                        can_Economic.dependency.Scores + 
                        can_Ethnocultural.composition.Scores + 
                        can_Situational.vulnerability.Scores + 
                        (1 + 
                           pm25_3yr_mean + 
                           sqrt(sqrt(sqrt(npri_pm25_density))) +
                           can_Residential.instability.Scores + 
                           can_Economic.dependency.Scores + 
                           can_Ethnocultural.composition.Scores + 
                           can_Situational.vulnerability.Scores | Province
                        ) + 
                        splag.adj_total_sp_tw_kde.Quintiles_binary,
                        data = da_processed,
                        family = binomial(),
                        control = lme4::glmerControl(optCtrl=list(maxfun=1e6), calc.derivs = FALSE), nAGQ=0)

pwd_logit = lme4::glmer(adj_total_sp_tw_kde.Quintiles_binary ~ 
                        pm25_3yr_mean + 
                        sqrt(sqrt(sqrt(npri_pm25_density))) +
                        phy_perc +
                        psy_perc +
                        (1 + 
                           pm25_3yr_mean + 
                           sqrt(sqrt(sqrt(npri_pm25_density))) +
                           phy_perc +
                           psy_perc | PRUID / CMANAME
                        ) +
                        splag.adj_total_sp_tw_kde.Quintiles_binary,
                        data = ct_processed_pwd,
                        family = binomial(),
                        control = lme4::glmerControl(optCtrl=list(maxfun=1e6), calc.derivs = FALSE), nAGQ=0)

pwd_logit_a = lme4::glmer(adj_total_sp_tw_kde.Quintiles_binary ~ 
                           pm25_3yr_mean + 
                           sqrt(sqrt(sqrt(npri_pm25_density))) +
                           chd_phy_perc +
                           chd_psy_perc +
                           (1 + 
                              pm25_3yr_mean + 
                              sqrt(sqrt(sqrt(npri_pm25_density))) +
                              chd_phy_perc +
                              chd_psy_perc | PRUID / CMANAME
                           ) +
                           splag.adj_total_sp_tw_kde.Quintiles_binary,
                           data = ct_processed_pwd_a,
                           family = binomial(),
                           control = lme4::glmerControl(optCtrl=list(maxfun=1e6), calc.derivs = FALSE), nAGQ=0)

pwd_logit_b = lme4::glmer(adj_total_sp_tw_kde.Quintiles_binary ~ 
                           pm25_3yr_mean + 
                           sqrt(sqrt(sqrt(npri_pm25_density))) +
                           eld_phy_perc +
                           eld_psy_perc +
                           (1 + 
                              pm25_3yr_mean + 
                              sqrt(sqrt(sqrt(npri_pm25_density))) +
                              eld_phy_perc +
                              eld_psy_perc | PRUID / CMANAME
                           ) +
                           splag.adj_total_sp_tw_kde.Quintiles_binary,
                           data = ct_processed_pwd_b,
                           family = binomial(),
                           control = lme4::glmerControl(optCtrl=list(maxfun=1e6), calc.derivs = FALSE), nAGQ=0)

