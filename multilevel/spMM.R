require(lme4)
require(spdep)


"""  objects inherited from previous scripts """

# source("../multilevel/census_zonal_stats.R")


#  outcome variable at DA level
da$adj_total_sp_tw_kde.Quintiles_binary = ifelse(da$adj_total_sp_tw_kde.Quintiles == 1, 1, 0)
#  outcome variable at CT level
ct$adj_total_sp_tw_kde.Quintiles_binary = ifelse(ct$adj_total_sp_tw_kde.Quintiles == 1, 1, 0)


#  create spatial lag term at DA level to account for the spatial autocorrelation in modelling the disparities
#  weighted by Queen Contiguity

geom_da = st_make_valid(subset(da, rownames(da) %in% rownames(cimd_logit_4@frame))$geometry)

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

da$Province <- numFactor(as.factor(da$Province))
da$ProvinceID <- as.numeric(da$Province)
da$centroid = st_centroid(st_make_valid(da$geometry))
da$x_coord = sapply(da$centroid, function(x) st_coordinates(x)[, "X"])
da$y_coord = sapply(da$centroid, function(x) st_coordinates(x)[, "Y"])
da$pos = numFactor(scale(da$x_coord), scale(da$y_coord))
da$pos_group = factor(rep(1, nrow(da)))


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

geom_ct = st_make_valid(subset(ct, rownames(ct) %in% rownames(pwd_logit_6@frame))$geometry)
queen_ct = poly2nb(geom_ct, queen = TRUE)
queen_ct_w = nb2listw(queen_ct, style = "W", zero.policy = TRUE)

ct$centroid = st_centroid(st_make_valid(ct$geometry))
ct$x_coord = sapply(ct$centroid, function(x) st_coordinates(x)[, "X"])
ct$y_coord = sapply(ct$centroid, function(x) st_coordinates(x)[, "Y"])
ct$pos = numFactor(scale(ct$x_coord), scale(ct$y_coord))
ct$pos_group = factor(rep(1, nrow(ct)))

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

