source("./main.R")


###  Dissemination Areas as monitoring gap (zero density) and exceeding the CAAQS (2030) and WHO (2021) standards/guidelines

kde_0_exceed_caaqs = subset(da, total_sp_tw_kde == 0 & pm25_3yr_mean > 8)
kde_0_exceed_who = subset(da, total_sp_tw_kde == 0 & pm25_3yr_mean > 5)
kde_1_exceed_caaqs = subset(da, `total_sp_tw_kde.Quintiles` == 1 & pm25_3yr_mean > 8)
kde_1_exceed_who = subset(da, `total_sp_tw_kde.Quintiles` == 1 & pm25_3yr_mean > 5)

###  Dissemination Areas as inadequate monitoring (Q1 density), exceeding the CAAQS (2030) and WHO (2021) standards/guidelines, and with the most deprived CIMD quintile

adj_kde_can_cimd_exceed_caaqs = subset(da, adj_total_sp_tw_kde.Quintiles == 1 & pm25_3yr_mean > 8 & can_Deprivation.Quintiles == 5)
adj_kde_can_cimd_exceed_who = subset(da, adj_total_sp_tw_kde.Quintiles == 1 & pm25_3yr_mean > 5 & can_Deprivation.Quintiles == 5)
adj_kde_prov_cimd_exceed_caaqs = subset(da, adj_total_sp_tw_kde.group_Quintiles == 1 & pm25_3yr_mean > 8 & prov_Deprivation.Quintiles == 5)
adj_kde_prov_cimd_exceed_who = subset(da, adj_total_sp_tw_kde.group_Quintiles == 1 & pm25_3yr_mean > 5 & prov_Deprivation.Quintiles == 5)

###  Dissemination Areas as monitoring gap (zero density) and with the most concentrated NPRI emission facilities

kde_can_npri_gap = subset(da, total_sp_tw_kde == 0 & npri_pm25_kde.Quintiles == 5)
kde_prov_npri_gap = subset(da, total_sp_tw_kde == 0 & npri_pm25_kde.group_Quintiles == 5)

###  Census Tracts as inadequate monitoring (Q1 density), exceeding the CAAQS (2030) and WHO (2021) standards/guidelines, and with high disability populations

adj_phys_kde_pwd_exceed_caaqs_q1 = subset(ct, adj_total_sp_tw_kde.Quintiles == 1 & pm25_3yr_mean > 8 & phy_perc >= 20)
adj_phys_kde_pwd_exceed_who_q1 = subset(ct, adj_total_sp_tw_kde.Quintiles == 1 & pm25_3yr_mean > 5 & phy_perc >= 20)
adj_psyc_kde_pwd_exceed_caaqs_q1 = subset(ct, adj_total_sp_tw_kde.Quintiles == 1 & pm25_3yr_mean > 8 & psy_perc >= 20)
adj_psyc_kde_pwd_exceed_who_q1 = subset(ct, adj_total_sp_tw_kde.Quintiles == 1 & pm25_3yr_mean > 5 & psy_perc >= 20)

adj_phys_kde_pwd_child_exceed_caaqs_q1 = subset(ct, adj_total_sp_tw_kde.Quintiles == 1 & pm25_3yr_mean > 8 & chd_phy_perc >= 5)
adj_phys_kde_pwd_child_exceed_who_q1 = subset(ct, adj_total_sp_tw_kde.Quintiles == 1 & pm25_3yr_mean > 5 & chd_phy_perc >= 5)
adj_psyc_kde_pwd_child_exceed_caaqs_q1 = subset(ct, adj_total_sp_tw_kde.Quintiles == 1 & pm25_3yr_mean > 8 & chd_psy_perc >= 10)
adj_psyc_kde_pwd_child_exceed_who_q1 = subset(ct, adj_total_sp_tw_kde.Quintiles == 1 & pm25_3yr_mean > 5 & chd_psy_perc >= 10)

adj_phys_kde_pwd_elderly_exceed_caaqs_q1 = subset(ct, adj_total_sp_tw_kde.Quintiles == 1 & pm25_3yr_mean > 8 & eld_phy_perc >= 40)
adj_phys_kde_pwd_elderly_exceed_who_q1 = subset(ct, adj_total_sp_tw_kde.Quintiles == 1 & pm25_3yr_mean > 5 & eld_phy_perc >= 40)
adj_psyc_kde_pwd_elderly_exceed_caaqs_q1 = subset(ct, adj_total_sp_tw_kde.Quintiles == 1 & pm25_3yr_mean > 8 & eld_psy_perc >= 20)
adj_psyc_kde_pwd_elderly_exceed_who_q1 = subset(ct, adj_total_sp_tw_kde.Quintiles == 1 & pm25_3yr_mean > 5 & eld_psy_perc >= 20)
