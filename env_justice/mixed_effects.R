require(lme4)


"""  objects inherited from previous scripts """
# source("/env_justice/census_zonal_stats.R")



da$total_sp_tw_kde.Quintiles_binary = ifelse(da$total_sp_tw_kde.Quintiles == 1, 1, 0)

logit = lme4::glmer(total_sp_tw_kde.Quintiles_binary ~ 
                      scale(pm25_3yr_mean) + 
                      scale(DA_pop_density) + 
                      can_Residential.instability.Scores + 
                      can_Economic.dependency.Scores + 
                      `can_Ethno-cultural.composition.Scores` + 
                      can_Situational.vulnerability.Scores + 
                      (1 + 
                         scale(pm25_3yr_mean) + 
                         scale(DA_pop_density) + 
                         can_Residential.instability.Scores + 
                         can_Economic.dependency.Scores + 
                         `can_Ethno-cultural.composition.Scores` + 
                         can_Situational.vulnerability.Scores || Province
                       ),
                    data = subset(da, !is.na(total_sp_tw_kde.Quintiles)),
                    family = binomial(),
                    control = lme4::glmerControl(optCtrl=list(maxfun=1e6), calc.derivs = FALSE), nAGQ=0)

summary(logit)
coef(logit)

