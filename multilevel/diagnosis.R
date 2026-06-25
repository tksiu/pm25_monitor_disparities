require(parameters)

"""  objects inherited from previous scripts """

# source("../multilevel/spMM.R")


model_stats = function(model) {
    ##  overall summary
    summary(model)
    ##  subject-specific / group-specific effects, sum of fixed effect and random effect
    coef(model)
    ##  purely random effect
    ranef(model)
    ##  confidence intervals (fixed effects)
    parameters::ci(model)
    ## standard errors (random effects)
    parameters::standard_error(model, effects = "random")
}


model_spatial_residual_test = function(model, queen_weights) {
    ##  apply Moran's I test to residuals
    moran.test(residuals(model, type="pearson"), queen_weights, zero.policy = TRUE)
    # moran.test(residuals(model, type="deviance"), queen_weights, zero.policy = TRUE)
}


model_selection = function(models) {
    ##  apply AIC to examine complexity of parameterization and compare between models
    ##  "models":  a list object
    AIC(models)
}


boxcox_transform_lambda_cv = function(y) {
    val = 1/(y + min(y[y>0], na.rm=T) / 2)
    boxcox_splag_y = boxcox(val ~ 1, lambda = seq(-8, 8, 0.5), objective.name="Log-Likelihood")
    boxcox_opt_lambda_cv_splag_y = boxcox_splag_y$x[which.max(boxcox_splag_y$y)]                    ##  check optimal lambda, y = outcome variable
}


predicted_probabilities_cimd = function(model) {

    pred_RI = predict_response(model, terms = "can_Residential.instability.Scores [all]")
    pred_EC = predict_response(model, terms = "can_Economic.dependency.Scores [all]")
    pred_EN = predict_response(model, terms = "can_Ethnocultural.composition.Scores [all]")
    pred_SV = predict_response(model, terms = "can_Situational.vulnerability.Scores [all]")

    pred_RI_grp = predict_response(model, terms = c("can_Residential.instability.Scores [all]", "Province"), type = "random")
    pred_EC_grp = predict_response(model, terms = c("can_Economic.dependency.Scores [all]", "Province"), type = "random")
    pred_EN_grp = predict_response(model, terms = c("can_Ethnocultural.composition.Scores [all]", "Province"), type = "random")
    pred_SV_grp = predict_response(cmodel, terms = c("can_Situational.vulnerability.Scores [all]", "Province"), type = "random")

    plot(pred_RI_grp, facet=TRUE, colors=c("#FF4136", "#0074D9", "#009E73", "#B10DC9", "#FF851B", "#85144b")) +
    labs(title="Predicted probabilities of Bottom-Quintile Monitor Density",
        y = "Bottom-Quintile Monitor Density (prob)",
        x = "Residential instability (scores)")

    plot(pred_EC_grp, facet=TRUE, colors=c("#FF4136", "#0074D9", "#009E73", "#B10DC9", "#FF851B", "#85144b")) +
    labs(title="Predicted probabilities of Bottom-Quintile Monitor Density",
        y = "Bottom-Quintile Monitor Density (prob)",
        x = "Economic dependency (scores)")

    plot(pred_EN_grp, facet=TRUE, colors=c("#FF4136", "#0074D9", "#009E73", "#B10DC9", "#FF851B", "#85144b")) +
    labs(title="Predicted probabilities of Bottom-Quintile Monitor Density",
        y = "Bottom-Quintile Monitor Density (prob)",
        x = "Ethnocultural composition (scores)") +
    scale_x_continuous(limits = c(-3.099, 9.301)) +
    scale_y_continuous(limits = c(0, 0.25), labels = scales::label_percent())

    plot(pred_SV_grp, facet=TRUE, colors=c("#FF4136", "#0074D9", "#009E73", "#B10DC9", "#FF851B", "#85144b")) +
    labs(title="Predicted probabilities of Bottom-Quintile Monitor Density",
        y = "Bottom-Quintile Monitor Density (prob)",
        x = "Situational vulnerability (scores)")

}


predicted_probabilities_disability_rate = function(model) {
    
    pred_phys_disability = predict_response(model, terms = "phy_perc [0:25]")
    pred_psyc_disability = predict_response(model, terms = "psy_perc [0:35]")

    pred_phys_disability_grp = predict_response(model, terms = c("phy_perc [0:25]", "PRUID"), type = "random")
    pred_psyc_disability_grp = predict_response(model, terms = c("psy_perc [0:35]", "PRUID"), type = "random")

    plot(pred_phys_disability_grp, facet=TRUE, colors=c("#0074D9", "#009E73", "#FF4136", "#85144b", "#B10DC9")) +
    labs(title="Predicted probabilities of Bottom-Quintile Monitor Density",
        y = "Bottom-Quintile Monitor Density (prob)",
        x = "Population Proportion with Physical Disabilities (%)")

    plot(pred_psyc_disability_grp, facet=TRUE, colors=c("#0074D9", "#009E73", "#FF4136", "#85144b", "#B10DC9")) +
    labs(title="Predicted probabilities of Bottom-Quintile Monitor Density",
        y = "Bottom-Quintile Monitor Density (prob)",
        x = "Population Proportion with Psychological Disabilities (%)")
    
}

