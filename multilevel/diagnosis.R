require(parameters)

"""  objects inherited from previous scripts """
# source("/multilevel/spMM.R")


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
    moran.test(residuals(model, type="deviance"), queen_weights, zero.policy = TRUE)
}


model_selection = function(models) {
    ##  apply AIC to examine complexity of parameterization and compare between models
    AIC(models)
}
