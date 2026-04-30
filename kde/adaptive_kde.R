require(dplyr)
require(sp)
require(sf)
require(spatstat)


""" precomputed weightings and bandwidths (incorporated in /kde/weights to avoid conflicts in workflow) """
# source("./kde/weights.R")


####  Create an output grid object  ####

minx = -95.0
maxx = -52.0
miny = 41.5
maxy = 63.0

interp_x = seq(from=minx, to=maxx, by=0.01)
interp_y = seq(from=miny, to=maxy, by=0.01)
interp_grid = expand.grid(interp_x, interp_y)
colnames(interp_grid) = c("x","y")



if (sys.nframe() == 0) {

    #  PurpleAir sensors: computed/supervised bandwidth selection, weighted by operating time (% being active)

    pa_sp_den_tw = densityAdaptiveKernel(
        X = pa_points, 
        bw = pa$bandwidth_shortest * 0.01, 
        dimyx=c(length(interp_y), length(interp_x)), 
        weights = pa$accessible_time + 1e-5
    )

    #  PurpleAir sensors: automatic bandwidth selection, weighted by operating time (% being active)

    pa_sp_den_tw_auto = densityAdaptiveKernel(
        X = pa_points, 
        dimyx=c(length(interp_y), 
        length(interp_x)), 
        weights = pa$accessible_time + 1e-5
    )

    #  FEM monitors: computed/supervised bandwidth selection, weighted by operating time (% being active)

    fem_sp_den_tw = densityAdaptiveKernel(
        X = fem_points, 
        bw = fem_coord$bandwidth_shortest * 0.01, 
        dimyx=c(length(interp_y), length(interp_x)), 
        weights = fem_coord$accessible_time + 0.0001
    )

    #  FEM monitors: automatic bandwidth selection, weighted by operating time (% being active)

    fem_sp_den_tw_auto = densityAdaptiveKernel(
        X = fem_points, 
        dimyx=c(length(interp_y), length(interp_x)), 
        weights = fem_coord$accessible_time + 0.0001
    )

    #  National Pollutant Release Inventory (NPRI) PM2.5 emission facility: bandwidth selection by MSE cross-validation, weighted by emission inventory

    npri = read_sf("./data/pm25_npri_2021-2023.shp")
    colnames(npri) = c(
        "NPRI ID","Facility name","Company name","Province","City","PostalCode","Longitude","Latitude",
        "Emissions_2020","Emission_2021","Emission_2022","Emission_2023","Emission_3yr_mean","geometry"
    ) 

    npri_ppp = ppp(
        x = npri$Longitude, y = npri$Latitude, 
        window=owin(xrange=c(-95.0, -52.0), yrange=c(41.5, 63.0))
    )

    npri_den = densityAdaptiveKernel(
        X = npri_ppp, bw = bw.diggle(npri_ppp), 
        dimyx=c(length(interp_y), length(interp_x)), 
        weights = subset(npri, Longitude >= -95.0 & Longitude <= -52.0 & Latitude >= 41.5 & Latitude <= 63.0)$Emission_3yr_mean
    )

} else {

}
