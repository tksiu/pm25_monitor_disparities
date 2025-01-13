require(spatstat)


if (sys.nframe() == 0) {

    #  PurpleAir sensors: computed/supervised bandwidth selection

    pa_sp_den_tw = densityAdaptiveKernel(
        X = pa_points, 
        bw = pa$bandwidth_shortest * 0.01, 
        dimyx=c(length(interp_y), length(interp_x)), 
        weights = pa$accessible_time + 1e-5
    )

    #  PurpleAir sensors: automatic bandwidth selection

    pa_sp_den_tw_auto = densityAdaptiveKernel(
        X = pa_points, 
        dimyx=c(length(interp_y), 
        length(interp_x)), 
        weights = pa$accessible_time + 1e-5
    )

    #  FEM monitors: computed/supervised bandwidth selection

    fem_sp_den_tw = densityAdaptiveKernel(
        X = fem_points, 
        bw = fem_coord$bandwidth_shortest * 0.01, 
        dimyx=c(length(interp_y), length(interp_x)), 
        weights = fem_coord$accessible_time + 0.0001
    )

    #  FEM monitors: automatic bandwidth selection

    fem_sp_den_tw_auto = densityAdaptiveKernel(
        X = fem_points, 
        dimyx=c(length(interp_y), length(interp_x)), 
        weights = fem_coord$accessible_time + 0.0001
    )

} else {

}
