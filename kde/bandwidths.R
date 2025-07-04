require(dplyr)
require(sp)
require(sf)
require(spatstat)


""" objects inherited from preprocess scripts """
# source("./preprocess/getPoints.R")
# source("./preprocess/rasterize_surfacePM25.R")



#   get pairwise distance between any pair of stations
pdist = pairdist(
    superimpose(pa_points, fem_points)
)
#   set the maximum bandwidth threshold allowed as the half distance from the nearest monitor (IQR rule of outliers)
maxdist = as.numeric(
  quantile(apply(pdist, 1, function(x) min(x[x > 0])) / 2, 0.75) * 111 + 
  1.5 * (quantile(apply(pdist, 1, function(x) min(x[x > 0])) / 2, 0.75) * 111 - quantile(apply(pdist, 1, function(x) min(x[x > 0])) / 2, 0.25) * 111)
)
#   in km (rounding up to integer)
ndist = ceiling(maxdist)
#   in numer of grids
ndist = ndist * 0.01


#   set the cut-off value of the percentage variation in PM2.5 concentration
pm25_percent_diff = 10


###  Calculating local bandwidths for FEM monitors  ###

bandwidth_selection = function(coord) {

    bandwidths = list()

    for (i in 1:dim(coord)[1]) {
    
        pt_lat = coord$Lat[i]
        pt_lon = coord$Lon[i]
    
        #  based on centroid of the grids
        midpoint_lat = 0.5 * (ceiling(pt_lat*100)/100 + floor(pt_lat*100)/100)
        midpoint_lon = 0.5 * (ceiling(pt_lon*100)/100 + floor(pt_lon*100)/100)
    
        #  buffering to ensure the same searching window size for each grid
        if (pt_lon > midpoint_lon) {
            adj_a = -0.005
            adj_b = 0
        } else if (pt_lon < midpoint_lon) {
            adj_a = 0
            adj_b = 0.005
        } else {
            adj_a = -0.005
            adj_b = 0.005
        }
        if (pt_lat > midpoint_lat) {
            adj_c = -0.005
            adj_d = 0
        } else if (pt_lat < midpoint_lat) {
            adj_c = 0
            adj_d = 0.005
        } else {
            adj_c = -0.005
            adj_d = 0.005
        }
    
        #  extract the searching window of the current grid
        extract_pm25 = ncvar_get(nc_data, "PM25")[
            which( nc_data$dim$longitude$vals >= pt_lon - ndist + adj_a & nc_data$dim$longitude$vals <= pt_lon + ndist + adj_b ), 
            which( nc_data$dim$latitude$vals >= pt_lat - ndist + adj_c & nc_data$dim$latitude$vals <= pt_lat + ndist + adj_d )
        ]
    
        #  get the central point (current grid)
        centroid_idx = (dim(extract_pm25)[1] + 1) / 2
    
        #  consider only surrounding grids within the searching window with PM2.5 conc. difference exceeding 10% from current grid
        gradient_pm25 = abs((extract_pm25 - extract_pm25[centroid_idx, centroid_idx]) / extract_pm25[centroid_idx, centroid_idx] * 100)
        mask_gradient_pm25 = which( gradient_pm25 > pm25_percent_diff, arr.ind = T)
        mask_gradient_pm25 = as.data.frame(mask_gradient_pm25)
        #  Euclidean distance between surrounding grids in the searching window and the current grid
        mask_gradient_pm25$euclidean_dist = mapply(function(a,b) {sqrt((a - centroid_idx)^2 + (b - centroid_idx)^2)}, 
                                                    mask_gradient_pm25$row, mask_gradient_pm25$col)
            
        #  select bandwidth to be the smallest distance between current grid and the retained grids (>10% PM2.5 conc. difference)
        #  select maximum bandwidth threshold if no surrounding grid in the searching window satisfied the criterion
        if (length(mask_gradient_pm25$euclidean_dist) > 0) {
            bandwidths[i] = min(mask_gradient_pm25$euclidean_dist, maxdist)
        } else {
            bandwidths[i] = maxdist
        }
    
    }

    return(bandwidths)
}




if (sys.nframe() == 0) {

    bandwidth_fem = bandwidth_selection(fem_coord)
    bandwidth_pa = bandwidth_selection(pa_coord)

    fem_coord$bandwidth_shortest = unlist(bandwidth_fem)
    pa_coord$bandwidth_shortest = unlist(bandwidth_pa)

} else {

}
