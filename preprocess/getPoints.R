require(sp)
require(sf)
require(spatstat)


minx = -95.0
maxx = -52.0
miny = 41.5
maxy = 63.0


####  Create a "ppp" point pattern object  ####

pa_points = ppp(x = pa$lng, y = pa$lat, window=owin(xrange=c(minx, maxx), yrange=c(miny, maxy)))
fem_points = ppp(x = fem_coord$Lon, y = fem_coord$Lat, window=owin(xrange=c(minx, maxx), yrange=c(miny, maxy)))


####  Create an output grid object  ####

interp_x = seq(from=minx, to=maxx, by=0.01)
interp_y = seq(from=miny, to=maxy, by=0.01)
interp_grid = expand.grid(interp_x, interp_y)
colnames(interp_grid) = c("x","y")

