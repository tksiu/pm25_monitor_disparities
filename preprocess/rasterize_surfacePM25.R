require(sp)
require(sf)
require(ncdf4)
require(raster)
require(terra)
require(tidyterra)



minx = -95.0
maxx = -52.0
miny = 41.5
maxy = 63.0



'''Method 1'''

###  Calling PM2.5 data  ###

nc_data <- nc_open('./data/V6GL02.02.CNNPM25.NA.3year_mean_2020-2022.nc')
names(nc_data$var) = c("PM25", "crs")
nc_data$var$PM25$name = "PM25"

###  Subsetting spatial extent  ###

LonIdx <- which( nc_data$dim$lon$vals >= minx & nc_data$dim$lon$vals <= maxx )
LatIdx <- which( nc_data$dim$lat$vals >= miny & nc_data$dim$lat$vals <= maxy )
ex_pm25 <- ncvar_get(nc_data, "PM25")[LonIdx, LatIdx]


'''Method 2'''

pm25 = rast('./data/V6GL02.02.CNNPM25.NA.3year_mean_2020-2022.nc')
extent <- c(minx, maxx, miny, maxy) |> ext()
pm25_crop <- crop(pm25, extent)

terra::writeRaster(pm25_crop, filename="./data/NA_3year_mean_2020-2022.tif", overwrite=TRUE)

