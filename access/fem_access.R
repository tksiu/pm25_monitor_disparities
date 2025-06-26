require(dplyr)
require(stringr)
require(reshape2)
require(openxlsx)
require(data.table)
require(lubridate)


###  Calling FEM monitor data (NAPS + AriNow)  ###

# 1) AirNow - summarized total counts of observations from hourly PM2.5 data

fem = read.xlsx("./data/fem_monitor_counts_2018-2024.xlsx")
fem$SITE_NAME = sapply(fem$SITE_NAME, function(x) trimws(str_split(x, ">")[[1]][2]))
fem$siteID = sapply(fem$SITE_NAME, function(x) substr(str_split(x, ";")[[1]][1], 4, nchar(str_split(x, ";")[[1]][1])))
fem$siteID = sapply(fem$siteID, function(x) ifelse(grepl('[A-Z]', substr(x, 1, 4)), paste0("000", substr(x, 4, nchar(x))), x))
colnames(fem)[1:2] = c("Lon","Lat")

fem_recent = read.xlsx("./data/fem_monitor_counts_2023-2024.xlsx")
fem_recent$SITE_NAME = sapply(fem_recent$SITE_NAME, function(x) trimws(str_split(x, ">")[[1]][2]))
fem_recent$siteID = sapply(fem_recent$SITE_NAME, function(x) substr(str_split(x, ";")[[1]][1], 4, nchar(str_split(x, ";")[[1]][1])))
fem_recent$siteID = sapply(fem_recent$siteID, function(x) ifelse(grepl('[A-Z]', substr(x, 1, 4)), paste0("000", substr(x, 4, nchar(x))), x))
colnames(fem_recent)[1:2] = c("Lon","Lat")
colnames(fem_recent)[4] = "count_recent"

##  calculate total counts of observations per station (site)  -->  active time proportion from 2018 to 2024
fem_recent = fem_recent %>% group_by(siteID) %>% summarise(count_recent = sum(count_recent))


# 2) Canadian NAPS - https://data-donnees.az.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/
                    
naps = read.xlsx("./data/2018-2022_hourly_PM2.5.xlsx")
## mask missing values
naps[,9:32][naps[,9:32] == -999] = NA
## invalid negative values
naps[,9:32][naps[,9:32] < 0] = NA
## calculate total counts of observations  -->  active time proportion; and first date of operation within the study period
naps$sum_hours = apply(naps[,9:32], 1, function(x) sum(!is.na(x)))
naps = naps %>% group_by(NAPS.ID, City, Province, Latitude, Longitude) %>% summarise(minDate = min(Date), naps_sum_hours = sum(sum_hours))

naps$Date = as.Date(naps$Date, origin="1899-12-30")
naps$siteID = sapply(naps$NAPS.ID, function(x) sprintf("%09d", x))


"""
Check PurpleAir sensor colocation with FEM stations;
PurpleAir sensor data access referred to:  ./access/purpleair_access.R  in this repository
"""
                     
# source("./access/purpleair_access.R")

pa_coord = pa[, c("site_id","lat","lng","prov_terr","nearest_fem","fem_dist_km","date_created","last_seen")]
pa_coord$create_year = year(pa_coord$date_created)
## define less than 1 km as colocation of PurpleAir sensors with FEM monitors
pa_coord$colocation = ifelse(pa_coord$fem_dist_km < 1, 1, 0)
## nearest FEM monitor site
pa_coord$nearest_fem = sapply(pa_coord$nearest_fem, function(x) sprintf("%09d", x))


fem_coord = merge(fem, pa_coord %>% group_by(nearest_fem) %>% summarise(colocation = max(colocation)), by.x="siteID", by.y="nearest_fem", all.x=T)

## fill missing colocation values
fem_coord$colocation[is.na(fem_coord$colocation)] = 0
## remove duplicated monitor information
fem_coord = fem_coord %>% group_by(siteID) %>% summarise(Lon = first(Lon), Lat = first(Lat), count = sum(count), colocation = max(colocation))

fem_coord = merge(fem_coord, naps[,c("siteID","minDate","naps_sum_hours")], by="siteID", all.x=T)
fem_coord = merge(fem_coord, fem_recent[,c("siteID","count_recent")], by="siteID", all.x=T)

