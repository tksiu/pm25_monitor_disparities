require(dplyr)
require(reshape2)
require(openxlsx)
require(data.table)
require(lubridate)


""" objects inherited from data access scripts """
""" directly call /kde/bandwidths.R to ensure a smooth workflow (avoid conflicts in object/variable dependency between scripts """
# source("./access/fem_access.R")
# source("./access/purpleair_access.R")

# source("./kde/bandwidths.R")


"""
Time weights for the Adaptive KDE
"""

##  Total number of days in 2018-2024

date_start = "2018-01-01"
date_end = "2024-09-30"

period_diff <- as.integer(as.Date(date_end) - as.Date(date_start))


##  fraction of hours with valid measurements for FEM:

fem_coord$validated_count = apply(cbind(fem_coord$count, apply(fem_coord[,c("naps_sum_hours","count_recent")], 1, function(x) sum(x, na.rm=T))), 1, max)
fem_coord$accessible_time = fem_coord$validated_count / (period_diff * 24)


##  fraction of hours with valid measurements for PurpleAir:
###  "local" = using only the deployed time (Start Date and End Date of the sensor) as denominator
###  "global" = using the entire study period as denominator

activeness_global = c()
activeness_local = c()

folder_path = "./data/"
folder_files = list.files(folder_path)

for (i in 1:length(pa$site_id)) {

  #  monitor site ID
  site = pa$site_id[i]
  #  check the monitored data files (recent & historical spanning the targeted period: 2018-2024)
  recent_name = paste0(site, "_recent.csv")
  history_name = paste0(site, "_history.csv")
  
  recency = 0
  history = 0
  recent_first = NA
  
  if (recent_name %in% folder_files) {
    #  count number of hours with valid measurements in recent records
    recent_file = read.csv(paste0(folder_path, recent_name))
    recency = dim(subset(recent_file, !is.na(pm25_cal)))[1]
    recent_first = min(recent_file$date)
  }
  
  if (history_name %in% folder_files) {

    #  check historical records
    history_file = read.csv(paste0(folder_path, history_name))

    #  filter dates (excluded recent period to avoid duplicated counting
    if (!is.na(recent_first)) {
        history_file = subset(history_file, date < recent_first & date >= "2018-01-01")
    }

    #  count number of hours with valid measurements in history records
    history_file$date = strftime(strptime(history_file$date, format="%Y-%m-%dT%H:%M:%SZ"), format="%Y-%m-%d %H:00:00")
    history_file = history_file %>% group_by(date) %>% summarise(pm2.5_validated_corrected = mean(`pm2.5_validated_corrected`, na.rm=T))
    history = dim(subset(history_file, !is.na(`pm2.5_validated_corrected`)))[1]
  }

  #  obtain the fractions
  activeness_global[i] = (recency + history) / (period_diff * 24)
  activeness_local[i] = (recency + history) / (as.integer(pa$last_seen[i] - pa$date_created[i]) * 24)
  
}


pa$active_time = activeness_local
pa$accessible_time = activeness_global
