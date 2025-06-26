require(dplyr)
require(stringr)
require(reshape2)
require(openxlsx)
require(data.table)
require(lubridate)


###   a function to retrieve the metadata of all PurpleAir sensors deployed in history
get_metadata = function() {
  url = "https://aqmap.ca/aqmap/data/"
  data_file = "pa_aqmap_meta.csv"
  metadata = data.table::fread(paste0(url, data_file), data.table = FALSE)
  return(metadata)
}

###   a function to retrieve the metadata of all FEM monitors
get_airnow_metadata = function() {
  url = "https://aqmap.ca/aqmap/data/agency/"
  data_file = "airnow.csv"
  metadata = data.table::fread(paste0(url, data_file), data.table = FALSE)
  return(metadata)
}


###   a function to retrieve the recent data of specific FEM indices
get_FEM_recent_data <- function(sensor_id, start_Date, end_Date, save_mode = FALSE, save_path = "./") {
  ### Link to download
  url_base <- "https://aqmap.ca/aqmap/data/plotting/agency/"
  data_url <- paste0(url_base, sensor_id, "_recent_hourly.csv")
  
  tryCatch(
    {
      ### Load in the data file
      data_all <- data.table::fread(data_url, data.table = FALSE)
      ### Convert the date column to POSIXct format
      data_all$date <- as.POSIXct(data_all$date, format = "%Y-%m-%d %H:%M:%S")
      ### Filter data for the study period
      start_date <- as.POSIXct(start_Date)
      end_date <- as.POSIXct(end_Date)
      data_all <- data_all %>% 
        filter(date >= start_date, date <= end_date) %>%
        mutate(sensid = sensor_id)
      
      ### Save the PM2.5 data as CSV file
      if (save_mode == TRUE) {
        filename <- paste0(save_path, sensor_id, ".csv")    # Replace the pathway to save the CSV where you want 
        readr::write_csv(data_all, file = filename)
      } else {
        return(data_all)
      }
    }, error = function(e){ 
      # (Optional)
      return(NULL)
    }
  )
}


###   a function to retrieve the older archived data of specific FEM indices
get_FEM_older_data <- function(sensor_id, start_Date, end_Date, save_mode = FALSE, save_path = "./") {
  ### Link to download
  url_base <- "https://aqmap.ca/aqmap/data/agency/"
  data_url <- paste0(url_base, "Airnow_HourlyPM25_aqmap.csv")
  
  tryCatch(
    {
      ### Load in the data file
      data_all <- data.table::fread(data_url, data.table = FALSE)
      ### Convert the date column to POSIXct format
      data_all$date <- as.POSIXct(paste0(substr(data_all$date, 1, 4), "-", 
                                         substr(data_all$date, 5, 6), "-", 
                                         substr(data_all$date, 7, 8), " ", 
                                         substr(data_all$date, 9, 10), ":00:00"),
                                  format="%Y-%m-%d %H:%M:%S")
      start_date <- as.POSIXct(start_Date)
      end_date <- as.POSIXct(end_Date)
      ### add padding site id 
      site_id = paste0("0000", sensor_id)
      ### Filter data for the study period]
      data_all <- data_all %>% 
        filter(date >= start_date, date <= end_date, siteID %in% site_id)
      
      ### Save the PM2.5 data as CSV file
      if (save_mode == TRUE) {
        filename <- paste0(save_path, sensor_id, ".csv")    # Replace the pathway to save the CSV where you want 
        readr::write_csv(data_all, file = filename)
      } else {
        return(data_all)
      }
    }, error = function(e){ 
      # (Optional)
      return(NULL)
    }
  )
}


###   a function to retrieve the recent data of specific PA sensor indices
get_PA_sensor_recent_data <- function(sensor_id, start_Date, end_Date, save_mode = FALSE, save_path = "./") {
  ### Link to download
  url_base <- "https://aqmap.ca/aqmap/data/plotting/purpleair/"
  data_url <- paste0(url_base, "sensor_", sensor_id, "_recent_hourly.csv")
  
  tryCatch(
    {
      ### Load in the data file
      data_all <-  read.csv(data_url)
      ### Convert the date column to POSIXct format
      data_all$date <- as.POSIXct(data_all$date, format = "%Y-%m-%d %H:%M:%S")
      ### Filter data for the study period
      start_date <- as.POSIXct(start_Date)
      end_date <- as.POSIXct(end_Date)
      data_all <- data_all %>% 
        filter(date >= start_date, date <= end_date) %>%
        mutate(sensid = sensor_id)
      
      ### Save the PM2.5 data as CSV file
      if (save_mode == TRUE) {
        filename <- paste0(save_path, sensor_id, "_recent.csv")     # Replace the pathway to save the CSV where you want 
        readr::write_csv(data_all, file = filename)
      } else {
        return(data_all)
      }
    }, error = function(e){ 
      # (Optional)
      return(NULL)
    }
  )
  
  Sys.sleep(1)
}


###   a function to retrieve the older archived data of specific PurpleAir sensor indices
get_PA_sensor_older_data <- function(sensor_id, start_Date, end_Date, save_mode = FALSE, save_path = "./") {
  ### Link to download
  url_base <- "https://aqmap.ca/aqmap/data/purpleair/"
  data_url <- paste0(url_base, "sensor_", sensor_id, ".csv")
  
  tryCatch(
    {
      ### Load in the data file
      data_all <- read.csv(data_url)
      ### Convert the date column to POSIXct format
      data_all$date <- as.POSIXct(data_all$date, format = "%Y-%m-%d %H:%M:%S")
      ### Filter data for the study period
      start_date <- as.POSIXct(start_Date)
      end_date <- as.POSIXct(end_Date)
      data_all <- data_all %>% 
        filter(date >= start_date, date <= end_date) %>%
        mutate(sensid = sensor_id)
      
      ### Save the PM2.5 data as CSV file
      if (save_mode == TRUE) {
        filename <- paste0(save_path, sensor_id, "_history.csv")    # Replace the pathway to save the CSV where you want 
        readr::write_csv(data_all, file = filename)
      } else {
        return(data_all)
      }
    }, error = function(e){ 
      # (Optional)
      return(NULL)
    }
  )
  
  Sys.sleep(1)
}





if (sys.nframe() == 0) {
    
    ###  Define parameters
    
    date_start = "2018-01-01"
    date_end = "2024-09-30"
    
    ca_province = c(
        "Nova Scotia", 
        "New Brunswick", 
        "Newfoundland and Labrador", 
        "Prince Edward Island", 
        "Ontario", 
        "Quebec"
        )
  
    save_folder = "./"

    ###  Downloading PurpleAir meta-data  ###

    pa_meta = get_metadata()
    pa = subset(pa_meta, prov_terr %in% ca_province & last_seen >= date_start & date_created <= date_end)

    ###  Downloading PurpleAir sensor data  ###

    pa_data = mapply(get_PA_sensor_recent_data, 
                     sensor_id = pa$site_id, 
                     start_Date="2023-01-01", end_Date=date_end, 
                     save_mode = TRUE, save_path = save_folder)
    pa_data_old = mapply(get_PA_sensor_older_data, 
                         sensor_id = pa$site_id, 
                         start_Date="2016-01-01", end_Date=date_end, 
                         save_mode = TRUE, save_path = save_folder)

} else {

}
