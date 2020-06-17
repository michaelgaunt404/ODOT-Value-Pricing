## Dora Wu 04-28-2020
## This script read ODOT 15-min counts for various locations
## and summarize them to average hourly volume for each of the 
## 24 hours. Non-weekdays and holidays are removed.

library(data.table)
library(tidyverse)
library(readxl)
library(xlsx)
library(timeDate)

COUNT_FOLDER <- "C:\\Projects\\80564A_PortlandValuePricing\\Data\\Traffic Data\\"
VALIDATION_FOLDER <- "C:\\Projects\\80564A_PortlandValuePricing\\Tasks\\Validation\\"

holiday_list  <- c("USNewYearsDay", "USMLKingsBirthday", "USPresidentsDay",
                   "USMemorialDay", "USIndependenceDay", "USLaborDay",
                   "USVeteransDay", "USThanksgivingDay", "USChristmasDay")

holiday_dates <- dates(as.character(holiday(2000:2025, holiday_list)),
                       format = "Y-M-D")

# function to average counts to hourly
averageCountData <- function(path_to_file){

  df <- read.xlsx(path_to_file,
                  sheetIndex=2, startRow = 8, endRow = 1000,
                  header = F, stringsAsFactors = F)

  colnames(df) <- c("time_stamp_string", "volume")

  df_summary <- df %>%
    filter(volume != "-") %>%
    mutate(volume = as.numeric(volume),
           date = as.Date(time_stamp_string, format = "%m/%d/%Y %I:%M %p"),
           time_stamp_string = as.POSIXct(strptime(time_stamp_string,
                                                   format = "%m/%d/%Y %I:%M %p")),
           hour = as.numeric(str_sub(as.POSIXlt(time_stamp_string), 12, 13)),
           minute = as.numeric(str_sub(as.POSIXlt(time_stamp_string), 15, 16)),
           date_of_year = str_sub(date, 6, 10),
           day_of_week = weekdays(date)) %>%
    filter(day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday")) %>%
    filter(!is.holiday(date, holiday_dates)) %>%
    group_by(hour, minute) %>%
    mutate(average_volume = mean(volume)) %>%
    ungroup() %>%
    mutate(flag = ifelse(volume < 5 & average_volume > 50, 1, 0))

  flag_sum <- sum(df_summary$flag)

  if (flag_sum > 0) print(path_to_file)

  df_summary <- df_summary %>%
    filter(flag == 0) %>%
    group_by(hour, minute) %>%
    summarise(average_volume = mean(volume)) %>%
    group_by(hour) %>%
    summarise(hourly_volume = sum(average_volume))

  return(df_summary)

}

folder_list <- list.dirs(COUNT_FOLDER, full.names = F)
folder_list <- folder_list[!folder_list == ""]

location_list_df <- data.frame(CountYear = character(),
                               Index = numeric(),
                               Location = character(),
                               Longitude = numeric(),
                               Latitude = numeric(),
                               stringsAsFactors = F)

count_table <- data.frame(hour = 0:23,
                          stringsAsFactors = F)

i = 0

# loop through each count location
for (folder in folder_list){
  file_name_list <- list.files(paste0(COUNT_FOLDER, folder,"\\"))

  for (file_name in file_name_list) {
    file_path <- paste0(COUNT_FOLDER, folder, "\\", file_name)

    location_list_df[i, "CountYear"] <- folder
    location_list_df[i, "Index"] <- gsub(".xlsx", "", file_name)

    count_location <- read.xlsx(file_path, sheetIndex=3,
                                startRow = 1, endRow = 1,
                                header = F, stringsAsFactors = F)
    location_list_df[i, "Location"] <- as.character(count_location)

    coordinates <- read.xlsx(file_path, sheetIndex=3,
                             startRow = 2, endRow = 2,
                             header = F, stringsAsFactors = F)
    location_list_df[i, "Latitude"]  <- as.numeric(coordinates$X1)
    location_list_df[i, "Longitude"] <- as.numeric(coordinates$X2)

    sensor_count_data <- averageCountData(file_path)
    colnames(sensor_count_data)[2] <- paste(folder, gsub(".xlsx", "", file_name),
                                            sep = "_")
    count_table <- count_table %>%
      left_join(sensor_count_data, by = c("hour"))

    i = i+1

  }

}

count_table_t <- data.frame(t(count_table %>% select(-hour)))

count_table_t <- cbind(row.names(count_table_t), count_table_t)
colnames(count_table_t) <- c("count_name", count_table$hour)
count_table_t <- count_table_t %>%
  separate(count_name, c("Year", "Index"), sep = "_")

# write.csv(location_list_df, paste0(VALIDATION_FOLDER, "CountLocationList.csv"),
#           row.names = F)
write.csv(count_table_t, paste0(VALIDATION_FOLDER, "CountSummary.csv"),
          row.names = F)

