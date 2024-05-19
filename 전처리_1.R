rm(list=ls())

setwd('/Users/jiminhuh/Desktop/울산부산전처리')

# Load required packages
library(dplyr)

# Read the CSV files
data_0 <- read.csv("부산_닻끌림_train.csv")
data_1 <- read.csv("부산_닻끌림_정답.csv")

#inserting drag_0_1 column
data_0$drag_0_1 <- 0
data_1$drag_0_1 <- 1

#changing column names
colnames(data_0) <- c("x","num","date","lat_N","long_E","sog","cog","hdg","drag_0_1")
colnames(data_1) <- c("x","area","year","num","month","day","hour","minute","lat_N","long_E","drag_0_1")

#removing quotation marks
library(stringr)
data_0$date <- str_replace(data_0$date,'"','')
data_0$lat_N <- str_replace(data_0$lat_N,'"','')
data_0$lat_N <- str_replace(data_0$lat_N,'N','')
data_0$long_E <- str_replace(data_0$long_E,'E','')
data_0$long_E <- str_replace(data_0$long_E,'"','')
data_1$lat_N <- str_replace(data_1$lat_N,'N','')
data_1$long_E <- str_replace(data_1$long_E,'E','')

data_0$date <- str_replace(data_0$date,'"','')
data_0$lat_N <- str_replace(data_0$lat_N,'"','')
data_0$lat_N <- str_replace(data_0$lat_N,'N','')
data_0$long_E <- str_replace(data_0$long_E,'E','')
data_0$long_E <- str_replace(data_0$long_E,'"','')
data_1$lat_N <- str_replace(data_1$lat_N,'N','')
data_1$long_E <- str_replace(data_1$long_E,'E','')

# combine date
data_1$date <- sprintf("%04d-%02d-%02d %02d:%02d", data_1$year, data_1$month, data_1$day, data_1$hour, data_1$minute)

#delete original date column
data_1$year <- NULL
data_1$month <- NULL
data_1$day <- NULL
data_1$hour <- NULL
data_1$minute <- NULL

data_1$area <- NULL
str(data_0)
str(data_1)

data_0$lat_N <- as.numeric(data_0$lat_N)
data_0$long_E <- as.numeric(data_0$long_E)

data_1$lat_N <- as.numeric(data_1$lat_N)
data_1$long_E <- as.numeric(data_1$long_E)

data_0$date <- as.character(data_0$date)

#-----------------------------------------
# Split the date and time components
datetime <- strsplit(data_0$date, " ")

# Extract the time component
time <- sapply(datetime, "[", 2)

# Split the time component by ':' and add leading zero if necessary
padded_time <- sapply(strsplit(time, ":"), function(x) {
  if (nchar(x[1]) == 1) {
    paste0("0", x[1], ":", x[2])
  } else {
    paste0(x[1], ":", x[2])
  }
})

# Combine the date and updated time components
data_0$date <- paste0(sapply(datetime, "[", 1), " ", padded_time)
#-----------------------------------------

# Merge data_0 and data_1 based on matching columns
merged_data <- merge(data_1, data_0[, c("lat_N", "long_E", "date", "sog", "cog", "hdg")], by = c("lat_N", "long_E", "date"), all.x = TRUE)
merged_data2 <- rbind(data_0,merged_data)

# Remove duplicates where drag_0_1 is 0
filtered_data <- merged_data2 %>%
  group_by(lat_N, long_E, date) %>%
  filter(!(drag_0_1 == 0 & duplicated(drag_0_1)))

##### KHOA data
khoa <- read.csv("KHOA_Buoy_train.csv")

# changing column names
str(khoa)
colnames(khoa) <- c("X","date","khoa_stn_name","khoa_ws","khoa_wd_point","khoa_wd")
khoa$date <- as.numeric(khoa$date)

khoa$date <- format(khoa$date, format = "%Y-%m-%d %H:%M")
khoa$date <- strptime(khoa$date, format = "%Y%m%d%H%M")
khoa$date <- format(khoa$date, format = "%Y-%m-%d %H:%M")

#--khoa$date <- sprintf("%04d-%02d-%02d %02d:%02d", khoa$year, khoa$month, khoa$day, khoa$hour, khoa$minute)
######combining filtered_data and khoa based on 'date'
library(dplyr)

main_khoa <- left_join(filtered_data, khoa, by = "date")

##### KHNP data
khnp <- read.csv("KHNP_Buoy_train.csv")
str(khnp)
colnames(khnp) <- c("X","date","khnp_stn_name","khnp_ws","khnp_wd")
khnp$date <- as.numeric(khnp$date)

khnp$date <- format(khnp$date, format = "%Y-%m-%d %H:%M")
khnp$date <- strptime(khnp$date, format = "%Y%m%d%H%M")
khnp$date <- format(khnp$date, format = "%Y-%m-%d %H:%M")

######combining main_khoa and khnp
main_khoa_khnp <- left_join(main_khoa, khnp, by = "date")


###KMA data
kma <- read.csv("KMA_PagoBuoy_train.csv")
str(kma)
colnames(kma) <- c("X","date","kma_n","kma_stn_name","kma_max_wh","kma_sig_wh","kma_mean_wh")
kma$kma_n <- NULL
kma$date <- as.character(kma$date)

kma$date <- strptime(kma$date, format = "%Y%m%d%H")
kma$date <- format(kma$date, format = "%Y-%m-%d %H")

# Install required packages if not already installed
#install.packages("tidyverse")
#install.packages("lubridate")

# Load required libraries
library(tidyverse)
library(lubridate)

# Convert the 'date' column to POSIXct format
kma$date <- ymd_h(kma$date)

# Generate a sequence of minutes for each hour in the 'date' column
minutes <- seq(0, 59, by = 10)  # Change the interval as per your requirement

# Create a new dataframe with additional rows for each minute
new_rows <- kma
new_rows$date <- lapply(kma$date, function(x) seq.POSIXt(x, x + minutes(59), by = "min"))
new_rows <- tidyr::unnest(new_rows, cols = c(date))

# Bind the new rows with the original dataframe
kma_with_minutes <- rbind(kma, new_rows)
kma_with_minutes$date <- format(kma_with_minutes$date, format = "%Y-%m-%d %H:%M")

#####final merge

######combining main_khoa_khnp and kma
#install.packages("dplyr")
library(dplyr)

#----kma_with_minutes에 date 는 같은데 stn_name 별로 값이 달라서 busan_final_merge 가 많이 나옴.
#----따라서 임의로 날짜가 겹치는 것 중 kma_max_wh가 가장 높은 값을 남겨달라고 하는 코드
kma_with_minutes <- kma_with_minutes %>%
  group_by(date) %>%
  filter(kma_max_wh == max(kma_max_wh))

main_khoa_khnp$x <- NULL
main_khoa_khnp$X.x <- NULL
main_khoa_khnp$X.y <- NULL

library(dplyr)

# Perform left join
busan_final_merge <- left_join(main_khoa_khnp, kma_with_minutes, by = "date")

complete_busan <- busan_final_merge[complete.cases(busan_final_merge), ]
complete_busan <- complete_busan[!duplicated(complete_busan), ]

###결측값
# Assuming your data frame is named 'complete_busan'
# Remove rows where any column contains -99
complete_busan ← complete_busan[!apply(complete_busan == -99, 1, any), ]
complete_busan ← complete_busan[!apply(complete_busan == -99.9, 1, any), ]
complete_busan ← complete_busan[!apply(complete_busan == -999, 1, any), ]


###separating date column again finally!!
library(tidyr)

complete_busan ← separate(complete_busan, date, into = c("year", "month", "day", "hour", "minute"), sep = "[- :]")
str(complete_busan)

complete_busan$area ← 'BUSAN'


##################################################################################

# Read the CSV files
data_3 <- read.csv("울산_닻끌림_train.csv")
data_4 <- read.csv("울산_닻끌림_정답.csv")

#inserting drag_0_1 column
data_3$drag_0_1 <- 0
data_4$drag_0_1 <- 1

#changing column names
colnames(data_3) <- c("x","num","date","lat_N","long_E","sog","cog","hdg","drag_0_1")
colnames(data_4) <- c("x","area","year","num","month","day","hour","minute","lat_N","long_E","drag_0_1")

#removing quotation marks
library(stringr)
data_3$date <- str_replace(data_3$date,'"','')
data_3$lat_N <- str_replace(data_3$lat_N,'"','')
data_3$lat_N <- str_replace(data_3$lat_N,'N','')
data_3$long_E <- str_replace(data_3$long_E,'E','')
data_3$long_E <- str_replace(data_3$long_E,'"','')
data_4$lat_N <- str_replace(data_4$lat_N,'N','')
data_4$long_E <- str_replace(data_4$long_E,'E','')

data_3$date <- str_replace(data_3$date,'"','')
data_3$lat_N <- str_replace(data_3$lat_N,'"','')
data_3$lat_N <- str_replace(data_3$lat_N,'N','')
data_3$long_E <- str_replace(data_3$long_E,'E','')
data_3$long_E <- str_replace(data_3$long_E,'"','')
data_4$lat_N <- str_replace(data_4$lat_N,'N','')
data_4$long_E <- str_replace(data_4$long_E,'E','')

# combine date
data_4$date <- sprintf("%04d-%02d-%02d %02d:%02d", data_4$year, data_4$month, data_4$day, data_4$hour, data_4$minute)

#delete original date column
data_4$year <- NULL
data_4$month <- NULL
data_4$day <- NULL
data_4$hour <- NULL
data_4$minute <- NULL

data_4$area <- NULL
str(data_3)
str(data_4)

data_3$lat_N <- as.numeric(data_3$lat_N)
data_3$long_E <- as.numeric(data_3$long_E)

data_4$lat_N <- as.numeric(data_4$lat_N)
data_4$long_E <- as.numeric(data_4$long_E)

data_3$date <- as.character(data_3$date)

#-----------------------------------------
# Split the date and time components
datetime <- strsplit(data_3$date, " ")

# Extract the time component
time <- sapply(datetime, "[", 2)

# Split the time component by ':' and add leading zero if necessary
padded_time <- sapply(strsplit(time, ":"), function(x) {
  if (nchar(x[1]) == 1) {
    paste0("0", x[1], ":", x[2])
  } else {
    paste0(x[1], ":", x[2])
  }
})

# Combine the date and updated time components
data_3$date <- paste0(sapply(datetime, "[", 1), " ", padded_time)
#-----------------------------------------

# Merge data_3 and data_4 based on matching columns
merged_data <- merge(data_4, data_3[, c("lat_N", "long_E", "date", "sog", "cog", "hdg")], by = c("lat_N", "long_E", "date"), all.x = TRUE)
merged_data2 <- rbind(data_3,merged_data)

# Remove duplicates where drag_0_1 is 0
filtered_data <- merged_data2 %>%
  group_by(lat_N, long_E, date) %>%
  filter(!(drag_0_1 == 0 & duplicated(drag_0_1)))

##### KHOA data
khoa <- read.csv("KHOA_Buoy_train.csv")

# changing column names
str(khoa)
colnames(khoa) <- c("X","date","khoa_stn_name","khoa_ws","khoa_wd_point","khoa_wd")
khoa$date <- as.numeric(khoa$date)

khoa$date <- format(khoa$date, format = "%Y-%m-%d %H:%M")
khoa$date <- strptime(khoa$date, format = "%Y%m%d%H%M")
khoa$date <- format(khoa$date, format = "%Y-%m-%d %H:%M")

#--khoa$date <- sprintf("%04d-%02d-%02d %02d:%02d", khoa$year, khoa$month, khoa$day, khoa$hour, khoa$minute)
######combining filtered_data and khoa based on 'date'
library(dplyr)

main_khoa <- left_join(filtered_data, khoa, by = "date")

##### KHNP data
khnp <- read.csv("KHNP_Buoy_train.csv")
str(khnp)
colnames(khnp) <- c("X","date","khnp_stn_name","khnp_ws","khnp_wd")
khnp$date <- as.numeric(khnp$date)

khnp$date <- format(khnp$date, format = "%Y-%m-%d %H:%M")
khnp$date <- strptime(khnp$date, format = "%Y%m%d%H%M")
khnp$date <- format(khnp$date, format = "%Y-%m-%d %H:%M")

######combining main_khoa and khnp
main_khoa_khnp <- left_join(main_khoa, khnp, by = "date")


###KMA data
kma <- read.csv("KMA_PagoBuoy_train.csv")
str(kma)
colnames(kma) <- c("X","date","kma_n","kma_stn_name","kma_max_wh","kma_sig_wh","kma_mean_wh")
kma$kma_n <- NULL
kma$date <- as.character(kma$date)

kma$date <- strptime(kma$date, format = "%Y%m%d%H")
kma$date <- format(kma$date, format = "%Y-%m-%d %H")

# Install required packages if not already installed
#install.packages("tidyverse")
#install.packages("lubridate")

# Load required libraries
library(tidyverse)
library(lubridate)

# Convert the 'date' column to POSIXct format
kma$date <- ymd_h(kma$date)

# Generate a sequence of minutes for each hour in the 'date' column
minutes <- seq(0, 59, by = 10)  # Change the interval as per your requirement

# Create a new dataframe with additional rows for each minute
new_rows <- kma
new_rows$date <- lapply(kma$date, function(x) seq.POSIXt(x, x + minutes(59), by = "min"))
new_rows <- tidyr::unnest(new_rows, cols = c(date))

# Bind the new rows with the original dataframe
kma_with_minutes <- rbind(kma, new_rows)
kma_with_minutes$date <- format(kma_with_minutes$date, format = "%Y-%m-%d %H:%M")

#####final merge

######combining main_khoa_khnp and kma
#install.packages("dplyr")
library(dplyr)

#----kma_with_minutes에 date 는 같은데 stn_name 별로 값이 달라서 ulsan_final_merge 가 많이 나옴.
#----따라서 임의로 날짜가 겹치는 것 중 kma_max_wh가 가장 높은 값을 남겨달라고 하는 코드
kma_with_minutes <- kma_with_minutes %>%
  group_by(date) %>%
  filter(kma_max_wh == max(kma_max_wh))

main_khoa_khnp$x <- NULL
main_khoa_khnp$X.x <- NULL
main_khoa_khnp$X.y <- NULL

library(dplyr)

# Perform left join
ulsan_final_merge <- left_join(main_khoa_khnp, kma_with_minutes, by = "date")

complete_ulsan <- ulsan_final_merge[complete.cases(ulsan_final_merge), ]
complete_ulsan <- complete_ulsan[!duplicated(complete_ulsan), ]

###결측값
# Assuming your data frame is named 'complete_ulsan'
# Remove rows where any column contains -99
complete_ulsan ← complete_ulsan[!apply(complete_ulsan == -99, 1, any), ]
complete_ulsan ← complete_ulsan[!apply(complete_ulsan == -99.9, 1, any), ]
complete_ulsan ← complete_ulsan[!apply(complete_ulsan == -999, 1, any), ]


###separating date column again finally!!
library(tidyr)

complete_ulsan ← separate(complete_ulsan, date, into = c("year", "month", "day", "hour", "minute"), sep = "[- :]")
str(complete_ulsan)

complete_ulsan_$area ← 'ULSAN'


##################################################################################


sort(colnames(complete_ulsan))
sort(colnames(complete_busan))

final_ulsan_busan <- rbind(complete_ulsan,complete_busan)
final_ulsan_busan <- final_ulsan_busan[!duplicated(final_ulsan_busan),]

write.csv(final_ulsan_busan, 'final_ulsan_busan.csv', fileEncoding = 'UTF-8')