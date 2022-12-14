# **************************************** #
# AI attendance in relation to temperature # 
# **************************************** #

library(RSQLite)
library(lubridate)
library(dplyr)
library(MetBrewer)
library(ggplot2)


#### read attendance data ####

## figure out which timestamps are 2020
as.numeric(as.POSIXct("2020-01-01 00:00:00 UTM"))
as.numeric(as.POSIXct("2020-12-31 00:00:00 UTM"))

## connect to db
dbname = "data/FARALLON3.db"
con = dbConnect(drv = RSQLite::SQLite(), 
                dbname = dbname)


## extract data 
adults = dbGetQuery(conn=con, 
                     statement=
                       "SELECT timestamp, object_count 
      FROM pred 
      WHERE class = 0
      AND timestamp > 1577833200
      AND timestamp < 1609369200
      ORDER BY timestamp ASC")

# disconnect db
dbDisconnect(con)

# get number per frame
adult_count = aggregate(object_count ~ timestamp, data = adults, FUN = "length")

# fix time column
adult_count$timestamp = adult_count$timestamp - 2*3600
adult_count$timedate = as.POSIXct(adult_count$timestamp, origin = "1970-01-01 00:00:00")

# add in empty interval
source("scripts/Video_timestamps.R")
ints = data.frame(timestamp = full_seq)
adult_count = merge(adult_count, ints, all = T)
adult_count = adult_count[adult_count$timestamp > 1577833200 & adult_count$timestamp < 1609369200,] 
adult_count$object_count[is.na(adult_count$object_count)] = 0
adult_count$timedate[adult_count$object_count == 0] = as.POSIXct(adult_count$timestamp[adult_count$object_count == 0], origin = "1970-01-01 00:00:00")

#### sort out time column for merging ####

# temperature every other minute - maximum observed birds in this minute
# sort out format
adult_count$timedate[minute(adult_count$timedate) %% 2 == 1] = adult_count$timedate[minute(adult_count$timedate) %% 2 == 1]-60
adult_count$minute = format(adult_count$timedate, "%Y-%m-%d %H:%M")

# per minute 
adultMin = aggregate(object_count ~ minute, data = adult_count, FUN = "median")
adultMin$time = as.POSIXct(paste(adultMin$minute, "00", sep = ":"))


#### load temperature data ####

temp_df = read.csv("data/Temperature_StoraKarlso.csv", sep=";")
names(temp_df) = c("date", "temp_sun", "temp_shade")

temp_df$time = as.POSIXlt(temp_df$date)
temp_df$date = NULL
temp_df$time = temp_df$time + 3600 # temp probe time in central European standard time - add one hour to get summer time


#### merge with temperature data ####

# join with temp data 
temp_df = left_join(adultMin, temp_df, by = "time")


#### add info on active breeding attempts ####
source("scripts/activeBreeders.R")

temp_df$date = as.Date(temp_df$time)
temp_df = left_join(temp_df, active_df[active_df$shelf == "Farallon3",], by = c("date"))


#### calculate attendance ~ temperature ####

# subset to data with at least one active breeding attempt and existing temperature data
sub = temp_df[temp_df$present > 0 & !is.na(temp_df$temp_sun),]

# fix variables
sub$presence_perc = sub$object_count/sub$present # AI birds divided by active attempts
sub$hour = as.numeric(format(sub$time, "%H"))

# subset to period used in dygnsstudier
sub = sub[sub$hour %in% 15:20,]

# subset to days on which 50% of breeding attempts active 
sub = sub[sub$present/max(temp_df$present, na.rm = T) >= 0.5,]

# remove time when chicks were ringed 
sub = sub[!(sub$time %in% as.POSIXct(c(
  "2020-07-07 17:18:00",
  "2020-07-07 17:20:00",
  "2020-07-07 17:22:00",
  "2020-07-07 17:24:00"
  
))),]

### plot ###

ggplot(data = sub, aes(x = temp_sun, y = round(object_count))) + 
  
  geom_point(alpha = 0.3, col = met.brewer("Nattier", 2)[2]) + 
  geom_smooth(size = 0.8, col = "black", method = "loess", fill = NA) +
  
  ylab("Birds present") + xlab("Temperature (\u00B0C)") +
  
  theme_classic() +
  theme(legend.position = "bottom")

ggsave("FigAI_temp2.jpg", width = 10, height = 10, units = "cm")


