
# ******************************************************* #
# AI Adults Detect Disturbance Events
# ******************************************************* #

# Required libraries 
library(RSQLite)
library(plyr)
library(ggplot2)
library(MetBrewer)

## connect to db
dbpath = "~/Library/CloudStorage/OneDrive-Sverigeslantbruksuniversitet/SeabirdAI/detection_logs/yolov5/"
dbname = "FARALLON3.db"
con = dbConnect(drv = RSQLite::SQLite(), dbname = paste0(dbpath, dbname))


time = Sys.time()

## Read from db
# Takes about 5 min, 130 million records
Adults <- dbGetQuery(conn=con, 
    statement=
      "SELECT timestamp, object_count
      FROM pred 
      WHERE class = 0")
Sys.time() - time


# Disconnect db
dbDisconnect()

## Only save number of birds per time stamp
# Takes about 8 mins, reduces the data to 18 million rows
Adults = aggregate(object_count ~ timestamp, data = Adults, FUN = "max")

# Change to local time 
Adults$timestamp = Adults$timestamp-7200

# Add 0s where there are no detections, full_seq from Video_timestamps.R  
source("scripts/Video_timestamps.R")
full_seq = data.frame(timestamp = full_seq)

# Merge Adults with full vector from object detection
Adults2 = merge(Adults, full_seq, by = "timestamp", all.y = TRUE)
Adults2[is.na(Adults2[,"object_count"]), "object_count"] <- -1

# Order data 
Adults2 = Adults2[order(Adults2[,"timestamp"]),]

# Translate to time format 
Adults2$Time = as.POSIXct(Adults2$timestamp, origin = "1970-01-01")

# Translate to time format 
Adults2$Date = format(Adults2$Time, "%Y-%m-%d")


# summarise to max birds per minute
# Takes about 47 seconds 
Adults2$minnum = as.numeric(round.POSIXt(Adults2$Time, "min"))

Adultsmin = aggregate(object_count ~ minnum, data = Adults2, FUN = "max")
Adultsmin$minute = as.POSIXct(Adultsmin$minnum, origin = "1970-01-01")
Adultsmin$Yr = format(Adultsmin$minute, "%Y")
Adultsmin$Date = format(Adultsmin$minute, "%Y-%m-%d")


# Diff in birds from one minute to another
Adultsmin$diff = c(0, diff(Adultsmin$object_count))


# Only include consecutive observations
Adultsmin = Adultsmin[order(Adultsmin$minute),]
Adultsmin$timediff = c(1, diff(Adultsmin$minute))
Adultsmin = subset(Adultsmin, timediff == 1)


# How many disturbances per year?
distx = subset(Adultsmin, diff < -3)
distx$Day = as.numeric(format(distx$minute, "%j"))
distx = subset(distx, Day %in% 121:159)
disturbances = aggregate(data = distx, object_count ~ Yr + diff, FUN = "length")

# Make df for plotting
full = expand.grid(Yr = 2019:2021, dbirds = -13:-4)
disturbances = merge(disturbances, full, by = 1:2, all.y = TRUE)
disturbances[is.na(disturbances)] = 0
disturbances$Yr = as.factor(disturbances$Yr)

# Plot time series 
ggplot(data = distx) + geom_bar(aes(x = Day, y = -diff), 
  stat = "identity", fill = "darkgreen") + 
  facet_wrap(~Yr, ncol = 1) +
  scale_x_continuous(name = "Day of the year") + 
  scale_y_continuous(name = "Daily aggregated disturbance (number of birds fly off)") + 
  scale_fill_manual(values = met.brewer("Demuth", 3), name = "") + 
  theme_classic() + 
  theme(strip.background = element_blank())




# Save for validation
df_validation = distx[sample(1:nrow(distx), 30),] 
#write.csv(df_validation, "aux_data/validation_disturbances.csv")


# Save
ggsave("figures/FigAI_Disturb.jpg", 
  width = 10, height = 12, units = "cm")



