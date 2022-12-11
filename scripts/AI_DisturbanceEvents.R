
# ******************************************************* #
# AI Adult Detect Disturbance Events
# ******************************************************* #

# Required libraries 
library(RSQLite)
library(plyr)
library(ggplot2)
library(MetBrewer)

## connect to db
con <- dbConnect(drv=RSQLite::SQLite(), 
    dbname="/Users/jonas/Downloads/FARALLON3_221204_small_065_03.db")


time = Sys.time()

## Read from db
# Takes about 5 min, 130 million records
Adults <- dbGetQuery(conn=con, 
    statement=
      "SELECT timestamp, object_count
      FROM pred 
      WHERE class = 0
      AND score > 0.5")
Sys.time() - time



# Disconnect db
dbDisconnect()

## Only save number of birds per time stamp
# Takes about 8 mins, reduces the data to 18 million rows
Adults = aggregate(object_count ~ timestamp, data = Adults, FUN = "max")

# Change to local time 
Adults$timestamp = Adults$timestamp-7200

# Add 0s where there are no detections, full_seq from Video_timestamps.R  
full_seq = data.frame(timestamp = full_seq)

# Merge Adults with full vector from object detection
# Takes about 
Adults2 = merge(Adults, full_seq, by = "timestamp", all.y = TRUE)
Adults2[is.na(Adults2[,"object_count"]), "object_count"] <- -1

# Order data 
Adults2 = Adults2[order(Adults2[,"timestamp"]),]

# Translate to time format 
Adults2$Time = as.POSIXct(Adults2$timestamp, origin = "1970-01-01")

# Translate to time format 
Adults2$Date = format(Adults2$Time, "%Y-%m-%d")


# Plot by day 
dates = unique(Adults2$Date)
#for (i in 1:length(dates)) {
#pd1 = subset(Adults2, Date == dates[i])
#ggplot() + geom_line(data = pd1, aes(x = Time, y = object_count)) +
#  scale_y_continuous(limits = c(-1, 18)) + 
#  ggtitle(dates[i])
#ggsave(paste0("figures/Imagecheck_", dates[i], ".png"), 
#  width = 12, height = 8, unit = "cm") }



# summarise to max birds per minute
# Takes about 47 seconds 
Adults2$minnum = as.numeric(round.POSIXt(Adults2$Time, "min"))

Adultsmin = aggregate(object_count ~ minnum, data = Adults2, FUN = "max")
Adultsmin$minute = as.POSIXct(Adultsmin$minnum, origin = "1970-01-01")
Adultsmin$Yr = format(Adultsmin$minute, "%Y")
Adultsmin$Date = format(Adultsmin$minute, "%Y-%m-%d")



# Diff in birds
Adultsmin$diff = c(0, diff(Adultsmin$object_count))


# only include consecutive observations
Adultsmin = Adultsmin[order(Adultsmin$minute),]
Adultsmin$timediff = c(1, diff(Adultsmin$minute))
Adultsmin = subset(Adultsmin, timediff == 1)


# how many disturbances per year?
distx = subset(Adultsmin, diff < -3)
distx$Day = as.numeric(format(distx$minute, "%j"))
distx = subset(distx, Day %in% 121:159)

# Aggregate
disturbances = aggregate(data = distx, object_count ~ Yr + diff, FUN = "length")

# subset to cases where at least 4 birds left and make df for plotting
full = expand.grid(Yr = 2019:2021, dbirds = -13:-4)
disturbances = merge(disturbances, full, by = 1:2, all.y = TRUE)
disturbances[is.na(disturbances)] = 0
disturbances$Yr = as.factor(disturbances$Yr)


ggplot(data = distx) + geom_bar(aes(x = Day, y = -diff), 
  stat = "identity", fill = "darkgreen") + 
  facet_wrap(~Yr, ncol = 1) +
  scale_x_continuous(name = "Day of the year") + 
  scale_y_continuous(name = "Daily aggregated disturbance") + 
  scale_fill_manual(values = met.brewer("Demuth", 3), name = "") + 
  theme_classic()



timex = as.POSIXct("2019-06-01 13:05:00"); timex2 = timex+600
timex = as.POSIXct("2019-05-28 20:49:00"); timex2 = timex+600
subset(Adults2, Time > timex & Time < timex2)
subset(distx, Yr == 2019)





# plot and save
ggplot(disturbances, aes(x = -diff, y = object_count, group = Yr, fill = Yr)) + 
  geom_bar(color = "black", stat = "identity", position = "dodge") + 
  scale_x_continuous(name = "Disturbance magnitude (change in # birds)", breaks = 4:10, labels = 4:10) + 
  scale_y_continuous(name = "Number of events") + 
  scale_fill_manual(values = met.brewer("Demuth", 3), name = "") + 
  theme_classic() + 
  theme(legend.position = c(0.7, 0.7))

#cowplot::plot_grid(p1, p2, ncol = 2, labels = c("a.", "b."), label_fontface = "plain")

# Save
ggsave("figures/FigAI_Disturb.jpg", width = 10, height = 9, units = "cm")



