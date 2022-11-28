
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
    dbname="/Users/jonas/Downloads/FARALLON3.db")

time = Sys.time()
## Read from db
# Takes about 5 min, 130 million records
Adults <- dbGetQuery(conn=con, 
    statement=
      "SELECT timestamp, object_count 
      FROM pred 
      WHERE class = 0
      ORDER BY timestamp ASC 
      LIMIT 100000000")
Sys.time() - time

# Disconnect db
dbDisconnect()

## Only save number of birds per time stamp
# Takes about 8 mins, reduces the data to 14 million rows
Adults = aggregate(object_count ~ timestamp, data = Adults, FUN = "max")

# Add 0s where there are no detections, with one time vector per year 
fullvec = data.frame(timestamp = 
  c(1556674140:1564757100, 1587903120:1596747660, 1619527260:1621846020))

# Merge Adults with full vector from object detection
Adults = merge(Adults, fullvec, by = "timestamp", all.y = TRUE)
Adults[is.na(Adults[,"object_count"]), "object_count"] <- -1


times = c(1621620480, 1621700520, 1621785600)

focustime = 1621700520
diff = 2000
timeint = (focustime-diff):(focustime+diff)
p1 = subset(Adults, timestamp %in% timeint)
p2 = subset(perMIN, minnum %in% timeint)
plot(p1$timestamp, p1$object_count, type = "b"); 
points(p2$minnum, p2$object_count, type = "p", 
  cex = 3, col = "blue")
p2

# Translate to time format 
Time = as.POSIXct(Adults$timestamp, origin = "1970-01-01")

# summarise to max birds per minute
# Takes about 47 seconds 
time = Sys.time()
minute = round.POSIXt(Time, "min")
Adults$minnum = as.numeric(minute)
rm(minute)

perMIN = aggregate(object_count ~ minnum, data = Adults, FUN = "max")
perMIN$minute = as.POSIXct(perMIN$minnum, origin = "1970-01-01")
perMIN$Yr = format(perMIN$minute, "%Y")


# change in numbers between consecutive minutes 
perMIN$dbirds = c(0, diff(perMIN$object_count))

# only include consecutive observations
perMIN = perMIN[order(perMIN$minute),]
perMIN$timediff = c(1, diff(perMIN$minute))
perMIN = subset(perMIN, timediff == 1)

# subset to same days each year
#ddply(adults, .(Yr), summarize, max = max(j), min = min(j))
#perMIN = subset(perMIN, yday(perMIN$min) %in% 131:187)

# how many disturbances per year?
disturbances = ddply(perMIN, .(Yr), summarize, disturbance = sum(dbirds < -3 ))

# over all minutes, during what proportion do we observe a disturbance?
#1000* disturbances/ddply(perMIN, .(Yr), summarize, observations = length(dbirds))

# subset to cases where at least 4 birds left and make df for plotting
perMIN = subset(perMIN, dbirds < -3)
perMIN = ddply(perMIN, .(Yr, dbirds), summarize, num = length(dbirds))
full = expand.grid(Yr = 2019:2021, dbirds = -10:-4)
perMIN = merge(perMIN, full, by = 1:2, all.y = TRUE)
perMIN[is.na(perMIN)] = 0
perMIN$Yr = as.factor(perMIN$Yr)


# comparison data from traditional analysis
#comp_data = read.csv("Data/ComparableDataDisturbances.csv")


# Comparison between field observations and AI
#perYR = aggregate(num ~ Yr, data = perMIN, FUN = "sum")
#comp_dist = merge(comp_data, perYR, by = "Yr")
  
#p2 = ggplot(comp_dist, aes(x = DistNum, y = num, color = as.factor(Yr))) + geom_point(size = 5) + scale_colour_manual(values = met.brewer("Demuth", 3), name = "")  +  theme_classic() +  theme(legend.position = "none") + geom_errorbar(aes(xmin = DistNum-DistNum_se, xmax = DistNum+DistNum_se, y = num)) + xlab("Field observations") + ylab("Object detection")

# Manual data check
subset(disturbances, dbirds == -10 & Yr == 2019)


# plot and save
ggplot(perMIN, aes(x = -dbirds, y = num, group = Yr, fill = Yr)) + 
  geom_bar(color = "black", stat = "identity", position = "dodge") + 
  scale_x_continuous(name = "Disturbance magnitude (change in # birds)", breaks = 4:10, labels = 4:10) + 
  scale_y_continuous(name = "Number of events") + 
  scale_fill_manual(values = met.brewer("Demuth", 3), name = "") + 
  theme_classic() + 
  theme(legend.position = c(0.7, 0.7))

#cowplot::plot_grid(p1, p2, ncol = 2, labels = c("a.", "b."), label_fontface = "plain")

# Save
ggsave("figures/FigAI_Disturb.jpg", width = 10, height = 9, units = "cm")



