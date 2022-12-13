# ************************** #
# AI attendance general plot # 
# ************************** #

library(RSQLite)
library(lubridate)
library(ggplot2)
library(MetBrewer)
library(cowplot)


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
adult_count$object_count[is.na(adult_count$object_count)] = 0
adult_count$timedate[adult_count$object_count == 0] = as.POSIXct(adult_count$timestamp[adult_count$object_count == 0], origin = "1970-01-01 00:00:00")

## aggregate per hour ##
adult_count$h =  format(adult_count$timedate, "%Y-%m-%d %H")

adults_h = aggregate(object_count ~ h, adult_count, median)
adults_h$h = as.POSIXct(adults_h$h, format = "%Y-%m-%d %H")


# subset to middle of night and correct years (IR light not functioning in 2019)
pd = subset(adults_h, hour(h) %in% c(00) & year(h) %in% 2020:2021) # 00 is the best
pd$Yr = year(pd$h)
#pd$object_count = pd$object_count + 1

## parallel field data
source("scripts/activeBreeders.R")
pd2 = subset(active_df, shelf == "Farallon3" & year(date) %in% 2020:2021)
pd2$Yr = year(pd2$date)

## plot for paper ##

# annotation layer
annot = data.frame(Yr = c(2020, 2021), x = c(122, 122), y = c(9, 9), lab = c("a.", "b."))

# make plot
p1 = ggplot() + 
  geom_line(data = pd2, aes(x = yday(date), y = present), size = 4, col = "lightgrey") + 
  geom_line(data = pd, aes(x = yday(h), y = object_count)) + 
  facet_wrap(~ Yr, ncol = 3) + 
  scale_x_continuous(name = "", limits = c(120, 217)) + 
  scale_y_continuous(name = "Number of adults/pairs", breaks = c(0, 2, 4, 6, 8, 10), labels = c(0, 2, 4, 6, 8, 10)) + 
  geom_text(data = annot, aes(x = x, y = y, label = lab), size = 4) +
  theme_classic() + 
  theme(strip.background = element_blank(), strip.text.x = element_blank())  




# diel
df = read.delim("data/BreedingDataAuklabUntil2021.txt")
df = df[, 1:18]
df = df[year(df$EggDate) == 2021, ]
df = df[substr(df$PairID, 1,10) == "Farallon-3",]
df$EggDate = as.POSIXct(df$EggDate, format = "%Y-%m-%d %H:%M:%S")
df$HatchDate = as.POSIXct(df$HatchDate, format = "%Y-%m-%d %H:%M:%S")
df$ChickGoneDate = as.POSIXct(df$ChickGoneDate, format = "%Y-%m-%d %H:%M:%S")

range(yday(adults_h$h[year(adults_h$h)== 2021]))
yday(min(df$EggDate)) # 125 = well before egg laying
yday(mean(df$EggDate) + (mean(df$HatchDate, na.rm = T)-mean(df$EggDate))/2) # 154 = peak incubation
yday(mean(df$HatchDate, na.rm = T) + (mean(df$ChickGoneDate, na.rm = T)-mean(df$HatchDate, na.rm = T))/2) # 179 = peak chick-rearing
yday(max(df$ChickGoneDate, na.rm = T)) # 210 = well after fledging


# "5th May", "3rd June", "28th June", "29th July"
p2 = ggplot(data = adults_h[year(adults_h$h)== 2021 & yday(adults_h$h) %in%  c(125, 154, 179, 210),], # one early season, one peak breeding
       aes(x = hour(h), y = object_count, group = as.factor(yday(h)), linetype = as.factor(yday(h)), colour = as.factor(yday(h)))) +
  
  geom_line(size = 1) +
  
  scale_colour_manual(values = met.brewer("Tiepolo", 8)[c(1,3,5,8)], labels = c("before laying", "peak incubation", "peak chick-rearing", "after fledging"), name = "") +
  scale_linetype_manual(values = 1:4, labels = c("before laying", "peak incubation", "peak chick-rearing", "after fledging"), name = "") +
  
  ylab("Birds present") + xlab("Hour") +
  
  annotate("text",x = 0, y = 10, label = "c.", size = 4) +
  
  theme_classic() +
  
  theme(legend.text=element_text(size = 11))


# minute to minute

# eagle disturbance
adult_count$min =  format(adult_count$timedate, "%Y-%m-%d %H-%M")
adults_min = aggregate(object_count ~ min, adult_count[year(adult_count$timedate) == 2020,], median)
adults_min$min = as.POSIXct(adults_min$min, format = "%Y-%m-%d %H-%M")

p3 = ggplot(data = adults_min[yday(adults_min$min) %in% c(140, 141) & hour(adults_min$min) %in% c(06) ,], # one with eagle disturbance, one without 
       aes(x = minute(min), y = object_count, group = as.factor(yday(min)), colour = as.factor(yday(min)), linetype = as.factor(yday(min)))) +

  geom_line(size = 1) +
  
  scale_x_continuous(breaks = c(0, 30, 60), labels = c("06:00", "06:30", "07:00")) +
  scale_colour_manual(values = met.brewer("Tiepolo", 8)[c(3,6)], labels = c("19th May 2020", "20th May 2020"), name = "") +
  scale_linetype_manual(values = 1:2, labels = c("19th May 2020", "20th May 2020"), name = "") +
  
  ylab("Birds present") + xlab("Time") +
  
  ylim(0, 11.5) +
  
  annotate("text", x = 0, y = 11, label = "d.", size = 4) +
  
  theme_classic() +
  
  theme(legend.text=element_text(size = 11))


plot_grid(p1, p2, p3, ncol = 1)


ggsave("FigAI_Attendance.jpg", width = 3.5*5, height = 5*5, units = "cm")


## look at eggs and chicks too
