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
#as.numeric(as.POSIXct("2020-01-01 00:00:00 UTM"))
#as.numeric(as.POSIXct("2020-12-31 00:00:00 UTM"))

## connect to db
dbname = "aux_data/FARALLON3_m_960.db"
con = dbConnect(drv = RSQLite::SQLite(), dbname = dbname)

## extract data 
adults = dbGetQuery(conn=con, 
                     statement=
                       "SELECT timestamp, object_count 
      FROM pred 
      WHERE class = 0
      AND timestamp > 1577833200
      AND timestamp < 1609369200")

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
source("scripts/Active_breeders2020.R")

# Join with active breeding attempts 
df_far3$time = as.POSIXct(as.numeric(as.character(df_far3$full_seq)), origin = "1970-01-01")
temp_df$present = df_far3[match(temp_df[,"time"], df_far3[,"time"]),"Freq"]
temp_df[is.na(temp_df[,"present"]),"present"] = 0

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
sub = sub[sub$time < "2020-07-07 17:06:00" | sub$time > "2020-07-07 17:26:00",] 

## More fewer or equal number of birds at a given time stamp
sub$cat = ifelse(sub$object_count > sub$present, "more", 
  ifelse(sub$object_count < sub$present, "fewer", "equal"))

## Summarize per temp interval 
temps = seq(5, 50, 5)
sub$temp_round = temps[cut(sub$temp_sun, temps)]

# table of observations per rounded temperature
tab = as.matrix(table(sub$temp_round, sub$cat), ncol = 3)

# Number of observations per category
n_values = as.numeric(rowSums(tab))

# divide by total number of observations per temperature group
pd = data.frame(tab/rowSums(tab))

# fix format temp variable
pd$Var1 = as.numeric(as.character(pd$Var1))

# Fix class variable
pd$Class = factor(pd$Var2, levels = c("fewer", "equal", "more"))

cols = met.brewer("Nattier", 3)
ggplot(data = subset(pd), aes(x = Var1, y = Freq*100, group = Class, fill = Class)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = cols, name = "Birds present") +
  ylab("Percentage (%)") + xlab("Temperature (\u00B0C)") +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(5, 45, 5)) +
  annotate("text", x = seq(5, 45, 5), y = 5, label = n_values)

ggsave("figures/FigAI_temperature_bars.jpg", width = 17, height = 14, units = "cm")



# Check detector error 26 june
#pd = sub[sub$time < "2020-05-26 17:00:00" & sub$time > "2020-05-26 15:00:00",] 
#ggplot() + geom_line(data = pd, aes(x = time, y = object_count))

#pd = adult_count[adult_count$timedate< "2020-05-31 16:00:00" & adult_count$timedate > "2020-05-31 15:00:00",] 
#ggplot() + geom_line(data = pd, aes(x = timedate, y = object_count))
