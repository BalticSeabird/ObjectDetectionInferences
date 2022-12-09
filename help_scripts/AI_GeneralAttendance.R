# ************************** #
# AI attendance general plot # 
# ************************** #

library(RSQLite)
library(lubridate)
library(ggplot2)


## connect to db
con = dbConnect(drv=RSQLite::SQLite(), 
                dbname="Data/FARALLON3.db")


## extract data 
adults = dbGetQuery(conn=con, 
                    statement=
                      "SELECT timestamp, object_count 
      FROM pred 
      WHERE class = 0
      ORDER BY timestamp ASC")

# disconnect db
dbDisconnect(con)

adults_count = aggregate(object_count ~ timestamp, adults, length)

adults_count$timedate = as.POSIXct(adults_count$timestamp, origin = "1970-01-01 00:00:00") + hours(2)
adults_count$h =  format(adults_count$timedate, "%Y-%m-%d %H")

adults_h = aggregate(object_count ~ h, adults_count, median)
adults_h$h = as.POSIXct(adults_h$h, format = "%Y-%m-%d %H")

library(colortools)
cols = wheel("steelblue", num = 24)



ggplot(data = adults_h, aes(x = yday(h), y = object_count, group = hour(h), colour = hour(h))) +
  #scale_colour_manual(values = cols) +
  
  geom_line() +
  
  facet_grid(~year(h)) +
  
  ylab("Birds present") + xlab("Day of year") +
  
  theme_classic() 

# three different temporal scales: seasonal, diurnal, minute to minute

# seasonal
ggplot(data = adults_h[hour(adults_h$h) == 0,], 
       aes(x = yday(h), y = object_count, group = as.factor(year(h)), colour = as.factor(year(h)))) +
  geom_line() +
  theme_classic()



# diel
ggplot(data = adults_h[year(adults_h$h)== 2021 & yday(adults_h$h) %in%  c(120, 150, 180, 210),], # one early season, one peak breeding
       aes(x = hour(h), y = object_count, as.factor(group = yday(h)), colour = as.factor(yday(h)))) +
  geom_line() +
  ylab("Birds present") + xlab("Hour") +
  theme_classic()


# minute to minute
adults_count$min =  format(adults_count$timedate, "%Y-%m-%d %H-%M")
adults_min = aggregate(object_count ~ min, adults_count[year(adults_count$timedate) == 2020,], median)
adults_min$min = as.POSIXct(adults_min$min, format = "%Y-%m-%d %H-%M")

ggplot(data = adults_min[yday(adults_min$min) %in% c(148) & hour(adults_min$min) %in% c(14) ,], # one with eagle disturbance, one without 
       aes(x = minute(min), y = object_count, group = yday(min), colour = yday(min))) +
  geom_line() +
  #xlim() + # restrict to 1-2 h
  
  theme_classic()

# colour scale
# remove CI




