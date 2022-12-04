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

ggplot(data = adults_h, aes(x = hour(h), y = object_count)) +
  geom_point() +
  geom_smooth()


ggplot(data = adults_h, aes(x = yday(h), y = object_count)) +
  geom_point() +
  geom_smooth()


ggplot(data = adults_h, aes(x = yday(h), y = object_count)) +
  geom_point() +
  geom_smooth() +
  facet_grid(~year(h))




ggplot(data = adults_h, aes(x = yday(h), y = object_count, group = hour(h), colour = hour(h))) +
  geom_point() +
  geom_smooth() +
  facet_grid(~year(h))

# colour scale
# remove CI




