
# Required libraries 
library(RSQLite)
library(plyr)
library(ggplot2)
library(MetBrewer)
library(drc)

## connect to db with Object Detection results 
con <- dbConnect(drv=RSQLite::SQLite(), 
    dbname="/Users/jonas/Downloads/FARALLON3_small_640.db")

## Read from db
time = Sys.time()
Adults <- dbGetQuery(conn=con, 
    statement=
      "SELECT timestamp, width
      FROM pred 
      WHERE class = 0
      AND score > .7
      LIMIT 1000000000")
Sys.time()-time

# Aggregate
Adults = aggregate(data = Adults, width ~ timestamp, 
    FUN = function(x) c(mean = mean(x), n = length(x)))
Adults[,c("mean", "n")] = unlist(Adults[[2]])

# Date format 
Adults$Time = as.POSIXct(Adults$timestamp, origin = "1970-01-01")

# Hour 
Adults$Hour = as.numeric(round.POSIXt(Adults$Time, "hour"))
Adults$class = 0


## Egg and Chicks
## Read from db
time = Sys.time()
Eggs <- dbGetQuery(conn=con, 
    statement=
      "SELECT timestamp, width, class
      FROM pred 
      WHERE class = 2
      OR class = 1
      AND score > .7
      LIMIT 10000000000")
Sys.time()-time


# Date format 
Eggs$Time = as.POSIXct(Eggs$timestamp, origin = "1970-01-01")

# Hour 
Eggs$Hour = as.numeric(round.POSIXt(Eggs$Time, "hour"))
Eggs$n = 1

## Birds
birds = rbind(Adults[,c("Hour", "class", "width", "n")], 
    Eggs[,c("Hour", "class", "width", "n")])
birds$Time = as.POSIXct(birds$Hour, origin = "1970-01-01")
birds$Day = format(birds$Time, "%Y%m%d")

# Summarize by hour
birds1 = aggregate(data = birds, width ~ Day + class, 
    FUN = function(x) mean(x))
birds2 = aggregate(data = birds, n ~ Day + class, 
    FUN = function(x) sum(x))
birds1$n = birds2$n

birds1$Yr = as.numeric(substr(birds1$Day, 1, 4))
birds1$Date = as.numeric(format(as.Date(birds1$Day, format = "%Y%m%d"), "%j"))

pd1 = subset(birds1, Yr == 2021 & n > 10000 & Date < 195) 
ggplot(data=pd1, aes(x = Date, y = mean)) + geom_point(aes(size = n, alpha = .2)) +
     facet_wrap(~class, scales = "fixed", ncol = 1) + 
     geom_smooth() + 
  scale_x_continuous(name = "Day of the year", breaks = 4:10, labels = 4:10) + 
  scale_y_continuous(name = "Mean size (width)") + 
  scale_size_continuous(guide = "none") +
  scale_alpha_continuous(guide = "none") +
  scale_fill_manual(values = met.brewer("Demuth", 3), name = "") + 
  theme_classic()
ggsave("Sizes.png", width = 22, height = 30, units = "cm")

ggplot(data=pd1, aes(x = Date, y = n)) + geom_bar(stat = "identity") +
     facet_wrap(~class, scales = "free_y") + 
  scale_x_continuous(name = "Day of the year", breaks = 4:10, labels = 4:10) + 
  scale_y_continuous(name = "Mean size (width)") + 
  scale_fill_manual(values = met.brewer("Demuth", 3), name = "") + 
  theme_classic() + 
  theme(legend.position = c(0.7, 0.7))



