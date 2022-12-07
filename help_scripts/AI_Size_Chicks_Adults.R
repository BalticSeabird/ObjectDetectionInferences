
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

# Date format 
Adults$Time = as.POSIXct(Adults$timestamp, origin = "1970-01-01")

# Day 
Adults$Date = as.Date(Adults$Time)



## Read from db
time = Sys.time()
Chicks <- dbGetQuery(conn=con, 
    statement=
      "SELECT timestamp, width
      FROM pred 
      WHERE class = 1
      AND score > .7
      LIMIT 1000000000")
Sys.time()-time

# Date format 
Chicks$Time = as.POSIXct(Chicks$timestamp, origin = "1970-01-01")

# Day 
Chicks$Date = as.Date(Chicks$Time)



## Read from db
time = Sys.time()
Eggs <- dbGetQuery(conn=con, 
    statement=
      "SELECT timestamp, width
      FROM pred 
      WHERE class = 2
      AND score > .7
      LIMIT 1000000000")
Sys.time()-time

# Date format 
Eggs$Time = as.POSIXct(Eggs$timestamp, origin = "1970-01-01")

# Day 
Eggs$Date = as.Date(Eggs$Time)


