# Read SQLite file to data frame

library(RSQLite)

## connect to db
con <- dbConnect(drv=RSQLite::SQLite(), 
    dbname="/Users/jonas/Downloads/FARALLON3.db")

Chicks <- list()

## create a data.frame for each table
Chicks <- dbGetQuery(conn=con, 
    statement="SELECT * FROM pred WHERE class = 0 LIMIT 10")
