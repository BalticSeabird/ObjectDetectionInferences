# Read SQLite file to data frame

library(RSQLite)


## connect to db
con <- dbConnect(drv=RSQLite::SQLite(), 
    dbname="/Users/jonas/Downloads/FARALLON3.db")



# Read from db
test <- dbGetQuery(conn=con, 
    statement=
      "SELECT * 
      FROM pred 
      WHERE class = 0
      ORDER BY timestamp ASC 
      LIMIT 50")
