# **************************************** #
# AI attendance in relation to temperature # 
# **************************************** #


#### load temperature data ####

temp_df = read.csv("Data/Temperature_StoraKarlso.csv", sep=";")
names(temp_df) = c("date", "temp_sun", "temp_shade")

temp_df$time = as.POSIXlt(temp_df$date)
temp_df$date = NULL
temp_df$timestamp = as.numeric(temp_df$time)


#### read attendance data ####

## figure out which timestamps are 2020
as.numeric(as.POSIXct("2020-01-01 00:00:00 UTM"))
as.numeric(as.POSIXct("2020-12-31 00:00:00 UTM"))


## connect to db
con = dbConnect(drv=RSQLite::SQLite(), 
                dbname="Data/FARALLON3.db")


## extract data 
adults = dbGetQuery(conn=con, 
                     statement=
                       "SELECT timestamp, object_count 
      FROM pred 
      WHERE class = 0
      AND timestamp > 1577833200
      AND timestamp < 1609369200
      ORDER BY timestamp ASC")

# disconnect db
dbDisconnect(con)


#### sort out data file and merge ####


# temperature every other minute - maximum observed birds in this minute

# sort out format
dfj$time[minute(dfj$time) %% 2 == 1] = dfj$time[minute(dfj$time) %% 2 == 1]-60
adults$timedate = as.POSIXct(adults$timestamp, origin = "1970-01-01 00:00:00")
adults$minute = format(adults$timedate, "%Y-%m-%d %H:%M")

# per minute 
adultMin = aggregate(object_count ~ minute, data = adults, FUN = "max")
adultMin$time = as.POSIXct(paste(adultMin$minute, "00", sep = ":"))

# join with temp data 
temp_df = left_join(adultMin, temp_df, by = "time")


# add info on active breeding attempts 
# active_df data frame generated in the activeBreeders.R script file
temp_df$date = as.Date(temp_df$time)
temp_df = left_join(temp_df, active_df[active_df$shelf == "Farallon3",], by = c("date"))


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
sub = sub[!(sub$time %in% as.POSIXct(c(
  "2020-07-07 17:18:00",
  "2020-07-07 17:20:00",
  "2020-07-07 17:22:00",
  "2020-07-07 17:24:00"
  
))),]


# sort out response variables (three levels)
sub$presGroup = 1
sub$presGroup[sub$presence_perc == 1] = 0
sub$presGroup[sub$presence_perc < 1] = -1
sub$presGroup = as.integer(sub$presGroup+2)

# aggregate per rounded temperature
sub$temp_round = round(sub$temp_sun)



#### distribution of data as function of temperature ####

# table of observations per rounded temperature
tab = as.matrix(table(sub$temp_round, sub$presGroup), ncol = 3)

# divide by total number of observations per temperature group
pd = data.frame(tab/rowSums(tab))

# fix format temp variable
pd$Var1 = as.numeric(as.character(pd$Var1))

# fix naming of categories
dfx = data.frame(Var2 = 1:3, Cat = factor(c("fewer", "same", "more"), levels = c("fewer", "same", "more")))
pd$Cat = dfx[match(pd[,"Var2"], dfx[,"Var2"]), "Cat"]

# plot

cols = met.brewer("Nattier", 3) 

ggplot(data = subset(pd), aes(x = Var1, y = Freq*100, group = Cat, fill = Cat)) + 
  
  geom_area() + 
  scale_fill_manual(values = cols, name = "Birds present") +
  
  ylab("Percentage (%)") + xlab("Temperature (\u00B0C)") +
  
  theme_classic() +
  theme(legend.position = "bottom")


