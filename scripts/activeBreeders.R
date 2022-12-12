
#### calculating active breeders ####

# load breeding data
df = read.delim("data/BreedingDataAuklabUntil2021.txt")
df = df[, 1:18]


# sort out ledge info and subset to ledges and years of interest
PairIds = strsplit(df$PairID, "-")

df$keep = FALSE; df$shelf = NA; df$year = NA

for(i in 1:nrow(df)){
  if(
    PairIds[[i]][3] %in% c("2020", "2021") &
    PairIds[[i]][2] %in% c("3") &
    PairIds[[i]][1] != c("Bonden")
  ) df$keep[i] = TRUE
  df$shelf[i] = paste0(PairIds[[i]][1], PairIds[[i]][2])
  df$year[i] = PairIds[[i]][3]
  
}
df = df[df$keep == TRUE, ]

# sort out dates (end date = end of breeding attempt)
df$EggDate = as.Date(df$EggDate)
df$endDate = as.Date(df$ChickGoneDate)
df$endDate[df$ChickGoneDate == ""] = as.Date(df$EggLossDate[df$ChickGoneDate == ""])


# create list of no of active breeding attempts
shelves = unique(df$shelf)
dates = as.Date(1:(365*2), origin = "2020-01-01")

active_df = data.frame(
  date = rep(dates, length(shelves)),
  shelf = rep(shelves, each = length(dates))
  
)
active_df$present = 0

for(i in 1:nrow(df)){ # loop through all breeding records
  
  for(j in 1:nrow(active_df)){ # loop through list of dates and ledges
    if(active_df$shelf[j] == df$shelf[i] & # match w ledge
       active_df$date[j] >= df$EggDate[i] & # date is equal or more than start of breeding attempt
       active_df$date[j]  <= df$endDate[i]){ # date is equal or less than end of breeding attempt
      active_df$present[j] = active_df$present[j]+1 # add presence
      
    }
  
  }

}
bs_data = df

rm(df, PairIds, dates, i, j, shelves)

