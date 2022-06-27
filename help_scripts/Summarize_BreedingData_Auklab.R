# **************************** #
# clean up field breeding data 
# **************************** #

# load breeding data
dat = read.delim("Data/BreedingDataAuklabUntil2021.txt", na.strings = "")
dat = dat[,1:18]
unique(dat$PairID)


# sort out formats
dat$EggDate = as.POSIXlt(dat$EggDate)
dat$HatchDate = as.POSIXlt(dat$HatchDate)
dat$ChickGoneDate = as.POSIXlt(dat$ChickGoneDate)
dat$EggLossDate = as.POSIXlt(dat$EggLossDate)
dat$PairID = gsub("\xf6", "o", dat$PairID) # Byta ö mot o
dat$PairID = gsub("\x9a", "o", dat$PairID) # Byta ø mot o

# summary of breeding activity per pair
dat$pair_attempts = paste(dat$PairID, dat$BrAttempt, sep = "-")
dat$pair_attempts = gsub("-", "_", dat$pair_attempts)

# add ledge info
subs = strsplit(dat$pair_attempts, "_")
dat$LedgeSec = unlist(lapply(subs, `[[`, 1))
dat$LedgeNum = unlist(lapply(subs, `[[`, 2))

# make some time variables
dat$Yr = unlist(lapply(subs, `[[`, 3))
dat$PairNum = unlist(lapply(subs, `[[`, 4))
dat$AttemptNum = unlist(lapply(subs, `[[`, 5))
dat$Ledge = paste(dat$LedgeSec, dat$LedgeNum, sep = "-")
dat$HatchDay = format(dat$HatchDate, "%j")
dat$GoneDay = format(dat$ChickGoneDate, "%j")

# remove weird one with wonky dates
dat = dat[-368,]

# empty data frame to store data
df = data.frame()

for (i in 1:nrow(dat)) { # loop through data frame
  d = dat[i, ] # subset to row
  
  eggend = ifelse(!(is.na(as.character(d$HatchDate))), as.character(d$HatchDate), ifelse(!is.na(as.character(d$EggLossDate)), as.character(d$EggLossDate), as.character(d$EggDate))) # eggend stops at hatching or egg loss
  eggdays = seq(d$EggDate, as.POSIXlt(eggend), "days") # create sequence of egg days
  
  if(!is.na(as.character(d$HatchDate))) {
    chickend = ifelse(!is.na(as.character(d$ChickGoneDate)), as.character(d$ChickGoneDate), as.character(d$HatchDate)) # chickend at fledging or loss
    chickdays = seq(d$HatchDate, as.POSIXlt(chickend), "days")
  }  else { chickdays = NA } 
  
  df_out = data.frame(Pair_attempt = d$pair_attempts, rbind(data.frame(Class = 2, Date = eggdays), data.frame(Class = 1, Date = chickdays)))
  df = rbind(df, df_out)
}



## summarise breeding activity per ledge ##

# sort out variable names
subs = strsplit(df$Pair_attempt, "_")
df$LedgeSec = unlist(lapply(subs, `[[`, 1))
df$LedgeNum = unlist(lapply(subs, `[[`, 2))
df$Yr = unlist(lapply(subs, `[[`, 3))
df$PairNum = unlist(lapply(subs, `[[`, 4))
df$AttemptNum = unlist(lapply(subs, `[[`, 5))
df$Ledge = paste(df$LedgeSec, df$LedgeNum, sep = "-")
df$Day = as.numeric(format(df$Date, "%j"))


# summarise number of active attempt per ledge 
sumledge = aggregate(list(n = df$Yr), 
                     by = list(Yr = df$Yr, Ledge = df$Ledge, Date = df$Date, Day = df$Day, Class = df$Class), 
                     length)

# reshape to put chick and egg in separate columns
sumledge2 = dcast(sumledge,  Yr + Date + Day + Ledge ~ Class, value.var = "n")
sumledge2[is.na(sumledge2)] <- 0
sumledge2$n = rowSums(sumledge2[,5:6])
colnames(sumledge2)[5:6] <- c("Chicks", "Eggs")



