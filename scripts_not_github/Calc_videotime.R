

# Calculate when there is data based on time stamp and file size

files = paste("files", 19:21, ".txt", sep = "")

df = data.frame()
for (file in files) {
    f = read.table(paste("aux_scripts_data", file, sep = "/"), skip = 2)
    f = f[1:(nrow(f)-1),c("V9", "V5")]
    df = rbind(df, f)
}


df$starttime = as.POSIXct(substr(df$V9, 11, 25), format = "%Y%m%d_%H%M%S")
pd1 = subset(df, V5 > 2090000000)
hist(pd1$vidlength/60)

# Based on the video from 2019-05-02, 05:23:06 (exactly 30 min)
# Calculate number of bytes per second
bps = 478324244/(30*60)

# Length of video 
df$vidlength = df$V5/bps

# End time of videos 
df$end = df$starttime+(df$vidlength*1.031)
df = df[!is.na(df[,"starttime"]),]
df$diff = NA
df$diff_r = NA

# Look at jump between start and end in whole data set
for (i in 2:nrow(df)) {
    df[(i), "diff"] = as.numeric(difftime(df[(i), "starttime"], 
        df[(i-1), "end"], units = "secs"))
}


output = subset(df, diff < -1000 | diff_r < -1000, select = -c(V5, vidlength, diff_r))
output[,c("V9", "starttime", "end", "diff")]
write.csv(output, "Wrong_timestamps.csv")

subset(df, diff < -1000, select = -c(V9, V5))
subset(df, starttime > "2019-07-10" & starttime < "2019-07-12",
     select = -c(V9, V5))

hist(subset(df, diff < 1000)$diff)
df = df[order(df$starttime),]
subset(df, diff > 200)
head(subset(df, starttime > "2019-08-03"))

# Get complete time vector 
timevec = data.frame()
for (row in 1:nrow(df)) {
    vec = seq(df[row, "starttime"], 
        df[row, "end"], by = "min") 
    file = df[row, "V9"] 
    timevec = rbind(timevec, data.frame(diff = diff, file = file))
}

# Time diff between periods (minutes)
timevec$diff = c(0, diff(fullvec$times))

pd1 = subset(fullvec, diff < 10 & diff > -10)
hist(pd1$diff)

subset(fullvec, diff %in% -500:-200)

