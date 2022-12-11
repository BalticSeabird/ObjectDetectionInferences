# Video metadata 
# Read timestamps from videos to get full data frame of video times

dat = read.delim("data/farallon3_19-21_video_metadata.txt", colClasses = c("character", "POSIXct", "POSIXct"))
dat$elapse = -as.numeric(difftime(dat$start_time, dat$end_time, unit = "sec"))
dat$startt = as.numeric(dat$start_time)+120 # remove one minute because of rounding error
dat$endt = as.numeric(dat$end_time)-120 # remove one minute because of rounding error

# Full time sequence 
full_seq = vector()

for (i in 1:nrow(dat)) {
    seq1 = dat[i,"startt"]:dat[i,"endt"]
    full_seq = c(full_seq, seq1)
}

