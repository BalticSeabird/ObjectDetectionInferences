# Calculate number of active breeders at Farallon3 per minute in 2020

dat = read.csv("data/Farallon3_2020_activebreeding.csv", 
    colClasses = c("character", "POSIXct", "POSIXct"), 
    sep = ";")
dat$startt = as.numeric(dat$Start) # remove one minute because of rounding error
dat$endt = as.numeric(dat$End) # remove one minute because of rounding error

# Full time sequence 
full_seq = vector()

for (i in 1:nrow(dat)) {
    seq1 = dat[i,"Start"]:dat[i,"End"]
    full_seq = c(full_seq, seq1)
}

# Number of birds active per time unit
df_far3 = as.data.frame(table(full_seq))
