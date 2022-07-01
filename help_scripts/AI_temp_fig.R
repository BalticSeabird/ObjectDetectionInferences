# **************************************** #
# AI attendance in relation to temperature # 
# **************************************** #


#### load temperature data ####

temp_df = read.csv("Data/Temperature_StoraKarlso.csv", sep=";")
names(temp_df) = c("date", "temp_sun", "temp_shade")

temp_df$time = as.POSIXlt(temp_df$date)
temp_df$date = NULL



# Read attendance data 
dfj = subset(adults, Yr == 2020)

#### sort out data file and merge ####


# add temperature data
# temperature every other minute - maximum observed birds in this time
dfj$time[minute(dfj$time) %% 2 == 1] = dfj$time[minute(dfj$time) %% 2 == 1]-60


# Per minute 
dfj$minute = format(dfj$time, "%Y-%m-%d %H:%M")
dfj2 = aggregate(birds ~ minute, data = dfj, FUN = "max")
dfj2$time = as.POSIXct(paste(dfj2$minute, "00", sep = ":"))


# Join with temp data 
dfj2 = left_join(dfj2, temp_df, by = "time")

# add info on active breeding attempts 
dfj2$date = as.Date(dfj2$time)
dfj2 = left_join(dfj2, active_df[active_df$shelf == "Farallon3",], by = c("date"))



#### model attendance ~ temperature ####

# subset to data with at least one active breeding attempt and existing temperature data
sub = dfj2[dfj2$present > 0 & !is.na(dfj2$temp_sun),]

# fix variables
sub$presence_perc = sub$birds/sub$present # AI birds divided by active attempts
sub$yday = format(sub$time, "%j")
sub$hour = as.numeric(format(sub$time, "%H"))
sub$year = format(sub$date, "%Y")

# subset to period used in dygnsstudier
sub = sub[sub$hour %in% 15:20,]

# subset to days on which 50% of breeding attempts active 
sub = sub[sub$present/max(dfj2$present, na.rm = T) >= 0.5,]

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


#### plot ####

cols = met.brewer("Nattier", 3) #

# violin plot
p1 = ggplot(sub, aes(x=temp_sun, y=as.factor(presGroup), fill = as.factor(presGroup))) + 
  geom_violin() +
  scale_fill_manual(labels = c("<1 adult per\negg/chick","1 adult per\negg/chick",">1 adult per\negg/chick"), values = alpha(cols, 0.8)) +
  scale_y_discrete(labels = c("<1 adult per\negg/chick","1 adult per\negg/chick",">1 adult per\negg/chick")) + 
  labs(x = "Temperature (\u00B0C)", y = "Attendance", fill = "") +
  theme_classic() +
  theme(legend.key.size = unit(3,"line"),
        legend.position = "none")

# add mean and SD
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}
p1 = p1 + stat_summary(fun.data=data_summary)


# comparison with manually collected data
df_temp = read.csv("Data/ds_attendance_temp.csv")

p2 = ggplot(df_temp, aes(x=temp_sun, y=as.factor(presence), fill = as.factor(presence))) + 
  geom_violin() +
  scale_fill_manual(labels = c("0 parents per\negg/chick","1 parent per\negg/chick","2 parents per\negg/chick"), values = alpha(cols, 0.8)) +
  scale_y_discrete(labels = c("0 parents per\negg/chick","1 parent per\negg/chick","2 parents per\negg/chick")) + 
  labs(x = "Temperature (\u00B0C)", y = "Attendance", fill = "") +
  theme_classic() +
  theme(legend.key.size = unit(3,"line"),
        legend.position = "none")

# add mean and SD
p2 = p2 + stat_summary(fun.data=data_summary)

cowplot::plot_grid(p1, p2, ncol = 2, labels = c("a.", "b."), label_fontface = "plain")

ggsave("figures/FigAI_TempEffect2021.jpg", width = 18.5, height = 12, units = "cm")


