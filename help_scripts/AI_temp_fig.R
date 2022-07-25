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
# active_df data frame generated in the activeBreeders.R script file
dfj2$date = as.Date(dfj2$time)
dfj2 = left_join(dfj2, active_df[active_df$shelf == "Farallon3",], by = c("date"))



#### calculate attendance ~ temperature ####

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

p1 = ggplot(data = subset(pd), aes(x = Var1, y = Freq*100, group = Cat, fill = Cat)) + 
  
  geom_area() + 
  scale_fill_manual(values = cols, name = "Birds present") +
  
  ylab("Percentage (%)") + xlab("Temperature (\u00B0C)") +
  
  theme_classic() +
  theme(legend.position = "bottom")






#### add in comparison data ####

# load data
df_temp_comp = read.csv("Data/ds_attendance_temp.csv")

# subset to same year and ledge
df_temp_comp = df_temp_comp[df_temp_comp$ledge == "Farallon3" & df_temp_comp$year == 2020, ] 

# determine whether at least one parent absent per time stamp
MANtemp = aggregate(presence ~ time, df_temp_comp, function(x) sum(x == 0))
MANtemp$presence2 = as.numeric(MANtemp$presence > 0)

# make temperature data frame and merge
temps = df_temp_comp[, c("time", "temp_sun")]
MANtemp  = merge(MANtemp , temps, by = "time", all.x = T, all.y = F)

# round temperature
MANtemp$temp_round = round(MANtemp$temp_sun)
MANtemp = MANtemp[!is.na(MANtemp$temp_sun),]

# aggregate per rounded temperature
MANtemp = aggregate(presence2 ~ temp_round, MANtemp, function(x) sum(x == 1)/length(x)) 


#### merge data frames and plot ####
temp_comp = merge(ODtemp, MANtemp, by = "temp_round")

# remove temperature levels with very gew observations
temp_comp = temp_comp[temp_comp$temp_round %in% 13:47,]

# plot
p2 = ggplot(data = temp_comp, aes(x = presence2, y = presGroup, colour = temp_round)) +
  geom_point(size = 2) +
  scale_colour_gradientn(colours = rev(met.brewer("Greek")), name = "Temperature (\u00B0C)") +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  labs(x = "Manual observations",
       y = "Object detection") +
  lims(x = c(0,1), y = c(0,1)) +
  theme_classic() +
  theme(legend.position = "bottom")




cowplot::plot_grid(p1, p2, ncol = 2, labels = c("a.", "b."), label_fontface = "plain", align = "h")

ggsave("figures/FigAI_TempEffect2020.jpg", width = 18.5, height = 11, units = "cm")



