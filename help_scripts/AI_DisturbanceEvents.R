
# ******************************************************* #
# AI Adult Detect Disturbance Events
# ******************************************************* #

# summarise to max birds per minute
adults$min = as.character(round(adults$time, "mins"))
perMIN = aggregate(birds ~ min, data = adults, FUN = "max")
perMIN$min = as.POSIXct(perMIN$min)
perMIN$Yr = year(perMIN$min)

# change in numbers between consecutive minutes 
perMIN$dbirds = c(0, diff(perMIN$birds))

# only include consecutive observations
perMIN = perMIN[order(perMIN$min),]
perMIN$timediff = c(1, diff(perMIN$min))
perMIN = subset(perMIN, timediff == 1)

# subset to same days each year
ddply(adults, .(Yr), summarize, max = max(j), min = min(j))
perMIN = subset(perMIN, yday(perMIN$min) %in% 131:187)

# how many disturbances per year?
disturbances = ddply(perMIN, .(Yr), summarize, disturbance = sum(dbirds < -3 ))
disturbances

# over all minutes, during what proportion do we observe a disturbance?
1000* disturbances/ddply(perMIN, .(Yr), summarize, observations = length(dbirds))

# subset to cases where at least 4 birds left and make df for plotting
perMIN = subset(perMIN, dbirds < -3)
perMIN = ddply(perMIN, .(Yr, dbirds), summarize, num = length(dbirds))
full = expand.grid(Yr = 2019:2021, dbirds = -10:-4)
perMIN = merge(perMIN, full, by = 1:2, all.y = TRUE)
perMIN[is.na(perMIN)] = 0
perMIN$Yr = as.factor(perMIN$Yr)


# comparison data

comp_data = 
  as.data.frame(
rbind(
  cbind(2019, 4.100000,  49.20000, 1.423518),
  
  cbind(2020,      12.933333, 230.40000, 7.251814),
  
  cbind(2021,      2.285714,  43.14286, 1.258483)
))
names(comp_data) = c("year", "DistNum", "DistTime", "DistMagn")
comp_data$year = as.factor(comp_data$year)

p1 = ggplot(comp_data, aes(x = year, y = DistMagn, group = year, fill = year)) + 
  geom_bar(color = "black", stat = "identity", position = "dodge")  +
  xlab("Year") + 
  ylab("Disturbance magnitude (change in nr of birds)") + 
  scale_fill_manual(values = met.brewer("Demuth", 3), name = "")  + 
  theme_classic() + 
  theme(legend.position = "none")


# plot and save
p2 = ggplot(perMIN, aes(x = -dbirds, y = num, group = Yr, fill = Yr)) + 
  geom_bar(color = "black", stat = "identity", position = "dodge") + 
  scale_x_continuous(name = "Disturbance magnitude (change in nr of birds)", breaks = 4:10, labels = 4:10) + 
  scale_y_continuous(name = "Number of events") + 
  scale_fill_manual(values = met.brewer("Demuth", 3), name = "") + 
  theme_classic() + 
  theme(legend.position = c(0.7, 0.7))

cowplot::plot_grid(p1, p2, ncol = 2, labels = c("a.", "b."), label_fontface = "plain")

# add labels + weird legend thing

ggsave("figures/FigAI_Disturb.jpg", width = 18.5, height = 11, units = "cm")



