# ************************************************ #
# AI attendance in relation to breeding activities # 
# ************************************************ #


### adult attendance in relation to active breeding ###

# Required libraries 
library(RSQLite)
library(plyr)
library(ggplot2)


# Read from database
con <- dbConnect(drv=RSQLite::SQLite(), 
    dbname="data/FARALLON3_small_640.db")

## Read from db
time = Sys.time()
Adults <- dbGetQuery(conn=con, 
    statement=
      "SELECT timestamp, object_count 
      FROM pred 
      WHERE class = 0
      AND score > .7
      LIMIT 1000000000")
Sys.time()-time


## Save only number of birds
Adults = aggregate(object_count ~ timestamp, data = Adults, FUN = "max")

## AI data

# median no birds per hour
adults$H = as.vector(round.POSIXt(adults$time, "hours"))
medianH = aggregate(list(birdmed = adults$birds), 
                    by = list(time = as.character(adults$H)), 
                    function(x) median(x, na.rm = TRUE))

# fix date formats
medianH$hour = as.numeric(substr(medianH$time, 12, 13))
medianH$Yr = as.numeric(substr(medianH$time, 1, 4))
medianH$time = as.POSIXct(medianH$time)


# subset to middle of night and correct years (IR light not functioning in 2019)
pd = subset(medianH, hour %in% c(00) & Yr %in% 2020:2021) # 00 is the best

# fix time formats
pd$Time = as.POSIXct(pd$time)
pd$Day = as.numeric(format(pd$time, "%j"))

# add zeros before and after (if no birds are present no data exists)
expand_full = data.frame(Day = rep(120:210, each = 2), Yr = c(2020, 2021))
pd = merge(pd, expand_full, all = T)
pd$birdmed[is.na(pd$birdmed)] = 0

## parallel field data

pd2 = subset(active_df, shelf == "Farallon3" & year(date) %in% 2020:2021)
pd2$Day = yday(pd2$date)
pd2$Yr = year(pd2$date)

## plot for paper ##

# annotation layer
annot = data.frame(Yr = c(2020, 2021), x = c(135, 135), y = c(10, 10), lab = c("a.", "b."))

# make plot
p1 = ggplot() + 
  geom_line(data = pd2, aes(x = Day, y = present), size = 4, col = "lightgrey") + 
  geom_line(data = pd, aes(x = Day, y = birdmed)) + 
  facet_wrap(~Yr, ncol = 3) + 
  scale_x_continuous(name = "", limits = c(120, 217)) + 
  scale_y_continuous(name = "Number of adults/pairs", breaks = c(0, 2, 4, 6, 8, 10), labels = c(0, 2, 4, 6, 8, 10)) + 
  geom_text(data = annot, aes(x = x, y = y, label = lab), size = 4) +
  theme_classic() + 
  theme(strip.background = element_blank(), strip.text.x = element_blank())  


# look at correlation
comp = merge(pd, pd2, by = c("Yr", "Day"))
cor(comp$present[comp$Yr == 2020], comp$bird[comp$Yr == 2020])
cor(comp$present[comp$Yr == 2021], comp$bird[comp$Yr == 2021])




#### chicks ####

# create subset for relevant period
chicksub = subset(chicks, yday(time) %in% 135:210 & year(time) %in% 2020:2021)

# date formats etc 
chicksub$Yr = year(chicksub$time)
chicksub$yday = yday(chicksub$time)

# calculate average no of observations per frame per day and year
ch = aggregate(chicks ~ yday + Yr, data = chicksub, mean)


## make plot of chick detections ##

# arrows for plotting (timings from parallel breeding data)
arr = data.frame(x = c(173, 196, 167, 192), 
                 xend = c(173, 196, 167, 192), 
                 y = rep(-.2, 2.5), 
                 yend = rep(-.02, 2.5), 
                 Yr = c(2020, 2020, 2021, 2021))

# annotation layer
annot = data.frame(Yr = c(2020, 2021), x = c(135, 140), y = c(2.5, 2.5), lab = c("e.", "f."))

# number of chick detections per frame
p3 = ggplot(data = ch[ch$Yr != 2019,], aes(x = yday, y = chicks)) + 
  geom_line() + 
  facet_wrap(~Yr) + 
  scale_x_continuous(name = "Day of the year") + 
  scale_y_continuous(name = "Number of chick detections per frame") + 
  geom_segment(data = arr, aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(length = unit(0.1, "cm"))) +
  geom_text(data = annot, aes(x = x, y = y, label = lab), size = 4) +
  theme_classic() + 
  theme(strip.background = element_blank(), 
        strip.text.x = element_blank())  


# Correlation between AI and field observations 
comb = merge(pd, pd2, by = c("Yr", "Day"))
annot = data.frame(Yr = c(2020, 2021), x = c(1, 1), y = c(7, 7), lab = c("c.", "d."))

p2 = ggplot(data = comb, aes(x = birdmed, y = present)) + 
  geom_point(alpha = .2) + 
  facet_wrap(~Yr) + 
  scale_x_continuous(name = "Field observations", breaks = seq(0, 12, 2)) + 
  scale_y_continuous(name = "Object detection", breaks = seq(0, 12, 2)) + 
  theme_classic() + 
  theme(strip.background = element_blank(), 
        strip.text.x = element_blank())  +
  geom_abline(linetype = "solid") + 
  geom_smooth(method = "lm", se = FALSE, linetype = "dotted") + 
  geom_text(data = annot, aes(x = x, y = y, label = lab), size = 4) 
  


# p1 comes from script file AI_AdultAttendenceBreeding.R
cowplot::plot_grid(p1, p2, p3, ncol = 1)



ggsave("figures/FigAI_AdultsandChicks.jpg", width = 3.5*5, height = 5*5, units = "cm")


# numbers for text
chicksub$chickperiod = NA

chicksub$chickperiod[chicksub$Yr == 2021 & chicksub$yday < 167 | chicksub$yday > 192 ] = 0
chicksub$chickperiod[chicksub$Yr == 2021 & chicksub$yday > 167 & chicksub$yday < 192] = 1
chicksub$chickperiod[chicksub$Yr == 2020 & chicksub$yday < 173 | chicksub$yday > 196 ] = 0
chicksub$chickperiod[chicksub$Yr == 2020 & chicksub$yday > 173 & chicksub$yday < 196 ] = 1

chicksub$presence = chicksub$chicks > 0
aggregate(presence ~ chickperiod, data = chicksub, mean)


