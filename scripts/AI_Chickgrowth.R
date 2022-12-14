# *************** #
# AI chick growth
# *************** #

# Required libraries 
library(RSQLite)
library(plyr)
library(ggplot2)
library(MetBrewer)
library(drc)

## connect to db with Object Detection results 
con <- dbConnect(drv=RSQLite::SQLite(), 
    dbname="aux_data/FARALLON3_m_960.db")


## Read from db
time = Sys.time()
Chicks <- dbGetQuery(conn=con, 
    statement=
      "SELECT timestamp, x_center, y_center, width, height
      FROM pred 
      WHERE class = 1")
Sys.time()-time

# Date format 
Chicks$Time = as.POSIXct(Chicks$timestamp, origin = "1970-01-01")

# Day 
Chicks$Date = as.Date(Chicks$Time)

# Read chick territory locations and assign chick detections to territories
chickterr = read.csv("aux_data/ChickTerritoryLocations.csv", sep = ";")
terrdates = unique(as.Date(chickterr$Date))
new_chicks = data.frame()
for (i in 1:length(terrdates)) {
  terr1 = subset(chickterr, Date == terrdates[i])
  terr1 = terr1[order(terr1[,"xmin"]),]
  missing = data.frame(xmin = c(-.1, terr1$xmax, 1.1))
  terr2 = merge(terr1, missing, by = "xmin", all.x = TRUE, all.y = TRUE)
  cuts = terr2$xmin
  levels = terr2[,"Territory"]
  testdat = subset(Chicks, Date == terrdates[i])
  terr_temp = levels[cut(testdat$x_center, cuts)]
  testdat$PairID = terr_temp
  new_chicks = rbind(new_chicks, testdat)
}


# Max dimension of chick size (width or height)
new_chicks$maxdim = apply(new_chicks[,c("width", "height")], 1, FUN = "max")

# Read data on chick age 
chickage = read.csv("data/AukLabBreedingDataCleaned.csv", 
  colClasses = c("factor", "integer", "Date"))

# Link detections to chick age 
new_chicks$HatchDate = chickage[match(new_chicks[,"PairID"], chickage[,"PairID"]),"HatchDate"]
new_chicks$Age = as.numeric(new_chicks$Date - as.Date(new_chicks$HatchDate))

# Hourly age of chicks 
new_chicks$Hour = as.numeric(format(new_chicks$Time, "%H"))
new_chicks$H2 = 0.01+new_chicks$Hour/24
new_chicks$Age2 = new_chicks$Age + new_chicks$H2
new_chicks = subset(new_chicks, Age > 0)

# Aggregate chick size by hour
chicksize = aggregate(maxdim ~ PairID + Age2, data = new_chicks, FUN = "mean")


## Run growth model for chicks on hourly data 
# Asymptotic regression, see:  https://www.statforbiology.com/articles/usefulequations/

# set up dataframe for storage
pred_data = data.frame()

# list chick IDs
chickids = unique(chicksize$PairID)

# loop through chick IDs
for (i in chickids) {         #JONAS CHANGED
  
  # subset to right chick and to relevant data
  dat_temp = subset(chicksize, PairID == (i))
  
  # fit model of width as a function of age
  model = drm(maxdim ~ Age2, 
              data = dat_temp, fct = llogistic())
  
  # make model prediction over full age range
  pred_x = min(dat_temp$Age2):max(dat_temp$Age2)
  pred_y = predict(model, data.frame(Age2 = pred_x))
  
  # store data
  pred_data_temp = data.frame(PairID = (i), 
                              Age2 = pred_x, 
                              maxdim = pred_y)  
  pred_data = rbind(pred_data, pred_data_temp)
  
  rm(pred_x, pred_y)
  
}


### PLOT OF BOUNDING BOX WIDTH BY AGE ###

# Sample 10 random chicks to plot (remove 1...)
plotchicks = sample(chickids, 10)

# annotation layers with labels
annot = data.frame(PairID = plotchicks, x = 10, y = 0.06) 
annot2 = data.frame(PairID = "Farallon-3-2019-1", x = 5, y = .14, text = "a.") 



# Generate plot
pd1 = chicksize[(chicksize[,"PairID"] %in% plotchicks),]
pd2 = pred_data[(pred_data[,"PairID"] %in% plotchicks),]
p1 = ggplot() + 
  geom_point(data = pd1, # data aggregated per hour
             aes(x = Age2, y = maxdim, group = PairID), alpha = .4, shape = 20, stroke = 0) + 
  geom_line(data = pd2, # model predictions
            aes(x = Age2, y = maxdim, group = PairID)) + 
  geom_text(data = annot, aes(x = x, y = y, group = PairID, label = PairID), size = 2) + 
  facet_wrap(~PairID, ncol = 5) + 
  scale_y_continuous(name = "Bounding box max dimension \n(rel. frame size)", 
    limits = c(0.05, 0.15)) + 
  scale_x_continuous(name = "Age (days)") + 
  geom_text(data = annot2, aes(x = x, y = y, label = text), size = 5) +
  theme_classic() + 
  theme(legend.position = "none", strip.background = element_blank(), strip.text.x = element_blank())  


### PLOT OF FIELD DATA ON WEIGHT ###

# load weight data
chick_weights = read.csv("data/RingingInfoChicks.csv", sep = ";")

# calculate mean weight per age group
chickw = aggregate(list(weight = chick_weights$Vikt), 
                   by = list(Age = chick_weights$Age), 
                   mean)


# fit growth model to average data
model = drm(weight ~ Age, data = chickw, fct = llogistic())


# make predictions from model
ageRange = seq(min(chickw$Age), max(chickw$Age), 0.1)
predval = data.frame(Age = ageRange, 
                     pred = predict(model, 
                      newdata = data.frame(Age = ageRange)))

# plot of growth curve in field data on chick weights
p2 = ggplot() + 
  geom_line(data = predval, aes(x = Age, y = pred), size = 1) + # predictions from model
  geom_point(data = chick_weights, aes(y = Vikt, x = Age), alpha = .5, size = 2)  + # raw data
  scale_x_continuous(name = "Age (days)") + 
  scale_y_continuous(name = "Weight (g)") +
  theme_classic()


# calculate median size per chick - regardless of ID
chicksize_age = aggregate(maxdim ~ Age, data = new_chicks, FUN = "median")


# combine weight data and bounding box size data
chicksize_age$Weight = chickw[match(chicksize_age$Age, chickw$Age), "weight"]

# fit linear model
lm_mod = lm(Weight ~ maxdim, data = subset(chicksize_age, Age > 0))
summary(lm_mod)

# plot of measured weight against the maximum dimension (width or height)
p3 = ggplot(data = chicksize_age, 
            aes(x = Weight, y = maxdim)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, col = "black") +
  scale_x_continuous(name = "Field observations (chick weight [g])") + 
  scale_y_continuous(name = "Object detection\n (bounding box size [rel. frame size])") +
  theme_classic()  



### COMBINE PLOTS FOR PAPER ###
px = cowplot::plot_grid(p2, p3, nrow = 1, labels = c("b.", "c."), label_fontface = "plain", label_size = 13, label_x = .3)
cowplot::plot_grid(p1, px, nrow = 2)
ggsave("figures/FigAI_ChickGrowth.jpg", width = 3.5*5, height = 3.5*5, units = "cm")

