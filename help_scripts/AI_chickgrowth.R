# *************** #
# AI chick growth
# *************** #

## get data on chick age for the different terrirories
source("help_scripts/Summarize_BreedingData_Auklab.R")

## load and clean up data on chick bounding boxes and territory assignments ##

# load data
chickBBdf = read.csv("Data/ChickOD_AssignedTerritories.csv")

# create new ID column
chickBBdf$ChickID = paste(chickBBdf$Yr, chickBBdf$terr)

# sort out time formats
chickBBdf$Time = as.POSIXct(chickBBdf$Time)
chickBBdf$Hour = round(chickBBdf$Time, "hours")
chickBBdf$Hournum = as.numeric(format(chickBBdf$Hour, "%H"))
chickBBdf$Time2 = chickBBdf$Day+chickBBdf$Hournum
chickBBdf$Day = as.numeric(chickBBdf$Day)

# max size (any dimension for chick) 
chickBBdf$maxdim = apply(chickBBdf[,c("w", "h")], 1, max)  


## combine object detection data with info on chick age ##

# subset breeding data to correct ledge
matchdata = subset(dat, Ledge == "Farallon-3")

# extract hatching day
chickBBdf$HatchDay = as.numeric(matchdata[match(paste(chickBBdf[,"Yr"],
                                                      chickBBdf[,"terr"]), 
                                                paste(matchdata[,"Yr"], 
                                                      matchdata[,"PairNum"])),"HatchDay"])

# calculate age of chick
chickBBdf$ChickAge = chickBBdf$Day - chickBBdf$HatchDay + 1


## summarize data for each chick by hour ##

# subset to certain detections
data_to_aggregate = subset(chickBBdf, confidence > .99)

# calculate mean values per hour
chickstats = aggregate(maxdim ~ ChickID + ChickAge + Hournum, 
                       FUN = function(x) c("mean" = mean(x), "n" = length(x)), 
                       data = data_to_aggregate)

chickstats$n = chickstats$maxdim[,"n"]
chickstats$maxdim = chickstats$maxdim[,"mean"]


## run growth model on hourly data ##

# asymptotic regression, see:  https://www.statforbiology.com/articles/usefulequations/

# set up dataframe for storage
pred_data = data.frame()

# list chick IDs
chickids = unique(chickstats$ChickID)

# loop through chick IDs
for (i in chickids) {         #JONAS CHANGED
  
  # subset to right chick and to relevant data
  dat_temp = subset(chickstats, 
                    ChickID == (i) & 
                      ChickAge > -1 & 
                      maxdim > 49)
  
  # fit model of width as a function of age
  model = drm(maxdim ~ ChickAge, 
              data = dat_temp, fct = llogistic())
  
  

  # make model prediction over full age range
  pred_x = min(dat_temp$ChickAge):max(dat_temp$ChickAge)
  pred_y = predict(model, data.frame(ChickAge = pred_x))
  
  # store data
  pred_data_temp = data.frame(ChickID = (i), 
                              ChickAge = pred_x, 
                              maxdim = pred_y)  
  pred_data = rbind(pred_data, pred_data_temp)
  
  rm(pred_x, pred_y)
  
}



### PLOT OF BOUNDING BOX WIDTH BY AGE ###

# annotation layers with labels
annot = data.frame(ChickID = chickids, x = 5, y = 60) 
annot2 = data.frame(ChickID = "2019 1", x = 7, y = 170, text = "a.") 


# plot of bounding box widths as a function of age
p1 = ggplot() + 
  geom_point(data = subset(chickstats, maxdim > 49), # raw data aggregated per hour
             aes(x = ChickAge, y = maxdim, group = ChickID), alpha = .4, shape = 20, stroke = 0) + 
  geom_line(data = pred_data, # model predictions
            aes(x = ChickAge, y = maxdim, group = ChickID)) + 
  geom_text(data = annot, aes(x = x, y = y, group = ChickID, label = ChickID), size = 2) + 
  facet_wrap(~ChickID, ncol = 5) + 
  scale_y_continuous(name = "Bounding box max dimension (pixels)") + 
  scale_x_continuous(name = "Age (days)") + 
  geom_text(data = annot2, aes(x = x, y = y, label = text), size = 5) +
  theme_classic() + 
  theme(legend.position = "none", strip.background = element_blank(), strip.text.x = element_blank())  


### PLOTS OF FIELD DATA ON WEIGHT ###

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
chicksize_age = aggregate(maxdim ~ ChickAge, data = data_to_aggregate, FUN = "median")


# combine weight data and bounding box size data
chicksize_age$Weight = chickw[match(chicksize_age$ChickAge, chickw$Age), "weight"]

# fit linear model
lm_mod = lm(Weight ~ maxdim, data = subset(chicksize_age, ChickAge > 0))
summary(lm_mod)

# plot of measured weight against the maximum dimension (width or height)
p3 = ggplot(data = subset(chicksize_age, ChickAge > 0), 
            aes(y = Weight, x = maxdim)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, col = "black") +
  scale_y_continuous(name = "Weight (g)") + 
  scale_x_continuous(name = "Max dimension of bounding box (pixels)") +
  theme_classic()  



### COMBINE PLOTS FOR PAPER ###
px = cowplot::plot_grid(p2, p3, nrow = 1, labels = c("b.", "c."), label_fontface = "plain", label_size = 13, label_x = .3)
cowplot::plot_grid(p1, px, nrow = 2)
ggsave("figures/FigAI_ChickGrowth.jpg", width = 3.5*5, height = 3.5*5, units = "cm")





