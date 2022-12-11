
## USE PREVIOUS DATA TO ASSIGN CHICKS TO TERRITORIES

# Read raw detection log and save only chicks 



dir =  "~/Library/CloudStorage/OneDrive-Sverigeslantbruksuniversitet/SeabirdAI/detection_logs/yolov4/detectionlog_1sec/"
folders = 2020:2021

for(y in folders) { # loop through years in study
  
  # Start of counter
  counter = 0
  
  # list all relevant files
  filedir = paste0(dir, y)
  files = list.files(filedir) 
  
  # remove size-0 files 
  files = files[file.size(paste0(filedir ,"/", files)) != 0]  
  
  # also remove files with incorrect time stamps in the file title
  subs = strsplit(files, "_")
  Date = unlist(lapply(subs, `[[`, 2))
  subs2 = strsplit(unlist(lapply(subs, `[[`, 3)), "\\.")
  Time = unlist(lapply(subs2, `[[`, 1))
  datetime = as.POSIXct(paste(Date, Time), format = "%Y%m%d %H%M%S")
  wrong = ifelse(is.na(datetime) | nchar(Time) != 6, 1, 0)
  files = files[wrong == 0]
  
  # set up dfs for storage
  results_chicks = data.frame()
  
  # load and bind all data from same year
  for(i in files){  # loop through each .csv file
    
    # Counter 
    counter = counter + 1
    
    # load and fix headers
    temp = read.csv(paste0(filedir, "/", (i)), header = FALSE)
    names(temp) = c("time", "x1", "y1", "w", "h", "classid", "confidence")
    
    # process chick data
    chicks = subset(temp, classid == 1 & confidence > .95)
    
    if(nrow(chicks) > 0){
      
      # save results (add on to the previous one)
      results_chicks = rbind(chicks, results_chicks) # avg no of birds on ledge per time step
      
    
    }
    
    # print progress with loop
    print(paste("Processed file", counter, "of", length(files), "in year", y, "(", round(100*counter/length(files), 1), "%)", sep = " "))
    
    
  }
  
  # Save
  write.csv(results_chicks, paste0(dir, "ChicksFull", y, ".csv"), row.names = FALSE)
  
}



# Read yolov 4 detection data
chicks19 = read.csv("~/Library/CloudStorage/OneDrive-Sverigeslantbruksuniversitet/SeabirdAI/detection_logs/yolov4/detectionlog_1sec/ChicksFull2019.csv")
chicks20 = read.csv("~/Library/CloudStorage/OneDrive-Sverigeslantbruksuniversitet/SeabirdAI/detection_logs/yolov4/detectionlog_1sec/ChicksFull2020.csv")
chicks21 = read.csv("~/Library/CloudStorage/OneDrive-Sverigeslantbruksuniversitet/SeabirdAI/detection_logs/yolov4/detectionlog_1sec/ChicksFull2021.csv")
chicks = rbind(chicks19, chicks20, chicks21)
chicks$Time = as.POSIXct(chicks$time, format = "%m/%d/%Y,%H:%M:%S")
rm(chicks19, chicks20, chicks21)

# Read previously annotated chick bounding boxes
chickterr = read.csv("aux_scripts_data/ChickOD_AssignedTerritories.csv")
chickterr$Date = as.Date(chickterr$Time)

# Combine original detections with annotation 
chickcomb = merge(chicks, chickterr, 
  by = c("Time", "w", "h"), all.x = TRUE)
chickcomb[is.na(chickcomb[,"terr"]),"terr"] <- 100
chickcomb$Date = as.Date(chickcomb$Time)

# Plot
pd1 = subset(chickcomb, Date == "2021-07-01")
ggplot() + geom_point(data = pd1, aes(x = x, y = y, colour = factor(terr)))

# Back-track decision rules by day and territory
terr_rules = aggregate(x ~ terr + Date, data = chickcomb, 
  FUN = function(x) c(min = min(x), max = max(x)))

# Save terr rules 
write.csv(terr_rules, "ChickTerritories.csv")


pd2 = subset(chickterr, Date == "2019-07-15")
ggplot() + geom_point(data = pd2, aes(x = x_center, 
  y = -y_center, colour = terr), alpha = .1) + ggtitle(dates[1]) + 
  theme(panel.grid.minor = element_line(color = "red",
                                        size = 0.2,
                                        linetype = 2)) +
  scale_x_continuous(breaks = seq(0, 1000, 50))
                

head(chickBBdf)


# Save plots 
terrdates = terrdates[substr(terrdates, 1, 4) == 2021]
for (i in 1:length(terrdates)) {

  pd1 = subset(new_chicks, Date == as.Date(terrdates[i]))
  ggplot() + geom_point(data = pd1, aes(x = x_center, y = -y_center, colour = PairID)) +
    ggtitle(pd1[1,"Date"]) +  scale_x_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) + 
      scale_y_continuous(limits = c(-1, 0))
                    
  ggsave(paste("ChicksTerrs", terrdates[i], ".jpg", sep = ""), 
      width = 30, height = 15, unit = "cm")
}


