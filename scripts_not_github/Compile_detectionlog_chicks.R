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
    
     
  # Save
  write.csv(results_chicks, paste0(dir, "ChicksFull", y, ".csv"), row.names = FALSE)
  
}
  }

