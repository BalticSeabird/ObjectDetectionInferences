# ******************************************************* #
# Compile CSV.files from Detection log
# ******************************************************* #



for(y in 2019:2021) { # loop through years in study
  
  # Start of counter
  counter = 0
  
    # list all relevant files
  filedir = paste0("detectionlog_1sec/", y)
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
  results_attendance = data.frame()
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
      
      # calculate number of chicks per time step
      attendance = aggregate(data = chicks, classid ~ time, length)
      names(attendance) = c("time", "chicks")
      attendance$time = as.POSIXct(attendance$time, format = "%m/%d/%Y, %H:%M:%OS")
      
      # generate sequence of time points so that instances with no detections are caught
      start = attendance$time[1]
      end = attendance$time[nrow(attendance)]
      full = data.frame(time = seq(start, end, "sec"))
      
      # merge together
      results = merge(attendance, full, by = "time", all = TRUE)
      results$chicks[is.na(results$chicks)] = 0
      
      # save results (add on to the previous one)
      results_chicks = rbind(results, results_chicks) # avg no of birds on ledge per time step

    }
    
    
    # Adults, confidence level 0.99
    ads = subset(temp, classid == 0 & confidence > .99)
    
    if(nrow(ads) > 0){
      
      # calculate number of adults per time step
      attendance = aggregate(data = ads, classid ~ time, length)
      names(attendance) = c("time", "birds")
      attendance$time = as.POSIXct(attendance$time, format = "%m/%d/%Y, %H:%M:%OS")
      
      # generate sequence of time points so that instances with no detections are caught
      start = attendance$time[1]
      end = attendance$time[nrow(attendance)]
      full = data.frame(time = seq(start, end, "sec"))
      
      # merge together
      results = merge(attendance, full, by = "time", all = TRUE)
      results$birds[is.na(results$birds)] = 0
      
      # Original file name
      results$filename = (i)
      
      # save results (add on to the previous one)
      results_attendance = rbind(results, results_attendance) # avg no of birds on ledge per time step
     
      # print progress with loop
      print(paste("Processed file", counter, "of", length(files), "in year", y, "(", round(100*counter/length(files), 1), "%)", sep = " "))
      
      
    }
      }
  
  
  # sort by time
  results_chicks = results_chicks[order(results_chicks$time, decreasing = FALSE),]
  results_attendance = results_attendance[order(results_attendance$time, decreasing = FALSE),]

  # missing data?
  #timediff_chicks = c(1, diff(results_chicks$time))
  #timediff_adults = c(1, diff(results_attendance$time))
  
  # save data
  write.csv(results_chicks, paste0("Data/Chicks", y, ".csv"), row.names = F)
  write.csv(results_attendance, paste0("Data/AttendanceAdults", y, ".csv"), row.names = F)
  
}