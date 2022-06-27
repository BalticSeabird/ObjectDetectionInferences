# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#### code AI guillemot paper ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# authors: Jonas Hentati Sundberg (jonas.sundberg@slu.se) & Agnes Olin
# 16 May 2022


## load libraries ##
library(aomisc) # R.squared for nls, available at github (dec 2021). Also for asymptotic regression (type van berthalanffy) # devtools::install_github("onofriAndreaPG/aomisc")
library(dplyr) # 1.0.7
library(ggplot2) # 3.3.5
library(lubridate) # 1.7.10
library(MetBrewer) # 0.2.0
library(plyr) # 1.8.6
library(reshape2) # 1.4.4

#### set working directory ####
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/data/work - synced/research/seabird/AI dygnsstudie")


#### load processed AI data ####

#source("help_scripts/AI_CompileDetectionlog.R") # this processes the AI detection files but takes a while so have also saved processed data

#### READ ADULT ATTENDENCE DATA ####
t1 = read.csv("Data/AttendanceAdults2019.csv")
t2 = read.csv("Data/AttendanceAdults2020.csv")
t3 = read.csv("Data/AttendanceAdults2021.csv")
adults = rbind(t1, t2, t3)
rm(t1, t2, t3)

# Remove a few duplicate time stamps from the detection log files  
adults = adults[!duplicated(adults[,"time"]),] 


## fix time formats
adults$time = as.POSIXct(adults$time)
adults$Yr = format(adults$time, "%Y")
adults$j = format(adults$time, "%j")



#### READ CHICK ATTENDENCE DATA ####
t1 = read.csv("Data/Chicks2019.csv")
t2 = read.csv("Data/Chicks2020.csv")
t3 = read.csv("Data/Chicks2021.csv")
chicks = rbind(t1, t2, t3)
rm(t1, t2, t3)


#### calculate no of active breeders per day ####
source("ObjectDetectionInference/help_scripts/activeBreeders.R")


#### FIGURE 3 ####

source("ObjectDetectionInference/help_scripts/AI_AttendanceBreeding.R")



#### FIGURE 4 ####

source("ObjectDetectionInference/help_scripts/AI_DisturbanceEvents.R")



#### FIGURE 5 ####

source("ObjectDetectionInference/help_scripts/AI_chickgrowth.R")



#### FIGURE 6 ####

source("ObjectDetectionInference/help_scripts/AI_temp_fig.R")
