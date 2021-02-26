###
# This file imports and descriptively explores GPS datasets.
# The raw data of each GPS collar is saved in a separate csv-file with a unique ID. The name of the 
# file is the name of the ID, which is also found in the first column of the study_duration.csv file
# (see below).
#
# The parameters descriptively explored per GPS unit are:
# - Battery capacity
# - projection of coordinates
# - time difference between consecutive GPS fixes
# - distance between between consecutive GPS fixes
# - speed between consecutive GPS fixes
# - number of GPS fixes per day 
# 
# First generated in 2013
##

## load packages

library(proj4)
library(sp)

## set directory and define results folders:

setwd ("XXX")
res_folder_graph <- "graphs"
res_folder_summs <- "summaries"
clean_tab_folder <- "tables_clean"

##### import data #######
# The study_dur file contains the ID, start and end time of the observation period per GPS collar
# with one line per collar
study_dur <- read.csv ("study_duration.csv",h=T)

## define fixed values:
long_TIMEDIFF <- 600   # time difference between two locations above which the difference is considered to be "too long"
max_speed <- 20        # max speed in km/h above which the dog was considered to be too fast and will therefore
                       # be excluded from the dataset as error  

# vector of all TIMEDIFFs of all collars in a row
TIMEDIFF_allCollars <- c()


#### start loop #######
# the loop goes through all collar ID (first column of study_dur) and calculates the
# summaries, produces the graphs and saves the final data of each collar as a new clean table

sapply (study_dur[,1], function (x) {

fileName <- paste (x)
dat <- read.csv(paste("files/",fileName,".csv",sep=""),h=T)
assign (fileName, dat, envir = .GlobalEnv) 

# creates a folder with the name "collarx" (x= number of the current collar) in the folder "graph"
# for saving the graphs in a better order
dirName <- paste(res_folder_graph,x,sep="/")
dir.create(dirName, recursive=T)             # recursive=T allows to produce subfolders as well


###
# TIME: 
###

time_col <- strptime(paste(dat$Date, dat$Time),                                 # original format: 25/07/2013 12:33:36
                     format = "%d/%m/%Y %H:%M:%S", tz = "CET")                  # transformed to the useful format   
dat$TIME <- paste(time_col)                                                                      

dat$TIMEDIFF <- rep(NA,length(dat$TIME))                                        # include an empty new column

for (i in 2: length(dat$TIME)) {                                                # new column TIMEDIFF expresses the time lag in sec 
dat$TIMEDIFF[i] <- difftime(dat$TIME[i], dat$TIME[i-1], unit="sec")             # from one location to the previous one
}

# define the time when collar was attached to (minTIME) and taken off from (maxTIME) the dog
current_minTIME <- paste(study_dur$start_time[which(study_dur$collarID == x)])
minTIME <- as.character(strptime(current_minTIME, format = "%d/%m/%Y %H:%M", tz = "CET"))

current_maxTIME <- paste(study_dur$end_time[which(study_dur$collarID == x)])
maxTIME <- as.character(strptime(current_maxTIME, format = "%d/%m/%Y %H:%M", tz = "CET"))

# analysis of the battery capacity; evaluate the very first and last record of the dataset and
# how long the battery was working
name_bat_cap <- paste("battery_cap_",x,sep="")
first_record <- min(dat$TIME)
last_record <- max(dat$TIME)
nb_records <- nrow(dat)
period <- difftime(last_record, first_record, unit="hour") 
bat_cap <- cbind (first_record, last_record, period, nb_records, records_per_hour = nb_records/as.double(period))
assign(name_bat_cap, bat_cap, envir = .GlobalEnv) 

# delete the recordings out of the range of minTIME - maxTIME and keep it as the new dataset
dat <- subset (dat, dat$TIME >= minTIME & dat$TIME <= maxTIME)
dat$TIMEDIFF[1] <- NA                  


## other exclusion criteria:
# Collar individual exclusion criteria based on time may be defined

###
# PROJECTION
### 

loc <- dat[,c("Longitude","Latitude")]              # extract the locations of the points (longitude, latitude)

### transformation of the coordinate system into .../Zone 53

# MGA 1994 AGM Zone 53; WKID: 28353: EPSG (https://www.spatialreference.org/ref/epsg/28353/)
loc_projected <- project(loc, "+proj=utm +zone=53 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
dat$proj_Lon <- projected$x                      # add projected lon to the data table
dat$proj_Lat <- projected$y                      # add projected lat to the data table

#  calculate the distances based on the projected data
dat$DIST <- rep(NA,nrow(dat))                       # add a new column of distance between a location to the previous one

for (i in 2:nrow(dat)){                                                 
 dat$DIST[i] <- sqrt((dat$proj_Lon[i]-dat$proj_Lon[i-1])^2 + 
                      (dat$proj_Lat[i]-dat$proj_Lat[i-1])^2)
 }

# explore the differences between the DIST of the projected data and those from the raw data (column "Distance")
differ <- sqrt((dat$DIST-dat$Distance)^2) 
summary(differ)                                         

# check the association between the distance itself and the differene betweent the two methods
name_graph1 <- paste("plot_difference_projections_",x,".tif",sep="")
tiff(file=paste(res_folder_graph,x,name_graph1, sep="/"))
par(mar=c(5,5,4,2))                  
plot(dat$DIST, differ, xlab="Distance based on projected data")   
title("differ = distance difference [m] between the projected data \n and the distance provided by the GPS unit", font.main=1)           
dev.off()


###
# SPEED: based on the column with the projected data (SPEED_2 based in the raw data "Distance")
###

## based on the projected data:
dat$SPEED <- 3.6*(dat$DIST / dat$TIMEDIFF)          # speed in km/h

# report locations with speed between two consecutive points > max_speed in a map                                                                
high_speed <- which(dat$SPEED>max_speed)  
                  
# "exclude" includes those data which have to be excluded because they have speed > 20km/h to the previous record
excludes <- unique(c(high_speed, high_speed-1) )
if (length(excludes)>0) {
dat <- dat[-excludes,]  } 

high_speed <- which (dat$SPEED>max_speed)  

# documentation of number of records excluded because of high speed
name_sum7 <- paste("nb_excluHighSpeed_",x,sep="")
sum7 <- length(excludes)
assign(name_sum7, sum7, envir = .GlobalEnv) 

# summary of the speed
name_sum4 <- paste("summary_SPEED_",x,sep="")
sum4 <- summary(dat$SPEED)
assign(name_sum4, sum4, envir = .GlobalEnv) 

# histogram of the speed
name_graph6 <- paste("SPEED_",x,".tif",sep="")
tiff(file=paste(res_folder_graph,x,name_graph6, sep="/"))
par(mfrow=c(1,1)) 
par(mar=c(5,5,5,2))  
hist(dat$SPEED, main="speed [km/h] between two consecutive locations", 
     xlab= paste(x), breaks=nrow(dat))
dev.off()


name_graph7 <- paste("high_SPEED_locs_",x,".tif",sep="")
tiff(file=paste(res_folder_graph,x,name_graph7, sep="/"))
par(mfrow=c(1,1))
par(mar=c(5,5,5,2)) 
plot(dat$Longitude, dat$Latitude, xlab="longitude", ylab="latitude")       

sapply (high_speed, 
function(i) {
  current_x1 <- dat$Longitude[i]
  current_y1 <- dat$Latitude[i]
  current_x2 <- dat$Longitude[i-1]
  current_y2 <- dat$Latitude[i-1]                                           
points(c(current_x1,current_x2),c(current_y1,current_y2), pch= 19, col=i)
})

title(main=paste("map of location pairs with speed > 20km/h between \n the two points, number of pairs=",length(high_speed),sep=" "),
       font.main=1)
dev.off()

# number of locations with speed > max_speed
name_sum5 <- paste("nb_highSpeed_",x,sep="")
sum5 <- length(high_speed)
assign(name_sum5, sum5, envir = .GlobalEnv) 

# number of locations with speed > max_speed
name_sum5 <- paste("nb_highSpeed_",x,sep="")
sum5 <- length(high_speed)
assign(name_sum5, sum5, envir = .GlobalEnv) 
       
         
###
# Distances between consecutive GPS fixes:
###

name_sum1 <- paste("summary_DIST_",x,sep="")
sum1 <- summary(dat$DIST)
assign(name_sum1, sum1, envir = .GlobalEnv) 

# total distance moved by the animal:
name_totDist <- paste("tot_DIST_",x,sep="")
tot_dist <- sum(dat$DIST,na.rm=T)
assign(name_totDist, tot_dist, envir = .GlobalEnv) 

# plot of DIST against TIMEDIFF
name_graph2 <- paste("plot_DIST_TIMEDIFF_",x,".tif",sep="")
tiff(file=paste(res_folder_graph,x,name_graph2, sep="/"))
par(mfrow=c(2,1)) 
par(mar=c(5,5,4,2))  
plot(dat$DIST,dat$TIMEDIFF, xlab= "distance between two consecutive locations",
     ylab= "time diff [sec]")  
title("time against space difference between two consecutive locations", font.main=1)           
plot(dat$DIST,dat$TIMEDIFF, ylim=c(0,long_TIMEDIFF),
      xlab= "distance between two consecutive locations", ylab= "time diff [sec]")
title("zoom for time difference up to 10 minutes", font.main=1) 
dev.off()


###
# TIME DIFF: analysis of the time difference between two points
###

# range of time difference 
name_sum2 <- paste("summary_TIMEDIFF_",x,sep="")
sum2 <- summary(dat$TIMEDIFF)
assign(name_sum2, sum2, envir = .GlobalEnv) 

name_graph3 <- paste("TIMEDIFF_",x,".tif",sep="")
tiff(file=paste(res_folder_graph,x,name_graph3, sep="/"))
par(mfrow=c(1,2)) 
par(mar=c(2,3,5,2))  
boxplot(dat$TIMEDIFF, main="time differences between \n two consecutive locations")
boxplot(dat$TIMEDIFF, ylim=c(0,long_TIMEDIFF), main="zoom for time \n difference up to 10 minutes")                 
dev.off()

# explore the locations and distances between points with long periods without recording
loc_long_TIMEDIFF <- which(dat$TIMEDIFF> long_TIMEDIFF)                         

name_sum3 <- paste("summary_DIST_long_TIMEDIFF_",x,sep="")
sum3 <- summary(dat$DIST[loc_long_TIMEDIFF])
assign(name_sum3, sum3, envir = .GlobalEnv) 

if (length(loc_long_TIMEDIFF) > 0) {

name_graph4 <- paste("DIST_longTIMEDIFF_",x,".tif",sep="")
tiff(file=paste(res_folder_graph,x,name_graph4, sep="/"))
par(mfrow=c(1,1)) 
par(mar=c(5,5,5,2))  
hist(dat$DIST[loc_long_TIMEDIFF], breaks=length(loc_long_TIMEDIFF), xlab="meters", main=NULL)
title(main= paste("distances between two consecutive locations with \n more than 10 minutes in between, n=", length(loc_long_TIMEDIFF),sep=" "),
      font.main=1)
dev.off()
}

# production of a map that depict the points with long time lags between each other
long_TIMEDIFF_0 <- which(dat$TIMEDIFF> long_TIMEDIFF & dat$DIST ==0)     # GPS fixes with DIST=0 between each other
long_TIMEDIFF_no0 <- which(dat$TIMEDIFF> long_TIMEDIFF & dat$DIST !=0)   # GPS fixes with DIST>0 between each other

name_graph5 <- paste("map_longTIMEDIFF_locs_",x,".tif",sep="")
tiff(file=paste(res_folder_graph,x,name_graph5, sep="/"))
par(mfrow=c(1,1)) 
par(mar=c(5,5,5,2))  

plot(dat$Longitude, dat$Latitude, xlab="longitude", ylab="latitude")

for (i in long_TIMEDIFF_0) {
  current_x1 <- dat$Longitude[i]
  current_y1 <- dat$Latitude[i]
  current_x2 <- dat$Longitude[i-1]
  current_y2 <- dat$Latitude[i-1]
  points(c(current_x1,current_x2),c(current_y1,current_y2), pch= 8, cex=1.2, col="red")
}

a <- 2                                                                          # a = counter to change the symbols and collars of                                                                                #     the point pairs
for (i in long_TIMEDIFF_no0) {
  current_x1 <- dat$Longitude[i]
  current_y1 <- dat$Latitude[i]
  current_x2 <- dat$Longitude[i-1]
  current_y2 <- dat$Latitude[i-1]
  a<- a+1
  if (a+12 < 26) {n= a+12} else { n=a+12-11}
  points(c(current_x1,current_x2),c(current_y1,current_y2), pch= n, cex=1.2, col=a, bg=a)
}
title(main="map of location pairs with > 10 min between each other \n red plus = location pair at the same position",
       font.main=1)
dev.off()

# number of locations with speed > max_speed
name_sum6 <- paste("nb_longTIMEDIFF_",x,sep="")
sum6 <- length(loc_long_TIMEDIFF)
assign(name_sum6, sum6, envir = .GlobalEnv) 

###
# number of GPS fixes per day
###

# extract the first day of the collar attached and the last day of the collar taken off
first_day <- study_dur$Date.On[order(format(as.Date(study_dur$Date.On, "%d/%m/%Y"),"%d%m%Y"))[1]]
last_day <- study_dur$Date.Off[order(format(as.Date(study_dur$Date.Off, "%d/%m/%Y"),"%d%m%Y"))[length(study_dur$Date.Off)]]

# build a vector with all dates from the first to the last day
dates <- c(as.Date(first_day,"%d/%m/%Y") : as.Date(last_day,"%d/%m/%Y"))    # gives a vector with number of days since the "origin"
dates <- as.Date(dates, origin = "1970-01-01")                              # "origin" had to be defined
dates <- format(dates, "%d/%m/%Y")                                          # format used in our database

# loop through all dates, extract the number of GPS fixes belonging to each date,
# collate them in different columns 
nb_locs_day <- c()
sapply(dates, function (day) {
  cur_day <- length(which(dat$Date==day))
  nb_locs_day <<- cbind(nb_locs_day,cur_day)
  })

# add the total number of locations at the end of the table in a new column
nb_locs <- cbind(nb_locs_day, nrow(dat))
colnames(nb_locs) <- c(dates, "locs_tot") 
# assign the current collar as row name 
name_nb_locations <- paste ("nb_locations_",x,sep="")
assign(name_nb_locations, nb_locs, envir = .GlobalEnv) 


## compile all graphs from one dog into one file

name_allGraphs <- paste("allGraphs_",x,".pdf",sep="")
pdf(file=paste(res_folder_graph,x,name_allGraphs, sep="/"), width = 8, height = 8)
# layout() arranges the graphs while the same number in the vector signify the same
layout(matrix(c(1,1,2,3,4,4,1,1,2,3,5,5,6,6,7,7,8,8,6,6,7,7,8,8,9,9,10,10,11,11,9,9,10,10,11,11), 6, 6, byrow = TRUE))
par(mar=c(5, 4, 4, 2) + 0.1 )
# graph 1; "differ" has to be recalculated based in the new data
differ <- sqrt((dat$DIST-dat$Distance)^2) 
plot(dat$DIST, differ, xlab="Distance based on projected data")
title("differ = distance difference [m] between \n the projected data and the distance \n provided by the GPS unit", cex.main=0.8)
# graph 2
boxplot(dat$TIMEDIFF, main="time differences \n between two \n consecutive locations", cex.main=0.8)
# graph 3
boxplot(dat$TIMEDIFF, ylim=c(0,long_TIMEDIFF), main="zoom for time \n difference up \n to 10 minutes",  cex.main=0.8)
# graph 4
plot(dat$DIST,dat$TIMEDIFF, xlab= "distance between two consecutive locations",
     ylab= "time diff [sec]")
title("time against space difference \n between two consecutive locations", cex.main=0.8)
# graph 5
plot(dat$DIST,dat$TIMEDIFF, ylim=c(0,long_TIMEDIFF),
      xlab= "distance between two consecutive locations", ylab= "time diff [sec]")
title("zoom for time difference \n up to 10 minutes", cex.main=0.8)
# graph 6
if (length(loc_long_TIMEDIFF) > 0) {
hist(dat$DIST[loc_long_TIMEDIFF], breaks=length(loc_long_TIMEDIFF), xlab="meters", main=NULL)
title(main= paste("distances between two consecutive \n locations with more than 10 \n minutes in between, n=", length(loc_long_TIMEDIFF),sep=" "),
      cex.main=0.8)
}   else {
plot(0, type="n", xlab="", ylab="", axes=FALSE, main="no consecutive locations\n  with more than 10 minutues in between", cex.main=0.8) }
# graph 7
plot(dat$Longitude, dat$Latitude, xlab="longitude", ylab="latitude")
for (i in long_TIMEDIFF_0) {
  current_x1 <- dat$Longitude[i]
  current_y1 <- dat$Latitude[i]
  current_x2 <- dat$Longitude[i-1]
  current_y2 <- dat$Latitude[i-1]
  points(c(current_x1,current_x2),c(current_y1,current_y2), pch= 8, cex=1.2, col="red")
}
a <- 2                                               # a = counter to change the symbols of collars                                                                                 #     the point pairs
for (i in long_TIMEDIFF_no0) {
  current_x1 <- dat$Longitude[i]
  current_y1 <- dat$Latitude[i]
  current_x2 <- dat$Longitude[i-1]
  current_y2 <- dat$Latitude[i-1]
  a<- a+1
  if (a+12 < 26) {n= a+12} else { n=a+12-11}
  points(c(current_x1,current_x2),c(current_y1,current_y2), pch= n, cex=1.2, col=a, bg=a)
}
title(main="map of location pairs with > 10 min \n between each other; red plus = location \n pair at the same position",
       cex.main=0.8)
# graph 8
plot(0, type="n", xlab="", ylab="", axes=FALSE)
# graph 9
hist(dat$SPEED, main="speed [km/h] between \n two consecutive locations", cex.main=0.8,
     xlab= paste(x), breaks=nrow(dat))
# graph 10
plot(dat$Longitude, dat$Latitude, xlab="longitude", ylab="latitude")
sapply (high_speed,
function(i) {
  current_x1 <- dat$Longitude[i]
  current_y1 <- dat$Latitude[i]
  current_x2 <- dat$Longitude[i-1]
  current_y2 <- dat$Latitude[i-1]
points(c(current_x1,current_x2),c(current_y1,current_y2), pch= 19, col=i)
})
title(main=paste("map of location pairs with \n speed > 20km/h between the two points, \n number of pairs=",length(high_speed),sep=" "),
       cex.main=0.8)

dev.off()


## save the current collar data as a new table for further analysis
dir.create(clean_tab_folder)          
write.csv(dat, file=paste(clean_tab_folder,"/",fileName,"_clean.csv",sep=""), row.names=F)

})
#### end of the loop


### saving of the summaries including all collar data
dir.create(res_folder_summs)          
# saving the summaries of the DIST of all collars in one file
DIST_summs <- c()
row_names <- c()
sapply (study_dur[,1], function (i) {
file_name <- paste("summary_DIST_",i,sep="")
DIST_summs <<- rbind(DIST_summs, eval(parse(text = file_name)))
row_names <<- c(row_names, paste(i))
})

# adding the sum of all distances in a separate column
tot_DIST <- c()
sapply (study_dur[,1], function (i) {
file_name_totDist <- paste("tot_DIST_",i,sep="")
tot_DIST <<- cbind(tot_DIST, eval(parse(text = file_name_totDist)))
})

DIST_summs <- data.frame(DIST_summs)
DIST_summs <- cbind(DIST_summs, tot_dist_moved= t(tot_DIST))

write.csv(DIST_summs, file=paste(res_folder_summs,"/summs_DIST.csv",sep=""),
          row.names=row_names)

# saving the summaries of the TIMEDIFF of all collars in one file          
TIMEDIFF_summs <- c()
row_names <- c()
sapply (study_dur[,1], function (i) {
file_name <- paste("summary_TIMEDIFF_",i,sep="")
TIMEDIFF_summs <<- rbind(TIMEDIFF_summs, eval(parse(text = file_name)))
row_names <<- c(row_names, paste(i))
})
write.csv(TIMEDIFF_summs, file=paste(res_folder_summs,"/summs_TIMEDIFF.csv",sep=""),
          row.names=row_names)
          
# saving the summaries of the DIST for those runs with long TIMEDIFF (i.e. > 10 min) 
Dist_longDIFF_summs <- c()
row_names <- c()
sapply (study_dur[,1], function (i) {
file_name <- paste("summary_DIST_long_TIMEDIFF_",i,sep="")
Dist_longDIFF_summs <<- rbind(Dist_longDIFF_summs, eval(parse(text = file_name)))
row_names <<- c(row_names, paste(i))
})
write.csv(Dist_longDIFF_summs, file=paste(res_folder_summs,"/summs_DIST_longTIMEDIFF.csv",sep=""),
          row.names=row_names)
          
# saving the summaries of the DIST for those runs with long TIMEDIFF (i.e. > 10 min) 
SPEED_summs <- c()
row_names <- c()
sapply (study_dur[,1], function (i) {
file_name <- paste("summary_SPEED_",i,sep="")
SPEED_summs <<- rbind(SPEED_summs, eval(parse(text = file_name)))
row_names <<- c(row_names, paste(i))
})
write.csv(SPEED_summs, file=paste(res_folder_summs,"/summs_SPEED.csv",sep=""),
          row.names=row_names)
          
# save the number of points with speed > max_speed
highSpeeds <- c()
row_names <- c()
sapply (study_dur[,1], function (i) {
file_name <- paste("nb_highSpeed_",i,sep="")     
highSpeeds <<- rbind(highSpeeds, eval(parse(text= file_name)))
row_names <<- c(row_names, paste(i))
})
write.csv(highSpeeds, file=paste(res_folder_summs,"/nb_highSpeeds.csv",sep=""),
          row.names=row_names)
          
# save the number of points with TIMEDIFF > 10 min
longTIMEDIFF <- c()
row_names <- c()
sapply (study_dur[,1], function (i) {
file_name <- paste("nb_longTIMEDIFF_",i,sep="")     
longTIMEDIFF <<- rbind(longTIMEDIFF, eval(parse(text= file_name)))
row_names <<- c(row_names, paste(i))
})
write.csv(longTIMEDIFF, file=paste(res_folder_summs,"/nb_longTIMEDIFF.csv",sep=""),
          row.names=row_names)

# save the number of points per day
nb_locs <- c()
row_names <- c()
sapply (study_dur[,1], function (i) {
file_name <- paste("nb_locations_",i,sep="")     
nb_locs <<- rbind(nb_locs, eval(parse(text= file_name)))
row_names <<- c(row_names, paste(i))
})
write.csv(nb_locs, file=paste(res_folder_summs,"/nb_locs_per_day.csv",sep=""),
          row.names=row_names)
          
# save the data of the battery capacity
bat_cap <- c()
row_names <- c()
sapply (study_dur[,1], function (i) {
file_name <- paste("battery_cap_",i,sep="")     
bat_cap <<- rbind(bat_cap, eval(parse(text= file_name)))
row_names <<- c(row_names, paste(i))
})
write.csv(bat_cap, file=paste(res_folder_summs,"/battery_capacity.csv",sep=""),
          row.names=row_names)

                    
# save the number of points excluded because of speed > max_speed
excludes <- c()
row_names <- c()
sapply (study_dur[,1], function (i) {
file_name <- paste("nb_excluHighSpeed_",i,sep="")     
excludes <<- rbind(excludes, eval(parse(text= file_name)))
row_names <<- c(row_names, paste(i))
})
write.csv(excludes, file=paste(res_folder_summs,"/nb_excluHighSpeed.csv",sep=""),
          row.names=row_names)
          
          