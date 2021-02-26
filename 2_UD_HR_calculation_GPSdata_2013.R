###
# This code import GPS dataset from the folder tables_clean (after running Cleaning_descriptives_GPSdata_2013.R code)
# and:
#   - generates maps with tracks of the dogs
#   - calculates the Utiliztion Distribution and home range size of different isoploeth levels
#     using the BRB method 
#
# Code first generated in 2013
###

## load packages ans set directory

library (adehabitatHR)
library (sp)
library(png)
library(RgoogleMaps)

setwd ("XXX")

# The study_dur file contains the ID, start and end time of the observation period per GPS collar
# with one line per collar
study_dur <- read.csv ("study_duration.csv",h=T)

# import all datasets (one file per collar) from the tables_clean folder and asign the name of the
# study_dur table ID to the table and save it in the global environment
sapply (study_dur[,1], function (x) {

fileName <- paste (x, "clean", sep="_")
dat <- read.csv(paste("tables_clean/",fileName,".csv",sep=""),h=T)
assign (paste(x), dat, envir = .GlobalEnv)
})


## convert the data into a SpatialPointsDataFrame (used by adehabitatHR)

# the first unit has to be set up to the "locs" object outside the loop
# to first produce a locs object
x <- study_dur[1,1]
current_unit <- eval(parse(text=paste(x)))         # this is needed to asign the actual table to y
current_col_id <- rep(paste(x), nrow(current_unit))
locs <- data.frame(current_col_id)
coordinates(locs) <- rbind(current_unit[,c("proj_Lon","proj_Lat")])

# after that, all the other units are attached to the first collar to finally have a data.frame "locs"
# with all units included
sapply (study_dur[,1], function (x) {

if (x != study_dur[1,1]) {                           # this loop is used to skip the first collar because already included in "locs"
  current_unit <- eval(parse(text=paste(x)))         # this is needed to asign the actual table to y
  current_col_id <- rep(paste(x), nrow(current_unit))

  current_locs <- data.frame(current_col_id)
  coordinates(current_locs) <- rbind(current_unit[,c("proj_Lon","proj_Lat")])

  locs <<- rbind(locs, current_locs)
}
})


class(locs)
#[1] "SpatialPointsDataFrame"
#attr(,"package")
#[1] "sp"

## depicting the locations of the data with all dogs, plots the locations with a different color for each animal
plot(locs, col=as.data.frame(locs)[,1])

dir.create("maps", recursive=T)
pdf(file="maps/locations.pdf")
plot(locs, col=as.data.frame(locs)[,1])
dev.off()

## map of the communites with the locations
center1 = c(Long,Lat)    # Long and Lat are the cooridnates of the center of the map
Location <- GetMap(center=center1, zoom=15, destfile = "maps/Maps.png", maptype=c("hybrid"), NEWMAP=T)

sapply(study_dur[,1], function (x) {

current_unit <- eval(parse(text=paste(x)))   
current_lats <- current_unit[,"Latitude"]
current_lons <- current_unit[,"Longitude"]

file_name <- paste("maps/",x,".png",sep="")
png(file= file_name)
PlotOnStaticMap(Location, lat=current_lats, lon=current_lons, pch=20, cex=1, FUN=lines, col="red")
legend ("topright", legend=paste(study_dur$Lot[which(study_dur$collarID==x)]), bg="white")
dev.off()
})



####
# Calculation of the home range 
####

library(adehabitatLT)
library (adehabitatHR)

# collate all collars together in one big table all_col
all_col <- data.frame()
sapply (study_dur[,1], function (x) {
current_col <- eval(parse(text=paste(x)))
all_col <<- rbind(all_col, current_col)
})

# add a column that specifies the ID of the collar:
col_id <- c()
sapply (study_dur[,1], function (x) {
current_col_name <- paste(x)
col_id <<- c(col_id, rep (current_col_name, nrow(eval(parse(text=current_col_name)))))    
})

all_col$id <- col_id


## transformation into data of class ltraj
# 2 functions to convert the time into the right class "POSIXct" used for as.ltraj
da <- strptime(as.character(all_col[,"TIME"]), "%Y-%m-%d %H:%M:%S")
da <- as.POSIXct(da)
tr_col <- as.ltraj(xy = all_col[,c("proj_Lon","proj_Lat")], date = da, id=all_col[,"id"])

# have a look at tr_col
tr_col     # list of the collars in the dataset with their start and end time and the number of observations 
colnames(tr_col[[1]])
# [1] "x"         "y"         "date"      "dx"        "dy"        "dist"      "dt"        "R2n"       "abs.angle"
# [10] "rel.angle"


# plots the trajects of the collars
dir.create("LT_graphs", recursive=T)
tiff(file="LT_graphs/trajectories.tiff")
par(xpd=TRUE)
plot(tr_col)
dev.off()

# plots of the distance moved over time to identify time lags of data recording
sapply(study_dur[,1], function (x) {

current_unit <- eval(parse(text=paste(x)))  
file_name <- paste(x)

tiff(file= paste ("LT_graphs/time_gaps", file_name, ".tiff", sep=""))
par(mar=c(5,4,4,1))
plotltr(tr_col[x])  
title(paste(x)) 
dev.off()

tiff(file=paste ("LT_graphs/trajects", file_name, ".tiff", sep=""))
plot(tr_col[x], xlab = "x", ylab = "y")
title(paste(x)) 
legend("bottomright",legend=c("Start", "End"), col=c("blue","red"), pch=c(2,0),bty="n")
dev.off()

})



## BRB method, see also adehabitatHR packge and a nice tutorial at
# https://mran.microsoft.com/snapshot/2017-12-11/web/packages/adehabitatHR/vignettes/adehabitatHR.pdf

BRB_D <- BRB.D(tr_col, Tmax=6*60, Lmin=45)
BRB_UD <- BRB(tr_col, D = BRB_D, Tmax = 6*60, Lmin = 45, hmin=18, type = "UD", 
              filtershort= F)
          
image (BRB_UD)


# A) vector mode calculation of the HR levels (getverticeshr):
sapply(1:25, function (x) {
current_collar_name <- paste(study_dur[x,1])
for (i in seq (50,95,by=5)) {
Name <- paste("BRB_UD_level",i,"_",current_collar_name,sep="")
kernUD_i <- getverticeshr(BRB_UD[[x]], percent=i)                                               
assign (Name, kernUD_i, envir=.GlobalEnv) }
})

# sum up into one table
BRB_HR_levels <- c()
for (i in seq (50,95,by=5)) { 

BRB_level_i_HR <- c()

sapply (study_dur[,1], function (x) {

cur_col <- paste (x)
cur_file_name <- paste("BRB_UD_level",i,"_", cur_col, sep="")
cur_BRB_level_i_HR <- eval(parse(text=cur_file_name))[[2]]

BRB_level_i_HR <<- rbind (BRB_level_i_HR, cur_BRB_level_i_HR)
})

BRB_HR_levels <<- cbind (BRB_HR_levels, BRB_level_i_HR)
}

dir.create("LT_summaries", recursive=T)
write.table(BRB_HR_levels, file="LT_summaries/BRB_HR_vectorMode.csv", row.names=paste(study_dur[,1]))

 
# B) raster mode calculation of the HR levels (getvolume, kernel.area):
# calculation of the levels of HR for the UD calculated by BRB
areas_BRB <- kernel.area(BRB_UD, percent=seq(50, 95, by=5))
colnames(areas_BRB) <- study_dur[,1]
write.csv(areas_BRB, file="LT_summaries/HR_levels_BRB_rasterMode.csv")

# get the volumes of the BRB kernels with methods a and b for D estimation
volUD_BRB <- getvolumeUD(BRB_UD)

# depict the volUD_BRB
pdf(file="LT_graphs/volUD_all.pdf")
par(mfrow=c(5, 5))
par(mar=c(0,0,2,0))
par(oma=c(0,0,3,0))
sapply (1:25, function (i) {
image(volUD_BRB[[i]])
xyzv_BRB <- as.image.SpatialGridDataFrame(volUD_BRB[[i]])
contour(xyzv_BRB, add=TRUE, levels = c(50,95,99)) 
title(paste(study_dur[i,1]))
})                   
title("volumes of BRB UDs", outer=T, cex.main=1.8)
dev.off()



