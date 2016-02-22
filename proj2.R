setwd("C:/Columbia Courses/Visualization/Project2")

library(RNetCDF)

noaa = open.nc('NOAA_Daily_phi_500mb.nc')

# Read geological data
data = read.nc(noaa)

# plot one 
library(fields)
library(maptools)

xlon = data$X
ylat = rev(data$Y)
z = data$phi[,,1]

# set transparency
add.alpha = function(COLORS, ALPHA){
  if(missing(ALPHA)) stop("provide a value for alpha between 0 and 1")
  RGB = col2rgb(COLORS, alpha=TRUE)
  RGB[4,] = round(RGB[4,]*ALPHA)
  NEW.COLORS = rgb(RGB[1,], RGB[2,], RGB[3,], RGB[4,], maxColorValue = 255)
  return(NEW.COLORS)
}
pal = colorRampPalette(c(rgb(0,0,1), rgb(0,1,0), rgb(1,0,0)))
COLORS = add.alpha(pal(100), 0.6)

# map data
data(wrld_simpl)
plot(wrld_simpl)
image.plot(xlon-180,ylat,z,add=TRUE, col = COLORS)

# flood data
master = read.csv("GlobalFloodsRecordMaster.csv", as.is = TRUE)
stat = read.csv("GlobalFloodsRecordAnalyses.csv", as.is = TRUE)

master$Began = as.Date(master$Began,format = "%d-%b-%y")
master$Ended = as.Date(master$Ended,format = "%d-%b-%y")

# check the distribution of begin dates
library(dplyr)
date = data.frame(date = master$Began,cnt = 1)
group = group_by(date,date)
summ = summarise(group,cnt = n())
summ$date[summ$cnt == max(summ$cnt)-1]

# found that "1998-05-20" and "2002-06-12" is the most
maxDate = summ$date[summ$cnt == max(summ$cnt)-1][1]
phi3 = as.numeric(maxDate-as.Date("1948-01-01"))+1

xlon = data$X
ylat = rev(data$Y)
z = data$phi[,,phi3]

# find the data of the floods that day
floodDay = master[master$Began==maxDate & is.na(master$Began)==0,]

# set transparency
add.alpha = function(COLORS, ALPHA){
  if(missing(ALPHA)) stop("provide a value for alpha between 0 and 1")
  RGB = col2rgb(COLORS, alpha=TRUE)
  RGB[4,] = round(RGB[4,]*ALPHA)
  NEW.COLORS = rgb(RGB[1,], RGB[2,], RGB[3,], RGB[4,], maxColorValue = 255)
  return(NEW.COLORS)
}
pal = colorRampPalette(c(rgb(0,0,1), rgb(0,1,0), rgb(1,0,0)))
COLORS = add.alpha(pal(100), 0.6)

# map data
data(wrld_simpl)
plot(wrld_simpl)
image.plot(xlon-180,ylat,z,add=TRUE, col = COLORS)
points(floodDay$Centroid.X, floodDay$Centroid.Y, pch = 20, col = "red")
