##################################################################################

# Extract Extended Reconstucted SST data for Sitka Sound from netcdf file 
# Adapted from script obtained from Dr. Franz Mueter

# CL Roberts

# Data source: http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.ersst.v5.html
# Find monthly mean surface temperatures and download or download directly from:
# https://downloads.psl.noaa.gov/Datasets/noaa.ersst.v5/ 
#  This is global monthly data and the data file should be something like
# 'sst.mnmean.nc' (~ 90 MB as of 2021, rename as necessary)

# outputs clean SST data file to be used in recruitment models

##################################################################################

library(ncdf4)
library(maps)
library(mapdata)
library(chron)
library(tidyverse) # Contains the ggplot2 and dplyr packages
library(PBSmapping) # From which we'll use the clipPolys function
library(marmap) # From which we get bathymetry
library(ggforce)


## Extract SST data for desired locations:
# Here, I'm selecting SST data for two regions in the Bering Sea
nc <- nc_open("recruitment_comparisons/data/sst.mnmean.nc")

# Examine data description:
nc
dat <- ncvar_get(nc)
dim(dat)  # 180 longitudes by 89 latitude bands by 2028 monthly values

# Time vector is mid-date of each month:
d <- dates(ncvar_get(nc, "time"), origin=c(1,15,0))
head(d)
tail(d)

# Pick start and end dates:
ST <- 1453  # start time January 14 1975
ET <- 2028 # end time December 15 2021 
d[ST]; d[ET]

## 1. Select latitude and longitude (based on centers of grid points):
# Extract 56 North, 136W (Jan 1975 - present):

# Longitude and Latitude are stored in intervals of 2 degrees, 
# Sitka Sound is approximately  57 N 135.5 W
# So the closest grid point is 56 N 136 W

# Longitude given as degrees W

# Select one longitude of 136 degrees W
xindex <- 113
x1 <- 360 - ncvar_get(nc, "lon", start=xindex, count=1)
# and one latitude of 56 degrees N
yindex <- 17
y1 <- ncvar_get(nc, "lat", start=yindex, count=1)
# x1; y1

# Extract the corresponding SST values:
dat1 <- t(ncvar_get(nc, "sst", start=c(xindex,yindex,1), count=c(1,1,-1), verbose = F))


# Add center of grid points
dimnames(dat1) <- list(paste("N", y1, "W", x1, sep=""), NULL)

# # Extract 58 North, 172W to 160W:
# x2 <- 360-ncvar_get(nc, "lon", start=95, count=7)
# y2 <- ncvar_get(nc, "lat", start=16, count=1)
# #x2; y2
# dat2 <- t(ncvar_get(nc, "sst", start=c(95,16,ST), count=c(7,1,-1), verbose = F))
# dimnames(dat2) <- list(NULL, paste("N", y2, "W", x2, sep=""))

# # Extract 56 North, 166W to 162W:
# x3 <- 360-ncvar_get(nc, "lon", start=98, count=3)
# y3 <- ncvar_get(nc, "lat", start=17, count=1)
# #x3; y3
# dat3 <- t(ncvar_get(nc, "sst", start=c(98,17,ST), count=c(3,1,-1), verbose = F))
# dimnames(dat3) <- list(NULL, paste("N", y3, "W", x3, sep=""))

# dat.S <- cbind(dat3, dat2, dat1)
# head(dat.S)

# # Extract 62 North, 174W to 166W (Jan 1900 - present):
# x4 <- 360-ncvar_get(nc, "lon", start=94, count=5)
# y4 <- ncvar_get(nc, "lat", start=14, count=1)
# #x4; y4
# dat4 <- t(ncvar_get(nc, "sst", start=c(94,14,ST), count=c(5,1,-1), verbose = F))
# dimnames(dat4) <- list(NULL, paste("N", y4, "W", x4, sep=""))

# # Extract 64 North, 172W to 166W (Jan 1900 - present):
# x5 <- 360-ncvar_get(nc, "lon", start=95, count=5)
# y5 <- ncvar_get(nc, "lat", start=13, count=1)
# #x5; y5
# dat5 <- t(ncvar_get(nc, "sst", start=c(93,13,ST), count=c(5,1,-1), verbose = F))
# dimnames(dat5) <- list(NULL, paste("N", y5, "W", x5, sep=""))

# # Extract 66 North, 170W to 168W (Jan 1900 - present):
# x6 <- 360-ncvar_get(nc, "lon", start=96, count=2)
# y6 <- ncvar_get(nc, "lat", start=12, count=1)
# #x6; y6
# dat6 <- t(ncvar_get(nc, "sst", start=c(96,12,ST), count=c(2,1,-1), verbose = F))
# dimnames(dat6) <- list(NULL, paste("N", y6, "W", x6, sep=""))

# dat.N <- cbind(dat4, dat5, dat6)
# head(dat.N)


## ------------------------------------------------------------------------
if(T) {
  xmin <- -137
  xmax <- -133
  ymin <- 55.5
  ymax <- 58

  myworld <- map_data('world') # 'world2' spans dateline
  names(myworld) <- c("X","Y","PID","POS","region","subregion")
  myworld <- clipPolys(myworld, xlim=c(xmin,xmax),ylim=c(ymin,ymax), keepExtra=TRUE)

  x_coords <- c(-136, -137, -135.5)
  y_coords <- c(56, 57, 56.5)

  ggplot() + 
    geom_polygon(data=myworld,
                aes(x=X,y=Y,group=factor(PID))) + 
    geom_point(aes(x=x_coords, 
            y=y_coords), 
            size=4, color="#66ddb9e3") +
    geom_text(aes(x = x_coords+.2, y = y_coords-.15, 
                  label = c("SST", "UPW", "precip"))) +
    geom_ellipse(aes(x0 = -135.5, y0 = 57, angle = -pi/2, a = .25, b = .25), color = "#d84c21d3") +
    annotate("text", x = -135.1, y = 57.25, label = "Sitka \n Sound", color = "#d84c21d3") +
    xlab("Degrees W") + ylab("Degrees N") +
    theme_bw()
}

## ------------------------------------------------------------------------

# Extract times (month, year) for each observation (row)
# Time units are days since 1/1/0:
d <- dates(ncvar_get(nc, "time"), origin=c(1,15,1800))[ST:ET]
head(d)
tail(d)  # End of time series

y <- as.numeric(as.character(chron::years(d)))
m <- months(d)

# Convert missing value designator to NA 
any(dat1 == nc$var$sst$missval)    # Any missing values?
dat1[dat1 == nc$var$sst$missval] <- NA

# Check units
nc$var$sst$units
# Look at distribution of values:
hist(dat1)

# only extract SST's from 1975-2021
SST.mon <- data.frame(Year = y, Month = m, as.data.frame(dat1[,ST:ET]))  
dim(SST.mon)

colnames(SST.mon)[3] <- "SST"

write.table(SST.mon, "recruitment_comparisons/data/sst_mon.csv", sep=",")

# Monthly anomalies of SST:
# Ann.S <- apply(SST.mon.S[,-(1:2)], 1, mean)
# Ann.N <- apply(SST.mon.N[,-(1:2)], 1, mean)

# write.table(cbind(South=Ann.S, North=Ann.N), "sst_mean_S.csv", sep=",")
# write.table(Ann.N, "sst_mean_N.csv", sep=",")

nc_close(nc)
rm(d,dat,dat.S,dat.N,m,nc,ST,y,x1,x2,x3,x4,x5,x6,y1,y2,y3,y4,y5,y6,
   dat1,dat2,dat3,dat4,dat5,dat6,xmin,xmax,ymin,ymax,myworld)



