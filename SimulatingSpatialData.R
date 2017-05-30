#Read in data with pest names
getwd()
setwd("~/Dropbox (ENVSECSP)/CEBRA/05 R code/Test/")
pests_ref<-read.csv("data/pests.csv", colClasses=c(rep("character",3),rep("numeric",3)))

library(RColorBrewer)
library(maps)
library(mapdata)
library(ggplot2)
#Simulate spatial data
#Simulate data
n<-20
set.seed(36)
names_obs<-array()
lat_obs<-array()
long_obs<-array()
col_obs<-array()

col_sp<-colorRampPalette(brewer.pal(9,"Set3"))(length(pests_ref$Name))

nobs<-round(runif(20,1,100))
for (i in 1:length(pests_ref$Name))
{
        names_obs<-c(names_obs,rep(pests_ref$Name[i],nobs[i]))
        #Simulate latitude
        mu_lat<-rnorm(1,-26,3)
        sd_lat<-rnorm(1,2,0.5)
        lat_obs<-c(lat_obs,rnorm(nobs[i],mu_lat,sd_lat))
        #Simulate longitude
        mu_long<-rnorm(1,136,7)
        sd_long<-rnorm(1,4,0.5)
        long_obs<-c(long_obs,rnorm(nobs[i],mu_long,sd_long))
        
        col_obs<-c(col_obs, rep(col_sp[i],nobs[i]))
        
}
names_obs<-names_obs[-1]
long_obs<-long_obs[-1]
lat_obs<-lat_obs[-1]
col_obs<-col_obs[-1]

data<-data.frame(latitude = lat_obs,longitude = long_obs)
data$species<-names_obs
data$colour<-col_obs
data$damage<-c(rep(100, length(data$latitude)))

dataMap<-subset(data, data$species== pests_ref$Name[10])
map("worldHires","Australia", xlim=c(105,155), ylim=c(-45,-10), col="gray90", fill=TRUE)
with(dataMap,points(longitude,latitude,pch=21,col=colour[1],bg=colour[1],cex=3))
