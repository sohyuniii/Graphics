library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(readr)
library(dplyr)
library(viridis)
library(ggpubr)
library(ggthemes)
library(ggridges)
setwd("C:/Users/user/Desktop/I/ewha2/Graphics/Project/Data")
options("scipen" = 100)

call <- read.csv("CALL_Hourly_TOTAL.csv",stringsAsFactors = FALSE)

ggplot(call,aes(x=hour,y=call,fill=type)) + geom_bar(stat='identity') +
  facet_wrap(~type)

