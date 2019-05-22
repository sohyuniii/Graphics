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

call <- read.csv("CALL_TOTAL.csv",stringsAsFactors = FALSE)
weather <- read.csv('weather_TOTAL.csv')
call$month <- paste0(call$month,"월")
weather$month <- paste0(weather$month,"월")

ggplot(weather) +
  geom_density_ridges(aes(x = mean.tem, y = month,group=month))

ggplot(weather, aes(x = mean.tem, y = month, fill=factor(..quantile..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles") +
  theme_tufte(base_family = "Helvetica")
