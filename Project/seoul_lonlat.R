library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
setwd("C:/Users/user/Desktop/I/ewha2/Graphics/Project")

# 구글 지도 API 인증키를 등록
register_google(key ='AIzaSyCOGOjnN1pOl8FmyscQhF5Nvn7KvN31Eb4')
qmap("seoul", zoom = 11, maptype ='roadmap',color='bw')

#################################################################

korea <- shapefile('SIG_201703/TL_SCCO_SIG.shp')
korea <- fortify(korea, region='SIG_CD')
seoul <- korea[korea$id <= 11740, ]
seoul <- seoul[,c(1,2,3,6,7)]
write.csv(seoul,"seoul.csv")

ggplot() + geom_polygon(data=seoul, aes(x=long, y=lat, group=group), fill='white', color='black')
ggplot() + geom_polygon(data=seoul, aes(x=long, y=lat, group=group, 
                                          fill=moon), alpha=.75) + theme_void() + guides(fill=F)

ex = read.csv("seoul.csv")


