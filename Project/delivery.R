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
call$month <- paste0(call$month,"월")

ggplot(data=call, aes(x=gu, y=call, fill=type)) + 
  geom_bar(position = 'dodge', stat='identity')


call %>% group_by(sex,type) %>% summarize(sum.call = sum(call, na.rm = TRUE)) %>% 
  ggplot(mapping = aes(x = type, y = sex)) + 
  geom_tile(mapping = aes(fill = sum.call)) +
  scale_fill_gradient(low = "yellow", high = "red") + labs(x="",y="") +
  guides(fill = guide_legend(title = "통화건수", title.position = "top"))

call %>% group_by(age,type) %>% summarize(sum.call = sum(call, na.rm = TRUE)) %>% 
  ggplot(mapping = aes(x = type, y = age)) + 
  geom_tile(mapping = aes(fill = sum.call)) +
  scale_fill_gradient(low = "yellow", high = "red") + labs(x="",y="") +
  guides(fill = guide_legend(title = "통화건수", title.position = "top"))

call %>% group_by(yoil,type) %>% summarize(sum.call = sum(call, na.rm = TRUE)) %>% 
  ggplot(mapping = aes(x = type, y = yoil)) + 
  geom_tile(mapping = aes(fill = sum.call)) +
  scale_fill_gradient(low = "yellow", high = "red") + labs(x="",y="") +
  guides(fill = guide_legend(title = "통화건수", title.position = "top"))

call$yoil <- factor(call$yoil, levels=c("월","화","수","목","금","토","일"))

month_call <- call %>% group_by(type,month,yoil,week) %>% summarize(call = sum(call, na.rm = TRUE))
month_call$week <- 
  factor(month_call$week, levels=rev(sort(unique(month_call$week))))

ggplot(data = month_call, aes(x = yoil, y = week)) + 
  geom_tile(aes(fill = call)) + 
  coord_equal(ratio = 1) + 
  scale_fill_viridis(option="magma") +
  facet_wrap(type~month, ncol = 3) +
  theme_tufte(base_family = "Helvetica") +
  # hide y-axis ticks and labels
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.y = element_blank()) +
  # hide main x and y-axis titles
  theme(axis.title.x = element_blank()) + 
  theme(axis.title.y = element_blank()) +
  # move x-axis labels (week names) to top, hide ticks
  scale_x_discrete(position = "top") +
  theme(axis.ticks.x = element_blank()) +
  # move panel title (month names) outside (above week names)
  theme(strip.placement = "outside") +
  theme(strip.text.x = element_text(size = "14", hjust = 0)) +
  # center-aligned plot title
  ggtitle("[ 2019년 일자별 배달건수 ]") + 
  theme(plot.title = element_text(size = "16", hjust = 0.5))

######### Map ######### 

ll <- read.csv('seoul.csv')
rr <- read_csv('id_seoul_gu.csv')
dong <- read_csv('id_dong.csv')
dong <- dong[,-3]
colnames(dong) <- c("id","dong","lat","lon")

t = call %>% 
  group_by(gu,type) %>%
  summarize(sum.call = sum(call, na.rm = TRUE))
total <- merge(t,rr,by='gu',all=FALSE)
seoul <- merge(ll, total, by='id')
ggplot() + theme_void() +
  scale_fill_viridis(option = 'D') + 
  geom_polygon(data=seoul,alpha=.75,
               aes(x=long, y=lat, group=group, fill=sum.call)) +
  facet_wrap(~type)

gangnam_ch <- call %>% filter(gu=="강남구" & type=="중국집") %>% 
      group_by(dong) %>% summarise(call = sum(call, na.rm = TRUE))
gangnam <-  merge(gangnam_ch ,dong, by='dong')  
gangnam <- gangnam[-8,]

center <- c(mean(gangnam$lon),mean(gangnam$lat))
map <- get_map(center, zoom = 13, maptype ='roadmap')
ggmap(map) + geom_point(data=gangnam, aes(x=lon, y=lat, size=call, colour=call)) +
  scale_color_gradient(low='blue', high='red')




library(leaflet)
leaflet(data = gangnam) %>% addTiles() %>%
  addCircleMarkers(lat = ~lat,lng = ~lon,
                   popup=paste(gangnam$dong,'<br>','Call :',gangnam$call),
                   opacity = 0.6, radius = gangnam$call/1000) # 반지름은 남은 자전거 수에 비례


center <- c(mean(seoul$long),mean(seoul$lat))
map <- get_map(center, zoom = 11, maptype ='roadmap',color='bw')
ggmap(map) + geom_polygon(data=seoul,alpha=.75,
                          aes(x=long, y=lat, group=group, fill=sum.call)) +
  scale_fill_viridis()




