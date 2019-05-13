library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)

library(readr)
library(dplyr)
library(viridis)
setwd("C:/Users/user/Desktop/I/ewha2/Graphics/Project/Data")
chicken1 <- read_csv("CALL_CHICKEN_01MONTH.csv")
chicken2 <- read_csv("CALL_CHICKEN_02MONTH.csv")
chicken3 <- read_csv("CALL_CHICKEN_03MONTH.csv")
chicken <- rbind(chicken1,chicken2,chicken3)
rm(chicken1,chicken2,chicken3)

cfood1 <- read_csv('CALL_CFOOD_01MONTH.csv')
cfood2 <- read_csv('CALL_CFOOD_02MONTH.csv')
cfood3 <- read_csv('CALL_CFOOD_03MONTH.csv')
cfood <- rbind(cfood1,cfood2,cfood3)
rm(cfood1,cfood2,cfood3)

food <- rbind(chicken,cfood)
rm(chicken,cfood)
write.csv(food,'food.csv')

length(unique(food$기준일)) # 90
table(food$성별)
table(food$연령대)
table(food$업종)
table(food$시군구)



unique(chicken$기준일)
chicken <- chicken[,c(1,2,3,4,6,7,9)]
colnames(chicken) <- c('date','yoil','sex','age','gu','dong','call')
chicken$date <- as.numeric(chicken$date)
chicken <- mutate(month = as.numeric(substr(date,5,6)), 
                 day = as.numeric(substr(date,7,8)))



table(chicken$age)
table(chicken$sex)
table(chicken$gu) ; length(unique(chicken$gu))
t = chicken %>%
  group_by(gu,age) %>%
  summarize(sum.call = sum(call, na.rm = TRUE))

seoul <- read_csv('seoul.csv')
rr <- read.csv('id_seoul_gu.csv')

total <- merge(t,rr,by='gu',all=FALSE)

seoul <- merge(seoul, total, by='id')

ggplot() +  
  scale_fill_viridis(option = "A") + 
  geom_polygon(data=seoul,alpha=.75,
               aes(x=long, y=lat, group=group, fill=sum.call)) + 
  theme_void() 
