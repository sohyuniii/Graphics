library(readr)
library(dplyr)
setwd("C:/Users/user/Desktop/I/ewha2/Graphics/Project/Data")

###### Data1 -> delivery ######
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

delivery <- rbind(chicken,cfood) # 165395 9
rm(chicken,cfood)

delivery <- delivery[,-5]
names(delivery)
colnames(delivery) <- c('date','yoil','sex','age','gu','dong','type','call')
delivery$date <- as.numeric(delivery$date)
delivery$month = substr(as.character(date),5,6)
delivery$day = substr(date,7,8)

###### Data2 -> weather ######
gu_id <- read_csv("gugucon.csv")
weather <- read.csv("weather.csv",stringsAsFactors = FALSE)
names(weather)
colnames(weather) <- c('id','date','min.tem','max.tem','rain','wind')
total <- merge(gu_id,weather,on="id")
total$month <- as.numeric(substr(total$date,6,7))
total$day <- as.numeric(substr(total$date,9,10))

df = read.csv("yellowDust.csv")
