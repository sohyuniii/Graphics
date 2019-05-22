library(readr)
library(dplyr)
library(data.table)
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

delivery <- rbind(chicken,cfood) 
rm(chicken,cfood)
delivery <- delivery[,-5]
names(delivery)
colnames(delivery) <- c('date','yoil','sex','age','gu','dong','type','call')

pizza1 <- read_csv('CALL_PIZZA_01MONTH.csv')
pizza2 <- read_csv('CALL_PIZZA_02MONTH.csv')
pizza3 <- read_csv('CALL_PIZZA_03MONTH.csv')
pizza <- rbind(pizza1,pizza2,pizza3)
pizza <- pizza[,-5]
colnames(pizza) <- c('date','yoil','sex','age','gu','dong','call')
pizza$type <- "피자"
rm(pizza1,pizza2,pizza3)
delivery <- rbind(delivery,pizza)
rm(pizza)

table(delivery$type)
str(delivery)

delivery$date <- as.character(delivery$date)
delivery$month <- as.numeric(substr(delivery$date,5,6))
delivery$day <- as.numeric(substr(delivery$date,7,8))

delivery$date <- as.character(delivery$date)
delivery$date <- paste(substr(delivery$date,1,4),substr(delivery$date,5,6),substr(delivery$date,7,8), sep = "-")
delivery$week <- week(delivery$date)

write.csv(delivery,"CALL_TOTAL.csv")


###### Data2 -> weather ######
gu_id <- read_csv("gugucon.csv")
weather <- read.csv("weather.csv",stringsAsFactors = FALSE)
dust <- read.csv("yellowDust.csv")

names(weather)
weather <- weather[,c(1,2,3,4,6,8,11)]
colnames(weather) <- c('id','date','mean.tem','min.tem','max.tem','rain','wind')
total <- merge(gu_id,weather,on="id")
total <- merge(total,dust,on="date")

total$month <- as.numeric(substr(total$date,6,7))
total$day <- as.numeric(substr(total$date,9,10))

total <- total %>% arrange(date)
filter(total %>% count(date), n!=25)
unique(total$district)
unique(filter(total,date=="2019-01-05")$district) # 금천구
unique(filter(total,date=="2019-01-06")$district) # 금천구

filter(total,date=="2019-01-05" & (id==410 | id==509))
e = apply(filter(total,date=="2019-01-05" & (id==410 | id==509))[,c(4,5,6,7,8,9)],2,mean)
ex = data.frame(date="2019-01-05",id="417",'district'="금천구")

write.csv(total,"weather_TOTAL.csv")
