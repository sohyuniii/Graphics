library(readr)
library(dplyr)
library(data.table)
setwd("C:/Users/user/Desktop/I/ewha2/Graphics/Project/Data")

###### Data1 -> delivery ######
chicken1 <- read_csv("CALL_CHICKEN_01MONTH.csv")
chicken2 <- read_csv("CALL_CHICKEN_02MONTH.csv")
chicken3 <- read_csv("CALL_CHICKEN_03MONTH.csv")
chicken4 <- read_csv("CALL_CHICKEN_04MONTH.csv")
chicken <- rbind(chicken1,chicken2,chicken3,chicken4)
rm(chicken1,chicken2,chicken3,chicken4)

cfood1 <- read_csv('CALL_CFOOD_01MONTH.csv')
cfood2 <- read_csv('CALL_CFOOD_02MONTH.csv')
cfood3 <- read_csv('CALL_CFOOD_03MONTH.csv')
cfood4 <- read_csv('CALL_CFOOD_04MONTH.csv')
cfood <- rbind(cfood1,cfood2,cfood3,cfood4)
rm(cfood1,cfood2,cfood3,cfood4)

delivery <- rbind(chicken,cfood) 
rm(chicken,cfood)
delivery <- delivery[,-5]
names(delivery)
colnames(delivery) <- c('date','yoil','sex','age','gu','dong','type','call')

pizza1 <- read_csv('CALL_PIZZA_01MONTH.csv')
pizza2 <- read_csv('CALL_PIZZA_02MONTH.csv')
pizza3 <- read_csv('CALL_PIZZA_03MONTH.csv')
pizza4 <- read_csv('CALL_PIZZA_04MONTH.csv')
pizza <- rbind(pizza1,pizza2,pizza3,pizza4)
pizza <- pizza[,-5]
colnames(pizza) <- c('date','yoil','sex','age','gu','dong','call')
pizza$type <- "피자"
rm(pizza1,pizza2,pizza3,pizza4)
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
# 1월1일(신정) / 2월4일~6일(설날) / 3월1일(삼일절)
delivery$weekend <- ifelse(delivery$yoil=="토",1,ifelse(delivery$yoil=="일",1,0))
delivery$month <- paste0(delivery$month,"월")

write.csv(delivery,"CALL_TOTAL.csv")


###### Data2 -> weather ######
gu_id <- read_csv("gugucon.csv")
weather <- read.csv("weather.csv",stringsAsFactors = FALSE)
dust <- read.csv("yellowDust.csv")

names(weather)
colnames(weather) <- c('id','date','mean.tem','min.tem','max.tem','rain','wind')
total <- merge(gu_id,weather,on="id")
total <- merge(total,dust,on="date")

total$month <- as.numeric(substr(total$date,6,7))
total$day <- as.numeric(substr(total$date,9,10))

total <- total %>% arrange(date)
filter(total %>% count(date), n!=25)
unique(total$gu)
unique(filter(total,date=="2019-01-05")$gu) # 금천구
unique(filter(total,date=="2019-01-06")$gu) # 금천구
unique(filter(total,date=="2019-04-25")$gu) # 중랑구
unique(filter(total,date=="2019-04-26")$gu)

filter(total,date=="2019-01-05" & (id==410 | id==509))
e = apply(filter(total,date=="2019-01-05" & (id==410 | id==509))[,c(4,5,6,7,8,9)],2,mean)
ex = data.frame(date="2019-01-05",id="417",'district'="금천구")

write.csv(total,"weather_TOTAL.csv")

###### Data3 -> air pollution ######
library(tidyr)
ozon4 <- read.csv('ozon4.csv')
ozon4 <- ozon4[,1:31]
ozon3 <- read.csv('ozon3.csv')
ozon2 <- read.csv('ozon2.csv')
ozon2 <- ozon2[,1:29]
ozon1 <- read.csv('ozon1.csv')

ozon4 <- gather(ozon4,day,ozon,2:31) %>% mutate(month=rep(4,30*25))
ozon3 <- gather(ozon3,day,ozon,2:32) %>% mutate(month=rep(3,31*25))
ozon2 <- gather(ozon2,day,ozon,2:29) %>% mutate(month=rep(2,28*25))
ozon1 <- gather(ozon1,day,ozon,2:32) %>% mutate(month=rep(1,31*25))
ozon <- rbind(ozon1,ozon2,ozon3,ozon4)

air <- merge(ozon,nano,by=c('gu','month','day'))
air$day = as.numeric(gsub("X","",air$day))
total <- merge(dust,air,by=c("gu","month","day"))
colSums(is.na(total)) # ozon 5개
write.csv(total,"airpollution.csv")
