library(ggplot2)
library(readr)
library(dplyr)
setwd("C:/Users/user/Desktop/I/ewha2/Graphics/Project/Data")
options("scipen" = 100)

call1 <- read_csv("CALL_NDELIVERY_01MONTH.csv")
call2 <- read_csv("CALL_NDELIVERY_02MONTH.csv")
call3 <- read_csv("CALL_NDELIVERY_03MONTH.csv")
call4 <- read_csv("CALL_NDELIVERY_04MONTH.csv")

call <- rbind(call1, call2, call3, call4)
rm(call1, call2, call3, call4)

call <- call %>% select(c(1,2,3,4,6,7,8)) %>% filter(업종!="음식점-족발/보쌈전문")
colnames(call) <- c('date','yoil','time','type','gu','dong',"call")
call$time <- as.numeric(call$time)

write.csv(call,"CALL_Hourly_TOTAL.csv")

ggplot(call,aes(x=time,y=call,fill=type)) + geom_bar(stat='identity') +
  facet_wrap(~type)


### 설날 연휴 ###
hour <- read.csv("CALL_Hourly_TOTAL.csv")
hour$yoil <- factor(hour$yoil, levels=c("월","화","수","목","금","토","일"))
hour <- hour %>% 
  group_by(type,date,time) %>% summarize(call = sum(call, na.rm = TRUE)) 

ggplot() + 
  geom_line(data=hour %>% filter(date=="20190207"),aes(time,call,colour="평일")) +
  geom_line(data=hour %>% filter(date=="20190205"),aes(time,call,colour="설날당일")) +
  geom_line(data=hour %>% filter(date=="20190204"),aes(time,call,colour="설날연휴")) +
  geom_line(data=hour %>% filter(date=="20190206"),aes(time,call,colour="설날연휴")) +
  labs(x="",y="",title="19.02.04-07 업종별 통화량 차이") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_hc() +
  theme(strip.background = element_rect(fill = "#e9e9e9")) +
  scale_fill_viridis_d(option="A") + facet_grid(~type) +
  scale_colour_manual("",values = c("평일" = "#48d1cc", "설날당일" = "#ff980f","설날연휴"="#666666"))

### 강서구 - 치킨 ###
chicken <- hour %>% filter(gu=="강서구" & type=="치킨")
ggplot(chicken) + geom_bar(aes(time,call,fill=dong),stat='identity') +
  facet_grid(~dong)

top3 <- chicken %>% filter(dong=="가양동" | dong=="내발산동" | dong=="화곡동")
ggplot(top3) + geom_bar(aes(time,call,fill=yoil),stat='identity') +
  facet_grid(~yoil)

top3 %>% group_by(time) %>% summarise(call = sum(call, na.rm = TRUE)) %>%
  arrange(desc(call))
top3 %>% filter(time>16 & time<23) %>% group_by(yoil) %>% summarise(call = sum(call, na.rm = TRUE)) %>%
  arrange(desc(call))
