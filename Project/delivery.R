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
call$month <- as.integer(substr(call$month,1,1))

call %>% group_by(type) %>% summarize(call = sum(call)) %>%
  ggplot(aes(type,call,fill=type)) + geom_bar(stat='identity') + labs(x="",y="") +
  theme_hc() + scale_fill_manual(values = c("red", "orange", "yellow") )

call %>% group_by(month) %>% summarize(call = sum(call)) %>%
  ggplot(aes(month,call,fill=month)) + geom_bar(stat='identity') + labs(x="",y="") +
  theme_hc() + scale_fill_viridis(option="magma") + coord_flip() +
  geom_text(aes(label=call), position=position_dodge(width=0.9), vjust=-0.5)

call %>% group_by(sex,type) %>% summarize(sum.call = sum(call, na.rm = TRUE)) %>% 
  ggplot(mapping = aes(x = type, y = sex)) + 
  geom_tile(mapping = aes(fill = sum.call)) + theme_hc()+
  scale_fill_gradient(low = "yellow", high = "red") + labs(x="",y="") +
  guides(fill = guide_legend(title = "통화건수", title.position = "top"))

call %>% group_by(age,type) %>% summarize(sum.call = sum(call, na.rm = TRUE)) %>% 
  ggplot(mapping = aes(x = type, y = age)) + theme_hc()+
  geom_tile(mapping = aes(fill = sum.call)) +
  scale_fill_gradient(low = "yellow", high = "red") + labs(x="",y="") +
  guides(fill = guide_legend(title = "통화건수", title.position = "top"))

# Rollipop chart
data = call %>% group_by(yoil,type) %>% summarize(sum.call = sum(call, na.rm = TRUE))
data$yoil <- factor(data$yoil, levels=c("일","토",'금','목','수','화','월'))
ggplot(data, aes(x=yoil, y=sum.call,label = round(sum.call))) +
  geom_segment( aes(x=yoil, xend=yoil, y=0, yend=sum.call), color="grey", size=1) +
  geom_point( color="orange", size=4, alpha=0.6) +
  theme_light() + geom_text(nudge_x = 0.2) +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + labs(x="",y="") + facet_wrap(~type) 

######### Calender #########

call$yoil <- factor(call$yoil, levels=c("월","화","수","목","금","토","일"))

month_call <- call %>% group_by(type,month,yoil,week) %>% summarize(call = sum(call, na.rm = TRUE))
month_call$week <- 
  factor(month_call$week, levels=rev(sort(unique(month_call$week))))

ggplot(data = month_call, aes(x = yoil, y = week)) + 
  geom_tile(aes(fill = call)) + 
  coord_equal(ratio = 1) + 
  scale_fill_viridis(option="magma") +
  facet_wrap(type~month, ncol = 4) +
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

### 1월 치킨 축구시청률 ###
library(patchwork)
# 필리핀 10시반 / 키르기스스탄 1시 / 중국 10시반 / 
# 바레인 10시 (16강) / 카타르 10시 (8강)
date <- c("01-07","01-12","01-16","01-22","01-25")
ratings <- c(12.577,7.426,16.867,21.245,23.099) # 시청률 
chicken <- data.frame(date,ratings,call=c(10481,16388,14101,14037,19690))
g1 <- ggplot(chicken) + geom_point(aes(date,ratings)) 
g2 <- ggplot(chicken) + geom_point(aes(date,call)) 
g1 + g2 + plot_layout(ncol = 2)

hour <- read.csv("CALL_Hourly_TOTAL.csv")
hour_chicken <- hour %>% filter(type=='치킨') %>% 
  group_by(date,hour) %>% summarize(call = sum(call, na.rm = TRUE)) 

g <- ggplot() + xlim(0,23) + theme(panel.background = element_blank()) + 
  labs(x="",y="") + theme(plot.title = element_text(hjust = 0.5)) 

g2 <- g + ggtitle("01.12.토 01:00 - 한국 vs 키르기스스탄") +
  geom_line(data=hour_chicken %>% filter(date=="2019-01-12"),aes(hour,call),col=2) +
  geom_line(data=hour_chicken %>% filter(date=="2019-01-05"),aes(hour,call)) +
  geom_line(data=hour_chicken %>% filter(date=="2019-01-19"),aes(hour,call)) +
  geom_line(data=hour_chicken %>% filter(date=="2019-01-26"),aes(hour,call)) +
  annotate("rect", xmin=0, xmax=1, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
  annotate("rect", xmin=17.5, xmax=19, ymin=0, ymax=Inf, alpha=0.07, fill="blue")

g3 <- g + ggtitle("01.16.수 10:30 - 한국 vs 중국") +
  geom_line(data=hour_chicken %>% filter(date=="2019-01-16"),aes(hour,call),col=2) +
  geom_line(data=hour_chicken %>% filter(date=="2019-01-09"),aes(hour,call)) +
  geom_line(data=hour_chicken %>% filter(date=="2019-01-02"),aes(hour,call)) +
  geom_line(data=hour_chicken %>% filter(date=="2019-01-23"),aes(hour,call)) +
  annotate("rect", xmin=20.5, xmax=22, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
  annotate("rect", xmin=18, xmax=19.5, ymin=0, ymax=Inf, alpha=0.07, fill="blue")

g5 <-  g + ggtitle("01.25.금 10:00 - 한국 vs 카타르") + 
  geom_line(data=hour_chicken %>% filter(date=="2019-01-25"),aes(hour,call),color="salmon",size=1) +
  geom_line(data=hour_chicken %>% filter(date=="2019-01-18"),aes(hour,call)) +
  geom_line(data=hour_chicken %>% filter(date=="2019-01-11"),aes(hour,call)) +
  geom_line(data=hour_chicken %>% filter(date=="2019-01-04"),aes(hour,call)) +
  annotate("rect", xmin=20.5, xmax=22, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
  annotate("rect", xmin=18, xmax=20, ymin=0, ymax=Inf, alpha=0.07, fill="blue")

g2 + g3 + g5 + plot_layout(ncol = 3)

### 4월 피자 검색어 ###
library(tidyr)
search <- read_csv('search.csv')
search$day <- seq(1,30)
search <- gather(search,keyword,value,2:5)

ggplot(data=search) + 
  geom_line(aes(day,value,group = keyword, colour = keyword)) +
  facet_wrap(~keyword,ncol=2) +
  scale_fill_viridis(option="magma") +
  theme_tufte(base_family = "Helvetica") +
  annotate("rect", xmin=13, xmax=14, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("2019년 4월 검색어 통계량")

### 블랙데이 ###
chine <- call %>% filter(type=='중국음식' & yoil=="일") %>%
  group_by(date,sex,age) %>% summarize(call = sum(call, na.rm = TRUE)) 
dd <- chine %>% group_by(date) %>% summarize(call = sum(call))
dd$black <- ifelse(dd$date=='2019-04-14',"YES","NO")
dd$date <- substr(dd$date,6,10)

ggplot(dd, aes(date,call,fill=black)) + theme_hc() +
  geom_col(colour = "black", size = .25) + labs(x='날짜',y='') +
  scale_fill_manual(values = c("#f7f7f7", "#000000"), guide = FALSE)

no <- call %>% filter(type=='중국음식' & yoil=="일" & month=='4월') %>%
  filter(date!='2019-04-14') %>% 
  group_by(sex,age) %>% summarize(call = sum(call)/3)
black <- call %>% filter(type=='중국음식' & date=='2019-04-14') %>% 
  group_by(sex,age) %>% summarize(call = sum(call))


b = (black %>% group_by(sex,age) %>% filter(sex=='여') %>%
       summarize(call = sum(call)))
n = (no %>% group_by(age) %>% filter(sex=='여') %>%
       summarize(call = sum(call)))
ratio <- data.frame(age=b$age,black=b$call,no=n$call)
ratio_m <- ratio %>% mutate(r = (black-no)/no, sex='남자',
                          plus_minus = ifelse(r>0, 'plus','minus'))
ratio_w <- ratio %>% mutate(r = (black-no)/no, sex='여자',
                          plus_minus = ifelse(r>0, 'plus','minus'))

ratio <- rbind(ratio_m,ratio_w)[,c(1,4,5,6)]
  
ggplot(ratio,aes(x=age, y=r, fill=plus_minus)) + 
  geom_bar(stat="identity", position="identity", colour="white", width=0.2) + # width 막대 폭 좁게
  scale_fill_manual(values=c("red", "dark blue"), guide=FALSE) + # guide=F 범례 생략
  theme_hc() + facet_grid(~sex)

### 강서구 - 치킨 ###
gangseo <- call %>% filter(gu=="강서구" & type!="중국음식")  
unique(gangseo$dong)
  group_by(type,dong) %>% summarise(call = sum(call, na.rm = TRUE))

######### Map ######### 

ll <- read.csv('seoul.csv')
rr <- read_csv('id_seoul_gu.csv')
dong <- read_csv('id_dong.csv')
dong <- dong[,-3]
colnames(dong) <- c("id","dong","lat","lon")

tt = call %>% 
  group_by(type,gu,type) %>%
  summarize(sum.call = sum(call, na.rm = TRUE))

map_fun <- function(name){
  t = tt %>% filter(type==name)
  total <- merge(t,rr,by='gu',all=FALSE)
  seoul <- merge(ll, total, by='id')
  ggplot() + theme_void() +
    scale_fill_viridis(option = 'D') + 
    geom_polygon(data=seoul,alpha=.75,
                 aes(x=long, y=lat, group=group, fill=sum.call)) +
    theme(legend.position = "none") +
    theme(plot.title = element_text(hjust = 0.5,size=20))
}
p1 <- map_fun("피자") + ggtitle("피자")
p2 <- map_fun("치킨") + ggtitle("치킨")
p3 <- map_fun("중국음식") + ggtitle("중국음식")
p2 + p1 + p3 + plot_layout(ncol = 3)

### 강남구 - 중국집 ###
gangnam_ch <- call %>% filter(gu=="강남구" & type=="중국집") %>% 
      group_by(dong) %>% summarise(call = sum(call, na.rm = TRUE))
gangnam <-  merge(gangnam_ch ,dong, by='dong')  
gangnam <- gangnam[-8,]

center <- c(mean(gangnam$lon),mean(gangnam$lat))
map <- get_map(center, zoom = 13, maptype ='roadmap')
ggmap(map) + geom_point(data=gangnam, aes(x=lon, y=lat, size=call, colour=call)) +
  scale_color_gradient(low='blue', high='red')

### 강서구 - 치킨/피자 ###
gangseo <- call %>% filter(gu=="강서구" & type!="중국음식") %>% 
  group_by(type,dong) %>% summarise(call = sum(call, na.rm = TRUE))
gangseo <-  merge(gangseo ,dong, by='dong')  
gangseo <- gangseo[-8,]

center <- c(mean(gangseo$lon),mean(gangseo$lat))
map <- get_map(center, zoom = 13, maptype ='roadmap')
ggmap(map) + geom_point(data=gangseo, aes(x=lon, y=lat, size=call, colour=call)) +
  scale_color_gradient(low='blue', high='red') + facet_grid(~type)