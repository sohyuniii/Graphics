library(readr)
library(dplyr)
chicken <- read_csv("CALL_CHICKEN_03MONTH.csv")
unique(chicken$기준일)
chicken <- chicken[,c(1,2,3,4,6,7,9)]
colnames(chicken) <- c('date','yoil','sex','age','gu','dong','call')
table(chicken$age)
table(chicken$sex)
table(chicken$gu) ; length(unique(chicken$gu))
t = chicken %>%
  group_by(gu) %>%
  summarize(sum.call = mean(call, na.rm = TRUE))

seoul <- read_csv('seoul.csv')
rr <- read.csv('result.csv')
rr <- filter(rr,si=='서울')

total <- merge(t,rr,by='gu',all=FALSE)

seoul <- merge(seoul, total, by='id')

ggplot() +  scale_fill_viridis() + 
  geom_polygon(data=seoul, aes(x=long, y=lat, group=group, 
                                        fill=sum.call), alpha=.75) + theme_void() 
