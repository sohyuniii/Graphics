####### CIRCULAR BARPLOT #######
library(tidyverse)

### 25개의 구 ###
ex = call %>% group_by(gu) %>% summarize(call = sum(call, na.rm = TRUE)) %>% arrange(desc(call))
ex[3,1] = "서초구" ; ex[4,1] = "동작구"
data=data.frame(id=seq(1,25), individual = ex$gu, value = ex$call)
# calculate the ANGLE of the labels
label_data=data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5)/number_of_bar 
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

ggplot(data, aes(x=as.factor(id), y=value)) +     
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  ylim(-100000,max(data$value)+100) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")    
  ) +
  coord_polar(start = 0) +
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

### top3 3개의 구 ###
call %>% group_by(gu) %>% summarize(call = sum(call)) %>% arrange(desc(call))
data = call %>% filter(gu=='강서구' | gu=='강남구' | gu=='서초구' ) %>% 
  group_by(gu,dong) %>% summarize(call = sum(call, na.rm = TRUE))
data = data.frame(data)
colnames(data) <- c('group','individual','value')
data = data %>% arrange(group, value)

# Set a number of 'empty bar' to add at the end of each group
empty_bar=2
to_add = data.frame( matrix(NA, empty_bar*3, ncol(data)) )
colnames(to_add) = colnames(data)
to_add$group=rep(unique(data$group), each=empty_bar)
data=rbind(data, to_add)
data$group <- factor(data$group, levels=c('강서구','서초구','강남구'))
data=data %>% arrange(desc(group)) ; data[22,3] = 94157
data$id=seq(1, nrow(data))

# Get the name and the y position of each label
label_data=data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data=data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = c(34,23,13)
grid_data$start = grid_data$end + 1
grid_data=grid_data[-1,]

# Make the plot
ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 100000, xend = start, yend = 100000), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 75000, xend = start, yend = 75000), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50000, xend = start, yend = 50000), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 25000, xend = start, yend = 25000), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(25000,50000,75000,100000), label = c('25,000','50,000','750,000','100,000') , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-100000,max(data$value)+100) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -10000, xend = end, yend = -10000), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -30000, label=group), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)