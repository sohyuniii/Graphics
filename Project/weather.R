library(ggplot2)
library(readr)
library(dplyr)
library(viridis)
library(ggpubr)
library(ggthemes)
library(ggridges)
setwd("C:/Users/user/Desktop/I/ewha2/Graphics/Project/Data")
options("scipen" = 100)

call <- read.csv("CALL_TOTAL.csv",stringsAsFactors = FALSE)
weather <- read.csv('weather_TOTAL.csv')
day <- read_csv('days.csv')
call$month <- as.integer(substr(call$month,1,1))

ggplot(weather, aes(x = mean.tem, y = month, fill=factor(..quantile..))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles") +
  theme_tufte(base_family = "Helvetica")

call <- call %>% group_by(type,date,gu) %>% summarize(call = sum(call, na.rm = TRUE)) 
call <- merge(call,weather,on=c(date,gu))
day$date <- as.character(day$date)
call <- merge(call,day,on=date)

###### CORR PLOT ######
library(corrplot)
library(Hmisc)
df1 = call %>% filter(type=='치킨' & (gu=="강서구") & weekend==0)
df2 = call %>% filter(type=='중국음식' & (gu=="강남구") & weekend==0)
df3 = call %>% filter(type=='피자' & (gu=="강남구") & weekend==0)

df = df3
df$month <- as.integer(substr(df$month,1,1))
cor_5 <- rcorr(as.matrix(df[,c(4,7,8,9,10,11,12,13,14)]))
M <- cor_5$r
p_mat <- cor_5$P
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method = "color", col = col(200),  
         type = "upper",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "darkblue", tl.srt = 45, #Text label color and rotation
         # Combine with significance level
         p.mat = p_mat, sig.level = 0.01,  
         # hide correlation coefficient on the principal diagonal
         diag = FALSE 
)

###### SCATTER PLOT ###### 
attach(df)
colnames(df) # "max.tem" "rain""wind" "dust" "ozon" "nano"

scatter_plot <- function(x){
  ggplot(df, aes(x,call)) + 
    geom_point(col=rgb(0.4,0.4,0.8,0.6)) + ylab('주문량') +
    geom_smooth(method=lm , color="orange", se=FALSE) + theme_hc() 
}

library(patchwork)
g1 = scatter_plot(max.tem) + xlab('')
g2 = scatter_plot(rain) + xlab('강수량')
g3 = scatter_plot(wind) + xlab('강수량')
g4 = scatter_plot(dust) + xlab('강수량')
g5 = scatter_plot(nano) + xlab('강수량')
g6 = scatter_plot(ozon) + xlab('강수량')

g1+g2+g3+g4+g5+g6+plot_layout(ncol = 3)