#### indoor temperature ####
#setwd ('C:/Users/user/Dropbox/beehavior/data')
tem <- read.csv('indoor temperature.csv')
tem <- dplyr::filter(tem,date %in% c('2020/9/21',  '2020/9/22',  '2020/10/1',  '2020/10/2',  
                                     '2020/10/3',  '2020/10/4',  '2020/10/8',  '2020/10/15', 
                                     '2020/10/28', '2020/10/29', '2020/10/30', '2020/10/31', 
                                     '2020/11/1',  '2020/11/4',  '2020/11/5',  '2020/11/6',  
                                     '2020/11/7',  '2020/11/9',  '2020/11/10', '2020/11/11', 
                                     '2020/11/12', '2020/11/13', '2020/11/14', '2020/11/18', 
                                     '2020/11/19') )
sort(tem$temperature)
tem <- tem[!is.na(tem$temperature),]
tem$heat.up <- as.factor(tem$heat.up)

tem$time <- as.POSIXct(as.character(tem$time), format = "%H:%M" )
#is.numeric(flow_day$time)

#### analysis ####
require(Matrix)
library(lme4)
require(carData)
library(car)

### check if warming treatment has an effect on temperature
model=glmer(temperature~heat.up+(1|hive/date),gaussian,data=tem)
Anova(model, type = 3)
summary(model)


#### plot ####
library(ggplot2)
# pdf ('Time vs Temperature.pdf') 6*6
TT <- ggplot(tem, aes(x=time, y=temperature, col=heat.up)) +
  geom_point(alpha=0.2, size=3) +
  stat_smooth(method='glm',formula=y~poly(x,2),se=F,aes(group=heat.up)) +
  theme_classic() +
  labs(x="Time",y="Beehive temperature (°C)") +
  scale_color_manual(values=c("#4393c3", "#d6604d"),
                     breaks=c("0", "1"), labels=c("Control", "Warming"))

TT + theme(axis.text = element_text(size=13),
           axis.title = element_text(size=16),
           #legend.position = "top",
           legend.text = element_text(size=12),
           #legend.title = element_text(size=14),
           #legend.key.size = unit(1.5, "cm"),
           #legend.key.width = unit(2,"cm")+
           legend.title=element_blank()
)            





#### indoor temperature mean ####
flow_ctrl <- dplyr::filter(tem,heat.up == '0')
flow_heat <- dplyr::filter(tem,heat.up == '1')

mean(flow_heat$temperature)-mean(flow_ctrl$temperature)
# 33.36607 - 31.01475





#### indoor v outdoor temperature ####
flow  <- read.csv('bee activity new.csv')
flow_day <- dplyr::filter(flow,date %in% c('2020/9/21',  '2020/9/22',  '2020/10/1',  '2020/10/2',  
                                     '2020/10/3',  '2020/10/4',  '2020/10/8',  '2020/10/15', 
                                     '2020/10/28', '2020/10/29', '2020/10/30', '2020/10/31', 
                                     '2020/11/1',  '2020/11/4',  '2020/11/5',  '2020/11/6',  
                                     '2020/11/7',  '2020/11/9',  '2020/11/10', '2020/11/11', 
                                     '2020/11/12', '2020/11/13', '2020/11/14', '2020/11/18', 
                                     '2020/11/19') )

flow_day  <- dplyr::filter(flow_day,!remove == 'V')
flow_day$heat.up <- as.factor(flow_day$heat.up)


flow_day$time <- as.POSIXct(as.character(flow_day$time), format = "%H:%M" )

flow_day <- flow_day[!is.na(flow_day$indoor.temperature..top.),]
flow_day <- flow_day[!is.na(flow_day$temperature.1min.mean..5minn.),]

#### analysis ####
require(Matrix)
library(lme4)
require(carData)
library(car)

flow_ctrl <- dplyr::filter(flow_day,heat.up == '0')
model=glmer(indoor.temperature..top.~temperature.1min.mean..5minn.+(1|hive/date),gaussian,data=flow_ctrl)
Anova(model,type=3)
summary(model)

# correlation
cor(flow_ctrl$indoor.temperature..top., flow_ctrl$temperature.1min.mean..5minn.)
cor.test(flow_ctrl$indoor.temperature..top., flow_ctrl$temperature.1min.mean..5minn.)



#### plot ####
# in vs out temperature 6*6
library(ggplot2)
IT <- ggplot(flow_ctrl, aes(x=temperature.1min.mean..5minn., y=indoor.temperature..top.)) +
  geom_point(alpha=0.5, size=3, col="#4393c3") +
  stat_smooth(method='glm', formula=y~poly(x,1), se=F, col="#4393c3") +
  theme_classic() +
  labs(x="Ambient temperature (°C)",y="Beehive temperature (°C)")

IT + theme(axis.text = element_text(size=13),
           axis.title = element_text(size=16),
           #legend.position = "top",
           legend.text = element_text(size=12),
           #legend.title = element_text(size=14),
           #legend.key.size = unit(1.5, "cm"),
           #legend.key.width = unit(2,"cm")+
           legend.title=element_blank()
)
