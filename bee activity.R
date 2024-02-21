#### bee-havior ####
#setwd ('C:/Users/user/Dropbox/beehavior/data')
flow  <- read.csv('bee activity new.csv')
flow_day <- dplyr::filter(flow,date %in% c('2020/9/16',  '2020/9/17',  '2020/9/18',  '2020/9/21',
                                           '2020/9/22',  '2020/9/23',  '2020/10/1',  '2020/10/2',  
                                           '2020/10/3',  '2020/10/4',  '2020/10/8',  '2020/10/15', 
                                           '2020/10/25', '2020/10/26', '2020/10/27', '2020/10/28', 
                                           '2020/10/31', '2020/11/1',  '2020/11/4',  '2020/11/5',  
                                           '2020/11/6',  '2020/11/7',  '2020/11/11', '2020/11/12', 
                                           '2020/11/18', '2020/11/19') )
#sort(flow_day$indoor.temperature..top.)
flow_day <- flow_day[!is.na(flow_day$indoor.temperature..top.),]
flow_day  <- dplyr::filter(flow_day,!remove == 'V')
flow_day$heat.up <- as.factor(flow_day$heat.up)

flow_day$time <- as.POSIXct(as.character(flow_day$time), format = "%H:%M" )
flow_day$number.of.bees.sc <- as.numeric(scale(flow_day$number.of.bees))


library(lubridate)
flow_day$time <- hour(flow_day$time)
#is.numeric(flow_day$time)

flow_day <- flow_day[!is.na(flow_day$temperature.1min.mean..5minn.),]

flow_ctrl <- dplyr::filter(flow_day,heat.up == '0')
flow_heat <- dplyr::filter(flow_day,heat.up == '1')


#### Analysis ####

require(Matrix)
library(lme4)
require(carData)
library(car)

# if warming affect indoor temperature
model=glmer(indoor.temperature..top.~heat.up+(1|hive/date),gaussian,data=flow_day)
Anova(model,type=3)
summary(model)

# correlation
cor(flow_day$temperature.1min.mean..5minn., flow_day$time)
cor.test(flow_day$temperature.1min.mean..5minn., flow_day$time)

# test if corelative between indoor temperature and time or not
#model0=glmer(log(average.number.of.bee+1)~(indoor.temperature..top.+time)*heat.up+(hive|date),gaussian,data=flow_day)
#check coleanarity
vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}
# all <5 is good
vif.mer(model0)

####test different model

##<3 indicate no overdispersion
#sum(resid(model2.1.1, type = "pearson")^2)/model2.1.1$df.resid

##standardised variable
#flow_day$number.of.bees.sc <- as.numeric(scale(flow_day$number.of.bees))




## temperature
# indoor
model1=glmer.nb(number.of.outport~poly(indoor.temperature..top., degree=2)[,2]*heat.up+poly(indoor.temperature..top., degree=2)[,1]+number.of.bees.sc+(1|hive/date),data=flow_day)
Anova(model1,type=3)
#hist(resid(model1))

summary(model1)
#anova(model1,model1.1)
#AIC(model1,model1.1)
#vif.mer(model1)
#vif.mer(model1.1)




#outdoor
#flow_day <- flow_day[!is.na(flow_day$temperature.1min.mean..5minn.),]
#modelo2=glmer.nb(number.of.outport~poly(temperature.1min.mean..5minn., degree=2)[,2]+heat.up+poly(temperature.1min.mean..5minn., degree=2)[,1]+number.of.bees.sc+(1|hive/date),data=flow_day)
#Anova(modelo2,type=3)

modelo3=glmer.nb(number.of.outport~poly(temperature.1min.mean..5minn., degree=3)[,3]+(poly(temperature.1min.mean..5minn., degree=3)[,2])+heat.up+poly(temperature.1min.mean..5minn., degree=3)[,1]+number.of.bees.sc+(1|hive/date),data=flow_day)
Anova(modelo3,type=3)
vif.mer(modelo3)
summary(modelo3)

#modelo4=glmer.nb(number.of.outport~poly(temperature.1min.mean..5minn., degree=4)[,4]+(poly(temperature.1min.mean..5minn., degree=4)[,3])+heat.up+poly(temperature.1min.mean..5minn., degree=4)[,2]+poly(temperature.1min.mean..5minn., degree=4)[,1]+number.of.bees.sc+(1|hive/date),data=flow_day)
#Anova(modelo4,type=3)

anova(modelo4,modelo3)

# make sure what kind of linear control and warming are 

#modelc=glmer.nb(number.of.outport~poly(temperature.1min.mean..5minn., degree=3)[,3]+poly(temperature.1min.mean..5minn., degree=3)[,2]+poly(temperature.1min.mean..5minn., degree=3)[,1]+number.of.bees.sc+(1|hive/date),data=flow_ctrl)
#modelw=glmer.nb(number.of.outport~poly(temperature.1min.mean..5minn., degree=3)[,3]+poly(temperature.1min.mean..5minn., degree=3)[,2]+poly(temperature.1min.mean..5minn., degree=3)[,1]+number.of.bees.sc+(1|hive/date),data=flow_heat)
#Anova(modelc,type=3)
#Anova(modelw,type=3)


#modelo.1=glmer.nb(number.of.outport~heat.up+number.of.bees.sc+(1|hive/date),data=flow_day[flow_day$temperature.1min.mean..5minn.>= 26.2& flow_day$temperature.1min.mean..5minn.<=32.3,])
#modelo.1=glmer.nb(number.of.outport~heat.up+number.of.bees.sc+(1|hive/date),data=flow_day[flow_day$temperature.1min.mean..5minn.<= 20.5,])
#modelo.1=glmer.nb(number.of.outport~heat.up+number.of.bees.sc+(1|hive/date),data=flow_day[flow_day$temperature.1min.mean..5minn.> 32.3,])
#Anova(modelo.1,type=3)


## time
modelt=glmer.nb(number.of.outport~(poly(time, degree=3)[,3]+poly(time, degree=3)[,2])*heat.up+poly(time, degree=3)[,1]+number.of.bees.sc+(1|hive/date),data=flow_day)
Anova(modelt,type=3)
vif.mer(modelt)

modeltc=glmer.nb(number.of.outport~poly(time, degree=3)[,3]+poly(time, degree=3)[,2]+poly(time, degree=3)[,1]+number.of.bees.sc+(1|hive/date),data=flow_ctrl)
modeltw=glmer.nb(number.of.outport~poly(time, degree=3)[,3]+poly(time, degree=3)[,2]+poly(time, degree=3)[,1]+number.of.bees.sc+(1|hive/date),data=flow_heat)
Anova(modeltc,type=3)
Anova(modeltw,type=3)

modelt.1=glmer.nb(number.of.outport~heat.up+number.of.bees.sc+(1|hive/date),data=flow_day[flow_day$time==6,])
Anova(modelt.1,type=3)




## time ( + ambient temp)
#model2=glmer.nb(number.of.outport~(poly(time, degree=2)[,2])+heat.up+poly(time, degree=2)[,1]+temperature.1min.mean..5minn.+number.of.bees.sc+(1|hive/date),data=flow_day)
#Anova(model2,type=3)
#hist(resid(model2))

model23=glmer.nb(number.of.outport~(poly(time, degree=3)[,3]+poly(time, degree=3)[,2])*heat.up+poly(time, degree=3)[,1]+temperature.1min.mean..5minn.+number.of.bees.sc+(1|hive/date),data=flow_day)
Anova(model23,type=3)
vif.mer(model23)

#model24=glmer.nb(number.of.outport~poly(time, degree=4)[,4]+(poly(time, degree=4)[,3]+poly(time, degree=4)[,2])*heat.up+poly(time, degree=4)[,1]+temperature.1min.mean..5minn.+number.of.bees.sc+(1|hive/date),data=flow_day)
#Anova(model24,type=3)

#anova(model24,model23)

model2c=glmer.nb(number.of.outport~poly(time, degree=3)[,3]+poly(time, degree=3)[,2]+poly(time, degree=3)[,1]+temperature.1min.mean..5minn.+number.of.bees.sc+(1|hive/date),data=flow_ctrl)
model2w=glmer.nb(number.of.outport~poly(time, degree=3)[,3]+poly(time, degree=3)[,2]+poly(time, degree=3)[,1]+temperature.1min.mean..5minn.+number.of.bees.sc+(1|hive/date),data=flow_heat)
Anova(model2c,type=3)
Anova(model2w,type=3)


##each time
model2.1=glmer.nb(number.of.outport~heat.up+temperature.1min.mean..5minn.+number.of.bees.sc+(1|hive/date),data=flow_day[flow_day$time>= 8& flow_day$time<=12,])
model2.1=glmer.nb(number.of.outport~heat.up+temperature.1min.mean..5minn.+number.of.bees.sc+(1|hive/date),data=flow_day[flow_day$time<=7,],control=lmerControl(check.nobs.vs.nlev="ignore",check.nobs.vs.rankZ = "ignore",check.nobs.vs.nRE = "ignore"))
model2.1=glmer.nb(number.of.outport~heat.up+temperature.1min.mean..5minn.+number.of.bees.sc+(1|hive/date),data=flow_day[flow_day$time>=14,])
model2.1=glmer.nb(number.of.outport~heat.up+temperature.1min.mean..5minn.+number.of.bees.sc+(1|hive/date),data=flow_day[flow_day$time==6 | flow_day$time==7,])
model2.1=glmer.nb(number.of.outport~heat.up+temperature.1min.mean..5minn.+number.of.bees.sc+(1|hive/date),data=flow_day[flow_day$time==6,])
Anova(model2.1,type=3)
#hist(resid(model2.1))

summary(model2)
#anova(model2,model2.1)
#AIC(model2,model2.1)
#vif.mer(model2)
#vif.mer(model2.1)


## time + ambient temperature
modelt2=glmer.nb(number.of.outport~(poly(time, degree=2)[,2])*heat.up+poly(time, degree=2)[,1]+(poly(temperature.1min.mean..5minn., degree=2)[,2])+poly(temperature.1min.mean..5minn., degree=2)[,1]+heat.up+number.of.bees.sc+(1|hive/date),data=flow_day)

modelt3=glmer.nb(number.of.outport~poly(time, degree=3)[,3]+(poly(time, degree=3)[,2])*heat.up+poly(time, degree=3)[,1]+(poly(temperature.1min.mean..5minn., degree=3)[,3])*heat.up+poly(temperature.1min.mean..5minn., degree=3)[,1]+poly(temperature.1min.mean..5minn., degree=3)[,2]+number.of.bees.sc+(1|hive/date),data=flow_day)

Anova(modelt3,type=3)

#anova(modelt2,modelt3)

modeltc=glmer.nb(number.of.outport~poly(time, degree=2)[,2]+poly(time, degree=2)[,1]+poly(temperature.1min.mean..5minn., degree=3)[,3]+poly(temperature.1min.mean..5minn., degree=3)[,2]+poly(temperature.1min.mean..5minn., degree=3)[,1]+number.of.bees.sc+(1|hive/date),data=flow_ctrl)
modeltw=glmer.nb(number.of.outport~poly(time, degree=2)[,2]+poly(time, degree=2)[,1]+poly(temperature.1min.mean..5minn., degree=3)[,3]+poly(temperature.1min.mean..5minn., degree=3)[,2]+poly(temperature.1min.mean..5minn., degree=3)[,1]+number.of.bees.sc+(1|hive/date),data=flow_heat)
Anova(modeltc,type=3)
Anova(modeltw,type=3)



#### plot ####
library(ggplot2)
# setwd ('C:/Users/user/Dropbox/beehavior/R/bee activity')
# pdf ('heat up or not(temperature).pdf') 6*6

p <- ggplot(flow_day, aes(x=indoor.temperature..top., y=number.of.outport, col=heat.up)) +
  geom_point(alpha=0.5, size=3) +
  stat_smooth(method='glm',formula=y~poly(x,2),
              se=F,aes(group=heat.up)) +
  theme_classic() +
  labs(x="Beehive temperature (°C)",y="Number of departing bees (5 min.)") +
  scale_color_manual(values=c("#4393c3", "#d6604d"),
                     breaks=c("0", "1"), labels=c("Control", "Warming"))

p + theme(axis.text = element_text(size=13),
          axis.title = element_text(size=16),
          #legend.position = "top",
          legend.text = element_text(size=12),
          #legend.title = element_text(size=14),
          #legend.key.size = unit(1.5, "cm"),
          #legend.key.width = unit(2,"cm"),
          legend.title=element_blank()
) 




# pdf ('heat up or not(ambient temp).pdf') 6*6
p <- ggplot(flow_day, aes(x=temperature.1min.mean..5minn., y=number.of.outport)) +
  geom_point(alpha=0.5, size=3, aes(col=heat.up)) +
  stat_smooth(method='glm',formula=y~poly(x,3), se=F, colour="#9970ab") +
  theme_classic() +
  labs(x="Ambient temperature (°C)",y="Number of foragers (5 min.)") +
  scale_color_manual(values=c("#4393c3", "#d6604d"),
                     breaks=c("0", "1"), labels=c("Control", "Warming"))

p + theme(axis.text = element_text(size=13),
          axis.title = element_text(size=16),
          #legend.position = "top",
          legend.text = element_text(size=12),
          #legend.title = element_text(size=14),
          #legend.key.size = unit(1.5, "cm"),
          #legend.key.width = unit(2,"cm"),
          legend.title=element_blank()
) 




# pdf ('heat up or not(time).pdf') 6*6
# flow_day$time <- as.POSIXct(as.character(flow_day$time), format = "%H:%M" )

q <- ggplot() +
  geom_point(data=flow_day, aes(x=time, y=number.of.outport, col=heat.up), alpha=0.5, size=3) +
  stat_smooth(data=flow_ctrl, aes(x=time, y=number.of.outport), method='glm',formula=y~poly(x,1), se=F, col = "#4393c3") +
  stat_smooth(data=flow_heat, aes(x=time, y=number.of.outport), method='glm',formula=y~poly(x,3), se=F, col = "#d6604d") +
  theme_classic() +
  labs(x="Time",y="Number of foragers (5 min.)") +
  scale_color_manual(values=c("#4393c3", "#d6604d"),
                     breaks=c("0", "1"), labels=c("Control", "Warming"))

q + theme(axis.text = element_text(size=13),
          axis.title = element_text(size=16),
          #legend.position = "top",
          legend.text = element_text(size=12),
          #legend.title = element_text(size=14),
          #legend.key.size = unit(1.5, "cm"),
          #legend.key.width = unit(2,"cm")+
          legend.title=element_blank()
)



#### outdoor temperature vs. indoor temperature ####
In_Out <- ggplot(flow_ctrl, aes(x=outdoor.temperature, y=indoor.temperature..top.)) +
  geom_point(alpha=0.5, size=3) +
  stat_smooth(method='glm',formula=y~poly(x,2)) +
  theme_classic() 

In_Out + theme(axis.text = element_text(size=13),
               axis.title = element_text(size=16),
               #legend.position = "top",
               legend.text = element_text(size=12),
               #legend.title = element_text(size=14),
               #legend.key.size = unit(1.5, "cm"),
               #legend.key.width = unit(2,"cm")+
               legend.title=element_blank()
)




#### bar plot ####
flow  <- read.csv('bee activity new.csv')
flow_day <- dplyr::filter(flow,date %in% c('2020/9/16',  '2020/9/17',  '2020/9/18',  '2020/9/21',
                                           '2020/9/22',  '2020/9/23',  '2020/10/1',  '2020/10/2',  
                                           '2020/10/3',  '2020/10/4',  '2020/10/8',  '2020/10/15', 
                                           '2020/10/25', '2020/10/26', '2020/10/27', '2020/10/28', 
                                           '2020/10/31', '2020/11/1',  '2020/11/4',  '2020/11/5',  
                                           '2020/11/6',  '2020/11/7',  '2020/11/11', '2020/11/12', 
                                           '2020/11/18', '2020/11/19') )
sort(flow_day$indoor.temperature..top.)
flow_day <- flow_day[!is.na(flow_day$indoor.temperature..top.),]
flow_day <- flow_day[!is.na(flow_day$number.of.outport),]
flow_day$heat.up <- as.factor(flow_day$heat.up)
flow_day <- dplyr::filter(flow_day,!remove == 'V')

### tukey
#flow_day$time <- as.factor(flow_day$time)
flow_day$time <- as.POSIXct(as.character(flow_day$time), format = "%H:%M" )

library(lubridate)
flow_day$time <- hour(flow_day$time)

model2.1.1=glmer(number.of.outport~time*heat.up+number.of.bees+(hive|date),poisson,data=flow_day)

#post-hoc comparisons
library(lsmeans)
lsmeans(model2.1.1,pairwise~heat.up|time, adjust="tukey")
#lsmeans(model3,pairwise~time|heat.up, adjust="tukey")

# create SE
library(Rmisc)
SE <- summarySE(flow_day, measurevar="number.of.outport", groupvars=c("heat.up","time"))



library(ggplot2)
#setwd ('C:/Users/user/Dropbox/beehavior/R')
#pdf ('heat up or not(each time).pdf') 6*8

range(flow_day$average.number.of.bee)

r <- ggplot(SE, aes(x=time, y=number.of.outport, fill=heat.up)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_classic() +
  labs(x="Time",y="Number of departing bees (5 min.)") +
  scale_fill_manual(values=c("#3ca5e3", "#f48441"),
                    breaks=c("0", "1"), labels=c("Control", "Warming")) +
  geom_errorbar(aes(ymin=number.of.outport-se, ymax=number.of.outport+se, color=heat.up),width=.2,                    # Width of the error bars
                position=position_dodge(.9), size=1) +
  scale_color_manual("heat.up", breaks=c(0,1),values=c("#3ca5e3", "#f48441"),
                     labels=c("Control", "Warming")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,600)) +
  scale_x_continuous(limits = c(5.5,16.5), breaks = seq(6,16,2)) +
  geom_text(aes(label = (c('a', '', '', '', '', '', '', '', 'b', '', '', '', '', '', '', '')), y = number.of.outport+se), vjust = -0.8,  size=6, position=position_dodge(.9))


bar <- r + theme(axis.text = element_text(size=13),
                 axis.title = element_text(size=16),
                 #legend.position = "top",
                 legend.text = element_text(size=12),
                 #legend.title = element_text(size=14),
                 #legend.key.size = unit(1.5, "cm"),
                 #legend.key.width = unit(2,"cm"),
                 legend.title=element_blank()
)
bar