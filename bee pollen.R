#### pollen analysis ####
#setwd ('C:/Users/user/Dropbox/beehavior/data')
flow  <- read.csv('bee pollen new.csv')
flow_day <- dplyr::filter(flow,date %in% c('2020/9/16',  '2020/9/17',  '2020/9/18',  '2020/9/21',
                                           '2020/9/22',  '2020/9/23',  '2020/10/1',  '2020/10/2',  
                                           '2020/10/3',  '2020/10/4',  '2020/10/8',  '2020/10/15', 
                                           '2020/10/25', '2020/10/26', '2020/10/27', '2020/10/28', 
                                           '2020/10/31', '2020/11/1',  '2020/11/4',  '2020/11/5',  
                                           '2020/11/6',  '2020/11/7',  '2020/11/11', '2020/11/12', 
                                           '2020/11/18', '2020/11/19') )
#sort(flow_day$indoor.temperature..top.)
flow_day <- flow_day[!is.na(flow_day$indoor.temperature..top.),]
flow_day <- flow_day[!is.na(flow_day$temperature.1min.mean..1.2hr.),]

flow_day  <- dplyr::filter(flow_day,!remove16 == 'V')
flow_day$heat.up <- as.factor(flow_day$heat.up)

flow_day$time <- as.POSIXct(as.character(flow_day$time), format = "%H:%M" )

library(lubridate)
flow_day$time <- hour(flow_day$time)
#is.numeric(flow_day$time)

flow_ctrl <- dplyr::filter(flow_day,heat.up == '0')
flow_heat <- dplyr::filter(flow_day,heat.up == '1')


#### Analysis ####
require(Matrix)
library(lme4)
require(carData)
library(car)


### check if warming treatment has an effect on temperature

model=glmer(indoor.temperature..top.~heat.up+(1|hive/date),gaussian,data=flow_day)
Anova(model, type = 3)
summary(model)

# correlation
cor(flow_day$temperature.1min.mean..1.2hr., flow_day$time)
cor.test(flow_day$temperature.1min.mean..1.2hr., flow_day$time)

#ggerrorplot(flow_day_o, x = "time", y = "average.weight.of.pollen..mg.", 
#            desc_stat = "mean_se",
#            color="heat.up",
#            error.plot = "errorbar",            # Change error plot type
#            add = "mean"                        # Add mean points
#)
#ggbarplot(flow_day_o, x = "time", y = "average.weight.of.pollen..mg.", 
#           add = c("mean_se","jitter"),
#           color="heat.up",
#           palete="jco",
#           fill="heat.up",alpha=0.5,
#           position=position_dodge(0.8)
#)
#ggline(flow_day, x = "heat.up", y = "indoor.temperature..top.", 
#      add = c("mean_se", "jitter"))

#ggpaired(flow_day, x = "heat.up", y = "indoor.temperature..top.",
#         color = "heat.up", line.color = "gray", line.size = 0.4,
#         palette = "npg")


#### pollen vs temperature under control temperature  ####

# remove outlier
#flow_day_o <- dplyr::filter(flow_day,!outlier == 'V')

# test if corelative between indoor temperature and time or not
#model0=glmer(log(average.weight.of.pollen..mg.)~indoor.temperature..top.+time+(1|hive/date),gaussian,data=flow_day_o)
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
#vif.mer(model0)
# select what factors need to consider

flow_day$number.of.bees.sc <- as.numeric(scale(flow_day$number.of.bees))

# model0.1=glmer(log(average.weight.of.pollen..mg.)~indoor.temperature..top.*heat.up*temperature.1min.mean..1.2hr.+(hive|date),gaussian,data=flow_day_o)
model0.1=glmer(log(weight.of.pollen..1hr.)~indoor.temperature..top.*heat.up+number.of.bees.sc+(1|hive/date),gaussian,data=flow_day)
Anova(model0.1,type=3)
summary(model0.1)
hist(resid(model0.1))
#vif.mer(model0.1)

# model0.2=glmer(log(average.weight.of.pollen..mg.)~time*heat.up*temperature.1min.mean..1.2hr.+(hive|date),gaussian,data=flow_day_o)
model0.2=glmer(log(weight.of.pollen..1hr.)~time*heat.up+number.of.bees.sc+(1|hive/date),gaussian,data=flow_day)
Anova(model0.2,type=3)
summary(model0.2)
hist(resid(model0.2))
#vif.mer(model0.2)

##test different model
#beehive temperature
#model1=glmer(log(average.weight.of.pollen..mg.)~indoor.temperature..top.*heat.up+(1|hive/date),gaussian,data=flow_day)
model1.1=glmer(log(weight.of.pollen..1hr.)~poly(indoor.temperature..top.,degree=2)[,2]+poly(indoor.temperature..top., degree=2)[,1]*heat.up+number.of.bees.sc+(1|hive/date),gaussian,data=flow_day)
Anova(model1.1,type=3)
#anova(model1,model1.1)
#AIC(model1,model1.1)
#vif.mer(model1)
hist(resid(model1.1))


#environmental temperature
#flow_day <- flow_day[!is.na(flow_day$temperature.1min.mean..1.2hr.),]
#modelo=glmer(log(weight.of.pollen..1hr.)~(poly(temperature.1min.mean..1.2hr.,degree=2)[,2])+heat.up+poly(temperature.1min.mean..1.2hr., degree=2)[,1]+number.of.bees.sc+(1|hive/date),gaussian,data=flow_day)
#hist(resid(modelo))
#Anova(modelo,type=3)

modelo3=glmer(log(weight.of.pollen..1hr.)~poly(temperature.1min.mean..1.2hr.,degree=3)[,3]+(poly(temperature.1min.mean..1.2hr.,degree=3)[,2])+heat.up+poly(temperature.1min.mean..1.2hr., degree=3)[,1]+number.of.bees.sc+(1|hive/date),gaussian,data=flow_day)
Anova(modelo3,type=3)
vif.mer(modelo3)

#modelo4=glmer(log(weight.of.pollen..1hr.)~poly(temperature.1min.mean..1.2hr.,degree=4)[,3]+poly(temperature.1min.mean..1.2hr.,degree=4)[,2]+poly(temperature.1min.mean..1.2hr.,degree=4)[,4]+(poly(temperature.1min.mean..1.2hr., degree=4)[,1])+heat.up+number.of.bees.sc+(1|hive/date),gaussian,data=flow_day)
#Anova(modelo4,type=3)

#anova(modelo4,modelo3)

flow_ctrl <- dplyr::filter(flow_day,heat.up == '0')
flow_heat <- dplyr::filter(flow_day,heat.up == '1')

modelc=glmer(log(weight.of.pollen..1hr.)~poly(temperature.1min.mean..1.2hr., degree=3)[,3]+poly(temperature.1min.mean..1.2hr., degree=3)[,2]+poly(temperature.1min.mean..1.2hr., degree=3)[,1]+number.of.bees.sc+(1|hive/date),data=flow_ctrl)
modelw=glmer(log(weight.of.pollen..1hr.)~poly(temperature.1min.mean..1.2hr., degree=3)[,3]+poly(temperature.1min.mean..1.2hr., degree=3)[,2]+poly(temperature.1min.mean..1.2hr., degree=3)[,1]+number.of.bees.sc+(1|hive/date),data=flow_heat)
Anova(modelc,type=3)
Anova(modelw,type=3)



modelo.1=glmer(log(weight.of.pollen..1hr.)~heat.up+number.of.bees.sc+(1|hive/date),gaussian,data=flow_day[flow_day$temperature.1min.mean..1.2hr.>= 23.8& flow_day$temperature.1min.mean..1.2hr.<=30.6,])
modelo.1=glmer(log(weight.of.pollen..1hr.)~heat.up+number.of.bees.sc+(1|hive/date),gaussian,data=flow_day[flow_day$temperature.1min.mean..1.2hr.> 30.6,])
modelo.1=glmer(log(weight.of.pollen..1hr.)~heat.up+number.of.bees.sc+(1|hive/date),gaussian,data=flow_day[flow_day$temperature.1min.mean..1.2hr.< 23.8,])
Anova(modelo.1,type=3)





##time
modelt=glmer(log(weight.of.pollen..1hr.)~poly(time, degree=3)[,2]+poly(time, degree=3)[,3]+(poly(time, degree=3)[,1])*heat.up+number.of.bees.sc+(1|hive/date),gaussian,data=flow_day)
Anova(modelt,type=3)
vif.mer(modelt)

flow_ctrl <- dplyr::filter(flow_day,heat.up == '0')
flow_heat <- dplyr::filter(flow_day,heat.up == '1')

modeltc=glmer(log(weight.of.pollen..1hr.)~poly(time, degree=3)[,3]+poly(time, degree=3)[,2]+poly(time, degree=3)[,1]+number.of.bees.sc+(1|hive/date),data=flow_ctrl)
modeltw=glmer(log(weight.of.pollen..1hr.)~poly(time, degree=3)[,3]+poly(time, degree=3)[,2]+poly(time, degree=3)[,1]+number.of.bees.sc+(1|hive/date),data=flow_heat)
Anova(modeltc,type=3)
Anova(modeltw,type=3)

modelt.1=glmer(log(weight.of.pollen..1hr.)~heat.up+number.of.bees.sc+(1|hive/date),data=flow_day[flow_day$time==6,],control=lmerControl(check.nobs.vs.nlev="ignore",check.nobs.vs.rankZ = "ignore",check.nobs.vs.nRE = "ignore"))
Anova(modelt.1,type=3)







##time ( + ambient temp)
# model2=glmer(log(average.weight.of.pollen..mg.)~time*heat.up+(hive|date),gaussian,data=flow_day_o)
#model2.1=glmer(log(weight.of.pollen..1hr.)~poly(time, degree=2)[,2]+(poly(time, degree=2)[,1])*heat.up+temperature.1min.mean..1.2hr.+number.of.bees.sc+(1|hive/date),gaussian,data=flow_day)
#Anova(model2.1,type=3)
#anova(model2,model2.1)
#AIC(model2,model2.1)
#vif.mer(model2)
#vif.mer(model2.1)
hist(resid(model2.1))

model2.13=glmer(log(weight.of.pollen..1hr.)~poly(time, degree=3)[,2]+poly(time, degree=3)[,3]+(poly(time, degree=3)[,1])*heat.up+temperature.1min.mean..1.2hr.+number.of.bees.sc+(1|hive/date),gaussian,data=flow_day)
Anova(model2.13,type=3)
vif.mer(model2.13)

#model2.14=glmer(log(weight.of.pollen..1hr.)~poly(time, degree=4)[,4]+poly(time, degree=4)[,2]+poly(time, degree=4)[,3]+(poly(time, degree=4)[,1])*heat.up+temperature.1min.mean..1.2hr.+number.of.bees.sc+(1|hive/date),gaussian,data=flow_day)
#Anova(model2.14,type=3)

#anova(model2.14,model2.13)

flow_ctrl <- dplyr::filter(flow_day,heat.up == '0')
flow_heat <- dplyr::filter(flow_day,heat.up == '1')

modelc=glmer(log(weight.of.pollen..1hr.)~poly(time, degree=3)[,3]+poly(time, degree=3)[,2]+poly(time, degree=3)[,1]+temperature.1min.mean..1.2hr.+number.of.bees.sc+(1|hive/date),data=flow_ctrl)
modelw=glmer(log(weight.of.pollen..1hr.)~poly(time, degree=3)[,3]+poly(time, degree=3)[,2]+poly(time, degree=3)[,1]+temperature.1min.mean..1.2hr.+number.of.bees.sc+(1|hive/date),data=flow_heat)
Anova(modelc,type=3)
Anova(modelw,type=3)

model2.13.1=glmer(log(weight.of.pollen..1hr.)~heat.up+temperature.1min.mean..1.2hr.+number.of.bees.sc+(1|hive/date),data=flow_day[flow_day$time==6,],control=lmerControl(check.nobs.vs.nlev="ignore",check.nobs.vs.rankZ = "ignore",check.nobs.vs.nRE = "ignore"))
Anova(model2.13.1,type=3)



#### plot ####
library(ggplot2)
# setwd ('C:/Users/user/Dropbox/beehavior/R')
# pdf ('heat up or not(temperature).pdf') 6*6
p <- ggplot(flow_day, aes(x=indoor.temperature..top., y=weight.of.pollen..1hr., col=heat.up)) +
  geom_point(alpha=0.5, size=3) +
  stat_smooth(method='glm',#formula=y~poly(x,1),
              se=F,aes(group=heat.up)) +
  theme_classic() +
  #scale_y_continuous(trans = 'log10') +
  labs(x="Beehive temperature (°C)",y="Log weight of pollen (mg)") +
  scale_color_manual(values=c("#4393c3", "#d6604d"),
                     breaks=c("0", "1"), labels=c("Control", "Warming")) +
  scale_y_continuous(trans = 'log10')

p + theme(axis.text = element_text(size=13),
          axis.title = element_text(size=16),
          #legend.position = "top",
          legend.text = element_text(size=12),
          #legend.title = element_text(size=14),
          #legend.key.size = unit(1.5, "cm"),
          #legend.key.width = unit(2,"cm"),
          legend.title=element_blank()
          ) 



# pdf ('heat up or not(environmental temp).pdf') 6*6
p <- ggplot(flow_day, aes(x=temperature.1min.mean..1.2hr., y=weight.of.pollen..1hr.)) +
  geom_point(alpha=0.5, size=3, aes(col=heat.up)) +
  stat_smooth(method='glm', formula=y~poly(x,3), se=F, colour="#9970ab") +
  theme_classic() +
  labs(x="Ambient temperature (°C)", y="Log weight of pollen (mg)") +
  scale_color_manual(values=c("#4393c3", "#d6604d"),
                     breaks=c("0", "1"), labels=c("Control", "Warming")) +
  scale_y_continuous(trans = 'log10')

p + theme(axis.text = element_text(size=13),
          axis.title = element_text(size=16),
          #legend.position = "top",
          legend.text = element_text(size=12),
          #legend.title = element_text(size=14),
          #legend.key.size = unit(1.5, "cm"),
          #legend.key.width = unit(2,"cm"),
          legend.title=element_blank()
)  




#pdf ('heat up or not(time).pdf')
q <- ggplot() +
  geom_point(data=flow_day, aes(x=time, y=weight.of.pollen..1hr., col=heat.up), alpha=0.5, size=3) +
  stat_smooth(data=flow_ctrl, aes(x=time, y=weight.of.pollen..1hr.), method="glm",formula=y~poly(x,2), se=F, col = "#4393c3") +
  stat_smooth(data=flow_heat, aes(x=time, y=weight.of.pollen..1hr.), method="glm",formula=y~poly(x,3), se=F, col = "#d6604d") +
  theme_classic() +
  scale_y_continuous(trans = 'log10') +
#  xlim('06:00', '16:00') +
  labs(x="Time",y="Log weight of pollen (mg)") +
  scale_color_manual(values=c("#4393c3", "#d6604d"),
                     breaks=c("0", "1"), labels=c("Control", "Warming")) +
  scale_y_continuous(trans = 'log10')

q + theme(axis.text = element_text(size=13),
          axis.title = element_text(size=16),
          #legend.position = "top",
          legend.text = element_text(size=12),
          #legend.title = element_text(size=14),
          #legend.key.size = unit(1.5, "cm"),
          #legend.key.width = unit(2,"cm")+
          legend.title=element_blank()
          )







#### bar plot ####
flow_day <- dplyr::filter(flow,date %in% c('2020/9/16',  '2020/9/17',  '2020/9/18',  '2020/9/21',
                                           '2020/9/22',  '2020/9/23',  '2020/10/1',  '2020/10/2',  
                                           '2020/10/3',  '2020/10/4',  '2020/10/8',  '2020/10/15', 
                                           '2020/10/25', '2020/10/26', '2020/10/27', '2020/10/28', 
                                           '2020/10/31', '2020/11/1',  '2020/11/4',  '2020/11/5',  
                                           '2020/11/6',  '2020/11/7',  '2020/11/11', '2020/11/12', 
                                           '2020/11/18', '2020/11/19') )
sort(flow_day$indoor.temperature..top.)
flow_day <- flow_day[!is.na(flow_day$indoor.temperature..top.),]
flow_day$heat.up <- as.factor(flow_day$heat.up)
flow_day_o <- dplyr::filter(flow_day,!outlier == 'V')
flow_day$time <- as.factor(flow_day$time)

model3=glmer(log(average.weight.of.pollen..mg.)~heat.up*time+(hive|date),gaussian,data=flow_day_o)
Anova(model3,type=3)
#post-hoc comparisons
library(lsmeans)
lsmeans(model3,pairwise~heat.up|time, adjust="tukey")
#lsmeans(model3,pairwise~time|heat.up, adjust="tukey")

# create SE
library(Rmisc)
SE <- summarySE(flow_day_o, measurevar="average.weight.of.pollen..mg.", groupvars=c("heat.up","time"))

library(ggplot2)
#setwd ('C:/Users/user/Dropbox/beehavior/R')
#pdf ('heat up or not(each time).pdf')
r <- ggplot(SE, aes(x=time, y=average.weight.of.pollen..mg., fill=heat.up)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_classic() +
  labs(x="Time",y="Weight of pollen per bee (mg)") +
  scale_fill_manual(values=c("#a6d96a", "#f46d43"),
                    breaks=c("0", "1"), labels=c("Control", "Warming")) +
  geom_errorbar(aes(ymin=average.weight.of.pollen..mg.-se, ymax=average.weight.of.pollen..mg.+se, color=heat.up),width=.2,                    # Width of the error bars
                position=position_dodge(.9), size=1) +
  scale_color_manual("heat.up", breaks=c(0,1),values=c("#66bd63", "#d73027"),
                     labels=c("Control", "Warming")) +
  scale_y_continuous(expand = c(0,0))
  
r + theme(axis.text = element_text(size=13),
          axis.title = element_text(size=16),
          #legend.position = "top",
          legend.text = element_text(size=12),
          #legend.title = element_text(size=14),
          #legend.key.size = unit(1.5, "cm"),
          #legend.key.width = unit(2,"cm"),
          legend.title=element_blank()
          )


#### mean of pollen weight change under warming ####
#setwd ('C:/Users/user/Dropbox/beehavior/data')
flow  <- read.csv('bee pollen new.csv')
flow_day <- dplyr::filter(flow,date %in% c('2020/9/16',  '2020/9/17',  '2020/9/18',  '2020/9/21',
                                           '2020/9/22',  '2020/9/23',  '2020/10/1',  '2020/10/2',  
                                           '2020/10/3',  '2020/10/4',  '2020/10/8',  '2020/10/15', 
                                           '2020/10/25', '2020/10/26', '2020/10/27', '2020/10/28', 
                                           '2020/10/31', '2020/11/1',  '2020/11/4',  '2020/11/5',  
                                           '2020/11/6',  '2020/11/7',  '2020/11/11', '2020/11/12', 
                                           '2020/11/18', '2020/11/19') )
flow_day <- flow_day[!is.na(flow_day$average.weight.of.pollen..mg.),]
# flow_day <- flow_day[!is.na(flow_day$indoor.temperature..top.),]
flow_day_o <- dplyr::filter(flow_day,!outlier == 'V')
flow_ctrl <- dplyr::filter(flow_day_o,heat.up == '0')
flow_heat <- dplyr::filter(flow_day_o,heat.up == '1')

flow_otime <- dplyr::filter(flow_day_o, time %in% '07:00')
flow_ctime <- dplyr::filter(flow_ctrl,  time %in% '06:00')
flow_htime <- dplyr::filter(flow_heat,  time %in% '06:00')

mean(flow_otime$average.weight.of.pollen..mg.)
mean(flow_ctime$average.weight.of.pollen..mg.)
mean(flow_htime$average.weight.of.pollen..mg.)
