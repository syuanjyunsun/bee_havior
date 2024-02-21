#setwd ('C:/Users/user/Dropbox/beehavior/data')
#### analysis ####
anthesis  <- read.csv('Bidens anthesis.csv')
onset <- tidyr::gather(anthesis, duplication, amount, X1:X39, na.rm = TRUE)

onset$time <- as.POSIXct(as.character(onset$time), format = "%H:%M" )

#is.numeric(onset_12$time)
library(lubridate)
onset$time <- hour(onset$time)

 
require(Matrix)
library(lme4)
require(carData)
library(car)


## before 12:00
onset_12 <- dplyr::filter(onset,time <= 12)

model1.2=glmer(amount~(poly(time,degree=2)[,2])*treatment+poly(time,degree=2)[,1]+(1|duplication),poisson,data=onset_12)
Anova(model1.2,type=3)
#sum(resid(model1.2, type = "pearson")^2)/model1.2$df.resid
summary(model1.2)
#sum(resid(model1.2, type = "pearson")^2)/822 # =1.464
#hist(resid(model1.2))




## consider total flowers
#library(dplyr)
#total <- group_by(onset, treatment) %>% summarise(onset_sum = sum(amount))

#model1.2=glmer(amount~(poly(time,degree=2)[,2]+poly(time,degree=2)[,1])*treatment+(1|duplication),offset=log(TOTAL),poisson,data=onset)







#### plot "Bidens anthesis" 6*6 ####
library(ggplot2)
anthesis16  <- read.csv('Bidens anthesis no 16.csv')
onset16 <- tidyr::gather(anthesis16, duplication, amount, X1:X39, na.rm = TRUE)

onset16$time <- as.POSIXct(as.character(onset16$time), format = "%H:%M" )
q <- ggplot(onset16, aes(x=time, y=amount, col=treatment)) +
  geom_count(alpha=0.4, position = position_jitter(h=0.1,w=200), show.legend=FALSE) +
  scale_size_area(max_size = 10) +
#  geom_point(alpha=0.3,size=3, position = position_jitter(h=0.15,w=300), show.legend=FALSE) +
  stat_smooth(method='glm',formula=y~poly(x,2),se=F,aes(group=treatment)) +
  theme_classic() +
  labs(x="Time",y="Number of flowers at anthesis") +
  scale_color_manual(values=c("#4393c3", "#d6604d"),
                     breaks=c("control", "warming"), labels=c("Control", "Warming"))

q + theme(axis.text = element_text(size=13),
          axis.title = element_text(size=16),
          #legend.position = "top",
          legend.text = element_text(size=12),
          #legend.title = element_text(size=14),
          #legend.key.size = unit(1.5, "cm"),
          #legend.key.width = unit(2,"cm")+
          legend.title=element_blank())+ 
  scale_y_continuous(breaks = seq(0, 12, len = 7))
