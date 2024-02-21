#setwd ('C:/Users/user/Dropbox/beehavior/data')
feeding <- read.csv('feeding exp.csv')

#### weight analysis ####
require(Matrix)
library(lme4)
require(carData)
library(car)

#class(feeding$weight)
#class(feeding$treatment)


feeding_T1 <- dplyr::filter(feeding,treatment %in% 'T1')
feeding_T2 <- dplyr::filter(feeding,treatment %in% 'T2')
feeding_T3 <- dplyr::filter(feeding,treatment %in% 'T3')
feeding_T4 <- dplyr::filter(feeding,treatment %in% 'T4')

# remove weight's outlier
mod <-lm(weight~eaten, data=feeding_T3)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

T1_w_outlier <- feeding_T1[-10,]
T2_w_outlier <- feeding_T2[-c(11,20),]
T3_w_outlier <- feeding_T3[-17,]
T4_w_outlier <- feeding_T4[-c(12,15),]

#remove convert outlier
outliers_T2 <- boxplot(feeding_T2$convert, plot=T)$out
#T2_outlier<-feeding_T2
#T2_outlier<- T2_outlier[-which(T2_outlier$convert %in% outliers),]
T2_outlier <- feeding_T2[-c(20),] 
boxplot(T2_outlier$convert, plot=T)$out

outliers_T4 <- boxplot(feeding_T4$convert, plot=T)$out
#T4_outlier<-feeding_T4
#T4_outlier<- T4_outlier[-which(T4_outlier$convert %in% outliers),]
T4_outlier <- feeding_T4[-c(12,15),]
boxplot(T4_outlier$convert, plot=T)$out

T1234_outlier <- dplyr::bind_rows(feeding_T1, T2_outlier, feeding_T3, T4_outlier)







### analysis
shapiro.test(T1234_outlier$convert)

model3.1=glm(log(convert)~treatment,gaussian,data=T1234_outlier)
#model3.4=glm(weight~treatment*eaten,gaussian,data=feeding_T124)

Anova(model3.1)
hist(resid(model3.1))
# contrast weight
library(lsmeans)
lsmeans(model3.1,pairwise~treatment, adjust="tukey")
# slope
#a=lstrends (model3.4, ~ treatment, var = "eaten")
#pairs(a)




### add the Compact Letter Display (CLD)
library(multcomp)

T1234_outlier$treatment<-as.factor(T1234_outlier$treatment)
model3.1=glm(log(convert)~treatment,gaussian,data=T1234_outlier)
Treat.comp <- glht(model3.1, mcp(treatment="Tukey", interaction_average=TRUE))
summary(Treat.comp)
# add letters to each mean
model_means_cld <- cld(object = Treat.comp, decreasing = FALSE, adjust = "Tukey", Letters = letters, alpha = 0.05)
# show output
model_means_cld







#### mortality rate analysis & plot ####
#install.packages(c("survival", "survminer"))
class(feeding$status)
library("survival")
library("survminer")
#res.cox <- coxph(Surv(time, status) ~ treatment, data = feeding_T1234)
res.cox <- coxph(Surv(time, status) ~ treatment, data = feeding_T124)
summary(res.cox)

library(ggplot2)
ggsurvplot(survfit(res.cox, data = feeding_T124), palette = "#2E9FDF")
fit<- survfit(Surv(time, status) ~ treatment, data = feeding_T124)
ggsurvplot(fit, pval = TRUE)






#### efficiency & weight plot ####
T1234_outlier <- T1234_outlier[!is.na(T1234_outlier$weight),]
#T1234_outlier$efficiency <- T1234_outlier$convert*100

col <- c("#4393c3", "#fdae61", "#f46d43", "#d6604d")

library(Rmisc)
SE <- summarySE(T1234_outlier, measurevar="convert", groupvars="treatment")

#6*6
library(ggplot2)
e <- ggplot(SE, aes(x=treatment, y=convert)) +
  geom_bar(stat="identity", position=position_dodge(), fill=col) +
  theme_classic() +
  labs(x="Treatment",y="Larval growth efficiency index (mg/µL)") +
  coord_cartesian(ylim=c(0.8,1)) +
  geom_errorbar(aes(ymin=convert-se, ymax=convert+se), color=col, width=.2, position=position_dodge(.9), size=1) +
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_discrete(breaks=c("T1","T2","T3","T4"),
                   labels=c("Control", "Warming", "Warming plus", expression(paste(italic("Bidens"), " only")))) + 
  geom_text(aes(label = (c('a', 'b', 'b', 'b')), y = convert+se), vjust = -0.8,  size=6) +
  theme(axis.text = element_text(size=13), axis.title = element_text(size=16), legend.text = element_text(size=12), legend.title=element_blank())

e 

#### weight plot
SEw <- summarySE(feeding_T1234, measurevar="weight", groupvars="treatment")

w <- ggplot(SEw, aes(x=treatment, y=weight)) +
  geom_bar(stat="identity", position=position_dodge(), fill=col) +
  theme_classic() +
  labs(x="Treatment",y="Weight (mg)") +
  coord_cartesian(ylim=c(105,125)) +
  geom_errorbar(aes(ymin=weight-se, ymax=weight+se), color=col, width=.2, position=position_dodge(.9), size=1) +
  scale_y_continuous(expand = c(0,0)) + 
  theme(axis.text = element_text(size=13), axis.title = element_text(size=16), legend.text = element_text(size=12), legend.title=element_blank(), axis.text.x = element_blank(), axis.title.x=element_blank())

w 


#### efficiency & weight plot 6*6 ####
# pdf ('feeding result.pdf') 6*6
library(ggplot2)
p <- ggplot(feeding_T124, aes(x=eaten, y=weight, col=treatment)) +
  geom_point(alpha=0.8, size=3) +
  stat_smooth(method='glm',se=F,aes(group=treatment)) +
  theme_classic() +
  labs(x="Semi-artificial diet fed (??L)",y="Biomass (mg)") +
  scale_color_manual(values=c("#4393c3", "#d6604d", "#feb24c"),
                     breaks=c("T1", "T2", "T4"), labels=c("Control", "Warming", expression(paste(italic("Bidens"), " only")))) + 
  xlim(100,160)

p + theme(axis.text = element_text(size=13),
          axis.title = element_text(size=16),
          #legend.position = "top",
          legend.text = element_text(size=12),
          #legend.title = element_text(size=14),
          #legend.key.size = unit(1.5, "cm"),
          #legend.key.width = unit(2,"cm"),
          legend.title=element_blank(),
          legend.text.align = 0 ) 

# pdf ('feeding result.pdf') 6*6
pp <- ggplot(feeding_T1234, aes(x=eaten, y=weight, col=treatment)) +
  geom_point(alpha=0.8, size=3) +
  stat_smooth(method='glm',se=F,aes(group=treatment)) +
  theme_classic() +
  labs(x="Semi-artificial diet fed (??L)",y="Biomass (mg)") +
  scale_color_manual(values=c("#4393c3", "#d6604d", "#fee090", "#feb24c"),
                     breaks=c("T1", "T2", "T3", "T4"), labels=c("Control", "Warming", "Warming plus", expression(paste(italic("Bidens"), " only")))) + 
  xlim(100,160)
pp
