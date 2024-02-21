# setwd ("D:/Megan新世代/台大碩班/論文/beehavior/data")
flow  <- read.csv('bee pollen new.csv')
flow_day <- dplyr::filter(flow,date %in% c('2020/9/21',  '2020/9/22',  '2020/9/23',  '2020/10/1',  
                                           '2020/10/2',  '2020/10/3',  '2020/10/4',  '2020/10/25', 
                                           '2020/10/26', '2020/10/27', '2020/10/28', '2020/10/31', 
                                           '2020/11/1',  '2020/11/4',  '2020/11/5',  '2020/11/6',  
                                           '2020/11/7',  '2020/11/11', '2020/11/12', '2020/11/18', 
                                           '2020/11/19') )


# NMDS
flow_w <- flow_day[,-grep("^number_", colnames(flow_day))]

#remove empty columns
empty_columns <- sapply(flow_w, function(x) all(is.na(x) | x == ""))
flow_w<-flow_w[, !empty_columns]
flow_w[is.na(flow_w)] <- 0
#remove empty rows
names(flow_w)
ncol(flow_w)
flow_w1<-flow_w[apply(flow_w[,26:93], 1, function(x) !all(x==0)),]
flow_w1$heat.up <- as.factor(flow_w1$heat.up)
flow_w1$time <- as.POSIXct(as.character(flow_w1$time), format = "%H:%M" )
flow_w1$time <- as.numeric(flow_w1$time)
is.numeric(flow_w1$time)

flow_w1$time
#flow_w1<-flow_w1[flow_w1$time!="16",]

names(flow_w1)
Y=as.matrix(flow_w1[,26:93])

library(vegan)
fit=adonis(Y~heat.up+time, data=flow_w1, strata=flow_w1$dategroup, method="bray", permutations=999)
fit

nmds = metaMDS(Y, distance = "bray")
nmds
env = flow_w1[,c(3)]

en = envfit(nmds, env, permutations = 999, na.rm = TRUE)
#plot(nmds,type="n")
#plot(en)

#points(nmds, display = "sites", cex = 0.8, pch=21, col="green", bg="yellow")
#text(nmds, display = "spec", cex=0.7, col="red")

data.scores = as.data.frame(scores(nmds))
data.scores$heat.up = flow_w1$heat.up
data.scores$time = flow_w1$time
data.scores$heat.up<-as.factor(data.scores$heat.up)

#write.csv(data.scores,'data.scores.csv')

species.scores <- as.data.frame(scores(nmds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)
#en_coord_cat = as.data.frame(scores(en, "factors")) * ordiArrowMul(en)

data.scores$time<-as.numeric(data.scores$time)

# plot   pollen NMDS 6*6
library(ggplot2)
gg = ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(data = data.scores, aes(colour = heat.up), size = 3, alpha = 0.8) + 
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), arrow = arrow(length = unit(0.2, "inches")),
               data = en_coord_cont, size =1, alpha = 0.5, colour = "grey30") +
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
            fontface = "bold", label = replace(rownames(en_coord_cont), rownames(en_coord_cont) == "1", "Time"), position = position_nudge(y = +0.07)) + 
  #geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  #labs(colour = "Treatment")+
  theme_bw() + 
  scale_color_manual(values=c("#4393c3", "#d6604d"),
                     breaks=c("0", "1"), labels=c("Control", "Warming"))

gg + theme(axis.text = element_text(size=13),
           axis.title = element_text(size=16),
           #legend.position = "top",
           legend.text = element_text(size=12),
           #legend.title = element_text(size=14),
           #legend.key.size = unit(1.5, "cm"),
           #legend.key.width = unit(2,"cm"),
           legend.title=element_blank())
