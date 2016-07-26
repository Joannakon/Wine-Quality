# Wine-Quality
Wine quality
#histogram
getwd()
setwd("~/Desktop/Data Science")
red <- read.csv("winequality-red.csv",sep=";",header=TRUE)
white <- read.csv("winequality-white.csv",sep=";",header=TRUE)
all <- rbind(red, white)

ggplot(all, aes(x=quality, fill=color)) +geom_histogram(binwidth=.5, alpha=.5, position="identity")
Comment: 
Mostly frequent quality levels of red and white wine are 5 and 6.
#Ggally
library(GGally)
ggpairs(all)
Comment: 
We could tell some linear relationship between each variable and their correlation of each one. We could use this observation to do further analysis. 

#compare
p1<-ggplot(all, aes(x=fixed.acidity,fill=color)) + geom_histogram (binwidth = 0.02, alpha=.5, position="identity")
p2<-ggplot(all, aes(factor(quality),fixed.acidity )) + geom_boxplot()
p3<-ggplot(all, aes(x=volatile.acidity,fill=color)) + geom_histogram (binwidth = 0.02, alpha=.5, position="identity")
p4<-ggplot(all, aes(factor(quality),volatile.acidity )) + geom_boxplot()
p5<-ggplot(all, aes(x=citric.acid,fill=color)) + geom_histogram (binwidth = 0.02, alpha=.5, position="identity")
p6<-ggplot(all, aes(factor(quality),citric.acid )) + geom_boxplot()
grid.arrange(p1,p2,p3,p4,p5,p6, ncol=2) 

p7<-ggplot(all, aes(x=residual.sugar,fill=color)) + geom_histogram (binwidth = 1, alpha=.5, position="identity")
p8<-ggplot(all, aes(factor(quality),residual.sugar )) + geom_boxplot()
p9<-ggplot(all, aes(x=chlorides,fill=color)) + geom_histogram (binwidth = 0.02, alpha=.5, position="identity")
p10<-ggplot(all, aes(factor(quality),chlorides )) + geom_boxplot()

p11<-ggplot(all, aes(x=free.sulfur.dioxide,fill=color)) + geom_histogram (binwidth = 1, alpha=.5, position="identity")
p12<-ggplot(all, aes(factor(quality),free.sulfur.dioxide )) + geom_boxplot()
p13<-ggplot(all, aes(x=total.sulfur.dioxide,fill=color)) + geom_histogram (binwidth = 0.8, alpha=.5, position="identity")
p14<-ggplot(all, aes(factor(quality),total.sulfur.dioxide )) + geom_boxplot()
grid.arrange(p11,p12,p13,p14, ncol=2)

p15<-ggplot(all, aes(x=density,fill=color)) + geom_histogram (binwidth = 0.0005, alpha=.5, position="identity")
p16<-ggplot(all, aes(factor(quality),density )) + geom_boxplot()
p17<-ggplot(all, aes(x=sulphates,fill=color)) + geom_histogram (binwidth = 0.02, alpha=.5, position="identity")
p18<-ggplot(all, aes(factor(quality),sulphates )) + geom_boxplot()
grid.arrange(p15,p16,p17,p18, ncol=2)
ggplot(red, aes(factor(quality), pH)) + geom_boxplot() 

Comment：
The mean of density is decreasing and the mean of sulphates is mostly keep on the 0.5.
>

#ggplot1
ggplot(aes(x=alcohol,y=pH),data = all)+geom_jitter(aes(color = color,bg = color),alpha=1/10,,pch=21,cex=4)+facet_wrap(~quality)+scale_color_brewer(type = 'div')+ggtitle('Alcohol and pH Relationship')
#ggplot2
ggplot(data = all,aes(x = density, y = alcohol, color = factor(quality)))+geom_point(alpha = 0.7, position = position_jitter(h = 0), size = 1.5) +coord_cartesian(xlim=c(min(all$density),1.005), ylim=c(8,14)) +scale_color_brewer(type='qual') +xlab('Density') + ylab('Alcohol') + ggtitle('Density vs. Alcohol correlation by Quality')
Comment:
 It looks like negative correlation. In this pattern, we could tell that with low alcohol or high density it’s more usual to be low quality wine.
>
#Separate each quality, and take a look at their linearly relationship. 
ggplot(data=subset(all,density < 1.02) , aes(x=alcohol, y=density, color=quality.factor)) + geom_point() + facet_wrap(~quality.factor) + geom_smooth(colour='black')

Comment:
 It looks like it indeed has linear tendency. 
>
#Now, separate the red wine and white wine.
ggplot(data = all, aes(x = density, y = alcohol, color = color)) +geom_point(alpha = 0.3, position = position_jitter(h = 0), size = 3)+geom_smooth(method = 'lm') +coord_cartesian(xlim=c(min(all$density),1.005), ylim=c(8,15)) +xlab('Density') +ylab('Alcohol') +ggtitle('Density vs. Alcohol correlation by Color')

Comment:
 Red wine in average is stronger than white wine. 

>
#ggplot3
ggplot(data = all,aes(x = density, y =residual.sugar , color = factor(quality)))+geom_point(alpha = 0.7, position = position_jitter(h = 0), size = 1.5) +coord_cartesian(xlim=c(min(all$density),1.005), ylim=c(8,14)) +scale_color_brewer(type='qual') +xlab('Density') + ylab('Alcohol') + ggtitle('Density vs. Residual.sugar  correlation by Quality')


Comment:
 It looks like negative correlation. It’s much more obvious when we separate the qualities. 
>

#Separate each quality, and take a look at their linearly relationship. 
ggplot(data=subset(all,density < 1.02) , aes(x=density, y=residual.sugar, color=quality.factor)) + geom_point() + facet_wrap(~quality.factor) + geom_smooth(colour='black')
Comment: 
Mostly it’s positive correlation
>
#Now, separate the red wine and white wine.


ggplot(data = all, aes(x = density, y = residual.sugar, color = color)) +geom_point(alpha = 0.3, position = position_jitter(h = 0), size = 3)+geom_smooth(method = 'lm') +coord_cartesian(xlim=c(min(all$density),1.005), ylim=c(0,11)) +xlab('Density') +ylab('Residual.sugar') +ggtitle('Density vs. Residual.sugar correlation by Color')
#Look other variables

ggplot(all , aes(x=volatile.acidity, y=citric.acid, color=quality.factor)) + geom_point() + facet_wrap(~quality.factor) + geom_smooth(colour='black')

#Take a look at free sulfur dioxide and total sulfur dioxide influence to quality.

ggplot(aes(x = quality, y = free.sulfur.dioxide), data = all) + geom_point(aes(color=color),alpha=0.5, position = 'jitter')+ggtitle(' Free.sulfur.dioxide and  Quality Relationship')
ggplot(aes(x = quality, y = total.sulfur.dioxide), data = all) + geom_point(aes(color=color),alpha=0.5, position = 'jitter')+ggtitle('Total.sulfur.dioxide and  Quality Relationship')
all$quality.factor <- factor(all$quality, ordered=TRUE)
ggplot(data=subset(all, free.sulfur.dioxide < 180), aes(x=free.sulfur.dioxide, y =total.sulfur.dioxide , color = quality.factor)) + xlab("free.sulfur.dioxide") +ylab("total.sulfur.dioxide") + ggtitle("Free.sulfur.dioxide and Total.sulfur.dioxide by quality") +stat_binhex()

