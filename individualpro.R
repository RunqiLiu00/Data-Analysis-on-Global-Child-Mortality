library(readxl)
data<- read_excel("data - Copy.xlsx")
library("ggplot2")
library("MASS")
library("caret")
library("psych")
# View(data)
#exclude two country
data=data[-c(123,141),]
names(data)
summary(data)
#scatterplot
library("car")
scatterplotMatrix(data[,-c(1,2,3,28)])
##split group
hgroup=subset(data,data$U5MR.level=="high")
mgroup=subset(data,data$U5MR.level=="medium")
lgroup=subset(data,data$U5MR.level=="low")
#Overall
mort=factor(data$U5MR.level,labels=c(1,2,3))
y=cbind(data$EduExpe,data$FertRate,data$AdoFert,data$PPP,data$HealthPub,data$DPT,data$measles)
overallfit=manova(y~mort)
summary(overallfit,intercept=T,test="Wilks")
summary.aov(ecofit)



#sex differences; region difference
colnames(data)[22:24] =c("U5MR","U5MR.F","U5MR.M")

boxplot(data[22:24],names)

par(mfrow=c(1,3))
boxplot(mortality.U5~Region,data,names)
boxplot(mortality.U5.F~Region,data,names)
boxplot(mortality.U5.M~Region,data,names)
par(mfrow=c(1,1))
t.test(data$mortality.U5.F,data$mortality.U5.M)
wilcox.test(data$mortality.U5.F,data$mortality.U5.M)

#compare mortality rate in different regions
#assume...
fit=aov(U5MR~Region,data=data)
summary(fit)
region_box=ggplot(data,aes(x=Region,y=U5MR,fill=Region))+geom_boxplot()
region_box+geom_jitter()+geom_text(aes(y = U5MR + .5, label = Code))
#corr
data_cor = cor(data[,-c(1,2,3)])
library(pheatmap)
library(corrgram)
corrplot::corrplot(data_cor,method = "color")
corrgram(data_cor,order = FALSE,upper.panel = panel.cor,main="correlation matrix")
data_cor

data_cor2=cor(data[,c(4,5,10,13,14,22,26,27)])
corrplot::corrplot(data_cor2,method = "color")
corrgram(data_cor2,order = FALSE,upper.panel = panel.cor,main="correlation matrix2")
data_cor2
scatterplotMatrix(data[,c(4,5,10,13,14,22,26,27)])
#rename cols
colnames(data)[4] =c("EduExpe")
colnames(data)[1] =c("Name")
colnames(data)[2] =c("Code")
colnames(data)[5] =c("AdoFert")
colnames(data)[10] =c("FertRate")
colnames(data)[11:16] =c("GDP","GDPg","PPP","HealthExpe","HealthPub","HealthPub2")
colnames(data)[26:27] =c("DPT","measles")
colnames(data)[28]=c("U5MR.level")
datatrim1=data[,c(4,5,10,11,12,13,14,15,16,22,26,27)]


data_cor3=cor(datatrim1)
corrplot::corrplot(data_cor3,method = "color")
corrgram(data_cor3,order = FALSE,upper.panel = panel.cor,main="correlation matrix2")
data_cor3
## region data
ssadata=subset(data,data$Region=="SSA")
##1 education effect
edu_point=ggplot(data,aes(EduExpe,U5MR))+geom_point(aes(colour=Region))
edu_point+geom_smooth(method = "lm",se = FALSE)
mytext=geom_text(aes(y = U5MR + .5, label = Code))
edutext=geom_text(aes(y = EduExpe + .5, label = Code))
ggplot(data,aes(x=U5MR.level,y=EduExpe,fill=U5MR.level))+geom_boxplot()+ylab("Education Expenditure")

#edcation effect in different regions
summary(hgroup$EduExpe)
summary(mgroup$EduExpe)
summary(lgroup$EduExpe)
edu_point_facet=ggplot(data,aes(EduExpe,U5MR,colour=Region))+geom_point()
edu_point_facet+facet_wrap(~Region)+geom_smooth(method = "lm",se = FALSE)
edu_point_facet+facet_wrap(~Region)+geom_smooth(method = "lm",se = FALSE)+mytext
edufit=aov(EduExpe~U5MR.level,data=data)
summary(edufit)
ggplot(ssadata,aes(x=mort.level,y=EduExpe,fill=mort.level))+geom_boxplot()
##2 adolescent fertility effect
AdoFert_point=ggplot(data,aes(AdoFert,mortality.U5))+geom_point(aes(colour=Region))
AdoFert_point+geom_smooth(method = "lm",se = FALSE)

#adolescent fertility effect in different regions
AdoFert_point_facet=ggplot(data,aes(AdoFert,mortality.U5,colour=Region))+geom_point()
AdoFert_point_facet+facet_wrap(~Region)+geom_smooth(method = "lm",se = FALSE)
AdoFert_point_facet+facet_wrap(~Region)+geom_smooth(method = "lm",se = FALSE)+mytext

AdoFert_point_facet=ggplot(data,aes(AdoFert,U5MR,colour=U5MR.level))+geom_point()+xlab("Adolescent Fertility Rate")
AdoFert_point_facet+facet_wrap(~U5MR.level)+geom_smooth(method = "lm",se = FALSE)
##3 Fertility effect
FertR_point=ggplot(data,aes(FertRate,mortality.U5))+geom_point(aes(colour=Region))
FertR_point+geom_smooth(method = "lm",se = FALSE)
cor(hgroup[,-c(1,2,3,28)])
cor(mgroup[,-c(1,2,3,28)])
cor(lgroup[,-c(1,2,3,28)])
cor(data[,-c(1,2,3,28)])
fertfit=aov(FertRate~U5MR.level,data=data)
summary(fertfit)
#Fertility effect in different regions
FertR_point_facet=ggplot(data,aes(FertRate,U5MR,colour=U5MR.level))+geom_point()+xlab("Fertility Rate")
FertR_point_facet+facet_wrap(~U5MR.level)+geom_smooth(method = "lm",se = FALSE)
FertR_point_facet+facet_wrap(~Region)+geom_smooth(method = "lm",se = FALSE)+mytext
# compare 2 fertility rate

ggplot(data,aes(FertRate,AdoFert))+geom_point(aes(colour=Region))+
geom_smooth(method = "lm",se = FALSE)+geom_text(aes(y = AdoFert + .5, label = Code))
###economic
mort=factor(data$U5MR.level,labels=c(1,2,3))
y=cbind(data$GDP,data$GDPg,data$PPP)
ecofit=manova(y~mort)
summary(ecofit,intercept=T,test="Wilks")
summary.aov(ecofit)

##4 PPP effect
PPP_point=ggplot(data,aes(PPP,mortality.U5))+geom_point(aes(colour=Region))
PPP_point+geom_smooth(method = "lm",se = FALSE)

#PPP effect in different regions
PPP_point_facet=ggplot(data,aes(PPP,U5MR,colour=U5MR.level))+geom_point()
PPP_point_facet+facet_wrap(~U5MR.level)+geom_smooth(method = "lm",se = FALSE)
PPP_point_facet+facet_wrap(~U5MR.level)+geom_smooth(method = "lm",se = FALSE)+mytext

lmppph=lm(U5MR~PPP,data=hgroup)
summary(lmppph)
##5 Health expenditure effect
HealthExpe_point=ggplot(data,aes(HealthExpe,mortality.U5))+geom_point(aes(colour=Region))
HealthExpe_point+geom_smooth(method = "lm",se = FALSE)

#Health expenditure effect in different regions
HealthExpe_point_facet=ggplot(data,aes(HealthExpe,mortality.U5,colour=Region))+geom_point()
HealthExpe_point_facet+facet_wrap(~Region)+geom_smooth(method = "lm",se = FALSE)
HealthExpe_point_facet+facet_wrap(~Region)+geom_smooth(method = "lm",se = FALSE)+mytext

##6 DPT effect
DPT_point=ggplot(data,aes(DPT,mortality.U5))+geom_point(aes(colour=Region))
DPT_point+geom_smooth(method = "lm",se = FALSE)

#DPT effect in different regions
DPT_point_facet=ggplot(data,aes(DPT,mortality.U5,colour=Region))+geom_point()
DPT_point_facet+facet_wrap(~Region)+geom_smooth(method = "lm",se = FALSE)
DPT_point_facet+facet_wrap(~Region)+geom_smooth(method = "lm",se = FALSE)+mytext

##7 Measles effect
measles_point=ggplot(data,aes(measles,mortality.U5))+geom_point(aes(colour=Region))
measles_point+geom_smooth(method = "lm",se = FALSE)

#Measles effect in different regions
measles_point_facet=ggplot(data,aes(measles,mortality.U5,colour=Region))+geom_point()
measles_point_facet+facet_wrap(~Region)+geom_smooth(method = "lm",se = FALSE)
measles_point_facet+facet_wrap(~Region)+geom_smooth(method = "lm",se = FALSE)+mytext


####Regression Analysis
model= lm(mortality.U5~., data = data[,-c(1,2,3)])
summary(model)
model1= lm(mortality.U5~., data = data[,c(4,5,10,13,14,22,26,27)])
summary(model1)
model1log= lm(log(mortality.U5)~., data = data[,c(4,5,10,13,14,22,26,27)])
summary(model1log)
#not significant
summary(data[,c(4,5,10,13,14,22,26,27)])

#datatrim1
model2=lm(mortality.U5~.,data=datatrim1)
summary(model2)
model2log=lm(log(mortality.U5)~.,data=datatrim1)
summary(model2log)

###pca
#standardize data
dataz1=scale(datatrim1,center = T,scale = T)
#explore
pca1=fa.parallel(datatrim1[,-c(10)],fa="pc")
pca1
#4 eigenalues are larger than 1
pca2=princomp(dataz1[,-c(10)],cor=T)
summary(pca2,loadings=T)

#screeplot
screeplot(pca2,type="line",main="scree plot")

#pca regression, use the first 8 components
pca_scores=data.frame(pca2$scores)
pcadata=pca_scores[,1:8]
pcadata$mortality=datatrim1$mortality.U5
lmpca1=lm(mortality~.,data=pcadata)
summary(lmpca1)
#stepwise
lmpca2=step(lmpca1)
summary(lmpca2)
