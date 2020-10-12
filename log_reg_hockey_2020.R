install.packages("popbio")
install.packages("pROC")
install.packages("gridExtra")
install.packages("party")
install.packages("ROCR")
install.packages("pscl")
install.packages("sjPlot")
install.packages("sjlabelled")
install.packages("GGally")
install.packages("ggplot2")


library(pROC)
library(ggplot2)
library(readxl)
library(popbio)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(GGally)
library(party)
library(ROCR)
library(gridExtra)
library(pscl)
library(digest)
library(dplyr)


# Data reading and extra variables  -------------

andmed=read_excel("hoki.xlsx")
andmed2=read_excel("games3.xlsx")

# Filtering. Less than 20 games played in season are filtered out ------------
# Diff variable created 

andmed2<- andmed2[andmed2$home_games_played_season>20 | andmed2$away_games_played_season>20,]
andmed$goal_diff=andmed$home_team_score - andmed$away_team_score
andmed$ahg=andmed$home_team_score_for_avg20
andmed$aha=andmed$home_team_score_against_avg20
andmed$aag=andmed$away_team_score_for_avg20
andmed$aaa=andmed$away_team_score_against_avg20

# MODEL FITTING. ESTIMATION OF LOGISTIC REGRESSION ACCURACY ON 500 games --------------
# MODEL1 - OLS BASED ON AVERAGE GOALS IN LAST 20 GAMES -------------

n=500

data2<-andmed[is.na(andmed$aaa)==F,]
percents1<-rep(NA,n)
rmse<-rep(NA,n)
preds<-rep(NA,n)
for (i in c(1:n)){
  #Training based on seasons
  x<-data2[sample(1:nrow(data2),nrow(data2)-500,replace=F),]
  x.train<- x[x$season!="2017-18",]
  x.test1<- x[x$season=="2017-18",]
  x.model<- lm(data=x.train, goal_diff~ ahg+aha+aag+aaa)
  #Implemented on the test set
  predictions1<-predict(x.model,newdata=x.test1)
  #Correct by result estimation in the test set
  corrects<-sum(ifelse(sign(predictions1)==sign(x.test1$goal_diff),1,0))
  percents1[i]<-corrects/length(na.exclude(predictions1))
  #corrects<-sum(ifelse(sign(x.model$fitted.values)==sign(x.test1$goal_diff),1,0))
  #wrongs<- length(x.model$fitted.values)-corrects
  rmse[i]<- sqrt(sum((predictions1-x.test1$goal_diff)**2)/length(x.test1$goal_diff))
  preds[i]<- predictions1
}
# rmse histogram

hist(rmse, breaks = 100)

# INVERSE GAUSSIAN MODEL BASED ON SEASON STATISTICS IN LAST 20 GAMES ------------

data<-andmed2
data$b_home_win2<-data$b_home_win
data$b_home_win2[data$b_home_win==0]<-data$b_home_win[data$b_home_win==0]+0.001
percents2<-rep(NA,n)
R2s<-rep(NA,n)
for (i in c(1:n)){
  x<-data[sample(1:nrow(data),nrow(data)-500,replace=F),]
  x.train<- x[x$m_season!="2017-18",]
  x.test2<- x[x$m_season=="2017-18",]
  #Variables are selected based on statistical significance in train set
  x.model<- glm(data=x.train, b_home_win2~ home_goals_diff_season
                 + home_goals_expected_season + 
                  away_goals_expected_season  
                  #b_home_b2b + b_away_b2b
                #,family="binomial"
                ,family=inverse.gaussian(link = "1/mu^2"))
  predictions2<-predict.glm(x.model,newdata=x.test2,type="response")
  corrects<-sum(ifelse(predictions2>0.5 & x.test2$b_home_win==1,1,0),na.rm=T)+sum(ifelse(predictions2<0.5 & x.test2$b_home_win==0,1,0),na.rm=T)  
  #corrects<-sum(ifelse(x.model$fitted.values>0.5 & x.train$b_home_win==1,1,0))
  percents2[i]<-corrects/length(na.exclude(predictions2))
  
  #Here, R squared is calculated
  R2s[i]<-pR2(x.model)[4]
  #return(percents)
}
# R squared's histogram

hist(R2s, breaks = 100)

# LOGISTIC REGRESSION ---------

data<-andmed2
percents_logit<-rep(NA,n)
R2s<-rep(NA,n)
#Iga kord varieerub u 500 valitud read, 500 on välja jäetud 
for (i in c(1:n)){
  x<-data[sample(1:nrow(data),nrow(data)-500,replace=F),]
  x.train<- x[x$m_season!="2017-18",]
  x.test3<- x[x$m_season=="2017-18",]
  
  #Alternative division of test and train
  #x.train<- x[1:floor(nrow(x)*0.6),]
  #x.test3 <- x[floor((nrow(x)*0.6)+1):nrow(x),]
  x.model<- glm(data=x.train, b_home_win ~ home_goals_diff_season
                 #+ home_faceoff_win_pct_season
                 + home_goals_expected_season
                 + away_goals_expected_season
                ,family=binomial(link="logit"))
  #Logistic regression is predicted and accuracy is tested
  predictions3<-predict.glm(x.model,newdata=x.test3,type="response")
  corrects<-sum(ifelse(predictions3>0.5 & x.test3$b_home_win==1,1,0),na.rm=T)+sum(ifelse(predictions3<0.5 & x.test3$b_home_win==0,1,0),na.rm=T)  
  #wrongs<- length(x.model$fitted.values)-corrects #wrongs<- length(x.model$fitted.values)-corrects
  percents_logit[i]<-corrects/length(na.exclude(predictions3))
  R2s[i]<-pR2(x.model)[4]
  
}
# Accuracy:
hist(percents_logit, 100)

# Logistic regression visualization ------------------

x.model<- glm(data=x.train, b_home_win ~ home_goals_diff_season
              ,family=binomial(link="logit"))

plot(x.test3$home_goals_diff_season,x.test3$b_home_win)

x.test3_2<-x.test3

x.model<- glm(data=x.train, b_home_win ~ 
              + away_goals_expected_season
              ,family=binomial(link="logit"))

x.test3_2$away_goals_expected_season<-seq(1,5,4.0005/850)

#yvalues<-predict.glm(x.model,newdata=x.test3_2,type="response")
#yvalues<-predict(x.model,newdata=x.test3_2,type="link",se=T)$fit

predictions3<-predict.glm(x.model,newdata=x.test3,type="response")
yvalues<-predict(x.model,newdata=x.test3_2,type="response")
plot(x.test3_2$away_goals_expected_season,yvalues)

df1<-data.frame(x.test3$away_goals_expected_season,x.test3$b_home_win)
df2<-data.frame(x.test3$away_goals_expected_season,predictions3)
df3<-data.frame(x.test3_2$away_goals_expected_season,yvalues)

colnames(df1)<-c("Diff","Win")
colnames(df2)<-c("Diff","Win")
colnames(df3)<-c("Diff","Line")

df2$Home_win<-ifelse((predictions3>0.5 & x.test3$b_home_win==1)| (predictions3<0.5 & x.test3$b_home_win==0),1,0)

length(df3[,1])
length(df1[,1])

update_geom_defaults("point", list(shape = 19))
p1<-ggplot(aes(x=Diff,y=Win),data=df2)+
  geom_point(aes(x=Diff,y=Win,color=Home_win),data=df2,size=4,alpha=0.3)+
  xlab("Away team goals expected for season")+
  ylab("Predicted win probability in logistic regression")+
  #geom_point(aes(x=Diff,y=Line),data=df3)+
  geom_abline(slope=0,intercept=0.5,col="Red")

# Logistic regression visualized:

p1

# Visualizing ROC curves ---------------------

df2_1<-df2[df2$Home_win==1,]
df2_2<-df2[df2$Home_win==0,]

p2<-ggplot(aes(Diff),data=df2_1)+geom_histogram()+  xlab("Away team goals expected for season (actual home win)")+
  ylab("Predicted win probability in logistic regression")
p3<-ggplot(aes(Diff),data=df2_2)+geom_histogram(stat="bin")+scale_y_reverse()+ xlab("Away team goals expected for season (actual away win)")+
  ylab("Predicted win probability in logistic regression")

# Visualizing logistic regression along with histograms:

grid.arrange(p3,p1, p2, 
             widths=c(1, 3), as.table=FALSE, nrow=3)


# ROC curves for all of the models (option 1) ---------------

par(mfrow=c(1,3)) 

pred<-prediction(predictions1,x.test1$home_win)
perf1<- performance(pred,measure="tpr",x.measure="fpr")
p1<-plot(perf,main="OLS")
p1
abline(0,1)
perf

pred<-prediction(predictions2,x.test2$b_home_win2)
perf2<- performance(pred,measure="tpr",x.measure="fpr")
p2<-plot(perf,main="Inverse gaussian")
abline(0,1)

pred<-prediction(predictions3,x.test3$b_home_win)
perf3<- performance(pred,measure="tpr",x.measure="fpr")
p3<-plot(perf,main="Logistic regression")
p3
abline(0,1)

# ROC graph2 ------------------

df1 <- data.frame(Curve=as.factor(rep(c(1), each=length(perf1@x.values[[1]]))), 
                 FalsePositive=c(perf@x.values[[1]]),
                 TruePositive=c(perf@y.values[[1]]))

df2 <- data.frame(Curve=as.factor(rep(c("Inverse Gaussian"), each=length(perf2@x.values[[1]]))), 
                 FalsePositive=c(perf2@x.values[[1]]),
                 TruePositive=c(perf2@y.values[[1]]))

df3 <- data.frame(Curve=as.factor(rep(c("Logistic regression"), each=length(perf3@x.values[[1]]))), 
                 FalsePositive=c(perf3@x.values[[1]]),
                 TruePositive=c(perf3@y.values[[1]]))

#df_<-data.frame(df1,df2,df3)
df_<-merge(df2,df3,all.x=T,by=c("Curve", "FalsePositive", "TruePositive")) 
df_2<-merge(df_,df3,all.x=T,by=c("Curve", "FalsePositive", "TruePositive")) 

p1 <- ggplot(df2_2, aes(x=FalsePositive, y=TruePositive,color=Curve)) +
geom_line(data=df2,aes(color=Curve))+
geom_line(data=df3,aes(color=Curve))+
geom_abline(intercept=0,slope=1)

#ROC CURVE Graphs:
p1


# ACCURACY HISTOGRAMS -------------

protsendid1<-data.frame(percents1)
protsendid1$Model<-"OLS"

protsendid2<-data.frame(percents2)
protsendid2$Model <-"GLM inverse Gaussian"
colnames(protsendid2)<- c("percents1","Model")

protsendid3<-data.frame(percents_logit)
protsendid3$Model <-"Logistic regression"

colnames(protsendid3)<- c("percents1","Model")
comb<-rbind(protsendid1,protsendid2,protsendid3)

p<-ggplot(comb, aes(percents1))+geom_histogram(binwidth = 0.005,data=protsendid1,aes(fill=Model),alpha=0.2)+
            geom_histogram(binwidth = 0.005,data=protsendid2,alpha=0.2,aes(fill=Model))+
            geom_histogram(binwidth = 0.005,data=protsendid3,alpha=0.2,aes(fill=Model))+
  scale_x_continuous(breaks = seq(0.5,0.625, by = 0.01))+xlab("Model accuracy for 500 samples of 89% of original data")

p
