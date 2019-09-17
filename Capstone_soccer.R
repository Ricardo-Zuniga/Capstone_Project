if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(caret)

#function computes the percentage profit
profit_percentage<-function(x,a){
  10*x/(nrow(a))   
}  

#computes the efficiency of that model
efficiency<-function(a,b){
  100*confusionMatrix(a,as.factor(b$HomeResult))$overall[["Accuracy"]]
}  #computes the efficiency of that model

#computes the total profit
profit<-function(a){
  test_set3<-test_set2%>%mutate(my_bet=a)
sum(test_set3%>%mutate(gain=ifelse(HomeResult==my_bet,ifelse(my_bet=="WIN",(ODD.HOME-1)*10,ifelse(my_bet=="LOSS",(ODD.AWAY-1)*10,(ODD.DRAW-1)*10)),-10))%>%pull(gain))
} 


master<-read.csv("MASTER.csv")


#separate teams into home and away

master<-master%>%mutate(HomeTeam=TEAMS.H.A.)
master$HomeTeam<-gsub("\\ /.*","",master$HomeTeam)

master<-master%>%mutate(AwayTeam=TEAMS.H.A.)
master$AwayTeam<-gsub(".*/ ","",master$AwayTeam)  

#Verify if Teams were correctly separaded and remove the spaces
identical("Nurnberg II",master$HomeTeam[1])
identical("Pipinsried",master$AwayTeam[1])

#Add column to know which Team Won
master<-master%>%mutate(HomeResult=ifelse(HOME.SCORE.F.==AWAY.SCORE..F.,"DRAW",ifelse(HOME.SCORE.F.>AWAY.SCORE..F.,"WIN","LOSS")))
master<-master%>%mutate(AwayResult=ifelse(HomeResult=="DRAW","DRAW",ifelse(HomeResult=="WIN","LOSS","WIN")))

#Make comparissons for the results according if team play Home or Away
master%>%group_by(HomeResult)%>%ggplot(aes(HomeResult))+geom_histogram(stat="count",fill="blue",color="darkgreen")
master%>%group_by(AwayResult)%>%ggplot(aes(AwayResult))+geom_histogram(stat="count",fill="blue",color="darkgreen")

#precision
results<-data.frame() #create a data frame where results would be stored
results[1,1]<-"Bet Home"
results[1,2]<-100*mean(master%>%mutate(gain=ifelse(HomeResult=="WIN",1,0))%>%pull(gain))
results[1,2]
results[1,3]<-sum(master%>%mutate(gain=ifelse(HomeResult=="WIN",(ODD.HOME-1)*10,-10))%>%pull(gain)) #calculate profit
results[1,3]
results[1,4]<-profit_percentage(results[1,3],master)

#mske comparison for Teams with Higher Table Position
master_selected<-master%>%mutate(Result_for_higher=ifelse(HOME.POSITION<AWAY.POSITION,HomeResult,AwayResult))
master_selected%>%group_by(Result_for_higher)%>%ggplot(aes(Result_for_higher))+geom_histogram(stat="count",fill="blue",color="darkgreen")

#For convination

master_selected<-master%>%filter(HOME.POSITION<AWAY.POSITION) #filter  for match with Local Team has higher Table Position
results[2,1]<-"Home and Higher"
results[2,2]<-100*mean(master_selected%>%mutate(gain=ifelse(HomeResult=="WIN",1,0))%>%pull(gain))
results[2,2]
results[2,3]<-sum(master_selected%>%mutate(gain=ifelse(HomeResult=="WIN",(ODD.HOME-1)*10,-10))%>%pull(gain)) #calculate profit
results[2,3]
results[2,4]<-profit_percentage(results[2,3],master_selected)

#Eliminate The World Cup Matches since there is no RANK POSITIONS NEITHER HOME OR AWAY TEAMS
x<-unique(master$COUNTRY)
x<-x[1:99]
master<-master%>%filter(COUNTRY %in% x)

#Start of Machine Learning
#Data Partition
set.seed(13)
index<- createDataPartition(y = master$HomeResult,p = 0.9,list = FALSE)
train_set<- master[index,]
test_set<- master[-index,]

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
mod_fit <- train(HomeResult~ODD.HOME+ODD.DRAW+ODD.AWAY+HOME.POSITION+AWAY.POSITION+WIN.RATE.HOME+WIN.RATE.AWAY+LOCAL.MEAN.GOALS.H2H.+AWAY.MEAN.GOALS.H2H., data=train_set,
                 method = "knn",
                 trControl=control,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
mod_fit
mod_fit$results%>%ggplot(aes(k,Accuracy))+geom_point()+geom_line()



##### Attempts with different models
models<- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")
fits<- lapply(models, function(model){ 
       print(model)
       train(HomeResult~ODD.HOME+ODD.DRAW+ODD.AWAY+HOME.POSITION+AWAY.POSITION+WIN.RATE.HOME+WIN.RATE.AWAY+LOCAL.MEAN.GOALS.H2H.+AWAY.MEAN.GOALS.H2H., data=train_set,model=model)
   }) 

## Precision for each attempt
fit_accuracy<-data.frame()
for (i in 1:10) {
  fit_accuracy[i,1]<-models[i]
}
for (i in 1:10) {
  fit_accuracy[i,2]<-fits[[i]]$results$Accuracy[1]
}

fit_accuracy





### Check Countires, away and home positions
train_set2<-train_set%>%mutate(pred=predict(mod_fit))
correction<-train_set2%>%mutate(correct=ifelse(pred==HomeResult,1,0))%>%group_by(COUNTRY)%>%summarise(x=mean(correct),num=n())
correction%>%ggplot(aes(num,x))+geom_point()+scale_x_log10()

Home_stats<-train_set2%>%mutate(correct=ifelse(pred==HomeResult,1,0))%>%group_by(HOME.POSITION)%>%summarise(x=mean(correct))
Home_stats%>%ggplot(aes(HOME.POSITION,x))+geom_point()

Away_stats<-train_set2%>%mutate(correct=ifelse(pred==HomeResult,1,0))%>%group_by(AWAY.POSITION)%>%summarise(x=mean(correct))
Away_stats%>%ggplot(aes(AWAY.POSITION,x))+geom_point()

countries<-correction%>%filter(x>=.6)
countries<-countries%>%select(COUNTRY)
test_set2<-test_set%>%filter(COUNTRY %in% (countries$COUNTRY))

#eliminate the unnecesary countryies for train set
train_set_new<-train_set%>%filter(COUNTRY %in% (countries$COUNTRY))
naive_bayes_new<-train(HomeResult~ODD.HOME+ODD.DRAW+ODD.AWAY+HOME.POSITION+AWAY.POSITION+WIN.RATE.HOME+WIN.RATE.AWAY+LOCAL.MEAN.GOALS.H2H.+AWAY.MEAN.GOALS.H2H., data=train_set_new,model="naive_bayes")




#Make predictions
a<-predict(fits[[3]],test_set, type = "raw")
b<-predict(fits[[3]],test_set2, type = "raw")

results[3,1]<-"All Matches"

results[3,2]<-efficiency(a,test_set)
test_set3<-test_set%>%mutate(my_bet=a)
results[3,3]<-sum(test_set3%>%mutate(gain=ifelse(HomeResult==my_bet,ifelse(my_bet=="WIN",(ODD.HOME-1)*10,ifelse(my_bet=="LOSS",(ODD.AWAY-1)*10,(ODD.DRAW-1)*10)),-10))%>%pull(gain))
results[3,4]<-profit_percentage(results[3,3],test_set)
results[3,3]

results[4,1]<-"60% Countries Matches FD" 
results[4,2]<-efficiency(b,test_set2)

results[4,3]<-profit(b)
results[4,3]
results[4,4]<-profit_percentage(results[4,3],test_set2)

#make an ensemble
naive_bayes_p<-predict(fits[[3]],test_set2, type = "prob")
qda_p<-predict(fits[[8]],test_set2, type = "prob")
svmLinear_p<-predict(fits[[4]],test_set2, type = "prob")

pred_avg<-(naive_bayes_p+qda_p+svmLinear_p)/3

pred_final <- factor(apply(pred_avg, 1, which.max)-1)
levels(pred_final) <- c("DRAW", "LOSS","WIN")

results[5,1]<-"Ensemble FD"
results[5,2]<-efficiency(pred_final,test_set2)
results[5,3]<-profit(pred_final)
results[5,4]<-profit_percentage(results[5,3],test_set2)


#predict for the new data set
c<-predict(naive_bayes_new,test_set2, type = "raw")
results[6,1]<-"60% Countries Matches PD"
results[6,2]<-efficiency(c,test_set2)
results[6,3]<-profit(c)
results[6,4]<-profit_percentage(results[6,3],test_set2)

#make new ensemble
svmLinear_new<-train(HomeResult~ODD.HOME+ODD.DRAW+ODD.AWAY+HOME.POSITION+AWAY.POSITION+WIN.RATE.HOME+WIN.RATE.AWAY+LOCAL.MEAN.GOALS.H2H.+AWAY.MEAN.GOALS.H2H., data=train_set_new,model="svmLinear")
knn_new<-train(HomeResult~ODD.HOME+ODD.DRAW+ODD.AWAY+HOME.POSITION+AWAY.POSITION+WIN.RATE.HOME+WIN.RATE.AWAY+LOCAL.MEAN.GOALS.H2H.+AWAY.MEAN.GOALS.H2H., data=train_set_new,model="knn")
naive_bayes_newp<-predict(naive_bayes_new,test_set2, type = "prob")
knn_newp<-predict(knn_new,test_set2, type = "prob")
svmLinear_newp<-predict(svmLinear_new,test_set2, type = "prob")

pred_avg<-(naive_bayes_newp+knn_newp+svmLinear_newp)/3

pred_final_new<- factor(apply(pred_avg, 1, which.max)-1)
levels(pred_final_new) <- c("DRAW", "LOSS","WIN")

results[7,1]<-"Ensemble PD"
results[7,2]<-efficiency(pred_final_new,test_set2)
results[7,3]<-profit(pred_final_new)
results[7,4]<-profit_percentage(results[7,3],test_set2)
colnames(results)<-c("Situation","Precision %","Profit","Profit %")
results

test_set3<-test_set2%>%mutate(my_bet=pred_final_new)
sum(test_set3%>%mutate(gain=ifelse(HomeResult==my_bet,ifelse(my_bet=="WIN",((ODD.HOME*.95)-1)*10,ifelse(my_bet=="LOSS",((ODD.AWAY*.95)-1)*10,((ODD.DRAW*.95)-1)*10)),-10))%>%pull(gain))#If odds drop 5% this would happen to our profit



results<-results%>%mutate(Status=ifelse(Profit>0,"SUCCESS","FAIL"))
results%>%ggplot(aes(`Precision %`,Profit,colour=Situation))+geom_point(size=5)
results%>%ggplot(aes(`Precision %`,`Profit %`,colour=Situation))+geom_point(size=5)


ggplot(results,aes(Situation,Profit))+
geom_col(aes(fill = Status))+  
  coord_flip()+
  scale_fill_manual(values = c("darkred","green"))

