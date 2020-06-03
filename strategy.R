#load data
set.seed(10)
library(missRanger)
library(data.table)
library(reshape2)
library(dplyr)
library(ggplot2)
library(rpart)
library(randomForest)

members=read.csv("members.csv",header=T)
test=read.csv("test.csv",header=T)
train=read.csv("train.csv",header=T)
test=test[1:30000,]
train=train[1:30000,] #divided into sample and train dataset later

#missing values
colnames(members)<-c("user_id","city","age","gender","registration_method","registration_time","expiration_date")
boxplot(members$age) #show outliners
for (i in 1:length(members$age)){
  if(members$age[i]>120){
    members$age[i]=0
  }
  else if (members$age[i]<0){
    members$age[i]=0
  }
}
members$age[members$age==0]<-NA #remove abnormal data
boxplot(members$age)
text(x=boxplot.stats(members$age)$stats[4],labels=boxplot.stats(members$age)$stats[4]) #critical value for younger
members$gender=factor(members$gender,levels=c("","female","male"),labels=c(0,1,2))
members$gender[members$gender==0]<-NA
newx=data.frame(members$age)
newy=data.frame(members$gender)
output1=imputeUnivariate(newx,v=NULL,seed=NULL) #impute missing age data
output2=imputeUnivariate(newy,v=NULL,seed=NULL) #impute missing gender data
members=cbind.data.frame(members$user_id,output1,output2)
colnames(members)<-c("user_id","age","gender")
members=data.table(members)
members$gender=factor(members$gender,levels=c(1,2),labels=c("female","male")) #convert into original names

#data preparation
colnames(test)<-c("row_id","user_id","song_id","system_tab","layout_seen","entry_source")
for (i in 1:dim(test)[1]){
  if (test$system_tab[i]==""){
    test$system_tab[i]=c("unknown")
  }
  if (test$entry_source[i]==""){
    test$entry_source[i]=c("unknown")
  }
} #missing data labeled as unknown
colnames(train)<-c("user_id","song_id","system_tab","layout_seen","entry_source","target")
for (i in 1:dim(train)[1]){
  if (train$system_tab[i]==""){
    train$system_tab[i]=c("unknown")
  }
  if (train$entry_source[i]==""){
    train$entry_source[i]=c("unknown")
  }
} #missing data labeled as unknown
sample=train[22501:30000,] #sample data is to test the accuracy of model
train=train[1:22500,] #the size is 3 times larger than sample

#merge data
merger_train1<-merge(train,members,"user_id",all.x = TRUE)
merger_train2<-merge(sample,members,by = "user_id",all.x=TRUE)
merger_test<- merge.data.frame(test,members,by = "user_id",all.x = TRUE)

#top 10 song_id
song_based=data.frame(table(merger_train1$song_id))
colnames(song_based)=c("song_id","Frequency")
song_based=arrange(song_based,desc(Frequency))
song_based=as.data.frame(song_based)
top_n(song_based,10,Frequency)
#listen to songs within a month of first hearing it
ggplot(merger_train1,aes(x=target))+theme_bw(base_size=16)+theme(axis.text.x=element_text(angle=90,hjust=1))+geom_bar(aes(y = (..count..)/sum(..count..)))+ylab("target percentage")
prop.table(table(merger_train1$target))

#gender
ggplot(merger_train1,aes(x=gender))+theme_bw(base_size=16)+theme(axis.text.x=element_text(angle=90,hjust=1))+geom_bar(aes(y = (..count..)/sum(..count..)))+ylab("gender percentage")
prop.table(table(merger_train1$gender,merger_train1$target),1)

#system
ggplot(merger_train1,aes(x=system_tab))+theme_bw(base_size=16)+theme(axis.text.x=element_text(angle=90,hjust=1))+geom_bar(aes(y = (..count..)/sum(..count..)))+ylab("system percentage")
prop.table(table(merger_train1$system_tab, merger_train1$target),1)

#age
merger_train1$younger=0
merger_train1$younger[merger_train1$age<34]=1 #34 is chosen for the value in boxplot
ggplot(merger_train1,aes(x=younger))+theme_bw(base_size=16)+theme(axis.text.x=element_text(angle=90,hjust=1))+geom_bar(aes(y = (..count..)/sum(..count..)))+ylab("age percentage")
prop.table(table(merger_train1$younger,merger_train1$target),1)

#entry
ggplot(merger_train1,aes(x=entry_source))+theme_bw(base_size=16)+theme(axis.text.x=element_text(angle=90,hjust=1))+geom_bar(aes(y = (..count..)/sum(..count..)))+ylab("entry percentage")
prop.table(table(merger_train1$entry_source,merger_train1$target),1)

#model
aggregate(target~system_tab+gender+entry_source+gender,data=merger_train1,FUN=function(x) {sum(x)/length(x)})

#decision tree
fit1=rpart(target~age+system_tab+gender+entry_source,data=merger_train1,method='class')
pred1=predict(fit1, merger_train2,type=c("class"))
(sum(merger_train2$target==pred1))/7500

#random forest
fit2=randomForest(factor(target)~age+system_tab+gender+entry_source,data=merger_train1)
pred2=predict(fit2, merger_train2)
(sum(merger_train2$target==pred2))/7500

#submission file
row_id=test$row_id
testpred=predict(fit2,merger_test)
file=cbind.data.frame(row_id,testpred)
colnames(file)=c("id","target")
write.csv(file,file='submission.csv',row.names = FALSE)