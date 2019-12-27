rm(list=ls())
options(warn = -1)
X = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", 
      "C50", "dummy",
      "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees',"mlr",
      "gridExtra","outliers","partitions","class")
lapply(X, require, character.only = TRUE)
rm(X)
library(boot)
library(rpart)
library(caret)
library(mltools)
library(carData)
library(car)
library(caTools)
library(ANOVA.TFNs)
library(ANOVAreplication)
library(car)
library(class)
library(scales)
library(psych)
library(gplots) 
library(readxl)
library(corrplot)
library(rpart)
library(DMwR)
library(rpart.plot)
library(tree)
library(randomForest)
library(caTools)
library(DataExplorer)

setwd("F:/DATA SCIENCE/Edwisor/PROJECT/PROJECT-2")
getwd()
absent = read.csv("Absenteeism_at_work.csv",header = T)
df = absent
colnames(absent)
str(absent)
dim(absent)
plot_str(absent)
plot_missing(absent)
plot_missing(absent)
plot_intro(absent)
length(unique(absent$Son))
length(unique(absent$Pet))
length(unique(absent$`Absenteeism time in hours`))
View(df)
#renaming the variables
names(absent)[2]= "Reason.for.absence"
names(absent)[3]= "Month.of.absence"
names(absent)[4]= "Day.of.the.week"
names(absent)[6]= "Transportation.expense"
names(absent)[7]= "Distance.from.residence.to.work"
names(absent)[8]= "Service.time" 
names(absent)[10]= "Workload.average.perday"
names(absent)[11]= "Hit.target"
names(absent)[12]= "Disciplinary.failure"
names(absent)[15]= "Social.drinker"
names(absent)[16]= "Social.smoker" 
names(absent)[20]= "Body.mass.index"
names(absent)[21]= "Absenteeism.time.in.hours"

 
# Checking missing values
sum(is.na(absent))
missing_val = data.frame(apply(absent,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
row.names(missing_val) = NULL
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(absent)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
missing_val = missing_val[,c(2,1)]
View(missing_val)
# Missing value plots
ggplot(data = missing_val[1:8,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
ggtitle("Missing data percentage") + theme_bw()

ggplot(data = missing_val[9:15,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
  ggtitle("Missing data percentage") + theme_bw()

ggplot(data = missing_val[16:21,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
  ggtitle("Missing data percentage") + theme_bw()

#actual 31
# mean 
#median 25 
#absent[2,20]
#absent[2,20] = NA

# Freezing mean and median for missing value analysis

absent$Reason.for.absence[is.na(absent$Reason.for.absence)] = median(absent$Reason.for.absence,na.rm = T)
absent$Month.of.absence[is.na(absent$Month.of.absence)] = median(absent$Month.of.absence,na.rm = T)
absent$Transportation.expense[is.na(absent$Transportation.expense)] = median(absent$Transportation.expense,na.rm = T)
absent$Distance.from.residence.to.work[is.na(absent$Distance.from.residence.to.work)] = median(absent$Distance.from.residence.to.work,na.rm = T)
absent$Service.time[is.na(absent$Service.time)] = median(absent$Service.time,na.rm = T)
absent$Age[is.na(absent$Age)] = median(absent$Age,na.rm = T)
absent$Workload.average.perday[is.na(absent$Workload.average.perday)] = median(absent$Workload.average.perday,na.rm = T)
absent$Hit.target[is.na(absent$Hit.target)] = median(absent$Hit.target,na.rm = T)
absent$Disciplinary.failure[is.na(absent$Disciplinary.failure)] = median(absent$Disciplinary.failure,na.rm = T)
absent$Education[is.na(absent$Education)] = median(absent$Education,na.rm = T)
absent$Son[is.na(absent$Son)] = median(absent$Son,na.rm = T)
absent$Social.drinker[is.na(absent$Social.drinker)] = median(absent$Social.drinker,na.rm = T)
absent$Social.smoker[is.na(absent$Social.smoker)] = median(absent$Social.smoker,na.rm = T)
absent$Pet[is.na(absent$Pet)] = median(absent$Pet,na.rm = T)
absent$Weight[is.na(absent$Weight)] = median(absent$Weight,na.rm = T)
absent$Height[is.na(absent$Height)] = median(absent$Height,na.rm = T)
absent$Body.mass.index[is.na(absent$Body.mass.index)] = mean(absent$Body.mass.index,na.rm = T)
absent$Absenteeism.time.in.hours[is.na(absent$Absenteeism.time.in.hours)] = median(absent$Absenteeism.time.in.hours,na.rm = T)

sum(is.na(absent))

#changing numeric to categorical

absent$ID = as.factor(absent$ID)
absent$Reason.for.absence = as.factor(absent$Reason.for.absence)
absent$Month.of.absence = as.factor(absent$Month.of.absence)
absent$Day.of.the.week = as.factor(absent$Day.of.the.week)
absent$Seasons= as.factor(absent$Seasons)
absent$Disciplinary.failure = as.factor(absent$Disciplinary.failure)
absent$Education = as.factor(absent$Education)
absent$Social.drinker= as.factor(absent$Social.drinker)
absent$Social.smoker = as.factor(absent$Social.smoker)

str(absent)

# outlier analysis 

numeric_index = sapply(absent,is.numeric)
numeric_data = absent[,numeric_index]
numeric_data= as.data.frame(numeric_data)
cnames = colnames(numeric_data)

# boxplot for numeric data
for (i in 1:length(cnames))
   {
     assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Absenteeism.time.in.hours"), data = subset(absent))+ 
              stat_boxplot(geom = "errorbar", width = 0.5) +
             geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                           outlier.size=1, notch=FALSE) +
              theme(legend.position="bottom")+
              labs(y=cnames[i],x="Absenteeism.time.in.hours")+
              ggtitle(paste("Box plot of absenteeism for",cnames[i])))
}

  # ## Plotting plots together
 gridExtra::grid.arrange(gn1,gn2,ncol=2)
 gridExtra::grid.arrange(gn3,gn4,ncol=2)
 gridExtra::grid.arrange(gn5,gn6,ncol=2)
 gridExtra::grid.arrange(gn7,gn8,ncol=2)
 gridExtra::grid.arrange(gn9,gn10,ncol=2)
 gridExtra::grid.arrange(gn11,gn12,ncol=2)

 
 
val = absent$Transportation.expense[absent$Transportation.expense %in% boxplot.stats(absent$Transportation.expense)$out]
absent$Transportation.expense[(absent$Transportation.expense %in% val)] = NA
absent$Transportation.expense[is.na(absent$Transportation.expense)] = median(absent$Transportation.expense, na.rm = T)
summary(absent$Transportation.expense)

val1 = absent$Distance.from.residence.to.work[absent$Distance.from.residence.to.work
                                               %in% boxplot.stats(absent$Distance.from.residence.to.work)$out]
absent$Distance.from.residence.to.work[(absent$Distance.from.residence.to.work %in% val1)] = NA
absent$Distance.from.residence.to.work[is.na(absent$Distance.from.residence.to.work)] = median(absent$Distance.from.residence.to.work, na.rm = T)
summary(absent$Distance.from.residence.to.work)
        
val2 = absent$Service.time[absent$Service.time %in% boxplot.stats(absent$Service.time)$out]
absent$Service.time[(absent$Service.time %in% val2)] = NA
absent$Service.time[is.na(absent$Service.time)] = median(absent$Service.time, na.rm = T)
summary(absent$Service.time)

val3 = absent$Age[absent$Age %in% boxplot.stats(absent$Age)$out]
absent$Age[(absent$Age %in% val3)] = NA
absent$Age[is.na(absent$Age)] = median(absent$Age, na.rm = T)
summary(absent$Age)

val4 = absent$Workload.average.perday[absent$Workload.average.perday %in% boxplot.stats(absent$Workload.average.perday)$out]
absent$Workload.average.perday[(absent$Workload.average.perday %in% val4)] = NA
absent$Workload.average.perday[is.na(absent$Workload.average.perday)] = median(absent$Workload.average.perday, na.rm = T)
summary(absent$Workload.average.perday)

val5 = absent$Hit.target[absent$Hit.target %in% boxplot.stats(absent$Hit.target)$out]
absent$Hit.target[(absent$Hit.target %in% val5)] = NA
absent$Hit.target[is.na(absent$Hit.target)] = median(absent$Hit.target, na.rm = T)
summary(absent$Hit.target)

val6 = absent$Weight[absent$Weight %in% boxplot.stats(absent$Weight)$out]
absent$Weight[(absent$Weight %in% val6)] = NA
absent$Weight[is.na(absent$Weight)] = median(absent$Weight, na.rm = T)
summary(absent$Weight)

val7 = absent$Height[absent$Height %in% boxplot.stats(absent$Height)$out]
absent$Height[(absent$Height %in% val7)] = NA
absent$Height[is.na(absent$Height)] = median(absent$Height, na.rm = T)
summary(absent$Height)

val8 = absent$Body.mass.index[absent$Body.mass.index %in% boxplot.stats(absent$Body.mass.index)$out]
absent$Body.mass.index[(absent$Body.mass.index %in% val8)] = NA
absent$Body.mass.index[is.na(absent$Body.mass.index)] = median(absent$Body.mass.index, na.rm = T)
summary(absent$Body.mass.index)

val9 = absent$Pet[absent$Pet %in% boxplot.stats(absent$Pet)$out]
absent$Pet[(absent$Pet %in% val9)] = NA
absent$Pet[is.na(absent$Pet)] = median(absent$Pet, na.rm = T)
summary(absent$Pet)

val10 = absent$Son[absent$Son %in% boxplot.stats(absent$Son)$out]
absent$Son[(absent$Son %in% val10)] = NA
absent$Son[is.na(absent$Son)] = median(absent$Son, na.rm = T)
summary(absent$Son)

# correlation for continuous variables
corrgram(absent[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

# anova for categorical data
str(absent)
result = aov(formula=Absenteeism.time.in.hours~ID, data = absent)
summary(result)
result1 = aov(formula=Absenteeism.time.in.hours~Reason.for.absence, data = absent)
summary(result1)
result2 = aov(formula=Absenteeism.time.in.hours~Month.of.absence, data = absent)
summary(result2)
result3 = aov(formula=Absenteeism.time.in.hours~Day.of.the.week, data = absent)
summary(result3)
result4 = aov(formula=Absenteeism.time.in.hours~Seasons, data = absent)
summary(result4)
result5 = aov(formula=Absenteeism.time.in.hours~Disciplinary.failure, data = absent)
summary(result5)
result6 = aov(formula=Absenteeism.time.in.hours~Education, data = absent)
summary(result6)
result7 = aov(formula=Absenteeism.time.in.hours~Social.smoker, data = absent)
summary(result7)
result8 = aov(formula=Absenteeism.time.in.hours~Social.drinker, data = absent)
summary(result8)

# important variables
absent_final = subset(absent,select=-c(ID,Seasons,Disciplinary.failure,Pet,Age,Education,Son,Social.smoker,Body.mass.index,Height,Hit.target,Social.drinker))
str(absent_final)

#checking data distributions
qqnorm(absent_final$Transportation.expense)
hist(absent_final$Transportation.expense)
qqnorm(absent_final$Distance.from.residence.to.work)
hist(absent_final$Distance.from.residence.to.work)
qqnorm(absent_final$Service.time)
hist(absent_final$Service.time)
qqnorm(absent_final$Workload.average.perday)
hist(absent_final$Workload.average.perday)
qqnorm(absent_final$Weight)
hist(absent_final$Weight)


# Feature Scaling

cnames1 = c("Transportation.expense","Distance.from.residence.to.work","Service.time","Weight",
            "Workload.average.perday","Absenteeism.time.in.hours")
for (i in cnames1) 
  {
  print(i)
  absent_final[,i] = (absent_final[,i]-min(absent_final[,i]))/(max(absent_final[,i]-min(absent_final[,i])))
} 
range(absent_final$Transportation.expense)
range(absent_final$Distance.from.residence.to.work)
range(absent_final$Service.time)
range(absent_final$Workload.average.perday)
range(absent_final$Weight)

# histogram after normalisation

qqnorm(absent_final$Transportation.expense)
hist(absent_final$Transportation.expense)
qqnorm(absent_final$Distance.from.residence.to.work)
hist(absent_final$Distance.from.residence.to.work)
qqnorm(absent_final$Service.time)
hist(absent_final$Service.time)
qqnorm(absent_final$Workload.average.perday)
hist(absent_final$Workload.average.perday)
qqnorm(absent_final$Weight)
hist(absent_final$Weight)

                                            ## ML Algorithm ##

absent_final = as.data.frame(absent_final)
train_ind = sample(1:nrow(absent_final),0.8*nrow(absent_final))                                                                                      
train = absent_final[train_ind,]
test = absent_final[-train_ind,]

 # Decision trees

set.seed(1234)
fit = rpart(Absenteeism.time.in.hours~.,data = train, method = 'anova')
summary(fit)
prediction_dt = predict(fit,test[,-9])
actual = test[,9]
predicted1 = data.frame(prediction_dt)
rmse(preds = prediction_dt, actuals = actual, weights = 1, na.rm = FALSE)


# Random forest

set.seed(1234)
rf_mod = randomForest(Absenteeism.time.in.hours~.,train,importance = TRUE,ntree = 100)
rf_pred = predict(rf_mod,test[,-9])
actual = test[,9]
predicted1 = data.frame(rf_pred)
rmse(preds = rf_pred, actuals = actual, weights = 1, na.rm = FALSE)


 #for ntree = 200

 set.seed(1234)
 rf_mod1 = randomForest(Absenteeism.time.in.hours~.,train,importance = TRUE,ntree = 200)
 rf_pred1 = predict(rf_mod1,test[,-9])
 actual = test[,9]
 predicted2 = data.frame(rf_pred1)
 rmse(preds = rf_pred1, actuals = actual, weights = 1, na.rm = FALSE) 
 
 
 # for ntree = 300
 
 set.seed(1234)
 rf_mod2 = randomForest(Absenteeism.time.in.hours~.,train,importance = TRUE,ntree = 300)
 rf_pred2 = predict(rf_mod2,test[,-9])
 actual = test[,9]
 predicted3 = data.frame(rf_pred2)
 rmse(preds = rf_pred2, actuals = actual, weights = 1, na.rm = FALSE)

 
 # for ntree = 500
 
 set.seed(1234)
 rf_mod3 = randomForest(Absenteeism.time.in.hours~.,train,importance = TRUE,ntree = 500)
 rf_pred3 = predict(rf_mod3,test[,-9])
 actual = test[,9]
 predicted4 = data.frame(rf_pred3)
 rmse(preds = rf_pred3, actuals = actual, weights = 1, na.rm = FALSE)
 
 
#LINEAR REGRESSION
#vif
 
mymodel = lm(Absenteeism.time.in.hours~.,data= absent_final)
vif(mymodel)

# LR Model

set.seed(1234)
absent = as.data.frame(absent)
droplevels(absent_final$Reason.for.absence)
train_in1 = sample(1:nrow(absent),0.8*nrow(absent_final))
train1 = absent_final[train_in1,]
test1 = absent_final[-train_in1,]
lr_model = lm(Absenteeism.time.in.hours~.,data = train1)
summary(lr_model)
predictions_lr = predict(lr_model,test1[,1:8])
predicted_lr = data.frame(predictions_lr)
actual1 = test1[,9]
rmse(preds = predictions_lr, actuals = actual, weights = 1, na.rm = FALSE)
 

#---------------------------------------part-2#----------------------------------------------#

# creating subset
   
loss = subset(absent,select = c(Month.of.absence,Service.time,Absenteeism.time.in.hours,Workload.average.perday))  

# Workloss/month = (absent time * workload)/service time    mathematical formula
loss["month.loss"]=with(loss,((loss[,"Workload.average.perday"]*loss[,"Absenteeism.time.in.hours"])/loss[,"Service.time"]))
for (i in 9) {
  emp = loss[which(loss["Month.of.absence"]==i),]
  print(sum(emp$month.loss))
}

print(emp$month.loss) 





