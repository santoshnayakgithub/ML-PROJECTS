#Exploratory Data Analysis
#Load the libraries 
library(tidyverse) 
library(moments) 
library(DataExplorer) 
library(caret) 
library(Matrix) 
library(mlbench) 
library(caTools) 
library(randomForest) 
library(glmnet) 
library(mlr) 
library(unbalanced) 
library(vita) 
library(rBayesianOptimization) 
library(lightgbm) 
library(boot) 
library(pROC) 
library(DMwR) 
library(ROSE) 
library(yardstick) 
#loading the train data 
train_df<-read.csv('../input/train.csv') 
head(train_df) 

#Dimension of train data 
dim(train_df) 
#Summary of the dataset 
str(train_df) 
#convert to factor 
train_df$target<-as.factor(train_df$target)

#Target classes count in train data

require(gridExtra) 
#Count of target classes 
table(train_df$target) 
#Percenatge counts of target classes 
table(train_df$target)/length(train_df$target)*100 
#Bar plot for count of target classes 
plot1<-ggplot(train_df,aes(target))+theme_bw()+geom_bar(stat='count',fill='lightgreen')

#Violin with jitter plots for target classes 
plot2<-ggplot(train_df,aes(x=target,y=1:nrow(train_df)))+theme_bw()+geom_violin(fill='lightblue')+
facet_grid(train_df$target)+geom_jitter(width=0.02)+labs(y='Index') 
grid.arrange(plot1,plot2, ncol=2)

#Distribution of train attributes

#Distribution of train attributes from 3 to 102 
for (var in names(train_df)[c(3:102)]){
  target<-train_df$target 
  plot<-ggplot(train_df, aes(x=train_df[[var]],fill=target)) + 
  geom_density(kernel='gaussian') + ggtitle(var)+theme_classic() 
  print(plot) 
} 

#Distribution of train attributes from 103 to 202 
for (var in names(train_df)[c(103:202)]){ 
  target<-train_df$target 
  plot<-ggplot(train_df, aes(x=train_df[[var]], fill=target)) + 
  geom_density(kernel='gaussian') + ggtitle(var)+theme_classic() 
  print(plot) 
  }

#Distribution of test attributes

#loading test data 
test_df<-read.csv('../input/test.csv') 
head(test_df) 

#Dimension of test dataset 
dim(test_df) 

#Distribution of test attributes from 2 to 101 
plot_density(test_df[,c(2:101)], ggtheme = theme_classic(), 
geom_density_args = list(color='blue'))
#Distribution of test attributes from 102 to 201 
plot_density(test_df[,c(102:201)], ggtheme = theme_classic(), 
             geom_density_args = list(color='blue'))

#Distribution of mean values in train and test dataset

#Applying the function to find mean values per row in train and test data. 
train_mean<-apply(train_df[,-c(1,2)],MARGIN=1,FUN=mean) 
test_mean<-apply(test_df[,-c(1)],MARGIN=1,FUN=mean) 
ggplot()+ 
  
#Distribution of mean values per row in train data 
geom_density(data=train_df[,-c(1,2)],aes(x=train_mean),kernel='gaussian', 
            show.legend=TRUE,color='blue')+theme_classic()+

#Distribution of mean values per row in test data 
  geom_density(data=test_df[,-c(1)],aes(x=test_mean),kernel='gaussian', 
  show.legend=TRUE,color='green')+ 
  labs(x='mean values per row',title="Distribution of mean values per row in 
       train and test dataset") 

#Applying the function to find mean values per column in train and test data. train_mean<-apply(train_df[,-c(1,2)],MARGIN=2,FUN=mean) test_mean<-apply(test_df[,-c(1)],MARGIN=2,FUN=mean) ggplot()+ #Distribution of mean values per column in train data geom_density(aes(x=train_mean),kernel='gaussian',show.legend=TRUE, color='blue')+theme_classic()+ #Distribution of mean values per column in test data geom_density(aes(x=test_mean),kernel='gaussian',show.legend=TRUE, color='green')+ labs(x='mean values per column',title="Distribution of mean values per row in train and test dataset")

#Distribution of standard deviation in train and test dataset

#Applying the function to find standard deviation values per row in train and test data. 
train_sd<-apply(train_df[,-c(1,2)],MARGIN=1,FUN=sd)
test_sd<-apply(test_df[,-c(1)],MARGIN=1,FUN=sd) 
ggplot()+ 
  
#Distribution of sd values per row in train data 
geom_density(data=train_df[,-c(1,2)],aes(x=train_sd),kernel='gaussian',
             show.legend=TRUE,color='red')+theme_classic()+ 
  
#Distribution of mean values per row in test data 
geom_density(data=test_df[,-c(1)],aes(x=test_sd),kernel='gaussian', 
show.legend=TRUE,color='blue')+ 
labs(x='sd values per row',title="Distribution of sd values per row in train and test dataset") 

#Applying the function to find sd values per column in train and test data. 
train_sd<-apply(train_df[,-c(1,2)],MARGIN=2,FUN=sd) 
test_sd<-apply(test_df[,-c(1)],MARGIN=2,FUN=sd) 
ggplot()+
  
#Distribution of sd values per column in train data 
geom_density(aes(x=train_sd),kernel='gaussian',show.legend=TRUE,color='red')+ 
theme_classic()+ 
  
#Distribution of sd values per column in test data 
geom_density(aes(x=test_sd),kernel='gaussian',show.legend=TRUE,color='blue')+ 
labs(x='sd values per column',title="Distribution of std values per column in train and test dataset")

#Distribution of skewness values in train and test dataset

#Applying the function to find skewness values per row in train and test data.

train_skew<-apply(train_df[,-c(1,2)],MARGIN=1,FUN=skewness) 
test_skew<-apply(test_df[,-c(1)],MARGIN=1,FUN=skewness) 
ggplot()+ 
  
#Distribution of skewness values per row in train data 
geom_density(aes(x=train_skew),kernel='gaussian',show.legend=TRUE, color='green')+theme_classic()+ 
  
#Distribution of skewness values per column in test data 
geom_density(aes(x=test_skew),kernel='gaussian',show.legend=TRUE, color='blue') +labs(x='skewness values per row',title="Distribution of skewness values per row in train and test dataset") 
  
#Applying the function to find skewness values per column in train and test data. 
train_skew<-apply(train_df[,-c(1,2)],MARGIN=2,FUN=skewness) 
test_skew<-apply(test_df[,-c(1)],MARGIN=2,FUN=skewness) 
ggplot()+
  
#Distribution of skewness values per column in train data 
geom_density(aes(x=train_skew),kernel='gaussian',show.legend=TRUE, color='green')+theme_classic()+ 
  
#Distribution of skewness values per column in test data 
geom_density(aes(x=test_skew),kernel='gaussian',show.legend=TRUE, color='blue')+
labs(x='skewness values 
per column',title="Distribution of skewness values per column in train and test dataset") 
#Distribution of kurtosis values in train and test dataset


#Applying the function to find kurtosis values per row in train and test data. 
train_kurtosis<-apply(train_df[,-c(1,2)],MARGIN=1,FUN=kurtosis) test_kurtosis<-apply(test_df[,-c(1)],MARGIN=1,FUN=kurtosis) ggplot()+ 

#Distribution of sd values per column in train data 
geom_density(aes(x=train_kurtosis),kernel='gaussian',show.legend=TRUE, color='b lue')+theme_classic()+ 
  
#Distribution of sd values per column in test data 
geom_density(aes(x=test_kurtosis),kernel='gaussian',show.legend=TRUE, color='red')+labs(x='kurtosis values per row',title="Distribution of kurtosis values per row in train and test dataset")

#Applying the function to find kurtosis values per column in train and test data. 
train_kurtosis<-apply(train_df[,-c(1,2)],MARGIN=2,FUN=kurtosis)
test_kurtosis<-apply(test_df[,-c(1)],MARGIN=2,FUN=kurtosis) 
ggplot()+ 
  
#Distribution of sd values per column in train data 
geom_density(aes(x=train_kurtosis),kernel='gaussian',show.legend=TRUE, color='blue')+theme_classic()+ 
  
#Distribution of sd values per column in test data 
geom_density(aes(x=test_kurtosis),kernel='gaussian',show.legend=TRUE, color='red')+ labs(x='kurtosis values per column',title="Distribution of kurtosis values per column in train and test dataset")


#Missing value analysis and Correlations
#Finding the missing values in train data
missing_val<-data.frame(missing_val=apply(train_df,2, function(x){sum(is.na(x))})) 
missing_val<-sum(missing_val) missing_val 

#Finding the missing values in test data 
missing_val<-data.frame(missing_val=apply(test_df,2, function(x){sum(is.na(x))})) 
missing_val<-sum(missing_val) 
missing_val 

#Correlations in train data 
train_df$target<-as.numeric(train_df$target) 
train_correlations<-cor(train_df[,c(2:202)]) 
train_correlations 

#Correlations in test data 
test_correlations<-cor(test_df[,c(2:201)]) 
test_correlations


#Feature Engineering
#Split the training data 
train_index<-sample(1:nrow(train_df),0.75*nrow(train_df)) 
train_data<-train_df[train_index,] 
valid_data<-train_df[-train_index,] 
dim(train_data) dim(valid_data) 

#Training the Random forest classifier 
set.seed(2732) train_data$target<-as.factor(train_data$target) 
mtry<-floor(sqrt(200)) 
tuneGrid<-expand.grid(.mtry=mtry) 
rf<-randomForest(target~.,train_data[,-c(1)],mtry=mtry,ntree=10, 
  importance=TRUE) 

#Variable importance 
VarImp<-importance(rf,type=2) 
VarImp 

#Partial dependence plot 
#We will plot "var_81" 
#partialPlot(rf,valid_data[,-c(1,2)],valid_data$var_81,xlab='var_81 
#We will plot "var_12" 
partialPlot(rf,valid_data[,-c(1,2)],valid_data$var_12,xlab='var_12')

#Handling of imbalanced data
#Split the data using CreateDataPartition 
set.seed(689) 
train.index<-createDataPartition(train_df$target,p=0.8,list=FALSE) 
train.data<-train_df[train.index,] 
valid.data<-train_df[-train.index,] 
dim(train.data)
dim(valid.data) 

#training dataset 
set.seed(682) 
X_t<-as.matrix(train.data[-c(1,2)]) 
y_t<-as.matrix(train.data$target) 

#validation dataset X_v<-as.matrix(valid.data[-c(1,2)]) 
y_v<-as.matrix(valid.data$target) 

#test data 
test<-as.matrix(test_df[,-c(1)]) 

#Logistic regression model 
set.seed(667) 
lr_model <-glmnet(X_t, y_t, family = "binomial") summary(lr_model) 

#Cross validation prediction 
set.seed(8909) 
cv_lr <- cv.glmnet(X_t,y_t,family = "binomial", type.measure = "class") 
cv_lr 

#Minimum lambda 
cv_lr$lambda.min 
#plot the auc score vs log(lambda) 
plot(cv_lr) 

#Model performance on validation dataset 
set.seed(5363) 
cv_predict.lr<-predict(cv_lr,X_v,s = "lambda.min", type = "class") 
cv_predict.lr

#Confusion matrix 
set.seed(689) 
#actual target variable 
target<-valid.data$target 

#convert to factor 
target<-as.factor(target) 

#predicted target variable 
#convert to factor 
cv_predict.lr<-as.factor(cv_predict.lr) 
confusionMatrix(data=cv_predict.lr,reference=target)

#ROC_AUC score and curve 
set.seed(892) 
cv_predict.lr<-as.numeric(cv_predict.lr) 
roc(data=valid.data[,-c(1,2)],response=target,predictor=cv_predict.lr,
auc=TRUE,plot=TRUE) 

#predict the model 
set.seed(763) 
lr_pred<-predict(lr_model,test,type='class') 
lr_pred 

#Random Oversampling Examples(ROSE) 
set.seed(699) 
#train.data$target<-as.factor(train.data$target) 
train.rose <- ROSE(target~., data =train.data[,-c(1)],seed=32)$datatable(train.rose$target) 
valid.rose <- ROSE(target~., data =valid.data[,-c(1)],seed=32)$datatable(valid.rose$target) 

#Logistic regression model 
set.seed(462) 
lr_rose <-glmnet(as.matrix(train.rose),as.matrix(train.rose$target), family = "binomial") 
summary(lr_rose) 

#Cross validation prediction 
set.seed(473) 
cv_rose = cv.glmnet(as.matrix(valid.rose),as.matrix(valid.rose$target), family = "binomial", type.measure = "class") 
cv_rose 

#Minimum lambda 
cv_rose$lambda.min 
#plot the auc score vs log(lambda) 
plot(cv_rose) 

#Model performance on validation dataset 
set.seed(442) 
cv_predict.rose<-predict(cv_rose,as.matrix(valid.rose),s = "lambda.min", type = "class") 
cv_predict.rose 

#Confusion matrix 
set.seed(478) 
#actual target variable 
target<-valid.rose$target 
#convert to factor 
target<-as.factor(target) 
#predicted target variable 

#convert to factor

cv_predict.rose<-as.factor(cv_predict.rose) 
confusionMatrix(data=cv_predict.rose,reference=target) 

#ROC_AUC score and curve 
set.seed(843) 
cv_predict.rose<-as.numeric(cv_predict.rose) 
roc(data=valid.rose[,-c(1,2)],response=target,predictor=cv_predict.rose, 
auc=TRUE,plot=TRUE) 

#predict the model 
set.seed(6543) 
rose_pred<-predict(lr_rose,test,type='class') 
rose_pred #Convert data frame to matrix 
set.seed(5432) 
X_train<-as.matrix(train.data[,-c(1,2)]) 
y_train<-as.matrix(train.data$target) 
X_valid<-as.matrix(valid.data[,-c(1,2)]) 
y_valid<-as.matrix(valid.data$target) 
test_data<-as.matrix(test_df[,-c(1)]) 

#training data 
lgb.train <- lgb.Dataset(data=X_train, label=y_train) 
#Validation data 
lgb.valid <- lgb.Dataset(data=X_valid,label=y_valid) 

set.seed(653) 
lgb.grid = list(objective = "binary", 
           metric = "auc", min_sum_hessian_in_leaf = 1, 
           feature_fraction = 0.7, 
           bagging_fraction = 0.7, 
           bagging_freq = 5, 
           learning_rate=0.1, 
           num_leaves=100, 
           num_threads=8, 
           min_data = 100, 
           max_bin = 200, 
           lambda_l1 = 8, 
           lambda_l2 = 1.3, 
           min_data_in_bin=150, 
           min_gain_to_split = 20, 
           min_data_in_leaf = 40, 
           is_unbalance = TRUE) 
           
set.seed(7663) 
lgbm.model <- lgb.train(params = lgb.grid, data = lgb.train, nrounds =3000, 
eval_freq =100,valids=list(val1=lgb.train,val2=lgb.valid), 
early_stopping_rounds = 1000)


#lgbm model performance on test data 
set.seed(6532) 
lgbm_pred_prob <- predict(lgbm.model,test_data) 
print(lgbm_pred_prob) 

#Convert to binary output (1 and 0) with threshold 0.5 
lgbm_pred<-ifelse(lgbm_pred_prob>0.5,1,0) 
print(lgbm_pred)

set.seed(6521) 
tree_imp <- lgb.importance(lgbm.model, percentage = TRUE) 
lgb.plot.importance(tree_imp, top_n = 150, measure = "Gain") 

sub_df<-data.frame(ID_code=test_df$ID_code,
lgb_predict_prob=lgbm_pred_prob, 
lgb _predict=lgbm_pred,smote_predict=smote_pred) 
write.csv(sub_df,'submission.CSV',row.names=F)
head(sub_df)