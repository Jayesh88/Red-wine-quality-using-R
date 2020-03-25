
library(gmodels) 
library(tidyr)
library(dplyr)
library(ggplot2)
library(MASS)
library(caTools)
library(pROC)
library(caret)
library(glmnet)
library(mlbench)
library(psych)

rd<-read.csv(file.choose())
View(rd)

str(rd)

plot(rd)
#rd$decideQuality<-factor(ifelse(rd$quality>=7,'good','bad'))

#correlation
cor(rd)
pairs.panels(rd[c(-1,-2,-3,-4,-6)],cex=2)
pairs(rd[,5:12])


#ggplot(rd,aes(x=decideQuality,y=alcohol))+geom_point()+ggtitle('Wine quality')

#converting data set into test and training set 
set.seed(123)
samp<-sample(nrow(rd),0.75*nrow(rd))
train<-rd[samp,]
test<-rd[-samp,]



cu<-trainControl(method = 'repeatedcv',number=10,repeats = 5,verboseIter = T)
cu


#Ridge regression
set.seed(123)
ridge<-train(quality~.,rd,
             method='glmnet',
             tuneGrid=expand.grid(alpha=0,
                                  lambda=seq(0.00001,5,length=5)),
             trcontrol=cu)

#plotting
plot(ridge)
ridge
plot(ridge$finalModel,xvar = 'lambda',label = T)
plot(ridge$finalModel,xvar = 'dev',label = T)
plot(varImp(ridge,scale=T))

best_fit<-ridge$finalModel
coef(best_fit,s=ridge$bestTune$lambda)

training_matrix_of_x <- model.matrix(quality~., train)
cv.ridge=cv.glmnet(training_matrix_of_x,train$quality,alpha=0)
plot(cv.ridge)
y_predicted_ridge <- predict(ridge, s = cv.ridge$lambda.min, newx = test)
y_predicted_ridge

#lasso Regression
set.seed(123)
lasso<-train(quality~.,rd,
             method='glmnet',
             tuneGrid=expand.grid(alpha=1,
                                  lambda=seq(-0.3,4,length=5)),
             trcontrol=cu)

#plotting
plot(lasso)
ridge
plot(lasso$finalModel,xvar = 'lambda',label = T)
plot(lasso$finalModel,xvar = 'dev',label = T)
plot(varImp(lasso,scale=T))

best_fit2<-lasso$finalModel
coef(best_fit2,s=lasso$bestTune$lambda)

cv.lasso=cv.glmnet(training_matrix_of_x,train$quality)
plot(cv.lasso)
y_predicted_lasso <- predict(lasso, s = cv.lasso$lambda.min, newx = test)
y_predicted_lasso

#compare ridge and lasso
model_comp<-list(Ridge=ridge,Lasso=lasso)
result<-resamples(model_comp)
summary(result)


# Linear Regression
full_model<-lm(quality~.,data=train)
summary(full_model)

bckwd<-stepAIC(full_model,direction = 'backward')

reduced_model<-lm(quality ~ volatile.acidity + citric.acid + chlorides + free.sulfur.dioxide + 
                    total.sulfur.dioxide + density + pH + sulphates + alcohol, data=train)
summary(reduced_model)

anova(reduced_model,full_model)

layout(matrix(c(1,2,3,4),2,2))
plot(reduced_model)


y_pred_reg<-predict(reduced_model,newdata = test)
pred_quality_reg<-ifelse(y_pred_reg>=6.5,'good','bad')
CrossTable(pred_quality_reg)


#y_predicted<-factor(pred_quality_reg,levels = c('good','bad'))
#mean(y_pred_reg==test$quality,na.rm = TRUE)
roc_lm<-roc(test$quality,y_pred_reg)
auc(roc_lm)


#using Decision tree Regression
library(rpart)
library(rpart.plot)

reg<-rpart(quality~.,data = train)
pred_tree<-predict(reg,newdata = test)
pred_quality_reg<-ifelse(pred_tree>=6.5,'good','bad')
roc_obj<-roc(test$quality,pred_tree)
auc(roc_obj)

rpart.plot(reg, box.palette="RdBu", shadow.col="gray", nn=TRUE)

#using random forest
library(randomForest)
set.seed(123)
rndm_frst<-randomForest(quality~.,train)
imp<-importance(rndm_frst)
varImpPlot(rndm_frst)


#chek if there are outliers
qqnorm(rd$citric.acid)

qqline(rd$citric.acid)

#remove outliers
rm_out<-function(x,na.rm=TRUE)
{
  qnt=quantile(x,probs=c(0.25,0.75),na.rm=T)
  iqr_rng=1.5*IQR(x,na.rm=T)
  x[x<=(qnt[1]-iqr_rng)]=qnt[1]
  x[x>=(qnt[2]+iqr_rng)]=qnt[2]
  return(x)
}

boxplot(rd)

getcol<-ncol(rd)

for (i in 1:ncol(rd)) {
  rd[[i]]=rm_out(rd[[i]])
}

boxplot(rd)