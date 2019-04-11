source('Classification/code/carga_librerias.R')

raw_data<-fread('Datasets/Classification/UCI_Credit_Card.csv')
str(raw_data)

raw_data[, ID:=NULL]

raw_data[, which(sapply(raw_data, is.integer)):=lapply(.SD, as.factor), 
         .SDcols=sapply(raw_data, is.integer)]

raw_data[, AGE:=as.numeric(as.character(AGE))]




df<-caret::dummyVars(" ~ .", data = data.frame(raw_data), fullRank=T,sep='_')
df<-data.frame(predict(df, newdata = raw_data))
df<-data.table(df)

df[, target:=factor(default.payment.next.month_1==1)]
df[, default.payment.next.month_1:=NULL]

str(raw_data)
str(df)


dim(raw_data)
dim(df)

fwrite(df, 'Datasets/Classification/data_credit_ready.csv', row.names = F)

head(df,1)


set.seed(123)
train_index<-sample(nrow(df),floor(nrow(df)*0.8))
df_train<-df[train_index]
df_test<-df[-train_index]


str(df)
summary(as.factor(df_train[['target']]))

formula<-as.formula(target~.)

library(glmnet)
glmnet_0<-cv.glmnet(x = data.matrix(df_train[, !'target']), 
                    y = factor(df_train[['target']]==TRUE),
                    family = 'binomial',
                    alpha=0.5,lambda = seq(0,0.1,0.0001))
glmnet_0$lambda.min
glm0<-glmnet(x =  data.matrix(df_train[, !'target']), 
                 y = factor(df_train[['target']]==TRUE),
                 family = 'binomial',
                 alpha=0.5, lambda = glmnet_0$lambda.min)

print(glm0)
glm0$beta

ini<-now()
rf0<-ranger::ranger(formula,data=df_train,num.trees = 1000,probability = TRUE)
print(now()-ini)

print(rf0)

ini<-now()
xgb0<-xgboost::xgboost(booster='gbtree',
                       data=data.matrix(df_train[, !'target', with=F]),label=df_train$target==TRUE, nrounds = 500,
                       objective='binary:logistic',early_stopping_rounds = 10)
print(now()-ini)

print(xgb0)



pred_glm<-predict(glmnet_0, newx = as.matrix(df_test[, !'target']), type='response')
pred_rf<-predict(rf0, df_test)$predictions[, 'TRUE']
pred_xgb<-predict(xgb0,newdata=data.matrix(df_test[,-'target']))


t<-0.5

table(pred_glm>t, df_test$target)
table(pred_rf>t, df_test$target)
table(pred_xgb>t, df_test$target)

library(pROC)

roc_glm<-roc(response=factor(df_test$target==TRUE),
              predictor=as.numeric(pred_glm)); grid()
print(roc_glm)
plot(roc_glm)

roc_rf<-roc(response=factor(df_test$target==TRUE),
             predictor=as.numeric(pred_rf)); grid()
print(roc_rf)
plot(roc_rf)

roc_xgb<-roc(response=factor(df_test$target==TRUE),
             predictor=as.numeric(pred_xgb)); grid()
print(roc_xgb)
plot(roc_xgb)
