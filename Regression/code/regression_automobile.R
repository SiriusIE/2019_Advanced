source('Classification/code/carga_librerias.R')

raw_data<-fread('Datasets/Automobile.csv', stringsAsFactors = F)
str(raw_data)


cols<-names(raw_data)
raw_data[ , (cols) := lapply(.SD,function(x) return(gsub('\\?',NA,x))), .SDcols = cols]
raw_data
str(raw_data)

raw_data<-fwrite(raw_data,'Datasets/Automobile_2.csv')
data_proc<-fread('Datasets/Automobile_2.csv', stringsAsFactors = T)
str(data_proc)

data_proc[ , names(data_proc)[sapply(data_proc, is.integer)]:=
            lapply(.SD,as.numeric), 
          .SDcols = names(data_proc)[sapply(data_proc, is.integer)]]
str(data_proc)

data_proc<-data_proc[, !c('symboling','losses')]


df<-caret::dummyVars(" ~ .", data = data.frame(data_proc), fullRank=T,sep = "_")
df<-data.table(predict(df, newdata = data_proc))

names(df)<-gsub('-','_',names(df))


str(data_proc)
str(df)

df<-df[complete.cases(df)]
str(df)

set.seed(123)
train_index<-sample(nrow(df),floor(nrow(df)*0.9))
df_train<-df[train_index]
df_test<-df[-train_index]

formula<-as.formula(price~.)

ini<-now()  
glm0<-MASS::stepAIC(lm(as.formula(log(price)~.),data=df_train), trace=F)
print(now()-ini)

summary(glm0)


library(glmnet)

lasso<-glmnet(x=data.matrix(df_train[, !c('price'), with=F]),y=log(df_train[['price']]),
              family = "gaussian",alpha=0, lambda = 0.001)


ini<-now()
rf0<-ranger::ranger(formula,data=df_train,num.trees =500)
print(now()-ini)

print(rf0)





ini<-now()
xgb0<-xgboost::xgboost(data=as.matrix(df_train[, !'price', with=F]),label=df_train$price, nrounds = 50,
                       objective='reg:linear',early_stopping_rounds = 4)
print(now()-ini)

print(xgb0)



pred_glm<-predict(glm0, newdata=df_test,type='response')
pred_lasso<-predict(lasso, newx=data.matrix(df_test[, !c('price'), with=F]),s=1)
pred_rf<-predict(rf0, data=df_test)$predictions
pred_xgb<-predict(xgb0, newdata=as.matrix(df_test[, !'price', with=F]),type='response')

mean(abs((df_test$price-exp(pred_glm))/df_test$price))
mean(abs((df_test$price-exp(pred_lasso))/df_test$price))
mean(abs((df_test$price-pred_rf)/df_test$price))
mean(abs((df_test$price-pred_xgb)/df_test$price))
mean(abs((df_test$price-((pred_xgb+pred_rf)/2))/df_test$price))

qplot(exp(pred_glm),df_test$price)+geom_smooth()+geom_abline(col='red')
qplot(pred_rf,df_test$price)+geom_smooth()+geom_abline(col='red')
qplot(pred_xgb,df_test$price)+geom_smooth()+geom_abline(col='red')
qplot((pred_xgb+pred_rf)/2,df_test$price)+geom_smooth()+geom_abline(col='red')

n=20
set.seed(7364)
sample_plot<-sample(nrow(df_test), n)


df<-melt(data.table(id=1:length(sample_plot),
                    real=df_test[sample_plot]$price,
                    pred_glm=exp(pred_glm[sample_plot]),
                    pred_lasso=pred_lasso[sample_plot],
                    pred_rf=pred_rf[sample_plot],
                    pred_xgb=pred_xgb[sample_plot],
                    pred_ensemble=((pred_rf+pred_xgb)/2)[sample_plot]),
         id.vars='id')
df

ggplot(df[variable%in%c('real','pred_xgb')], aes(x=id, y=value, colour=variable))+geom_line(alpha=0.65)+
  scale_colour_manual(values = c('black','red'))
ggplot(df[variable%in%c('real','pred_rf')], aes(x=id, y=value, colour=variable))+geom_line(alpha=0.65)+
  scale_colour_manual(values = c('black','red'))
ggplot(df[variable%in%c('real','pred_ensemble')], aes(x=id, y=value, colour=variable))+geom_line(alpha=0.65)+
  scale_colour_manual(values = c('black','red'))
ggplot(df[variable%in%c('real','pred_glm')], aes(x=id, y=value, colour=variable))+geom_line(alpha=0.65)+
  scale_colour_manual(values = c('black','red'))
ggplot(df[variable%in%c('real','pred_lasso')], aes(x=id, y=value, colour=variable))+geom_line(alpha=0.65)+
  scale_colour_manual(values = c('black','red'))

