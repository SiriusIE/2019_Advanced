source('Classification/code/carga_librerias.R')

raw_data<-fread('Datasets/Classification/bank-full.csv')

raw_data[, day:=factor(day)]
raw_data[, campaign:=factor(campaign)]
raw_data[, pdays:=factor(pdays)]
raw_data[, previous:=factor(previous)]


df<-caret::dummyVars(" ~ .", data = data.frame(raw_data), fullRank=T)
df<-data.frame(predict(df, newdata = raw_data))
df<-data.table(df)

str(raw_data)
str(df)


dim(raw_data)
dim(df)

head(df,1)


set.seed(123)
train_index<-sample(nrow(df),floor(nrow(df)*0.9))
df_train<-df[train_index]
df_test<-df[-train_index]


target<-'yyes'

str(df)
summary(as.factor(df[[target]]))

formula<-as.formula(as.factor(yyes==1)~.)

ini<-now()  
glm0<-glm(formula,data=df_train,family = binomial)
print(now()-ini)

summary(glm0)

ini<-now()
rf0<-ranger::ranger(formula,data=df,num.trees =100)
print(now()-ini)

print(rf0)

ini<-now()
xgb0<-xgboost::xgboost(data=as.matrix(df_train[, !'price', with=F]),label=df_train$price, nrounds = 50,
                       objective='reg:linear',early_stopping_rounds = 4)
print(now()-ini)

print(xgb0)



pred_glm<-predict(glm0, type='response')>0.5
pred_rf<-predict(rf0, df)

table(pred_glm, df$yyes==1)
table(pred_rf$predictions, df$yyes==1)
