source('Regression/code/load_libraries.R')

raw_data<-fread('Datasets/Regression/BlackFriday.csv', stringsAsFactors = T)
str(raw_data)

raw_data[, length(User_ID)]
raw_data[, length(unique(User_ID))]

raw_data[, which(sapply(names(raw_data),function(x) grep(pattern='ID',x))>0):= NULL]

str(raw_data)

raw_data[, Occupation:=as.factor(Occupation)]

# Objective: predict the purchase amount based on user features

str(raw_data)



data_proc<-copy(raw_data)
str(data_proc)


# we create a function that turns discrete variables into factors

data_proc[,sapply(data_proc,function(x) length(unique(x)))]
data_proc[,which(sapply(data_proc,function(x) length(unique(x))<200)):=lapply(.SD, as.factor), 
          .SDcols=sapply(data_proc,function(x) length(unique(x))<200)]


str(data_proc)

data_proc[ , which(sapply(data_proc, is.integer)):=lapply(.SD,as.numeric),
           .SDcols = sapply(data_proc, is.integer)]

str(data_proc)

data_proc[, ggplot(data_proc, aes(x=Purchase))+geom_histogram()]
data_proc[, ggplot(data_proc, aes(x=log(Purchase)))+geom_histogram()]


# NA to level

data_proc[, which(sapply(data_proc, is.factor)):=lapply(.SD, as.character), 
          .SDcols=sapply(data_proc, is.factor)]

data_proc[, which(sapply(data_proc, is.character)):=lapply(.SD, function(x) ifelse(is.na(x),"__",x)), 
          .SDcols=sapply(data_proc, is.character)]

data_proc[, which(sapply(data_proc, is.character)):=lapply(.SD, as.factor), 
          .SDcols=sapply(data_proc, is.character)]

str(data_proc)

# We analyze the counting per Occupation:
sort(summary(data_proc$Occupation), dec=T)/nrow(data_proc)
p<-ggplot(data_proc, aes(x=Occupation))+geom_bar(stat='count')+
  theme(axis.text.x = element_text(angle=45))
p

ggplotly(p)

# lets re-order the factor levels of Occupation in decreasing order
data_proc[, Occupation:=factor(Occupation, levels=names(sort(summary(data_proc$Occupation), dec=T)))]
levels(data_proc$Occupation)
ggplotly(p)

# We will create a label that will agregate into "others" those Occupationrs with less than 3% of share
niche_Occupation<-names(which(summary(data_proc$Occupation)/nrow(data_proc)<0.01))
niche_Occupation

data_proc[, Occupation_agg:=as.factor(ifelse(Occupation%in%niche_Occupation,'others',as.character(Occupation)))]

summary(data_proc$Occupation)/nrow(data_proc)
summary(data_proc$Occupation_agg)/nrow(data_proc)
sum(summary(data_proc$Occupation_agg)/nrow(data_proc)) 

data_proc[, length(levels(Occupation_agg))]
data_proc[, length(levels(Occupation))]
data_proc[, length(levels(Occupation_agg))/length(levels(Occupation))-1] # important reduction in factor cathegories


data_proc[, Occupation_agg:=factor(Occupation_agg, levels=names(sort(summary(data_proc$Occupation_agg), dec=T)))]
p<-ggplot(data_proc, aes(x=Occupation_agg))+geom_bar(stat='count')+
  theme(axis.text.x = element_text(angle=45))
ggplotly(p)

# we drop off the former Occupation variable
data_proc[, Occupation:=NULL]
str(data_proc)

# same with Product Category 1

niche_Product_Category_1s<-names(which(summary(data_proc$Product_Category_1)/nrow(data_proc)<0.01))
niche_Product_Category_1s

data_proc[, Product_Category_1_agg:=as.factor(ifelse(Product_Category_1%in%niche_Product_Category_1s,'others',as.character(Product_Category_1)))]

summary(data_proc$Product_Category_1_agg)

data_proc[, Product_Category_1:=NULL]

# same with Product Category 2

niche_Product_Category_2s<-names(which(summary(data_proc$Product_Category_2)/nrow(data_proc)<0.01))
niche_Product_Category_2s

data_proc[, Product_Category_2_agg:=as.factor(ifelse(Product_Category_2%in%niche_Product_Category_2s,'others',as.character(Product_Category_2)))]

summary(data_proc$Product_Category_2_agg)

data_proc[, Product_Category_2:=NULL]

# same with Product Category 3

niche_Product_Category_3s<-names(which(summary(data_proc$Product_Category_3)/nrow(data_proc)<0.01))
niche_Product_Category_3s

data_proc[, Product_Category_3_agg:=as.factor(ifelse(Product_Category_3%in%niche_Product_Category_3s,'others',as.character(Product_Category_3)))]

summary(data_proc$Product_Category_3_agg)

data_proc[, Product_Category_3:=NULL]

str(data_proc)


#### summary
str(data_proc)  # ...just numeric & factor variables

sum(sapply(data_proc, is.numeric))
sum(sapply(data_proc, is.factor))



#### NA treatment

sum(is.na(data_proc))

# we do nothing


#### We check if any numeric variable has null variance

numeric_variables<-names(data_proc)[sapply(data_proc, is.numeric)]

# calculating sd and CV for every numeric variable
sd_numeric_variables<-sapply(data_proc[,numeric_variables, with=F], sd)
sd_numeric_variables
cv_numeric_variables<-sd_numeric_variables/colMeans(data_proc[,numeric_variables, with=F])
cv_numeric_variables


# allright!!!

# Now lets check the number of categories per factor variable

factor_variables<-names(data_proc)[sapply(data_proc, is.factor)]
count_factor_variables<-sapply(data_proc[,factor_variables, with=F], summary)
count_factor_variables

# lets define a rule... if a label weight less than 10% goes into the "others" bag:
f_other<-function(var,p){
  
  count_levels<-summary(var)/length(var)
  to_bag<-names(which(count_levels<p))
  
  reduced_var<-as.factor(ifelse(as.character(var)%in%to_bag,'others',as.character(var)))
  
  return(reduced_var)
}

# and we apply the function to our factor variables
data_proc[, (factor_variables):=lapply(.SD, f_other,p=0.01), .SDcols=factor_variables]

sapply(data_proc[,factor_variables, with=F], summary)



str(data_proc)



# Binary encoding our factor variables (needed for most algos)

data_ready<-caret::dummyVars(formula= ~., data = data_proc, fullRank=T,sep = "_")
data_ready<-data.table(predict(data_ready, newdata = data_proc))

names(data_ready)<-gsub('-','_',names(data_ready))
setnames(data_ready,"Stay_In_Current_City_Years_4+","Stay_In_Current_City_Years_4")
setnames(data_ready,"Age_55+","Age_55")

str(data_proc)
str(data_ready)
sum(is.na(data_ready))

fwrite(data_ready, 'Datasets/Regression/data_black_ready.csv', row.names = F)

# data partition for individual project
data_ready<-fread('Datasets/Regression/data_house_ready.csv')
source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/f_partition.R')
whole_data<-f_partition(df=fread('/Users/ssobrinou/IE/Advanced/2019_Advanced/Datasets/Regression/kc_house_data.csv'),
                        test_proportion = 0.2,
                        seed = 872367823)

plot(whole_data$test$price)


lapply(whole_data, dim)


fwrite(whole_data)


# geo-analysis
library(leaflet)

m<-leaflet(data=raw_data)%>%addTiles()%>%addCircleMarkers(lat=~lat, lng=~long,
                                                          radius=0.5,
                                                          color = 'gray',
                                                          opacity = 0.25,label = ~price)
print(m)
