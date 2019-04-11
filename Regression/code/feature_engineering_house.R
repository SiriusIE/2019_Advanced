source('Regression/code/load_libraries.R')

raw_data<-fread('Datasets/Regression/kc_house_data.csv', stringsAsFactors = T)
str(raw_data)

summary(raw_data$id)

raw_data[, date:=as.Date(date, format='%m/%d/%Y')]

summary(raw_data$date)

# Objective: predict the price of a house out of some of its features

str(raw_data)

# We have character, integer and numeric variables
# Some numeric variables are read as character


raw_data[, zipcode:=factor(zipcode)]
raw_data[, condition:=factor(condition)]
raw_data[, view:=factor(view)]


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

# We drop off a couple of variables with non interest for our goal
data_proc[, c('id'):=NULL]

str(data_proc)

# We analyze the counting per bath:
sort(summary(data_proc$bath), dec=T)/nrow(data_proc)
p<-ggplot(data_proc, aes(x=bathrooms))+geom_bar(stat='count')+
  theme(axis.text.x = element_text(angle=45))
p

ggplotly(p)

# lets re-order the factor levels of bath in decreasing order
data_proc[, bathrooms:=factor(bathrooms, levels=names(sort(summary(data_proc$bathrooms), dec=T)))]
levels(data_proc$bath)
ggplotly(p)

# We will create a label that will agregate into "others" those bathrs with less than 3% of share
niche_baths<-names(which(summary(data_proc$bath)/nrow(data_proc)<0.03))
niche_baths

data_proc[, bath_agg:=as.factor(ifelse(bathrooms%in%niche_baths,'others',as.character(bathrooms)))]

summary(data_proc$bathrooms)/nrow(data_proc)
summary(data_proc$bath_agg)/nrow(data_proc)
sum(summary(data_proc$bath_agg)/nrow(data_proc)) 

data_proc[, length(levels(bath_agg))]
data_proc[, length(levels(bathrooms))]
data_proc[, length(levels(bath_agg))/length(levels(bathrooms))-1] # important reduction in factor cathegories


data_proc[, bath_agg:=factor(bath_agg, levels=names(sort(summary(data_proc$bath_agg), dec=T)))]
p<-ggplot(data_proc, aes(x=bath_agg))+geom_bar(stat='count')+
  theme(axis.text.x = element_text(angle=45))
ggplotly(p)

# we drop off the former bath variable
data_proc[, bathrooms:=NULL]

# same with bedrooms

niche_beds<-names(which(summary(data_proc$bed)/nrow(data_proc)<0.005))
niche_beds

data_proc[, bed_agg:=as.factor(ifelse(bedrooms%in%niche_beds,'others',as.character(bedrooms)))]

summary(data_proc$bed_agg)

data_proc[, bedrooms:=NULL]

# same for yr_built:
p<-ggplot(data_proc, aes(x=yr_built))+geom_bar(stat='count')+
  theme(axis.text.x = element_text(angle=45))
ggplotly(p)

# we'll agreggate by decade

data_proc[, decade_built:=factor(substr(as.character(yr_built),1,3))]

p<-ggplot(data_proc, aes(x=decade_built))+geom_bar(stat='count')+
  theme(axis.text.x = element_text(angle=45))
ggplotly(p)

data_proc[, yr_built:=NULL]

data_proc[, decade_renov:=factor(substr(as.character(yr_renovated),1,3))]

p<-ggplot(data_proc, aes(x=decade_renov))+geom_bar(stat='count')+
  theme(axis.text.x = element_text(angle=45))
ggplotly(p)

data_proc[, yr_renovated:=NULL]


str(data_proc)




#### summary
str(data_proc)  # ...just numeric & factor variables

sum(sapply(data_proc, is.numeric))
sum(sapply(data_proc, is.factor))

data_proc[, date:=NULL]



#### NA treatment

sum(is.na(data_proc))



#### We check if any numeric variable has null variance

numeric_variables<-names(data_proc)[sapply(data_proc, is.numeric)]

# calculating sd and CV for every numeric variable
sd_numeric_variables<-sapply(data_proc[,numeric_variables, with=F], sd)
sd_numeric_variables
cv_numeric_variables<-sd_numeric_variables/colMeans(data_proc[,numeric_variables, with=F])
cv_numeric_variables

ggplot(data.table(var=names(cv_numeric_variables),cv=cv_numeric_variables),
       aes(var,fill=cv))+geom_bar()+coord_polar()+scale_fill_gradient(low='white', high = 'black')



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

str(data_proc)
str(data_ready)


fwrite(data_ready, 'Datasets/Regression/data_house_ready.csv', row.names = F)

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
