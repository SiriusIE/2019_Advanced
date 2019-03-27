source('Classification/code/load_libraries.R')

raw_data<-fread('Datasets/Automobile.csv', stringsAsFactors = F)
str(raw_data)


# We have character, integer and numeric variables
# Some numeric variables are read as character
# Use of the ? symbol to account for NAs

# We will first will turn all "?" symbols into NA, 
# then will re-write the dataset and re-read it as "data_proc"
# with the option stringsAsFactors = TRUE

cols<-names(raw_data)
raw_data[ , (cols) := lapply(.SD,function(x) return(gsub('\\?',NA,x))), .SDcols = cols]
str(raw_data)

raw_data<-fwrite(raw_data,'Datasets/Automobile_2.csv')
data_proc<-fread('Datasets/Automobile_2.csv', stringsAsFactors = T)
str(data_proc)


# still some integer variables...

data_proc[ , names(data_proc)[sapply(data_proc, is.integer)]:=
             lapply(.SD,as.numeric),.SDcols = 
             names(data_proc)[sapply(data_proc, is.integer)]]
str(data_proc)


# We drop off a couple of variables 
data_proc[, c('symboling','losses'):=NULL]


# We analyze the counting per maker:
sort(summary(data_proc$make), dec=T)/nrow(data_proc)
p<-ggplot(data_proc, aes(x=make))+geom_bar(stat='count')+
  theme(axis.text.x = element_text(angle=45))
p

library(plotly)
ggplotly(p)


# We will create a label that will agregate into "others" those makers with less than 3% of share

niche_cars<-names(which(summary(data_proc$make)/nrow(data_proc)<0.03))

data_proc[, make_agg:=as.factor(ifelse(make%in%niche_cars,'others',as.character(make)))]

summary(data_proc$make)/nrow(data_proc)
summary(data_proc$make_agg)/nrow(data_proc)
sum(summary(data_proc$make_agg)/nrow(data_proc))

# we drop off the former make variable
data_proc[, make:=NULL]

p<-ggplot(data_proc, aes(x=make_agg))+geom_bar(stat='count')+
  theme(axis.text.x = element_text(angle=45))
ggplotly(p)


#### summary
str(data_proc)  # ...just numeric & factor variables

qplot(sapply(data_proc, is.numeric))
qplot(sapply(data_proc, is.factor))



#### NA treatment

# We first delete every row where price is missing
nrow(data_proc)
data_proc<-data_proc[!is.na(price)]
nrow(data_proc)

Amelia::missmap(data_proc, legend = F, col=c('black', 'lightgray')); grid(col='azure4')

# NAs are just a few and concentrated in stroke & hp variables
data_proc<-data_proc[complete.cases(data_proc)]
nrow(data_proc)



#### We check if any numeric variable has null variance

numeric_variables<-names(data_proc)[sapply(data_proc, is.numeric)]

sd_numeric_variables<-sapply(data_proc[,numeric_variables, with=F], sd)
sd_numeric_variables
cv_numeric_variables<-sd_numeric_variables/colMeans(data_proc[,numeric_variables, with=F])
cv_numeric_variables

ggplot(data.table(var=names(cv_numeric_variables),cv=cv_numeric_variables),
       aes(var,fill=cv))+geom_bar()+coord_polar()+scale_fill_gradient(low='white', high = 'black')


summary(data_proc[, numeric_variables[cv_numeric_variables<0.1], with=F])

df<-data_proc[, numeric_variables[cv_numeric_variables<0.20], with=F]
sapply(df, function(x) sd(x)/mean(x))
df<-data.frame(scale(df))
df<-melt(df)
ggplot(df, aes(x=variable, y=scale(value)))+geom_boxplot()+geom_jitter(alpha=.25)+
  theme(axis.text.x = element_text(angle=45))


# allright!!!

# Now lets check the number of categories per factor variable

factor_variables<-names(data_proc)[sapply(data_proc, is.factor)]
count_factor_variables<-sapply(data_proc[,factor_variables, with=F], summary)
count_factor_variables

# lets define a rule... if a label weight less than 10% goes into the "others" bag:
f_other<-function(var){
  
  count_levels<-summary(var)/length(var)
  to_bag<-names(which(count_levels<0.1))
  
  reduced_var<-as.factor(ifelse(as.character(var)%in%to_bag,'others',as.character(var)))
  
  return(reduced_var)
}

# and we apply the function to our factor variables
data_proc[, (factor_variables):=lapply(.SD, f_other), .SDcols=factor_variables]

sapply(data_proc[,factor_variables, with=F], summary)



str(data_proc)



# Binary encoding our factor variables (needed for most algos)

data_ready<-caret::dummyVars(" ~ .", data = data.frame(data_proc), fullRank=T,sep = "_")
data_ready<-data.table(predict(data_ready, newdata = data_proc))

names(data_ready)<-gsub('-','_',names(data_ready))

str(data_proc)
str(data_ready)


fwrite(data_ready, 'Datasets/data_automobile_ready.csv', row.names = F)


