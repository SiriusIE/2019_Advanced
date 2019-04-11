source('Classification/code/carga_librerias.R')

raw_data<-fread('Datasets/Classification/heart.csv', stringsAsFactors = T)
str(raw_data)


data_proc<-copy(raw_data)

integer_variables<-names(data_proc)[sapply(data_proc, is.integer)]
count_integer_variables<-sapply(data_proc[,integer_variables, with=F], function(x) length(unique(x)))
count_integer_variables

to_factor<-integer_variables[which(count_integer_variables<10)]

data_proc[, (to_factor):=lapply(.SD, as.factor), .SDcols=to_factor]

to_numeric<-integer_variables[which(count_integer_variables>=10)]

data_proc[, (to_numeric):=lapply(.SD, as.numeric), .SDcols=to_numeric]

str(data_proc)

# Now lets check the number of categories per factor variable

factor_variables<-names(data_proc)[sapply(data_proc, is.factor)]
count_factor_variables<-sapply(sapply(data_proc[,factor_variables, with=F], summary),sort,dec=T)
count_factor_variables

# if a label weight less than x% goes into the "others" bag:
f_other<-function(var,min_presence=0.1){
  
  count_levels<-summary(var)/length(var)
  to_bag<-names(which(count_levels<min_presence))
  
  reduced_var<-as.factor(ifelse(as.character(var)%in%to_bag,'others',as.character(var)))
  
  return(reduced_var)
}

# and we apply the function to our factor variables
data_proc[, (factor_variables):=lapply(.SD, f_other), .SDcols=factor_variables]

sapply(sapply(data_proc[,factor_variables, with=F], summary),sort,dec=T)



str(data_proc)



#### NA treatment

sum(is.na(data_proc))




# Binary encoding our factor variables (needed for most algos)

data_ready<-caret::dummyVars(" ~ .", data = data.frame(data_proc), fullRank=T,sep = "_")
data_ready<-data.table(predict(data_ready, newdata = data_proc))

names(data_ready)<-gsub('-','_',names(data_ready))

str(data_proc)
str(data_ready)


fwrite(data_ready, 'Datasets/Classification/data_heart_ready.csv', row.names = F)


