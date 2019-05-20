source('/Users/ssobrinou/IE/Advanced/2019_Advanced/Regression/code/load_libraries.R')

data("AirPassengers")
plot(AirPassengers); grid()

y<-copy(AirPassengers)


library(forecast)

str(y)
frequency(y)

# stl function for time series de-composiong

decomp<-stl(y, s.window = 12)
plot(decomp); grid()



# creating fourier series of different amplitude

forecast::fourier(y,K=4)

df<-data.table(y=y,
               fourier(y,K=1))
df

colnames(df)<-gsub('-','_',colnames(df))

# let's calculate manually S1 and C1

t=seq(1,12)
N=12


par(mfrow=c(1,2))
c1<-cos(2*pi*t/N)
plot(c1, type='b', main='1st Sin Wave'); grid()
identical(round(c1,4), round(df$C1_12[1:12],4))

s1<-sin(2*pi*t/N)
plot(s1, type='b', main='1st Cos Wave'); grid()
identical(round(s1,4), round(df$S1_12[1:12],4))
par(mfrow=c(1,1))



df[, tend:=seq(1,nrow(df))]


lm_fourier<-lm(log(y)~., data=df)
summary(lm_fourier)  

plot(as.numeric(y), type='l'); grid()
lines(exp(fitted(lm_fourier)), col='red')
  
  
