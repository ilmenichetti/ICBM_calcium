
library(readxl)
library(forecast)
library(zoo)
library(dfoptim)
library(hydroGOF)

time_series<-as.data.frame((read_excel("./Data/Collected_data_from_reports.xlsx", sheet=1)))
dim(time_series)


png("Balance_data.png", height=2000, width=3000, res=300)
par(mfrow=c(2,3))
plot(colnames(time_series[i,3:31]),time_series[1,3:31]*0.45*1000, ylab = time_series[6,2], xlab="Years", main= time_series[i,1])
lines(na.approx(ts(as.numeric(time_series[1,3:31]*0.45*1000), start=1991, end=2019)), lty=2)
for(i in 2:6){
  plot(colnames(time_series[i,3:31]),time_series[i,3:31], ylab = time_series[i,2], xlab="Years", main= time_series[i,1])
  lines(na.approx(ts(as.numeric(time_series[i,3:31]), start=1991, end=2019)), lty=2)
}
dev.off()


plot(colnames(time_series[i,3:31]),time_series[1,3:31]*0.45*1000, ylab = time_series[6,2], xlab="Years", main= time_series[i,1])
lines(na.approx(ts(as.numeric(time_series[1,3:31]*0.45*1000), start=1991, end=2019)), lty=2)




### Fit an exponential to the decay, utilizing ICBM young pool as reference for decomposition

#exponential function (in percent of materail decayed)
exp_decay_percent<-function(time, r){
  k=0.400297610547104 #ICBM k1 recalibration
  100*exp(-k*time*r)
}

#cost function
exp_decay_fit<-function(x){
  ts_sim<-exp_decay_percent(seq(1:3), x)
  return(rmse(ts, ts_sim))
}

#fitting the cost function to the vector
r_vec<-c()
for(i in 9:31){
ts<-time_series[7:9,i]
r_vec[i]<-optimize(exp_decay_fit, interval=c(0,10))$minimum
}

png("Decomposition_scaling.png", height=1500, width=2000, res=300)
plot(colnames(time_series[i,3:31]),r_vec[3:31], xlab="Years",  ylab="Decomposition scaling", pch=17, main="r scaling factor (assuming ICBM2022 kinetic of decay)")
dev.off()

time_series<-rbind(time_series, r_vec)
time_series[10,1]<-"r_scaling"
