library(tidyverse)
source('well_fxn_script.R')

alt_seq <- function (days,yrs) {
  intervals <- c(days,365)
  b<-rep(1:yrs,2) %>% sort()
  b<-(b-1)*365 + rep(intervals,yrs)
  return(b)
}

for(name in c("Oliver", "Henry")) {
  idx=1
  result_tbl<-tibble(c(100, 500, 1320, 2640, 3960, 5280),5) 
  names(result_tbl)<-c("distance_feet","mean_drawdown_20_yrs")
for (r in c(100, 500, 1320, 2640, 3960, 5280)) {
if (name=="Henry") {S<-0.125 } else {S<-0.175}
tprime<-95# days to run pumping per year
yrs<-20
b<-600
T<-b*1.66
Q<-950*192.5
#s<-(Q/(4*pi*T)) * (Wu - Wu2)
breaks<-alt_seq(tprime,yrs)
tshort<-seq(5,365,5)
tlong<-seq(5,365*yrs,5)
tlong<-tibble(tlong)
tlong<-tlong %>% mutate(Wu=999, s=999, saturated_production_interval=999)
names(tlong)<-c("time","Wu","s", "saturated_production_interval")
new_vec<-vector(mode="numeric", length=length(tshort))
for (yr in 0:(yrs-1)) {
j<-1
for (i in tshort) {
  if (i <= tprime) {
    u<-(r^2*S)/(4*T*i)
    new_vec[j] <- well_fxn(u,200)
    j<-j+1
  }
  else {
    u1<-(r^2*S)/(4*T*i)
    u2<-(r^2*S)/(4*T*(i-tprime))
    Wu<-well_fxn(u1,200)
    Wu2<-well_fxn(u2,200)
    new_vec[j] <- Wu-Wu2
    j=j+1
  }
 df<-tibble(tshort+yr*365, new_vec) %>% mutate(s = (Q/(4*pi*T)) * new_vec, saturated_production_interval=b-((Q/(4*pi*T)) * new_vec))
names(df)<-c("time","Wu", 's', 'saturated_production_interval')
}

sprime<-as.numeric((df %>% filter(time==max(time)) %>% dplyr::select(s)))
b<-b-sprime
T<-b*1.66
tlong<-dplyr::rows_upsert(tlong, df, by="time")
#print(c(yr, T, b))
}

tlong<-tlong %>% mutate(yr=floor(time/365.01))


#one year drawdown plot
#ggplot(tlong %>% filter(yr==0), aes(x=time,y=saturated_production_interval)) + geom_point() + geom_line()


p<-ggplot(tlong, aes(x=time,y=saturated_production_interval)) + geom_point() + geom_line() +
  ggtitle(paste0(tprime, " days operating annually, ",signif((tprime*1440*950)/7.48/43560,2), " AF annual cap\n",name, " Location, ", r, " feet distant"))
p
ggsave(filename=paste0(name,"_",r,".pdf"), path=paste0("./outputs/images/",name),plot = p, width = 5, height = 3, units="in")


original_sat_thick<-tlong %>% filter(time==min(time)) %>% dplyr::select(saturated_production_interval)
  s_final_mean<-tlong %>% filter(yr==max(yr)) %>% summarize(mean=mean(saturated_production_interval))
  result_tbl$mean_drawdown_20_yrs[idx]<-original_sat_thick$saturated_production_interval-s_final_mean$mean
  idx=idx+1
}
  write_csv(result_tbl,file = paste0("./outputs/images/",name,"/result_tbl.csv"))
}


