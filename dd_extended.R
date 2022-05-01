library(tidyverse)

alt_seq <- function (days,yrs) {
  intervals <- c(days,365)
  b<-rep(1:yrs,2) %>% sort()
  b<-(b-1)*365 + rep(intervals,yrs)
  return(b)
}

for (r in c(100, 500, 1320, 2640, 3960, 5280)) {

S<-0.125
tprime<-95  # days to run pumping per year
yrs<-20
T<-250*1.66
Q<-950*192.5
s<-(Q/(4*pi*T)) * (Wu - Wu2)
breaks<-alt_seq(tprime,yrs)
tshort<-seq(5,365,5)
tlong<-seq(5,365*yrs,5)
new_vec<-vector(mode="numeric", length=length(tshort))
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
  
}

df<-tibble(tshort, new_vec) %>% mutate(s = (Q/(4*pi*T)) * new_vec)
names(df)<-c("time","Wu", 's')

sprime<-as.numeric((df %>% filter(time==max(time)) %>% select(s)))

tbl<-as.data.frame(cbind(tlong,floor(tlong/365),df$time, df$s))
names(tbl)<-c("days","yrs","day_of_year", "s_raw")
tbl<-tbl %>% mutate(s=s_raw+yrs*sprime)

# one year drawdown plot
#ggplot(df, aes(x=time,y=-s)) + geom_point() + geom_line()


p<-ggplot(tbl, aes(x=days,y=-s)) + geom_point() + geom_line()
p
ggsave(paste0("outputs/images/",r,".jpg"), plot = p, width = 5, height = 3, units="in")
}


