S<-0.15
t<-40
r<-seq(100,4000,100)
T<-20000/7.48
u<-(r^2*S)/(4*T*t)
Wu<-list_well_fxn(u,100)
Q<-900*192.5
s<-(Q/(4*pi*T)) * Wu
plot(r,-s)

S<-0.15
t<-40
r<-seq(200,20000,200)
T<-10000/7.48
u<-(r^2*S)/(4*T*t)
Wu<-list_well_fxn(u,200)
Q<-900*192.5
s<-(Q/(4*pi*T)) * Wu
df<-data.frame(cbind(r,s))
ggplot(df %>% filter(!is.na(s)), aes(x=r, y=-s)) +
  geom_line(size=2)
     
     