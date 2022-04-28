library(tidyverse)

for (j in c(100, 1320, 2640, 3960, 5280)) {

S<-0.175
t<-seq(5,365,5)
r<-j # distance of interest
T<-250*1.66
Q<-950*192.5
s<-(Q/(4*pi*T)) * (Wu - Wu2)
tprime<-112
new_vec<-vector(mode="numeric", length=length(t))
j<-1
i<-45
for (i in t) {
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
df<-tibble(t, new_vec) %>% mutate(s = (Q/(4*pi*T)) * new_vec)
names(df)<-c("time","Wu", 's')

## one year drawdown plot
# ggplot(df, aes(x=time,y=-s)) + geom_point() 

enddd<-df[nrow(df),3]

df_new<-df %>% mutate(s_next= Wu*(Q/(4*pi*T)) +1.14)

ggplot(df_new, aes(x=time,y=-s_next)) + geom_point()
df_new<-df %>% mutate(s_next= Wu*(Q/(4*pi*T)) +1.14)

# cumulative maximum drawdown at this point after 20 years
sum(enddd*19,df$s %>% max())

# average drawdown at this point during the 20th season of pumping
sum(enddd*19,df$s %>% mean())

df<-df %>% mutate(t2=time+365, s2=s+enddd$s)
df<-df %>% mutate(t3=t2+365, s3=s2+enddd$s)
df<-df %>% mutate(t4=t3+365, s4=s3+enddd$s)
df<-df %>% mutate(t5=t4+365, s5=s4+enddd$s)
df<-df %>% mutate(t6=t5+365, s6=s5+enddd$s)
df<-df %>% mutate(t7=t6+365, s7=s6+enddd$s)
df<-df %>% mutate(t8=t7+365, s8=s7+enddd$s)
df<-df %>% mutate(t9=t8+365, s9=s8+enddd$s)
df<-df %>% mutate(t10=t9+365, s10=s9+enddd$s)
df<-df %>% mutate(t11=t10+365, s11=s10+enddd$s)
df<-df %>% mutate(t12=t11+365, s12=s11+enddd$s)
df<-df %>% mutate(t13=t12+365, s13=s12+enddd$s)
df<-df %>% mutate(t14=t13+365, s14=s13+enddd$s)
df<-df %>% mutate(t15=t14+365, s15=s14+enddd$s)
df<-df %>% mutate(t16=t15+365, s16=s15+enddd$s)
df<-df %>% mutate(t17=t16+365, s17=s16+enddd$s)
df<-df %>% mutate(t18=t17+365, s18=s17+enddd$s)
df<-df %>% mutate(t19=t18+365, s19=s18+enddd$s)
df<-df %>% mutate(t20=t19+365, s20=s19+enddd$s)

df<-df %>% pivot_longer(cols=c(1,seq(4,40,2)),names_to="varname", values_to = "newtime") 
# time<-time %>% pivot_longer(cols=2:21,names_to="varname2", values_to = "drawdown") 
# 
for (i in unique(df$varname)) {
assign(i,df %>% filter(varname==i))
}

# b<-time %>% filter(varname2=="s2", newtime<=365*2, newtime>365)
# c<-time %>% filter(varname2=="s3", newtime<=365*3, newtime>365*2)
# d<-time %>% filter(varname2=="s4", newtime<=365*4, newtime>365*3)
# e<-time %>% filter(varname2=="s5", newtime<=365*5, newtime>365*4)
# f<-time %>% filter(varname2=="s6", newtime<=365*6, newtime>365*5)
# g<-time %>% filter(varname2=="s7", newtime<=365*7, newtime>365*6)
# h<-time %>% filter(varname2=="s8", newtime<=365*8, newtime>365*7)
# e<-time %>% filter(varname2=="s8", newtime<=365*8, newtime>365*7)
# e<-time %>% filter(varname2=="s8", newtime<=365*8, newtime>365*7)
# e<-time %>% filter(varname2=="s8", newtime<=365*8, newtime>365*7)
# e<-time %>% filter(varname2=="s8", newtime<=365*8, newtime>365*7)


dd<-c(time$s,t2$s2, t3$s3, t4$s4, t5$s5, t6$s6, t7$s7, t8$s8, t9$s9, t10$s10, t11$s11, t12$s12, t13$s13,
      t14$s14, t15$s15,t16$s16,t17$s17,t18$s18,t19$s19,t20$s20)

df<-df %>% arrange(newtime)
tbl<-tibble(df$newtime,dd)
names(tbl)<-c("time","dd")

# 20 year dd plot


p<-ggplot(tbl, aes(x=time,y=-dd)) + geom_point()
ggsave(paste0("outputs/images/",r,".jpg"), plot = p, width = 5, height = 3, units="in")
}

