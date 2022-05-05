library(tidyverse)
source('well_fxn_script.R')

# tprime is days to operate per year assuming constant flow at 950 GPM, capped at x acre feet of annual production, where x is 250 and 400 respectively

for(tprime in c(60,95)) {
for (name in c("Henry", "Oliver")) {
yrs<-20
b<-250
T<-b*1.66
Q<-950*192.5

tbl_yrs<-rep(1:yrs,length(r)) %>% sort()
tbl<-tibble(r=rep(r,yrs),s=999,yr=tbl_yrs)


for (yr in 1:yrs) {
if (name=="Henry") {S<-0.125 } else {S<-0.175}
r<-c(100,500,seq(1320,5280,1320),5280+2640,5280*2,15000,20000,25000,30000)
u1<- (r^2*S)/(4*T*yr*365)
u2<- (r^2*S)/(4*T*(yr*(365-tprime)))
Wu<-list_well_fxn(u1,200)
Wu2<-list_well_fxn(u2,200)
s = (Q/(4*pi*T)) * (Wu-Wu2)
result<-tibble(r,s,yr)
tbl<-rows_upsert(tbl,result,by=c("r","yr"))
}

p<-ggplot(tbl, aes(group=yr,x=r,y=-s)) + geom_point(aes(color=yr)) + geom_line(aes(color=yr)) +
ggtitle(paste0(tprime, " days operating annually, ",signif((tprime*1440*950)/7.48/43560,2), " AF annual cap\n",name, " Location")) + 
  xlab("Radial distance from pumping well (ft)") + ylab("Drawdown from static water level (ft)") +
  ylim(-20,0) # + 
#  scale_x_continuous(trans = 'log')
p <- p + labs(color="Years since pumping began") 
p<- p + guides(color = guide_colorbar(reverse=TRUE))
p

ggsave(filename=paste0(name,"_",tprime,".pdf"), path=paste0("./outputs/images/"),plot = p, width = 10, height = 7.5, units="in")
assign(paste0(name,"_result"),  tbl %>% filter(yr==20, r %in% c(seq(1320,5280,1320),5280*2)) %>% mutate(location=name))

}

result_tbl<-tibble(rbind(Henry_result,Oliver_result)) %>% arrange(r,location)
write_csv(result_tbl,file = paste0("./outputs/images/",tprime,"_result_tbl.csv"))
}