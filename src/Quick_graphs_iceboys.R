library("data.table")
library("tidyverse")
library("ggplot2")
library("ggpubr")
library("lubridate")
library("svMisc")
library("nlme")
library("ggpp")
library("ggpmisc")
library("tidyr")
library("car")


#### ICE QUALITY PLOTTING ####
df=read.csv("C:/Users/kevin/OneDrive/Skrivbord/York Uni Work/ice_quality_update.csv")

ggplot(df)+geom_smooth(aes(x=mdy(date),y=snow_avg_cm),linetype="dashed",se=F,color="black")+geom_point(aes(x=mdy(date),y=snow_avg_cm))+
  geom_smooth(aes(x=mdy(date),y=white_ice_cm,color="grey"),se=F)+geom_point(aes(x=mdy(date),y=white_ice_cm),shape=17)+
  geom_smooth(aes(x=mdy(date),y=black_ice_cm,color="black"),se=F)+geom_point(aes(x=mdy(date),y=black_ice_cm),shape=17)+
  xlab("Date")+ylab("ice quality and snow thickness [cm]")+ scale_color_manual(labels = c("Black ice", "White ice"), values = c("black", "grey"))+theme_bw()+
  theme(legend.position = 'bottom', legend.direction = "horizontal")+guides(color=guide_legend("Ice type"))+facet_wrap(year(mdy(df[,"date"]))~lake,scales = "free_x")

#ggsave("C:/Users/kevin/OneDrive/Skrivbord/York Uni Work/Figures/ice and snow.tiff",width=25,height=18,units="cm",dpi=300)

ggplot(df)+geom_smooth(aes(x=mdy(date),y=white_ice_cm,color="grey"),se=F)+geom_point(aes(x=mdy(date),y=white_ice_cm),shape=17)+
  geom_smooth(aes(x=mdy(date),y=abs(white_ice_cm-wht_slush_cm),color="black"),se=F)+geom_point(aes(x=mdy(date),y=abs(white_ice_cm-wht_slush_cm)),shape=15)+
  xlab("Date")+ylab("ice quality and snow thickness [cm]")+ scale_color_manual(labels = c("Slush", "White ice"), values = c("black", "grey"))+theme_bw()+
  theme(legend.position = 'bottom', legend.direction = "horizontal")+guides(color=guide_legend("Ice type"))+facet_wrap(year(mdy(df[,"date"]))~lake,scales = "free_x")
#ggsave("C:/Users/kevin/OneDrive/Skrivbord/York Uni Work/Figures/WHite ice and Slushh build up.tiff",width=25,height=18,units="cm",dpi=300)

#### DUMB idea of snow and Slush relationship #### 
# Slush_layer<-(df[,"wht_slush_cm"]-df[,"white_ice_cm"])
# df<-cbind(df,Slush_layer)
# df["Slush_layer"][df["Slush_layer"] == 0] <- NA
# 
# fit = lm(df[,"snow_avg_cm"] ~ df[,"Slush_layer"] ,data=df )
# cooks.distance(fit)
# 
# plot(cooks.distance(fit),type="b",pch=18,col="red")
# abline(h=4/11)
# cooks.distance(fit)[which.max(cooks.distance(fit))]
# plot(fit,which=4)
# df_cooks<-df
# df_cooks[42,"Slush_layer"] <- NA
# 
# ggplot(df)+geom_point(aes(x=(snow_avg_cm),y=(Slush_layer),color=date),size=4)
# 
# fit_outlier = lm(df_cooks[,"snow_avg_cm"] ~ df_cooks[,"Slush_layer"] ,data=df_cooks )




#### WEATHER DATA PLOTTING ####
Weather_df<- read.csv("C:/Users/kevin/OneDrive/Skrivbord/York Uni Work/Climate/Climate data up to April 2025.csv")
Weather_df<-as.data.table(Weather_df)
TempSide<-vector()
for (i in 1:length(Weather_df[,Temp...C.])){
  if (Weather_df[i,Temp...C.]>0){TempSide[i]<-"above"} else{TempSide[i]<-"below"}
}
Weather_df<-cbind(Weather_df,TempSide)

## TEMPERATURE PLOT 
ggplot()+geom_point(data = Weather_df[TempSide=="above"],aes(x=dmy_hm(Date.Time..LST.),y=Temp...C.,color="blue"))+
geom_point(data = Weather_df[TempSide=="below"],aes(x=dmy_hm(Date.Time..LST.),y=Temp...C.,color="red"))+
  theme_bw()+xlab("")+ylab(expression('Temperature'*~degree*C*''))+theme(legend.position = "none")+
facet_wrap(WinterSeason~Station.Name,scales = "free")
#ggsave("C:/Users/kevin/OneDrive/Skrivbord/York Uni Work/Figures/tempside.tiff",width=25,height=18,units="cm",dpi=300)


## PRECIPITATION PLOT
Weather_df_NA<-Weather_df
Weather_df_NA<-Weather_df[Precip..Amount..mm. == 0, Precip..Amount..mm. := NA]

Precip_summary<- Weather_df_NA%>% group_by(Station.Name,WinterSeason,format(as.Date(dmy_hm(Weather_df[,Date.Time..LST.])),"%d/%m/%Y")) %>% summarise(
  AvgPerc=mean(Precip..Amount..mm.,na.rm=TRUE),
  TempAvg=mean(Temp...C.,na.rm=TRUE),)

Precip_summary<-as.data.table(Precip_summary)
TempSide<-vector()
for (i in 1:length(Precip_summary[,TempAvg])){
  if (Precip_summary[i,TempAvg]>0){TempSide[i]<-"above"} else{TempSide[i]<-"below"}
}
Precip_summary<-cbind(Precip_summary,TempSide)
colnames(Precip_summary)<-c("Station.Name","WinterSeason","Dates","AvgPerc","TempAVg","TempSide")

Precip_summary<-Precip_summary %>%
  mutate(Dates = Dates, 
         date = day(Dates), month = month(Dates), year = year(Dates))
# 
# Stations_2024<- Precip_summary %>% filter(WinterSeason==2024)
# Stations_2025<- Precip_summary %>% filter(WinterSeason==2025)
# Barrie_Prec<-Precip_summary %>% filter(Station.Name=="BARRIE-ORO")
# Beatrice_Prec<-Precip_summary %>% filter(Station.Name=="BEATRICE CLIMATE")
# 
# 
# Levene_2024<- leveneTest(AvgPerc ~ as.factor(Station.Name), data= Stations_2024)

ggplot()+geom_point(data = Precip_summary[TempSide=="above"],aes(x=dmy(Dates),y=AvgPerc,color="blue"))+
  geom_point(data = Precip_summary[TempSide=="below"],aes(x=dmy(Dates),y=AvgPerc,color="red"))+
  theme_bw()+xlab("")+ylab("Precipitation [mm]")+theme(legend.position = "none")+
  facet_wrap(WinterSeason~Station.Name,scales = "free_x")
#ggsave("C:/Users/kevin/OneDrive/Skrivbord/York Uni Work/Figures/precip.tiff",width=25,height=18,units="cm",dpi=300)

ggplot(Precip_summary)+geom_boxplot(aes(x=factor(month),y=AvgPerc,fill=factor(TempSide)),outlier.shape = NA)+
  xlab("")+ylab("Mean Precipitation [mm/month]")+ scale_y_continuous(limits = c(0,2.6))+
  scale_x_discrete(limits=c("11","12","1","2","3"),labels=c("Nov","Dec","Jan","Feb","Mar"))+theme_bw()+
  theme(legend.position = 'bottom', legend.direction = "horizontal",legend.title = element_blank())+facet_wrap(WinterSeason~Station.Name,scales = "free_x")
#ggsave("C:/Users/kevin/OneDrive/Skrivbord/York Uni Work/Figures/Mean_Box_precip.tiff",width=25,height=18,units="cm",dpi=300)
  
#### RBR DATA ANALYSIS & MAX DEPTHS ####

RBR_2024<- read.csv("C:/Users/kevin/OneDrive/Skrivbord/York Uni Work/RBR/2024 RBR Data.csv")
RBR_2025 <- read.csv("C:/Users/kevin/OneDrive/Skrivbord/York Uni Work/RBR/All Sites 2025 RBR.csv")

RBR_2024<-RBR_2024[!(RBR_2024$Site %in% c("simcoe.shallow")), ]
RBR24<-RBR_2024%>%filter(Sort_Negative==1)


Uni24<-unique(RBR24[,"Time"])
Max_depth_24<-vector()
Short_RBR_2024<-data.frame()
for (i in 1:length(Uni24[])){
  Separator <-RBR24 %>% filter(Time == Uni24[i])
  Station_uni<- unique(Separator[,"Site"])
  
  for (j in 1:length(Station_uni[])){
    print (paste("i=",i,"j=",j))
    Calcu<-Separator%>%filter(Site == Station_uni[j])
    Max_depth_24<-rbind(Max_depth_24,max(Calcu[,"Depth"]))
    Short_RBR_2024<-rbind(Short_RBR_2024,Calcu[1:which(Calcu[,"Depth"]==max(Calcu[,"Depth"])),])
    
  }
    
}

Short_RBR_2024<- Short_RBR_2024 %>% filter(Depth < 1)


ggplot(Short_RBR_2024)+geom_point(aes(x=Temperature,y=-Depth))+facet_wrap(Site~dmy(Time))
#ggsave("C:/Users/kevin/OneDrive/Skrivbord/York Uni Work/Figures/RBR_short_24.tiff",width=25,height=18,units="cm",dpi=300)


ggplot(Short_RBR_2024)+geom_boxplot(aes(x=Time,y=Temperature,fill=Site))+ylab("1m Below Ice 2024")+theme_bw()+
  theme(axis.text.x = element_blank(),legend.position = 'none',axis.title.y = element_text(size=12))+facet_wrap(~Site,nrow=1)+theme(strip.text = element_text(size=12))
#ggsave("C:/Users/kevin/OneDrive/Skrivbord/York Uni Work/Figures/Temp_below_ice_2024.tiff",width=25,height=18,units="cm",dpi=300)


RBR25<-RBR_2025%>%filter(Sort_Negative==1)
Uni25<-unique(RBR25[,"Time"])
Max_depth_25<-vector()
Short_RBR_2025<-data.frame()
for (i in 1:length(Uni25[])){
  Separator <-RBR25 %>% filter(Time == Uni25[i])
  Station_uni<- unique(Separator[,"Site"])
  
  for (j in 1:length(Station_uni[])){
    print (paste("i=",i,"j=",j))
    Calcu<-Separator%>%filter(Site == Station_uni[j])
    Max_depth_25<-rbind(Max_depth_25,max(Calcu[,"Depth"]))
    Short_RBR_2025<-rbind(Short_RBR_2025,Calcu[1:which(Calcu[,"Depth"]==max(Calcu[,"Depth"])),])
    
    #mean(Calcu[1:which(Calcu[,"Depth"]==max(Calcu[,"Depth"])),"Temperature"])
  }
  
}

Short_RBR_2025<- Short_RBR_2025 %>% filter(Depth < 1)

ggplot(Short_RBR_2025)+geom_point(aes(x=Temperature,y=-Depth))+facet_wrap(Site~dmy(Time))
#ggsave("C:/Users/kevin/OneDrive/Skrivbord/York Uni Work/Figures/RBR_short_25.tiff",width=25,height=18,units="cm",dpi=300)

ggplot(Short_RBR_2025)+geom_boxplot(aes(x=Time,y=Temperature,fill=Site))+ylab("1m Below Ice 2025")+theme_bw()+
  theme(axis.text.x = element_blank(),legend.position = 'none',axis.title.y = element_text(size=12))+facet_wrap(~Site)+theme(strip.text = element_text(size=12))
#ggsave("C:/Users/kevin/OneDrive/Skrivbord/York Uni Work/Figures/Temp_below_ice_2025.tiff",width=25,height=18,units="cm",dpi=300)


#### TESTING THE TWO YEARS AS POPULATIONS OF TEMPERATURE ####

Mann_Whitney_water_temp<-wilcox.test(Short_RBR_2024[,"Temperature"],Short_RBR_2025[,"Temperature"],exact=FALSE)

Density<-data.frame(Time=c(Short_RBR_2024$Time,Short_RBR_2025$Time),
                    Temperature=c(Short_RBR_2024$Temperature,Short_RBR_2025$Temperature),
                                  Site=c(Short_RBR_2024$Site,Short_RBR_2025$Site))

Density[,"Time"]<-dmy(Density[,"Time"])
Density<-cbind(Density,Year=year(Density[,"Time"]))

#ggdensity(Density,x="Temperature",color = as.character("Year"))#, palette = c("#00AFBB", "#E7B800"))
mean25=Density%>%filter(Year == 2025)
mean24=Density%>%filter(Year == 2024)

ggplot(Density)+geom_density(aes(x=Temperature,fill=factor(Year)),alpha=0.5)+
  geom_rug(aes(x=Temperature,color=factor(Year)))+geom_vline(aes(xintercept= median(mean24[,"Temperature"])),linetype="dashed")+
  geom_vline(aes(xintercept= median(mean25[,"Temperature"])),linetype="dashed")+theme_bw()+
  theme(legend.text = element_text(size=12),legend.position = 'bottom',legend.title = element_blank(),axis.text =element_text(size=12),axis.title = element_text(size=12))+
  ggtitle("Wilcox Test p< 0.01")
#ggsave("C:/Users/kevin/OneDrive/Skrivbord/York Uni Work/Figures/Temp_Distributuion.tiff",width=25,height=18,units="cm",dpi=300)

ggplot(Density)+geom_density(aes(x=Temperature,fill=factor(Year)),alpha=0.5)+
  geom_rug(aes(x=Temperature,color=factor(Year)))+theme_bw()+theme(legend.text = element_text(size=12),legend.position = 'bottom',legend.title = element_blank(),axis.text =element_text(size=12),axis.title = element_text(size=12))+
  facet_wrap(~Site,scales="free_y")+theme(strip.text = element_text(size=12))
#ggsave("C:/Users/kevin/OneDrive/Skrivbord/York Uni Work/Figures/Temp_Distributuion_Lake.tiff",width=25,height=18,units="cm",dpi=300)

ggplot(Density)+geom_boxplot(aes(x=factor(Time),y=Temperature,fill=factor(Year)))+xlab("")+ylab(expression('Temperature'*~degree*C*''))+theme_bw()+
  theme(legend.position = 'none',axis.text.x = element_text(angle = 90,vjust = 0.5),axis.title.y = element_text(size=12))+facet_wrap(Year~Site,scales = "free")#ggsave("C:/Users/kevin/OneDrive/Skrivbord/York Uni Work/Figures/Temp_boxplot_Lake.tiff",width=25,height=18,units="cm",dpi=300)
#ggsave("C:/Users/kevin/OneDrive/Skrivbord/York Uni Work/Figures/Temp_Boxplot_Lake.tiff",width=25,height=18,units="cm",dpi=300)

