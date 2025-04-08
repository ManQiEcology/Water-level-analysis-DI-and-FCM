rm(list=ls())
setwd(here::here())

library(readxl)
#install.packages("lubridate")
#install.packages("hydrostats")
library(hydrostats)
library(lubridate)
Sys.setlocale("LC_TIME", "English") #set the default system language to be English
#1.load well info
well_ele<-read.csv("Data/Well info.txt",sep="", header = T)
#install.packages('Rcpp')
library(Rcpp)
##########################################################################################
#1. load logger depth file
##########################################################################################
#1.1 load groundwater data of Deal Island
DI_TC<-read_excel("Data/20138781_DI_G_TC.xlsx")
DI_H<-read_excel("Data/20574343_DI_G_H.xlsx")
DI_E<-read_excel("Data/20574344_DI_G_E.xlsx")
DI_P<-read_excel("Data/20292500_DI_G_P.xlsx")
#assign site name to sensor depth
DI_TC$DI_TC<-DI_TC$`Sensor depth (Meters)`
DI_H$DI_H<-DI_H$`Sensor depth (Meters)`
DI_E$DI_E<-DI_E$`Sensor depth (Meters)`
DI_P$DI_P<-DI_P$`Sensor depth (Meters)`
#merge groundwater data from different sites from Deal Island
DI<-NULL
DI<-merge(DI_TC,DI_H,by="Time, GMT-04:00")
DI<-merge(DI,DI_E,by="Time, GMT-04:00")
DI<-merge(DI,DI_P,by="Time, GMT-04:00")
#extract groundwater data from different sites of Deal Island and generate groundwater data file for Deal Island
DI_SD<-data.frame("DateTime"=DI$`Time, GMT-04:00`,TC=DI$DI_TC,H=DI$DI_H,E=DI$DI_E,P=DI$DI_P)
DI_SD$DateTime<-ymd_hms(DI_SD$DateTime,tz="US/Eastern") #define time with lubridate
#DI_SD above contains sensor depth data of Deal Island

#remove original DI dataset
rm(DI_E,DI_H,DI_P,DI_TC,DI)

#1.2 load groundwater of Farm Creek Marsh
FCM_HC<-read_excel("Data/20574349_FCM_G_HC.xlsx")
FCM_DC<-read_excel("Data/20574347_FCM_G_DC.xlsx")
FCM_PC<-read.csv("Data/Site4S-FCM_G_PC.csv",header = T)#the time is in EST
#Sensor depth of healthy and dieback
FCM_HC$FCM_HC<-FCM_HC$`Sensor depth (Meters)`
FCM_DC$FCM_DC<-FCM_DC$`Sensor depth (Meters)`
FCM<-NULL
FCM<-merge(FCM_HC,FCM_DC,by="Time, GMT-04:00")
FCM_SD<-data.frame("DateTime"=FCM$`Time, GMT-04:00`,H=FCM$FCM_HC,E=FCM$FCM_DC) #datetime in EST
FCM_SD$DateTime<-ymd_hms(FCM_SD$DateTime,tz="US/Eastern") #define time with lubridate
FCM_SD<-subset(FCM_SD,DateTime>min(DI_SD$DateTime,na.rm = T)&DateTime<max(DI_SD$DateTime,na.rm = T))
#FCM_SD above contains sensor depth data healthy (H) and dieback (E) from May 2019 to March 2020
#remove orginal dataset from FCM
rm(FCM_DC,FCM_HC,FCM)


#preliminary process of groundwater data from FCM pond
yr2mth<-function(x) {
  for (i in 1:9) {
    x<-gsub(paste("200",i,sep = ""),i,x)
  }
  for (i in 10:12) {
    x<-gsub(paste("20",i,sep = ""),i,x)
  }
  x
}
FCM_PC$DateTime<-yr2mth(FCM_PC$DateTime)
FCM_PC$DateTime<-mdy_hm(FCM_PC$DateTime,tz="US/Eastern")# datetime is US/Eastern '(UTC-5)'
FCM_PC$waterlevel <-FCM_PC$waterlevel*0.3048 # Clevel is the water level relative to MSL(NAVD88),turn foot to meter 1 foot =0.3048*meter, adjust water level (-0.15m) to get it consistent with the healthy and the dieback
FCM_PC<-subset(FCM_PC,DateTime>min(FCM_SD$DateTime,na.rm = T)&DateTime<max(FCM_SD$DateTime,na.rm = T),tz="US/Eastern") #keep pond dataseries within dieback and healthy patch
#############################################################################################
#2. Turn water depth to water level relative to MSL (NAVD1988)
Logger_depth<-well_ele$Logger_depth_2019_.cm./100#logger depth has a unit of cm, transfer to m
surface_elevation<-well_ele$Soil_surface_elevation.m._2019
logger_elevation<-well_ele$Well_top_elevation.m._2020-well_ele$Well_top_to_logger.cm./100 #consider well height and well top to logger doesb't change much, within 2cm from 2019 to 2020, thus use this to calculate water level
#for DI, calckulate water depth relatuve to Mean Sea Level (NAVD1988)
DI_WL_MSL<-data.frame("DateTime"=DI_SD$DateTime,
                      TC=DI_SD$TC-Logger_depth[1]+surface_elevation[1], 
                      H=DI_SD$H+surface_elevation[4]-Logger_depth[4], #marsh elevation might decreased by 4cm from 2019 May to 2020 March
                      E=DI_SD$E+surface_elevation[2]-Logger_depth[2], #marsh surface elevation and well elevation might decreased by 2cm from 2019 May to 2020 Mar
                      P=DI_SD$P+surface_elevation[3]-Logger_depth[3]) # marsh surface elevation and well elevation variation is around 1cm from 2019 May to 2020 March

FCM_WL_MSL<-data.frame("DateTime"=FCM_SD$DateTime,
                       H=FCM_SD$H+surface_elevation[7]-Logger_depth[7], #marsh elevation decrease by 2cm, but surface accreation increase by 2cm
                       E=FCM_SD$E+0.5*(surface_elevation[6]-Logger_depth[6]+logger_elevation[6]))# since marsh surface elevation and well elevation increased by 0.58 cm from 2019 May to 2020 March, we use average logger height

#load bishop tide water,Datum:NAVD88
bishop<-as.data.frame(read.csv("Data/bishop.csv",header = T))
bishop$DateTime<-paste(bishop$Date,bishop$Time..GMT.) #merge data and hm
bishop$DateTime<-ymd_hm(bishop$DateTime,tz="GMT") #define the date format with GMT time zone
bishop$DateTime<-with_tz(bishop$DateTime,tz="US/Eastern") # convert the time zone to US/Eastern
bishop<-data.frame("DateTime"=bishop$DateTime,TH=as.numeric(bishop$Verified..m.))
bishop<-subset(bishop,DateTime>min(FCM_SD$DateTime,na.rm = T)&DateTime<max(FCM_SD$DateTime,na.rm=T))
#correct DI-TC data at low tide with bishop data
bishoplus0.07<-data.frame(DateTime=bishop$DateTime,
                          TH=bishop$TH+0.07) #water level in DI_TC is 0.07higher than that in bishop, so use bishop+0.07 as replacement
DI_WL_MSL$TC[which(DI_WL_MSL$TC<=0)]<-NA #it is found that data is incorrect when water level of DI_TC is less than zero, thus replace this value with NA

for (i in which(is.na(DI_WL_MSL$TC)==T)) { #replace NA data with bishop+0.07
  x<-which(bishoplus0.07$DateTime==DI_WL_MSL$DateTime[i])
  if (identical(x, integer(0))) next
  else (DI_WL_MSL$TC[i]<-bishoplus0.07$TH[x])
}
#interpolate DI-TC 
model <- lm(TC ~ DateTime, data = DI_WL_MSL) 
a<-approx(DI_WL_MSL$DateTime, DI_WL_MSL$TC, xout=DI_WL_MSL$DateTime[which(is.na(DI_WL_MSL$TC)==T)])
DI_WL_MSL$TC[which(is.na(DI_WL_MSL$TC)==T)] <- a$y
#data in healthy and dieback patch of DI is incorrect at high water level

################################################################################
#3. FIGURE1_Water level time series (NAVD 88)
################################################################################
Marsh_surf_ele<-well_ele$Soil_surface_elevation.m._2019
par(mai<-c(0.1,0.1,0.1,0.5),cex=1,font=1,cex.lab=1,cex.axis=1,cex.main=1)
tiff("Water level 2019-2020 .tiff",units="in",width = 6,height=8,res=300)
par(mfrow=c(2,1))

plot(DI_WL_MSL$DateTime,DI_WL_MSL$TC,type="l",col="grey", 
     xlim=c(ymd_hm("2019-05-02 00:00"),ymd_hm("2020-03-04 23:00")),
     #xlim=c(ymd_hm("2019-05-02 00:00"),ymd_hm("2019-05-10 23:00")), #alternative time range for inset
     ylim=c(-0.6,1.2),
     #ylim=c(-0.2,0.8), #alternative y axis scale for inset 
     xlab = "", 
     ylab="",
     #xaxt = "n"
     )

lines(DI_WL_MSL$DateTime,DI_WL_MSL$H,col="seagreen")
lines(DI_WL_MSL$DateTime,DI_WL_MSL$E,col="darkorange")
lines(DI_WL_MSL$DateTime,DI_WL_MSL$P,col="royalblue")
time1<-DI_WL_MSL$DateTime

lines(time1,rep(Marsh_surf_ele[1],length(time1)),lty="dashed",col="grey",lwd=1)
lines(time1,rep(Marsh_surf_ele[4],length(time1)),lty="dashed",col="seagreen",lwd=1) #plot "elevation of healthy path minus elevation of tidal creek"
lines(time1,rep(Marsh_surf_ele[2],length(time1)),lty="dashed",col="darkorange",lwd=1) #plot "elevation of dieback minus elevation of tidal creek"
lines(time1,rep(Marsh_surf_ele[3],length(time1)),lty="dashed",col="royalblue",lwd=1) #plot "elevation of pond minus elevation of tidal creek"
text(ymd_hms("2020-01-01 00:00:00",tz="US/Eastern"),1.0,"Deal Island",col="black",font=3,pos=4,xpd=T,cex = 1)
legend(x="topleft",legend=c("Tidal creek", "Pond","Dieback zone","Vegetated zone"),
       col=c("gray","royalblue","darkorange","seagreen"),
       lty = c("solid","solid","solid","solid"),
       ncol=2,bty = "n",cex = 1)
plot(bishop$DateTime,bishop$TH,type="l",col="gray", 
     xlim=c(ymd_hm("2019-05-02 00:00"),ymd_hm("2020-03-04 23:00")),
     #xlim=c(ymd_hm("2019-05-02 00:00"),ymd_hm("2019-05-10 23:00")),  #alternative time range for inset
     ylim=c(-0.6,1.2),
     #ylim=c(-0.2,0.8), #alternative y axis scale for inset 
     xlab = "Time (Date)", 
     ylab="Water level (m, NAVD 88) ", las=3
     #xlab = "", 
     #ylab="",
     )

lines(FCM_WL_MSL$DateTime,FCM_WL_MSL$H,col="seagreen")
lines(FCM_WL_MSL$DateTime,FCM_WL_MSL$E,col="darkorange")
lines(FCM_PC$DateTime,FCM_PC$waterlevel,col="royalblue")# FCM_PC is already relative to MSL NAVD88
lines(time1,rep(Marsh_surf_ele[7],length(time1)),lty="dashed",col="seagreen",lwd=1)
lines(time1,rep(Marsh_surf_ele[9],length(time1)),lty="dashed",col="royalblue",lwd=1)
lines(time1,rep(Marsh_surf_ele[6],length(time1)),lty="dashed",col="darkorange",lwd=1)
text(ymd_hms("2019-11-20 00:00:00",tz="US/Eastern"),1.0,"Farm Creek Marsh",col="black",font=3,pos=4,xpd=T,cex = 1)
dev.off()

################################################################################
#4. FIGURE S2_histogram of water level
################################################################################
#prepare data of DI--hist_DI
L<-dim(DI_WL_MSL)[1]
L_FCM_HE<-dim(FCM_WL_MSL)[1]
L_FCM_PC<-dim(FCM_PC)[1]
L_bishop<-dim(bishop)[1]
hist_DI<-data.frame(WL=c(DI_WL_MSL$TC,DI_WL_MSL$H,DI_WL_MSL$E,DI_WL_MSL$P),
                    Zone=c(rep("Tidal creek",L),rep("Vegetated zone",L),rep("Dieback zone",L),rep("Pond",L)))
hist_DI$Zone<-factor(hist_DI$Zone,levels=unique(hist_DI$Zone))
#prepare data of FCM--hist_FCM
hist_FCM<-data.frame(WL=c(bishop$TH,FCM_WL_MSL$H,FCM_WL_MSL$E,FCM_PC$waterlevel),
                     Zone=c(rep("Tidal creek",L_bishop),rep("Vegetated zone",L_FCM_HE),rep("Dieback zone",L_FCM_HE),rep("Pond",L_FCM_PC)))
hist_FCM$Zone<-factor(hist_FCM$Zone,levels=unique(hist_FCM$Zone))

#plot DI
library(ggplot2)
tiff("histogram DI 2019-2020 .tiff",units="in",width = 3.5,height=1.5,res=300)
DI<-ggplot(hist_DI, aes(x = WL, fill = Zone)) +                       # Draw overlaying histogram
  geom_histogram(aes(y = stat(width*density)), position = "identity", alpha = 0.6, bins =100)+
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,0.2))+
  xlab("Water level (m, NAVD88)") +
  ylab("Frequency")+
  xlim(-0.6,1)+
  geom_text(x=0.1,y=1.1,label="Deal Island",col="Black")+
  theme_bw()  +
  theme(legend.position = c(0.8,0.7),
        legend.key.size = unit(0.3,'cm'),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
DI+scale_fill_manual(values = c("black", "seagreen", "darkorange", "royalblue"))
stat(width*density)
dev.off()
geom_histogram(aes(y= stat(width*density)),binwidth = 1.5,position="dodge",alpha=0.5)+
  geom_density(alpha=0.2)+

#plot FCM                    
tiff("histogram FCM 2019-2020 .tiff",units="in",width = 3.5,height=1.5,res=300)
FCM<-ggplot(hist_FCM, aes(x = WL, fill = Zone)) +                       # Draw overlaying histogram
  geom_histogram(aes(y = stat(width*density)), position = "identity", alpha = 0.6, bins =100)+
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,0.2))+
  xlab("Water level (m, NAVD88)") +
  ylab("Frequency")+
  xlim(-0.6,1)+
  theme_bw()  +
  theme(legend.position = c(1.5,0.7),
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
FCM+scale_fill_manual(values = c("black", "seagreen", "darkorange", "royalblue"))
dev.off()
#######################################################################################
#5.FIGURE 2a and 2b: calculate and plot soil saturation index SSI=0 means hydroperid, SSI= 0.2 means fraction of tidal period over which the soil remains fully saturated at depth of SSI
##########################################################################################
#5.1 calculate on site monitored water level relative to soil surface, above zero-flooded, below zero-air exposed
#for DI, generate DI_WL_SS
#SSI<-0.2 #use this for FIGURE 2b-SSI0.2 
SSI<-0.0 #use this for FIGURE 2a-hydroperiod
DI_WL_SS<-data.frame("DateTime"=DI_WL_MSL$DateTime,
                     TC=DI_WL_MSL$TC-surface_elevation[1]+SSI,
                     H=DI_WL_MSL$H-surface_elevation[4]+SSI,
                     E=DI_WL_MSL$E-surface_elevation[2]+SSI,
                     P=DI_WL_MSL$P-surface_elevation[3]+SSI)
#for FCM, generate FCM_WL_SS_HE and FCM_WL_SS_P
FCM_WL_SS_HE<-data.frame("DateTime"=FCM_WL_MSL$DateTime,
                         H=FCM_WL_MSL$H-surface_elevation[7]+SSI,
                         E=FCM_WL_MSL$E-surface_elevation[6]+SSI)
FCM_WL_SS_P<-data.frame("DateTime"=FCM_PC$DateTime,P=FCM_PC$waterlevel-surface_elevation[9]+SSI )

#5.2 cacculate elevation derived water level relative to soil surface with tidal gauge records and elevation
bishop$DITC<-bishop$TH - surface_elevation[1]+SSI
bishop$DIH<-bishop$TH-surface_elevation[4]+SSI
bishop$DIE<-bishop$TH-surface_elevation[2]+SSI
bishop$DIP<-bishop$TH-surface_elevation[3]+SSI
bishop$FCMHC<-bishop$TH-surface_elevation[7]+SSI
bishop$FCMDC<-bishop$TH-surface_elevation[6]+SSI
bishop$FCMPC<-bishop$TH-surface_elevation[9]+SSI

#5.3. compare SSI                                                                  #
##########################################################################################
hydrpd <-function(x){
  y<-sum(x>0,na.rm=T)/sum(1-is.na(x),na.rm=T)
  return(y)
}
##in situ reading
#DI
DI_hydrpd<-as.matrix(DI_WL_SS[2:5],ncol=4)
DI_hydrpd<-apply(DI_hydrpd,2,hydrpd)
#FCM_HE
FCM_HE_hydrpd<-as.matrix(FCM_WL_SS_HE[2:3])
FCM_HE_hydrpd<-apply(FCM_HE_hydrpd,2,hydrpd)
#FCM_P
FCM_P_hydrpd<-hydrpd(FCM_WL_SS_P[2])
#Elevation derived
hydrpd_ele_drv<-as.matrix(bishop[3:9],col=7)
hydrpd_ele_drv<-apply(hydrpd_ele_drv,2,hydrpd)
hydrpd_TCNA<-subset(hydrpd,hydrpd$Zone!="TC")
#construct hydroperiod data frame
hydrpd<-data.frame(Hydroperiod=c(DI_hydrpd,FCM_HE_hydrpd,FCM_P_hydrpd,hydrpd_ele_drv),
                   Zone=c("TC","VG","DB","PD","VG","DB", "PD",
                          "TC","VG","DB","PD","VG","DB", "PD"),
                   Method=c(rep("Observed",7),rep("Estimation",7)),
                   Site=factor(rep(c(rep("DI",4),rep("FCM",3)),2),levels=unique(rep(c(rep("DI",4),rep("FCM",3)),2))))
hydrpd$SiteZone=factor(paste(hydrpd$Site,hydrpd$Zone),levels=unique(paste(hydrpd$Site,hydrpd$Zone)))
ele<-well_ele$Soil_surface_elevation.m._2019
hydrpd$Elevation<-c(ele[1],ele[4],ele[2],ele[3],ele[7],ele[6],ele[9])

#5.4 plot time inundated-eleveation curve                                                    #
##########################################################################################
library(ggrepel)
hydrpd_TCNA<-subset(hydrpd,hydrpd$Zone!="TC")
hydrpd_TC<-subset(hydrpd,hydrpd$Zone=="TC")

tiff("SSI or hydroperiod.tiff", unit="in",width = 2.3, height =2.3, res= 600,pointsize = 14 )
p<-ggplot(hydrpd_TCNA,aes(x=Elevation,y=Hydroperiod,fill=Method))+
  geom_point(aes(colour=Method,shape=Site),size=2)+
  scale_shape_manual(values=c(1, 19))+
  xlim(-0.1,0.3)+
  ylim(0.25,1.2)+
  labs(x = "Elevation (m, NAVD88)",
       y = "Soil satuation index")+
  geom_smooth(aes(colour=Method, fill=Method),method = "glm",  
              method.args = list(family = "binomial"), 
              se = FALSE)+
  geom_text_repel(aes(label=Zone),hjust=0.0,vjust=0.6, alpha=1,size=3,col="gray40")+
  theme_bw()  +
  theme(
    #legend.position = c(0.45,0.35) #legend appear at depth=0.2
    legend.position = "none",
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.5,'cm'),
    legend.background = element_rect(fill="transparent"),
    axis.line = element_line(color='black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank())
  
p+geom_point(data = hydrpd_TC, aes(x = Elevation, y = Hydroperiod, colour = Method, shape = Site), 
             size = 2, shape = 8) +
  geom_text_repel(data = hydrpd_TC, aes(x = Elevation, y = Hydroperiod, label = Zone), 
                  hjust = 0.0, vjust = 0.6, alpha = 1, size = 3, col = "gray40")

dev.off()
#get the regression equation in "inundation duration.tiff"
hydrd_insitu<-subset(hydrpd,Method=="Observed")
model1 <- lm(data=hydrd_insitu,formula=Hydroperiod*100 ~ poly(Elevation,2))
summary(model1)
hydrd_ele_drived<-subset(hydrpd,Method=="Estimated")
model2 <- lm(data=hydrd_ele_drived,formula=Hydroperiod*100 ~ poly(Elevation,2))
summary(model2)



################################################################################
#6. FIGURE 3 sensitivity to elevation loss
################################################################################

SSI<-0.0 #for FIGURE 3a- hydroperiod
#SSI<-0.2 #for for FIGURE 3b-SSI0.2
DI_WL_SS<-data.frame("DateTime"=DI_WL_MSL$DateTime,
                     H=DI_WL_MSL$H-surface_elevation[4]+SSI,
                     E=DI_WL_MSL$E-surface_elevation[2]+SSI,
                     P=DI_WL_MSL$P-surface_elevation[3]+SSI,
                     H0.7=DI_WL_MSL$H-0.7+SSI,
                     H0.65=DI_WL_MSL$H-0.65+SSI,
                     H0.6=DI_WL_MSL$H-0.5+SSI,
                     H0.55=DI_WL_MSL$H-0.55+SSI,
                     H0.5=DI_WL_MSL$H-0.5+SSI,
                     H0.45=DI_WL_MSL$H-0.45+SSI,
                     H0.4=DI_WL_MSL$H-0.4+SSI,
                     H0.35=DI_WL_MSL$H-0.35+SSI,
                     H0.3=DI_WL_MSL$H-0.3+SSI,
                     H0.25=DI_WL_MSL$H-0.25+SSI,
                     H0.2=DI_WL_MSL$H-0.2+SSI,
                     H0.15=DI_WL_MSL$H-0.15+SSI,
                     H0.1=DI_WL_MSL$H-0.1+SSI,
                     H0.05=DI_WL_MSL$H-0.05+SSI,
                     H0.0=DI_WL_MSL$H-0.0+SSI)
                     
#5.2 cacculate elevation drived water level relative to soil surface with tidal gauge records and elevation
bishop$DIH<-bishop$TH-surface_elevation[4]+SSI
bishop$DIE<-bishop$TH-surface_elevation[2]+SSI
bishop$DIP<-bishop$TH-surface_elevation[3]+SSI
bishop$H0.7<-bishop$TH-0.7+SSI
bishop$H0.65<-bishop$TH-0.65+SSI
bishop$H0.6<-bishop$TH-0.6+SSI
bishop$H0.55<-bishop$TH-0.55+SSI
bishop$H0.5<-bishop$TH-0.5+SSI
bishop$H0.45<-bishop$TH-0.45+SSI
bishop$H0.4<-bishop$TH-0.4+SSI
bishop$H0.35<-bishop$TH-0.35+SSI
bishop$H0.3<-bishop$TH-0.3+SSI
bishop$H0.25<-bishop$TH-0.25+SSI
bishop$H0.2<-bishop$TH-0.2+SSI
bishop$H0.15<-bishop$TH-0.15+SSI
bishop$H0.1<-bishop$TH-0.1+SSI
bishop$H0.05<-bishop$TH-0.05+SSI
bishop$H0.0<-bishop$TH-0.0+SSI

#5.3. compare hydroperiod                                                                  #
##########################################################################################
hydrpd <-function(x){
  y<-sum(x>0,na.rm=T)/sum(1-is.na(x),na.rm=T)
  return(y)
}
##in situ reading
#DI
DI_hydrpd<-as.matrix(DI_WL_SS[2:19],ncol=18)
DI_hydrpd<-apply(DI_hydrpd,2,hydrpd)

#Elevation derived
hydrpd_ele_drv<-as.matrix(bishop[,c(4:6,10:24)],col=18)
hydrpd_ele_drv<-apply(hydrpd_ele_drv,2,hydrpd)

#construct hydroperiod data frame
hydrpd_DI<-data.frame(Hydroperiod=c(DI_hydrpd,hydrpd_ele_drv),
                   Zone=rep(c("VG","DB","PD","H0.7","H0.65","H0.6","H0.55","H0.5","H0.45","H0.4","H0.35","H0.3","H0.25","H0.2","H0.15","H0.1","H0.05","H0.00"),2),
                   Section=c(rep("Interior marsh",18),rep("Creek bank",18)),
                   Site=factor(c(rep("Observed",3),rep("Estimated",33)),levels=unique(c(rep("Monitored",3),rep("Simulated",15)))))


ele<-well_ele$Soil_surface_elevation.m._2019
hydrpd_DI$Elevation<-rep(c(ele[4],ele[2],ele[3],0.7,0.65,0.6,0.55,0.5,0.45,0.4,0.35,0.3,0.25,0.2,0.15,0.1,0.05,0.0),2)
library(ggrepel)
library(ggplot2)
tiff("Fig. 7-DI-SS0.0.tiff", unit="in",width = 3, height =3, res= 600,pointsize = 14 )
ggplot(hydrpd_DI,aes(x=Elevation,y=Hydroperiod,fill=Section))+
  geom_point(aes(colour=Section,shape=Site),size=2)+
  scale_shape_manual(values=c(8,19))+
  xlim(0,0.7)+
  ylim(0,1.05)+
  labs(x = "Elevation (m, NAVD88)",
       y = "Hydroperiod")+
       #y = "Soil saturation index")+
  geom_smooth(aes(colour=Section, fill=Section),method = "glm",  
              method.args = list(family = "binomial"), 
              se = FALSE) +
  #geom_text_repel(aes(label=Zone),hjust=0.0,vjust=0.6, alpha=1,size=3,col="gray40")+
  theme_bw()  +
  theme(
    #legend.position = c(0.7,0.8), #legend appear at depth=0.2
    legend.position = "none",
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.5,'cm'),
    legend.background = element_rect(fill="transparent"),
    axis.line = element_line(color='black'),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank())
dev.off()

############################################################################################
#NOT USED-plot elevation devired result and insitu observation-with tidal creek data
tiff("elevation derived vs in situ observation with tidal creek.tiff", unit="in",width = 3.5, height =3.5, res= 600,pointsize = 9 )
par(mar=c(5,5,1,1))
insitu<-subset(hydrpd,Method=="Observed")
ed<-subset(hydrpd,Method=="Estimated")
data<-data.frame(x=insitu$Hydroperiod, y=ed$Hydroperiod,SiteZone=ed$SiteZone)
ggplot(data,aes(x=x*100,y=y*100))+
  geom_point(aes(x=x*100,y=y*100),shape=19,size=2)+
  geom_smooth(aes(),method = "lm", formula = y ~ poly(x, 2))+
  geom_text_repel(aes(label=SiteZone),hjust=-0.5,vjust=0, alpha=1,size=3)+
  labs(x = "Observed",y = "Estimated")+
  xlim(0,100)+
  ylim(0,100)+
  theme_bw()  +
  theme(legend.position = c(0.2,1),
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
dev.off()

#NOT_USED plot elevation devired result and insitu observation-with tidal creek data
tiff("elevation derived vs in situ observation without tidal creek.tiff", unit="in",width = 3.5, height =3.5, res= 600,pointsize = 9 )
par(mar=c(5,5,1,1))
insitu<-subset(hydrpd,Method=="Observed")
ed<-subset(hydrpd,Method=="Estimated")
data<-data.frame(x=insitu$Hydroperiod, y=ed$Hydroperiod,SiteZone=ed$SiteZone)
data<-data[-1,]
ggplot(data,aes(x=x*100,y=y*100))+
  geom_point(aes(x=x*100,y=y*100),shape=19,size=2)+
  geom_smooth(aes(),method = "lm", formula = y ~ x)+
  geom_text_repel(aes(label=SiteZone),hjust=-0.5,vjust=0, alpha=1,size=3)+
  labs(x = "Observed",y = "Estimated")+
  xlim(0,100)+
  ylim(0,100)+
  theme_bw()  +
  theme(legend.position = c(0.2,0.20),
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
dev.off()
#NOT USED get the regression equation in "elevation derived vs Observed without tidal creek.tiff"
model3 <- lm(data=data,formula=y ~ x)
summary(model3)


##########################################################################################

#6. keep time series with a 1hr interval
#############################################################################################
# extract hourly date series from DI and FCM
Time_DI<-intersect(DI_SD$DateTime,bishop$DateTime)
Time_FCM_HE<-intersect(FCM_SD$DateTime,bishop$DateTime)
Time_FCM_P<-intersect(FCM_WL_SS_P$DateTime,bishop$DateTime)


DI_WL_MSL_hr<-subset(DI_WL_MSL,is.element(DateTime,Time_DI))
DI_WL_SS_hr<-subset(DI_WL_SS,is.element(DateTime,Time_DI))
FCM_WL_MSL_HE_hr<-subset(FCM_WL_MSL,is.element(DateTime,Time_FCM_HE))
FCM_WL_SS_HE_hr<-subset(FCM_WL_SS_HE,is.element(DateTime,Time_FCM_HE))
FCM_WL_MSL_P_hr<-subset(FCM_PC,is.element(DateTime,Time_FCM_P))
FCM_WL_SS_P_hr<-subset(FCM_WL_SS_P,is.element(DateTime,Time_FCM_P))

#9. flood events duration                                                                #
##########################################################################################
#construct a function to recognize continious time and the length of continious time
FD<-function(fld_date) {
  Fld_event<-NULL
  i=1
  while (i<length(fld_date)) {
    str_time<-fld_date[i]
    
    for (j in 1:((length(fld_date)-i))) {
      if (i+j>length(fld_date)) {
        end_time<-fld_date[i+j-1]
        break}
      else if (fld_date[i+j]-fld_date[i]==j) {
        next}
      else {
        end_time<-fld_date[i+j-1]
        break
      }
    }
    
    if (str_time==end_time) {
      i<-i+1
    } 
    else {
      Fld_event<-rbind(Fld_event,data.frame("Begin time"=str_time,"End time"=end_time,"Duration (hr)"=end_time-str_time+1))
      i<-i+j}
  }
  if (!is.null(Fld_event)) {
    colnames(Fld_event)<- c("Begin time", "End time", "Duration (hr)")  
  } 
  Fld_event
}
#########################################################
#9.1 calculate flood event duration from in situ reading#                  
#########################################################
#DI_TC
SRC_data<-DI_WL_SS_hr
fld_date<-SRC_data$DateTime[which(SRC_data$TC>=0)] 
FD_DI_TC<-FD(fld_date)
FD_DI_TC$Zone<-"Tidal creek"
FD_DI_TC$Site<-"DI"
FD_DI_TC$Method<-"Observed"

#DI_H
SRC_data<-DI_WL_SS_hr
fld_date<-SRC_data$DateTime[which(SRC_data$H>=0)] 
FD_DI_H<-FD(fld_date)
FD_DI_H$Zone<-"Vegetated zone"
FD_DI_H$Site<-"DI"
FD_DI_H$Method<-"Observed"

#DI_E
SRC_data<-DI_WL_SS_hr
fld_date<-SRC_data$DateTime[which(SRC_data$E>=0)] 
FD_DI_E<-FD(fld_date)
FD_DI_E$Zone<-"Dieback zone"
FD_DI_E$Site<-"DI"
FD_DI_E$Method<-"Observed"

#DI_P
SRC_data<-DI_WL_SS_hr
fld_date<-SRC_data$DateTime[which(SRC_data$P>=0)] 
FD_DI_P<-FD(fld_date)
FD_DI_P$Zone<-"Pond"
FD_DI_P$Site<-"DI"
FD_DI_P$Method<-"Observed"

#FCM_H or FCM_HC
SRC_data<-FCM_WL_SS_HE_hr
fld_date<-SRC_data$DateTime[which(SRC_data$H>=0)] 
FD_FCM_HC<-FD(fld_date)
FD_FCM_HC$Zone<-"Vegetated zone"
FD_FCM_HC$Site<-"FCM"
FD_FCM_HC$Method<-"Observed"

#FCM_E or FCM_DC
SRC_data<-FCM_WL_SS_HE_hr
fld_date<-SRC_data$DateTime[which(SRC_data$E>=0)] 
FD_FCM_DC<-FD(fld_date)
FD_FCM_DC$Zone<-"Dieback zone"
FD_FCM_DC$Site<-"FCM"
FD_FCM_DC$Method<-"Observed"
#FCM_P
SRC_data<-FCM_WL_SS_P_hr
fld_date<-SRC_data$DateTime[which(SRC_data$P>=0)] 
FD_FCM_P<-FD(fld_date)
FD_FCM_P$Zone<-"Pond"
FD_FCM_P$Site<-"FCM"
FD_FCM_P$Method<-"Observed"
#merge 
FD_insitu<-rbind(FD_DI_TC,FD_DI_H,FD_DI_E,FD_DI_P,FD_FCM_HC,FD_FCM_DC,FD_FCM_P)
FD_insitu$Site<-factor(FD_insitu$Site,levels=unique(FD_insitu$Site))
FD_insitu$Zone<-factor(FD_insitu$Zone,levels=unique(FD_insitu$Zone))
FD_insitu$Method<-factor(FD_insitu$Method,levels=unique(FD_insitu$Method))


################################################################
#9.3 calculate flood event duration from elevation derived data#
################################################################
#bishop_DITC
fld_date<-bishop$DateTime[which(bishop$DITC>=0)] 
FD_bishop_DITC<-FD(fld_date)
FD_bishop_DITC$Site<-"DI"
FD_bishop_DITC$Zone<-"Tidal creek"
FD_bishop_DITC$Method<-"Estimation"

#bishop_DIH

fld_date<-bishop$DateTime[which(bishop$DIH>=0)] 
FD_bishop_DIH<-FD(fld_date)
FD_bishop_DIH$Site<-"DI"
FD_bishop_DIH$Zone<-"Vegetated zone"
FD_bishop_DIH$Method<-"Estimation"

#bishop_DIE
fld_date<-bishop$DateTime[which(bishop$DIE>=0)] 
FD_bishop_DIE<-FD(fld_date)
FD_bishop_DIE$Site<-"DI"
FD_bishop_DIE$Zone<-"Dieback zone"
FD_bishop_DIE$Method<-"Estimation"


#bishop_DIP
fld_date<-bishop$DateTime[which(bishop$DIP>=0)] 
FD_bishop_DIP<-FD(fld_date)
FD_bishop_DIP$Site<-"DI"
FD_bishop_DIP$Zone<-"Pond"
FD_bishop_DIP$Method<-"Estimation"

#bishop_FCMHC
fld_date<-bishop$DateTime[which(bishop$FCMHC>=0)] 
FD_bishop_FCMHC<-FD(fld_date)
FD_bishop_FCMHC$Site<-"FCM"
FD_bishop_FCMHC$Zone<-"Vegetated zone"
FD_bishop_FCMHC$Method<-"Estimation"

#bishop_FCMDC
fld_date<-bishop$DateTime[which(bishop$FCMDC>=0)] 
FD_bishop_FCMDC<-FD(fld_date)
FD_bishop_FCMDC$Site<-"FCM"
FD_bishop_FCMDC$Zone<-"Dieback zone"
FD_bishop_FCMDC$Method<-"Estimation"

#bishop FCMP
fld_date<-bishop$DateTime[which(bishop$FCMPC>=0)] 
FD_bishop_FCMP<-FD(fld_date)
FD_bishop_FCMP$Site<-"FCM"
FD_bishop_FCMP$Zone<-"Pond"
FD_bishop_FCMP$Method<-"Estimation"

FD_derived<-rbind(FD_bishop_DITC,FD_bishop_DIH,FD_bishop_DIE,FD_bishop_DIP,FD_bishop_FCMHC,FD_bishop_FCMDC,FD_bishop_FCMP)
FD_derived$Site<-factor(FD_derived$Site,levels=unique(FD_derived$Site))
FD_derived$Zone<-factor(FD_derived$Zone,levels=unique(FD_derived$Zone))
FD_derived$Method<-factor(FD_derived$Method,levels=unique(FD_derived$Method))

FD<-rbind(FD_insitu,FD_derived)
FD$Zone<-factor(FD$Zone,levels=unique(FD$Zone))
FD<-subset(FD,Zone!="Tidal creek")
############################################################################
#9.2 plot histgram of inundation duration with in situ groundwater readings#
############################################################################
tiff("inundation duration with in situ groundwater readings 2019-2020.tiff",unit="in",width = 6, height =4, res= 600,pointsize = 14 )
library(plyr)
mu <- ddply(FD, c("Zone","Site","Method"), summarise, grp.mean=mean(`Duration (hr)`))

p<-ggplot(FD, aes(x=`Duration (hr)`, color=Zone, fill=Zone)) +
  #ylim(0,1)+
  #xlim(0,24)+
  geom_histogram(aes(y= stat(width*density)),binwidth = 1,position="identity",alpha=0.5,bins=100)+ #position = "identity", alpha = 0.6, bins =100
  scale_y_continuous(labels = scales::percent_format(),limits = c(0,1.00))+
  geom_density(alpha=0.2)+
  xlim(0,24.5)+
  #geom_vline(data=mu, aes(xintercept=grp.mean, color=Zone),
  #           linetype="dashed")+# Add mean lines
  
  labs(x = "Daily inundation duration (hour)",
       y = "Frequency") +
  theme_bw()  +
  theme(panel.grid=element_blank(),
        plot.title = element_text(size = rel(1.5),
                                  face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        legend.direction = "horizontal", 
        legend.background = element_rect(fill = NA),
        legend.position =c(0.20, 0.9),
        legend.title = element_blank(),
        legend.key.size = unit(0.3, "cm"),
        #legend.key = element_rect(fill = "black"),
        axis.title.y = element_text(vjust= 1.8),
        axis.title.x = element_text(vjust= -0.5))+
  guides(fill=guide_legend(ncol=2))

p+facet_grid(rows=vars(Site),cols = vars(Method))+
  scale_fill_manual(values=c("seagreen", "darkorange", "royalblue"))+
  scale_color_manual(values=c("seagreen", "darkorange", "royalblue"))
  
dev.off()


#<below was not used in  the paper>

















####################################################################
#NOT USED 9.5 calculate drought event duration from in situ reading#                  
####################################################################
#DI_TC
SRC_data<-Ovlap_DI_TC
fld_date<-SRC_data$`Time, GMT-04:00`[which(SRC_data$`Water level(m)`<0)] 
DD_DI_TC<-FD(fld_date)
DD_DI_TC$Site<-"DI Tidal Creek"
#DI_H
SRC_data<-Ovlap_DI_H
fld_date<-SRC_data$`Time, GMT-04:00`[which(SRC_data$`Water level(m)`<0)] 
DD_DI_H<-FD(fld_date)
DD_DI_H$Site<-"DI Vegetated marsh"
#DI_E
SRC_data<-Ovlap_DI_E
fld_date<-SRC_data$`Time, GMT-04:00`[which(SRC_data$`Water level(m)`<0)] 
DD_DI_E<-FD(fld_date)
DD_DI_E$Site<-"DI Dieback patch"
#DI_P
SRC_data<-Ovlap_DI_P
fld_date<-SRC_data$`Time, GMT-04:00`[which(SRC_data$`Water level(m)`<0)] 
DD_DI_P<-FD(fld_date)
DD_DI_P$Site<-"DI Pond"
#FCM_HC
SRC_data<-Ovlap_FCM_HC
fld_date<-SRC_data$`Time, GMT-04:00`[which(SRC_data$`Water level(m)`<0)] 
DD_FCM_HC<-FD(fld_date)
DD_FCM_HC$Site<-"FCM Vegetated marsh"
#FCM_DC
SRC_data<-Ovlap_FCM_DC
fld_date<-SRC_data$`Time, GMT-04:00`[which(SRC_data$`Water level(m)`<0)] 
DD_FCM_DC<-FD(fld_date)
DD_FCM_DC$Site<-"FCM Dieback patch"
#since DI_E and DI_P has no droungt events, so for the integraty of data, construct two data frame representing them
DD_DI_E<-DD_DI_TC[1,]
DD_DI_E$`Begin time`<-NA
DD_DI_E$`End time`<-NA
DD_DI_E$`Duration (hr)`<-0
DD_DI_E$Site<-"DI Dieback patch"
DD_DI_P<-DD_DI_E
DD_DI_P$Site<-"DI Pond"
DD_insitu<-rbind(DD_DI_TC,DD_DI_H,
                 DD_DI_E,DD_DI_P,
                 DD_FCM_HC,DD_FCM_DC)
DD_insitu$Site<-factor(DD_insitu$Site,levels=unique(DD_insitu$Site))
############################################################################
#NOT USED 9.6 plot histgram of drought duration with in situ groundwater readings#
############################################################################
tiff("Drought duration with in situ groundwater readings.tiff",unit="in",width = 6, height =4, res= 600,pointsize = 14 )
library(plyr)
mu <- ddply(DD_insitu, "Site", summarise, grp.mean=mean(`Duration (hr)`))

p<-ggplot(DD_insitu, aes(x=`Duration (hr)`, color=Site, fill=Site)) +
  ylim(0,125)+
  xlim(0,25)+
  geom_histogram(binwidth = 0.5,position="dodge",alpha=0.5)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Site),
             linetype="dashed")+# Add mean lines
  labs(x = "Drought duration (hr)",
       y = "Number of drought events") +
  theme_bw()  +
  theme(panel.grid=element_blank(),
        plot.title = element_text(size = rel(1.5),
                                  face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        legend.direction = "horizontal", 
        legend.background = element_rect(fill = NA),
        legend.position =c(0.7, 0.9),
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        #legend.key = element_rect(fill = "black"),
        axis.title.y = element_text(vjust= 1.8),
        axis.title.x = element_text(vjust= -0.5)
  )
p
dev.off()
############################################################################
#NOT USED 9.7 calculate drought event duration from elevation derived data#
############################################################################
#bishop_DITC
SRC_data<-Ovlap_bishop$DITC
fld_date<-Ovlap_bishop$Time_GMT_4[which(SRC_data<0)] 
DD_bishop_DITC<-FD(fld_date)
DD_bishop_DITC$Site<-"DI tidal creek"
#bishop_DIH
SRC_data<-Ovlap_bishop$DIH
fld_date<-Ovlap_bishop$Time_GMT_4[which(SRC_data<0)] 
DD_bishop_DIH<-FD(fld_date)
DD_bishop_DIH$Site<-"DI Vegetated marsh"
#bishop_DIE
SRC_data<-Ovlap_bishop$DIE
fld_date<-Ovlap_bishop$Time_GMT_4[which(SRC_data<0)] 
DD_bishop_DIE<-FD(fld_date)
DD_bishop_DIE$Site<-"DI Dieback patch"
#bishop_DIP
SRC_data<-Ovlap_bishop$DIP
fld_date<-Ovlap_bishop$Time_GMT_4[which(SRC_data<0)] 
DD_bishop_DIP<-FD(fld_date)
DD_bishop_DIP$Site<-"DI Pond"

#bishop_FCMHC
SRC_data<-Ovlap_bishop$FCMHC
fld_date<-Ovlap_bishop$Time_GMT_4[which(SRC_data<0)] 
DD_bishop_FCMHC<-FD(fld_date)
DD_bishop_FCMHC$Site<-"FCM Vegetated marsh"
#bishop_FCMDC
SRC_data<-Ovlap_bishop$FCMDC
fld_date<-Ovlap_bishop$Time_GMT_4[which(SRC_data<0)] 
DD_bishop_FCMDC<-FD(fld_date)
DD_bishop_FCMDC$Site<-"FCM Dieback patch"
DD_bishop<-rbind(DD_bishop_DITC,DD_bishop_DIH,DD_bishop_DIE,DD_bishop_DIP,DD_bishop_FCMHC,DD_bishop_FCMDC)
DD_bishop$Site<-factor(DD_bishop$Site,levels=unique(DD_bishop$Site))
############################################################################
#NOT USED 9.8 plot histgram of drought duration with elevation derived groundwater#
############################################################################

tiff("Drought duration with elevation derived groundwater.tiff",unit="in",width = 6, height =4, res= 600,pointsize = 14 )
library(plyr)
library(ggplot2)
mu <- ddply(DD_bishop, "Site", summarise, grp.mean=mean(`Duration (hr)`))

p<-ggplot(DD_bishop, aes(x=`Duration (hr)`, color=Site, fill=Site)) +
  ylim(0,125)+
  xlim(0,25)+
  geom_histogram(binwidth = 0.5,position="dodge",alpha=0.5)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Site),
             linetype="dashed")+# Add mean lines
  labs(x = "drought duration (hr)",
       y = "Number of drought events") +
  theme_bw()  +
  theme(panel.grid=element_blank(),
        plot.title = element_text(size = rel(1.5),
                                  face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        legend.direction = "horizontal", 
        legend.background = element_rect(fill = NA),
        legend.position =c(0.7, 0.9),
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        #legend.key = element_rect(fill = "black"),
        axis.title.y = element_text(vjust= 1.8),
        axis.title.x = element_text(vjust= -0.5)
  )
p
dev.off()




