#processing data extracted from papers

rm(list=ls())
setwd(here::here())

data<-readxl::read_excel("Data/flood tolerance extracted from papers.xls")

data$Species<-factor(data$Species)
data$Index<-factor(data$Index)
data$Site<-factor(data$Site)
data$Source<-factor(data$Source)


normlze<-function(x) {
  mindata<-min(x$Value)
  maxdata<-max(x$Value)
  x$nm<-(x$Value-mindata)/(maxdata-mindata)
  return(x)
}
#here plants of the same species from the same region (same source of paper but might be from different sites)
data_nm<-NULL
for (i in levels(data$Species)) {
  for (j in levels(data$Index)) {
    for (k in levels(data$Source)) {
      data_x<-data[data$Species==i & data$Index==j & data$Source==k,]
      data_nm<-rbind(data_nm, normlze(data_x))
    }
  }
}

library(stringr)
data_nm$organ<-word(data_nm$Index,1)
data_nm$organ<-factor(data_nm$organ)

library(ggplot2)
data_nm$ele_dr_inundation<-data_nm$Inundation*0.5526+3.45 #this parameter were acquired from model 3 at line 338 of "comparison with bishop water gauge
#plot the response of aboveground-belowground biomass of marsh species to inundation from different regions/studies  
tiff("Result/Fig. S4-above and below- ground response to inudnation in different studies.tiff",  unit="in",width =5, height =5, res= 600,pointsize = 14)
P<-ggplot(data_nm, aes(x=Inundation, y=nm),fill=Source)+
  geom_point(aes(x=Inundation, y=nm,fill=Source,colour=Source), shape=3, size=0.5)+
  geom_smooth(aes(colour=Source, fill=Source),method = "lm", formula = y ~ poly(x, 2))+
  xlab("Inundation duration")+
  ylab("Performance")
P+facet_grid(rows = vars(Species),scales="free",cols=vars(organ))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

#plot the response marsh species to inundation  (merge above- and below- ground biomass response and studies from different regions)
tiff("Result/response to inudnation in DI.tiff",  unit="in",width =2.5, height =5, res= 600,pointsize = 14)
P<-ggplot(data_nm, aes(x=Inundation, y=nm),fill=Species)+
  #geom_rect(aes(xmin=34.18734, xmax=65.0834, ymin=0,ymax=1), alpha=0.01, fill="seagreen")+
  geom_vline(xintercept=38.48448, col="forestgreen", linetype=2, size=1)+ #for DI vegetated estimated
  geom_vline(xintercept=60.659, col="forestgreen", linetype=1, size=1)+ #for DI vegetated in situ
  geom_vline(xintercept=49.369, col="darkorange", linetype=2, size=1)+ #for DI dieback estimated
  geom_vline(xintercept=99.0, col="darkorange", linetype=1, size=1)+ #for DI dieback in situ
  geom_vline(xintercept=67.12756, col="royalblue", linetype=2, size=1)+ #for DI pond estimated
  geom_vline(xintercept=99.98686, col="royalblue", linetype=1, size=1)+ #for DI pond in situ
  geom_point(aes(x=Inundation, y=nm), shape=1, size=1,col="gray")+
  geom_smooth(aes(group=Species),col="gray40", method = "lm", formula = y ~ poly(x, 2))+
  xlab("Time inundated (%)")+
  ylab("Performance")
  
P+facet_grid(rows = vars(Species),scales="free")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

tiff("Result/response to inudnation in FCM.tiff",  unit="in",width =2.5, height =5, res= 600,pointsize = 14)
P<-ggplot(data_nm, aes(x=Inundation, y=nm),fill=Species)+
  #geom_rect(aes(xmin=34.18734, xmax=65.0834, ymin=0,ymax=1), alpha=0.01, fill="seagreen")+
  geom_vline(xintercept=34.18734, col="forestgreen", linetype=2, size=1)+ #for FCM vegetated estimated
  geom_vline(xintercept=65.0834, col="forestgreen", linetype=1, size=1)+ #for FCM vegetated in situ
  geom_vline(xintercept=35.89535, col="darkorange", linetype=2, size=1)+ #for FCM dieback estimated
  geom_vline(xintercept=70.75392, col="darkorange", linetype=1, size=1)+ #for FCM dieback in situ
  geom_vline(xintercept=60.78352, col="royalblue", linetype=2, size=1)+ #for FCM pond estimated
  geom_vline(xintercept=100.000, col="royalblue", linetype=1, size=1)+ #for FCM pond in situ
  geom_point(aes(x=Inundation, y=nm), shape=1, size=1,col="gray")+
  geom_smooth(aes(colour=Species),col="gray40", method = "lm", formula = y ~ poly(x, 2))+
  xlab("Time inundated (%)")+
  ylab("Performance")

P+facet_grid(rows = vars(Species),scales="free")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

#NOT USED validated inundation tolerance line
data_nm$vldt_inundation<-3.45+0.5526*data_nm$Inundation #the parameter were acquired from 'model 3' at line 338 of 'comparision with bishop water gauge'
tiff("Result/validated response to inudnation.tiff",  unit="in",width =4, height =5, res= 600,pointsize = 14)
P<-ggplot(data_nm,fill=Species)+
  geom_point(aes(x=Inundation, y=nm,fill=Species,colour=Species), shape=1, size=1)+
  geom_smooth(aes(x=Inundation, y=nm, colour=Species, fill=Species),method = "lm", formula = y ~ poly(x, 2))+
  geom_smooth(aes(x=vldt_inundation, y=nm), se = FALSE,color="Black",span = 0.3,method = "lm",  formula = y ~ poly(x, 2))+
  scale_linetype_manual(values=c("dashed"))+
  xlab("Inundation duration")+
  ylab("Performance")
P+facet_grid(rows = vars(Species),scales="free")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

dev.off()
#==========================================
#NOT USED get the regression equation in "response to inudnation.tiff"
data_nm_Juncus<-subset(data_nm,Species=="J. roemerianus")
model_Juncus <- lm(data=data_nm_Juncus,formula=nm ~ poly(Inundation,2))
summary(model_Juncus)

data_nm_alterniflora<-subset(data_nm,Species=="S. alterniflora")
model_alterniflora <- lm(data=data_nm_alterniflora,formula=nm ~ poly(Inundation,2))
summary(model_alterniflora)

data_nm_americanus<-subset(data_nm,Species=="S. americanus")
model_americanus <- lm(data=data_nm_americanus,formula=nm ~ poly(Inundation,2))
summary(model_americanus)

data_nm_patens<-subset(data_nm,Species=="S. patens")
model_patens <- lm(data=data_nm_patens,formula=nm ~ poly(Inundation,2))
summary(model_patens)

