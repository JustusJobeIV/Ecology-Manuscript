#### Biomass Analysis 
library(nlme)
library(car)
library(reshape2)
library(dplyr)
#sub setting by species
Phrag<- filter(Blackwater_Project_Growth_and_Biomass, spp=="Pa")
Panic<- filter(Blackwater_Project_Growth_and_Biomass, spp=="Pv")
Iva<- filter(Blackwater_Project_Growth_and_Biomass, spp=="If")
Bac<- filter(Blackwater_Project_Growth_and_Biomass, spp=="Bh")

###Fix the Labels of M and F to Marsh and Forest and change the column to Habitat not site 
Iva$Habitat<-ifelse(Iva$Site=="M", c("Marsh"),c("Forest"))
Bac$Habitat<-ifelse(Bac$Site=="M", c("Marsh"),c("Forest"))
Phrag$Habitat<-ifelse(Phrag$Site=="M", c("Marsh"),c("Forest"))
Panic$Habitat<-ifelse(Panic$Site=="M", c("Marsh"),c("Forest"))

### Site<- Habitat in which the plant occured (Forest or Marsh)
### Treatment <- Caging Treatment 
### Number<- Is the plot/site in which the 3 caging treatments occured

##Linear mixed effects model on Phragmites 
shapiro.test(Phrag$Bmass)
leveneTest(Bmass~Habitat*Treatment, data=Phrag)
Phrag_mlm<-lme(fixed=Bmass ~ Habitat*Treatment, data=Phrag, random = ~1|Number)
summary(Phrag_mlm)
anova(Phrag_mlm)

## Graphing on Phragmites
Phrag_2 = subset(Phrag, select = c("Habitat", "Treatment", "Bmass"))
str(na.omit(Phrag_2))
PhragB = melt(na.omit(Phrag_2), id.vars = c("Habitat", "Treatment"), measure.vars = "Bmass")
PhragB_N=acast(PhragB, Treatment~Habitat, length)
PhragB_mean = acast(PhragB, Treatment ~ Habitat, mean)
PhragB_sd = acast(PhragB, Treatment ~ Habitat, sd)
PhragB_se<- PhragB_sd/sqrt(PhragB_N)
bp3 = barplot(PhragB_mean[c(3,1,2),], beside = T, col = c("grey20","grey46","grey80"), ylim=c(0,60), main = bquote(paste("Biomass of ",italic("Phragmites australis"))), ylab="Biomass (g)", las=1)
arrows(bp1, PhragB_mean[c(3,1,2),], bp1, PhragB_mean[c(3,1,2),] + PhragB_se[c(3,1,2),], angle = 90, length = 0.05)
legend("topleft", legend=c("Control","Cage Control","Cage"),fill=c("grey20","grey46","grey80"))

##Linear mixed effects model on Panicum
shapiro.test(Panic$Bmass)
leveneTest(Bmass~Habitat*Treatment, data=Panic)
Panic_mlm<-lme(fixed=Bmass ~ Habitat*Treatment, data=Panic, random = ~1|Number)
summary(Panic_mlm)
anova(Panic_mlm)

## Graphing on Panicum
Panic_2 = subset(Panic, select = c("Habitat", "Treatment", "Bmass"))
str(na.omit(Panic_2))
PanicB = melt(na.omit(Panic_2), id.vars = c("Habitat", "Treatment"), measure.vars = "Bmass")
PanicB_N=acast(PanicB, Treatment~Habitat, length)
PanicB_mean = acast(PanicB, Treatment ~ Habitat, mean)
PanicB_sd = acast(PanicB, Treatment ~ Habitat, sd)
PanicB_se<- PanicB_sd/sqrt(PanicB_N)
bp4 = barplot(PanicB_mean[c(3,1,2),], beside = T, col = c("grey20","grey46","grey80"), ylim=c(0,60), main =bquote(paste("Biomass of ",italic("Panicum virgatum"))), ylab="Biomass (g)", las=1)
arrows(bp1, PanicB_mean[c(3,1,2),], bp1, PanicB_mean[c(3,1,2),] + PanicB_se[c(3,1,2),], angle = 90, length = 0.05)
legend("topright", legend=c("Control","Cage Control","Cage"),fill=c("grey20","grey46","grey80"))

##Linear mixed effects model on Iva
shapiro.test(Iva$Bmass)
leveneTest(Bmass~Habitat*Treatment, data=Iva)
Iva_mlm<-lme(fixed=Bmass ~ Habitat*Treatment, data=Iva, random = ~1|Number)
summary(Iva_mlm)
anova(Iva_mlm)

## Graphing on Iva
Iva_2 = subset(Iva, select = c("Habitat", "Treatment", "Bmass"))
str(na.omit(Iva_2))
IvaB = melt(na.omit(Iva_2), id.vars = c("Habitat", "Treatment"), measure.vars = "Bmass")
IvaB_N=acast(IvaB, Treatment~Habitat, length)
IvaB_mean = acast(IvaB, Treatment ~ Habitat, mean)
IvaB_sd = acast(IvaB, Treatment ~ Habitat, sd)
IvaB_se<- IvaB_sd/sqrt(IvaB_N)
bp2 = barplot(IvaB_mean[c(3,1,2),], beside = T, col = c("grey20","grey46","grey80"), ylim=c(0,60), main = bquote(paste("Biomass of ",italic("Iva fructescens"))), ylab="Biomass (g)", las=1)
arrows(bp1, IvaB_mean[c(3,1,2),], bp1, IvaB_mean[c(3,1,2),] + IvaB_se[c(3,1,2),], angle = 90, length = 0.05)
legend("topleft", legend=c("Control","Cage Control","Cage"),fill=c("grey20","grey46","grey80"))

##Linear mixed effects model on Baccharis
shapiro.test(Bac$Bmass)
leveneTest(Bmass~Habitat*Treatment, data=Bac)
Bmlm<-lme(fixed=Bmass ~ Habitat*Treatment, data=Bac, random = ~1|Number)
summary(Bmlm)
anova(Bmlm)

## Graphing on Baccharis
Bac_2 = subset(Bac, select = c("Habitat", "Treatment", "Bmass"))
str(na.omit(Bac_2))
BacB = melt(na.omit(Bac_2), id.vars = c("Habitat", "Treatment"), measure.vars = "Bmass")
BacB_N=acast(BacB, Treatment~Habitat, length)
BacB_mean = acast(BacB, Treatment ~ Habitat, mean)
BacB_sd = acast(IvaB, Treatment ~ Habitat, sd)
BacB_se<- BacB_sd/sqrt(BacB_N)
bp1 = barplot(BacB_mean[c(3,1,2),], beside = T, col = c("grey20","grey46","grey80"), ylim=c(0,10), main = bquote(paste("Biomass of ",italic("Baccharis halimifolia"))), ylab="Biomass (g)", las=1)
arrows(bp1, BacB_mean[c(3,1,2),], bp1, BacB_mean[c(3,1,2),] + BacB_se[c(3,1,2),], angle = 90, length = 0.05)
legend("topright", legend=c("Control","Cage Control","Cage"),fill=c("grey20","grey46","grey80"))

### Panels
par(mfrow=c(2,2))
par(cex = 0.6)
par(mar = c(1, 0.7, 2, 0.7), oma = c(2, 4, 1, 1))
par(mai=c(0.1,0.1,0.1,0.1))
par(cex.main=3) ### Main Title Text Size
par(cex.axis=2.1)
##Panel top-left
par(mai=c(0.1,0.2,0.11,0.1))
bp1 = barplot(BacB_mean[c(3,1,2),], beside = T, col = c("grey20", "grey46", "grey80"), ylim=c(0,60),las=1,xaxt="n")
title(main= bquote(paste(italic("Baccharis halimifolia"))),line=-2.5)
arrows(bp1, BacB_mean[c(3,1,2),], bp1, BacB_mean[c(3,1,2),] + BacB_se[c(3,1,2),], angle = 90, length = 0.05)
legend("left", cex=3,legend=c("Control","Cage Control","Cage"),fill=c("grey20","grey46","grey80"),bty="n")
##Panel top-right
par(mai=c(0.1,0.1,0.11,0.1))
bp2 = barplot(IvaB_mean[c(3,1,2),], beside = T, col = c("grey20", "grey46", "grey80"), ylim=c(0,60), las=1, xaxt="n",yaxt="n")
title(main = bquote(paste(italic("Iva fructescens"))),line=-2.5)
arrows(bp1, IvaB_mean[c(3,1,2),], bp1, IvaB_mean[c(3,1,2),] + IvaB_se[c(3,1,2),], angle = 90, length = 0.05)
###Panel bottom-left
par(mai=c(0.1,0.2,0.2,0.1))
bp3 = barplot(PhragB_mean[c(3,1,2),], beside = T, col = c("grey20", "grey46", "grey80"), ylim=c(0,60), las=1)
title( main = bquote(paste(italic("Phragmites australis"))), line=-2.5)
arrows(bp1, PhragB_mean[c(3,1,2),], bp1, PhragB_mean[c(3,1,2),] + PhragB_se[c(3,1,2),], angle = 90, length = 0.05)
###Panel bottom-right
par(mai=c(0.1,0.1,0.2,0.1))
bp4 = barplot(PanicB_mean[c(3,1,2),], beside = T, col = c("grey20", "grey46", "grey80"), ylim=c(0,60),las=1,yaxt="n")
title(main =bquote(paste(italic("Panicum virgatum"))), line=-2.5)
arrows(bp1, PanicB_mean[c(3,1,2),], bp1, PanicB_mean[c(3,1,2),] + PanicB_se[c(3,1,2),], angle = 90, length = 0.05)
mtext("Biomass(g)", side=2, outer = TRUE, line=0, cex=2.3)

##using Ghostscript to extract graph at much higher resolution
bitmap("Biomass.tiff", height = 4, width = 4, units = 'in', type="tiffgray", res=600)
plot(x, y)-##Run code for graph
  dev.off()
par(mfrow = c(1,1))



####Survival Curves 
library(survival)
library(survminer)

####Phragmites in the Forest
Phrag_CS<-subset(Combined_Survival, Combined_Survival$Spp=="PA")
Phrag_CSF<-subset(Phrag_CS, Phrag_CS$Site=="F")
## Because every indiviudal in the cage survived ggsurvplot cannot produce a confidence interval, it will not run. So I subset out the cage data and will put a line across for 100% survival for the cage. 
Phrag_CSFF<-subset(Phrag_CSF,Treatment==c("CT","CC"))
fit1<-survfit(Surv(TIME_F, STATUS_F)~Treatment, data=Phrag_CSFF)
Phrag2<-ggsurvplot(fit1, data=Phrag_CSFF,censor.shape="|", censor.size = 4,conf.int = TRUE,palette=c("red","green"),legend="none",ylab="",xlab="")
Phrag2<-ggpar(Phrag2, font.title=c(16,"plain","black"), font.subtitle = c(12,"italic","black"),xlim=c(0,449)) 
Phrag2


## Inserting a line for the 100% survival in the cage treatment. 
Phrag2$plot <- Phrag2$plot + geom_segment(aes(x = 0, xend = 449, y= 1.00, yend = 1.00),col="blue",size=0.8)
Phrag2

####Phrag in the Marsh

Phrag_CS<-subset(Combined_Survival, Combined_Survival$Spp=="PA")
Phrag_CSM<-subset(Phrag_CS, Phrag_CS$Site=="M")
fit2<-survfit(Surv(TIME_F, STATUS_F)~Treatment, data=Phrag_CSM)
Phrag2M<-ggsurvplot(fit2, data=Phrag_CSM,censor.shape="|", censor.size = 4,conf.int = TRUE,palette=c("blue","red","green"),legend="none",ylab="",xlab="")
Phrag2M<-ggpar(Phrag2M, font.title=c(16,"plain","black"), font.subtitle = c(12,"italic","black"),xlim=c(0,449))
Phrag2M

####Panicum in the Forest
Panic_CS<-subset(Combined_Survival, Combined_Survival$Spp=="PV")
Panic_CSF<-subset(Panic_CS, Phrag_CS$Site=="F")
## Again every indiviudal in the cage survived ggsurvplot cannot produce a confidence interval, it will not run. So I subset out the cage data and will put a line across for 100% survival for the cage.
Panic_CSFF<-subset(Panic_CSF,Treatment==c("CT","CC"))
fit5<-survfit(Surv(TIME_F, STATUS_F)~Treatment, data=Panic_CSFF)
Panic2F<-ggsurvplot(fit5, data=Panic_CSFF,censor.shape="|", censor.size = 4,conf.int = TRUE,palette = c("green","red"),legend="none",ylab="",xlab="")
## Inserting a line for the 100% survival in the cage treatment.
Panic2F$plot <- Panic2F$plot + geom_segment(aes(x = 0, xend = 449, y= 1.00, yend = 1.00),col="blue",size=0.8)
Panic2F

####Panicum in the Marsh

Panic_CSM<-subset(Panic_CS, Phrag_CS$Site=="M")
fit6<-survfit(Surv(TIME_F, STATUS_F)~Treatment, data=Panic_CSM)
Panic2M<-ggsurvplot(fit6, data=Panic_CSM,censor.shape="|", censor.size = 4,conf.int = TRUE,legend="none",palette = c("red","blue","green"),ylab="",xlab="")
Panic2M<-ggpar(Panic2M, font.subtitle = c(12,"italic","black"),xlim=c(0,449))
Panic2M

####Iva in the Forest

Iva_CS<-subset(Combined_Survival, Combined_Survival$Spp=="IF")
Iva_CSF<-subset(Iva_CS, Phrag_CS$Site=="F")
fit7<-survfit(Surv(TIME_F, STATUS_F)~Treatment, data=Iva_CSF)
Iva2F<-ggsurvplot(fit7, data=Iva_CSF,censor.shape="|", censor.size = 4,conf.int = TRUE,legend="none",palette = c("red","blue","green"), ylab="",xlab="")
Iva2F<-ggpar(Iva2F, font.subtitle = c(12,"italic","black"),xlim=c(0,449))
Iva2F

####Iva in the Marsh

Iva_CSM<-subset(Iva_CS, Phrag_CS$Site=="M")
fit8<-survfit(Surv(TIME_F, STATUS_F)~Treatment, data=Iva_CSM)
Iva2M<-ggsurvplot(fit8, data=Iva_CSM,censor.shape="|", censor.size = 4,conf.int = TRUE,legend="none",palette = c("green","blue","red"),ylab="",xlab="")
Iva2M<-ggpar(Iva2M, font.subtitle = c(12,"italic","black"),xlim=c(0,449))
Iva2M

####Baccharis in the Forest

#Baccharis in the Forest

Bac_CS<-subset(Combined_Survival, Combined_Survival$Spp=="BH")
Bac_CSF<-subset(Bac_CS, Bac_CS$Site=="F")
fit3<-survfit(Surv(TIME_F, STATUS_F)~Treatment, data=Bac_CSF)
Bac2F2<-ggsurvplot(fit3, data=Bac_CSF,censor.shape="|", censor.size = 4,conf.int = TRUE,palette=c("red","blue","green"),legend="none", xlab="Time in Days",ylab="")
Bac2F2<-ggpar(Bac2F2, font.title=c(16,"plain","black"), font.subtitle = c(12,"italic","black"),xlim=c(0,449))
Bac2F2

#Baccharis in the Marsh

Bac_CSM<-subset(Bac_CS, Bac_CS$Site=="M")
fit4<-survfit(Surv(TIME_F, STATUS_F)~Treatment, data=Bac_CSM)
Bac2M2<-ggsurvplot(fit4, data=Bac_CSM,censor.shape="|", censor.size = 4,legend="none",conf.int = TRUE,palette = c("red","blue","green"),xlab="Time in Days",ylab="")
Bac2M2<-ggpar(Bac2M2, font.title=c(16,"plain","black"), font.subtitle = c(12,"italic","black"),xlim=c(0,449))
Bac2M2

#####Panel all graphs together
eplots<-list()
eplots[[1]]<-Phrag2
eplots[[2]]<-Panic2F
eplots[[3]]<-Iva2F
eplots[[4]]<-Bac2F2
eplots[[5]]<-Phrag2M
eplots[[6]]<-Panic2M
eplots[[7]]<-Iva2M
eplots[[8]]<-Bac2M2
Surv_Panel<-arrange_ggsurvplots(eplots, print = TRUE, ncol = 2, nrow = 4)
mtext("Survival Probability", side=2, outer = TRUE, line=0, cex=2.3)

Surv_Panel+ theme(panel.spacing = unit(2, "lines"))
dev.off() # Turn off Quartz before using print(Surv_Panel) this allows you to export the image ### use 1000x1000 so it doesnt looked crunched together
print(Surv_Panel)
#### Labels for the survival panel graph were added in Abobe Illustrator


### Log-Rank Tests between caging treatments in the same habitat 
##Phragmites 
##Forest
Surv <- pairwise_survdiff(Surv(TIME_F, STATUS_F)~Treatment, data=Phrag_CSF)
Surv
##Marsh
Surv2<-pairwise_survdiff(Surv(TIME_F, STATUS_F)~Treatment, data=Phrag_CSM)
Surv2

##Panicum 
##Forest
Surv3<-pairwise_survdiff(Surv(TIME_F, STATUS_F)~Treatment, data=Panic_CSF)
Surv3

##Marsh
Surv4<-pairwise_survdiff(Surv(TIME_F, STATUS_F)~Treatment, data=Panic_CSM)
Surv4

##Baccharis 
##Forest
Surv5<-pairwise_survdiff(Surv(TIME_F, STATUS_F)~Treatment, data=Bac_CSF)
Surv5

##Marsh
Surv6<-pairwise_survdiff(Surv(TIME_F, STATUS_F)~Treatment, data=Bac_CSM)
Surv6

##Iva
##Forest
Surv7<-pairwise_survdiff(Surv(TIME_F, STATUS_F)~Treatment, data=Iva_CSF)
Surv7

##Marsh
Surv8<-pairwise_survdiff(Surv(TIME_F, STATUS_F)~Treatment, data=Iva_CSM)
Surv8
### Log-Rank Tests between the cage treatment across habitats
##Phragmites
Phrag_2CG<-filter(Phrag_CS, Treatment=="CG")
survdiff(Surv(TIME_F, STATUS_F)~Site, data=Phrag_2CG)
##Panicum
Panic_2CG<-filter(Panic_CS, Treatment=="CG")
survdiff(Surv(TIME_F, STATUS_F)~Site, data=Panic_2CG)
##Iva
Iva_2CG<-filter(Iva_CS, Treatment=="CG")
survdiff(Surv(TIME_F, STATUS_F)~Site, data=Iva_2CG)
##Baccharis
Bac_2CG<-filter(Bac_CS, Treatment=="CG")
survdiff(Surv(TIME_F, STATUS_F)~Site, data=Bac_2CG)

#####Natural Herbivory Transects
## Two-way ANOVA
Natural_Herbivory$P_E<-as.numeric(Natural_Herbivory$P_E)
NH.aov<-aov(P_E~Site*Spp, data=Natural_Herbivory)
summary(NH.aov)
TukeyHSD(NH.aov)
## Graphing
Herb = melt(Natural_Herbivory, id.vars = c("Site", "Spp"), measure.vars = "P_E")

Herb_N=acast(Herb, Site~Spp, length)
Herb_mean = acast(Herb, Site~Spp, mean)
Herb_sd = acast(Herb, Site~Spp, sd)
Herb_se<- Herb_sd/sqrt(Herb_N)


library(ggpubr)
ggbarplot(Herb, "Spp", "value",fill = "Site",
          label = FALSE,add="mean_se",
          position = position_dodge(0.9), ylab = "Proportion of Herbivory ") + scale_x_discrete(labels=c(expression(italic("Baccharis")),expression(italic("Iva")), expression(italic("Phragmites")), expression(italic("Panicum"))))+scale_fill_manual(values=c("grey20", "grey80"), breaks=c("E", "F"),labels=c("Marsh", "Forest"))+guides(fill=guide_legend(title=NULL))+ theme(axis.title.x = element_blank())+theme(legend.text=element_text(size=14))
##using Ghostscript to extract graph at much higher resolution
bitmap("NatHerb3.tiff", height = 4, width = 4, units = 'in', type="tiffgray", res=600)
plot(x, y)-##run code for graph here 
  dev.off()
par(mfrow = c(1,1))


#### Light Availability

## t-test for light availability 
shapiro.test(Light$value)
leveneTest(value~Site, data=Light)

t.test(value~Site, data=Light,var.equal = TRUE)


###Light availability average

light.F=subset(Light.Availability,Site=="F")
light.M=subset(Light.Availability,Site=="M")

LIGHTF<-light.F$light
mean(LIGHTF)
LIGHTFSE_F<-sd(LIGHTF)/sqrt(length(LIGHTF))
LIGHTFSE_F

LIGHTM<-light.M$light
mean(LIGHTM)
LIGHTMSE_M<-sd(LIGHTM)/sqrt(length(LIGHTM))
LIGHTMSE_M


#### Salinity 

Salinity<-Salinity.Data.2017.2018.BW
Salt.F=subset(Salinity,Site=="F")
Salt.M=subset(Salinity,Site=="M")


###Average salinity for Forest
SALTAVGF<-Salt.F$ppt
mean(SALTAVGF)
SALTAVGFSE<-sd(SALTAVGF)/sqrt(length(SALTAVGF))
SALTAVGFSE

##Range
min(SALTAVGF)
max(SALTAVGF)

###Average salinity for Ecotone
SALTAVGM<-Salt.M$ppt
mean(SALTAVGM)
SALTAVGMSE<-sd(SALTAVGM)/sqrt(length(SALTAVGM))
SALTAVGMSE

##Range
min(SALTAVGM)
max(SALTAVGM)

#### Salinity Time Series
library("scales")
library("ggpubr")

S1<-`2017_Salinity`
S2<- `2018Salinity`

S1$date<- lubridate::mdy_hm(S1$Date)
S2$date<- lubridate::mdy_hm(S2$Date)
S1$date2<- as.Date(S1$date, format="%m/%d")
S2$date2<- as.Date(S2$date, format="%m/%d")
PhragB$Habitat<-ifelse(PhragB$Site=="M", c("Ecotone"),c("Forest"))
S1$Site<-ifelse(S1$Site=="M", c("Marsh"),c("Forest"))
S2$Site<-ifelse(S2$Site=="M", c("Marsh"),c("Forest"))
Salinity_18<-ggplot(S2, aes(x=date2, y=ppt))+geom_line(aes(color=Site))+scale_y_continuous(limits =c(0,15))+theme_set(theme_classic())+scale_x_date(date_labels ="%B")+theme(legend.position = "bottom", legend.direction = "horizontal",axis.title.y = element_blank())
SL18<-print(Salinity_18+ ggtitle("2018 Salinity")+labs(y="Salinity (ppt)", x="Date")+labs(colour="Habitat"))


Salinity_17<-ggplot(S1, aes(x=date2, y=ppt))+geom_line(aes(color=Site))+scale_y_continuous(limits =c(0,15))+theme_set(theme_classic())+scale_x_date(breaks = seq(as.Date("2017-07-15"),as.Date("2017-09-15"),by = "1 month"), date_labels ="%B")+theme(legend.position = "bottom", legend.direction = "horizontal",axis.title.x = element_blank(), axis.title.y = element_blank())
SL17<-print(Salinity_17+ ggtitle("2017 Salinity")+labs(y="Salinity (ppt)", x="Date")+labs(colour="Habitat"))

figure<-ggarrange(SL18,SL17, ncol=1,nrow =2, common.legend = TRUE, legend = "bottom", label.x = "Salinity (ppt)")
figure
annotate_figure(figure, left = text_grob("Salinity (ppt)", rot = 90, size = 10, hjust = .2))

##using Ghostscript to extract graph at much higher resolution
bitmap("Salinity.tiff", height = 4, width = 4, units = 'in', type="png16m", res=600)
plot(x, y)-##Run code for graph
  dev.off()
par(mfrow = c(1,1))






