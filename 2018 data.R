library(readxl)
library(reshape2)
library(ggplot2)
library(plotly)
library("rstatix", lib.loc="C:/Program Files/R/R-4.0.0/library")
library("ggpubr", lib.loc="C:/Program Files/R/R-4.0.0/library")


rm(list=ls())

getwd()
setwd("C:/Users/YAKIRP/Dropbox/yakir/PhD/Thesis")
    
             ###########import data#########
Irr_2017=read_excel("C:/Users/YAKIRP/Dropbox (Harvard University)/yakir/PhD/Thesis/2017 data for Xue.xlsx",sheet = "Irr_2017")
#Con_2017= read_excel("C:/Users/YAKIRP/Dropbox/yakir/PhD/Thesis/2017 data for Xue.xlsx",sheet = "Con_2017")


          #########Export Data############
write.csv(Con_2017_Day, "C:/Users/YAKIRP/Dropbox/yakir/PhD/Thesis/Con_2017_Day.csv")
write.csv(Con_2017, "C:/Users/yakirp/Dropbox/Ecophysiology group/Itai/research/Con2018.csv")
write.csv(Con_2017_Night, "C:/Users/yakirp/Dropbox/Ecophysiology group/Itai/research/Con2018_Night.csv")
write.csv(Irr_2017_Night, "C:/Users/YAKIRP/Dropbox/yakir/PhD/Thesis/Irr_2017_Night.csv")
write.csv(Irr_2017_Day, "C:/Users/YAKIRP/Dropbox/yakir/PhD/Thesis/Irr_2017_Day.csv")
write.csv(Irr_2017_18, "C:/Users/yakirp/Dropbox/Ecophysiology group/Itai/research/Irr2018.csv")

write.csv(Trans_Irr, "C:/Users/YAKIRP/Dropbox/yakir/PhD/Thesis/Trans_Irr.csv")
write.csv(Trans_Con, "C:/Users/YAKIRP/Dropbox/yakir/PhD/Thesis/Trans_Con.csv")

write.csv(gbr_Irr, "C:/Users/YAKIRP/Dropbox/yakir/PhD/Thesis/gbr_Irr.csv")
write.csv(gbr_Con, "C:/Users/YAKIRP/Dropbox/yakir/PhD/Thesis/gbr_con.csv")

write.csv(Anet_Irr, "C:/Users/YAKIRP/Dropbox/yakir/PhD/Thesis/Anet_Irr.csv")
write.csv(Anet_Con, "C:/Users/YAKIRP/Dropbox/yakir/PhD/Thesis/Anet_Con.csv")

write.csv(VPD_Irr, "C:/Users/YAKIRP/Dropbox/yakir/PhD/Thesis/VPD_Irr.csv")
write.csv(VPD_Con, "C:/Users/YAKIRP/Dropbox/yakir/PhD/Thesis/VPD_Con.csv")
##EDA 2017
summary(Irr_2017)
str(Irr_2017)
pairs(Irr_2017[,5:15])

#####Jonathan#####
source("summarySE.R")
Irr_2017_Day2 = Irr_2017_Day
names(Irr_2017_Day2) = make.names(names(Irr_2017_Day2))
names(Irr_2017_Day2)

Con_2017_Day2 = Con_2017_Day
names(Con_2017_Day2) = make.names(names(Con_2017_Day2))
names(Con_2017_Day2)

  ####Monthly values summary#####
#Transpiration#
Trans_Irr = summarySE(data=as.data.frame(Irr_2017_Day2),measurevar = "corected.Trans", groupvars="Month")
head(Trans_Irr)
head(as.data.frame(Irr_2017_Day))

Trans_Con = summarySE(data=as.data.frame(Con_2017_Day2),measurevar = "corected.Trans", groupvars="Month")
head(Trans_Con)
head(as.data.frame(Con_2017_Day))

###gbr###
gbr_Irr = summarySE(data=as.data.frame(Irr_2017_Day2),measurevar = "gbr", groupvars="Month")
head(gbr_Irr)

gbr_Con = summarySE(data=as.data.frame(Con_2017_Day2),measurevar = "gbr", groupvars="Month")
head(gbr_Con)
head(as.data.frame(Con_2017_Day))

plot(gbr_Irr$mean,col="blue",type = 'l')
points(gbr_Con$mean,col="red",type = 'l')

#Photosynthesis#
Anet_Irr = summarySE(data=as.data.frame(Irr_2017_Day2),measurevar = "Anet", groupvars="Month")
head(Anet_Irr)

Anet_Con = summarySE(data=as.data.frame(Con_2017_Day2),measurevar = "Anet", groupvars="Month")
head(Anet_Con)
head(as.data.frame(Con_2017_Day))

plot(Anet_Irr$mean,col="blue",type = 'l')
points(Anet_Con$mean,col="red",type = 'l')

#VPD#
VPD_Irr = summarySE(data=as.data.frame(Irr_2017_Day2),measurevar = "VPD.Chamber", groupvars="Month")
head(VPD_Irr)
head(as.data.frame(Irr_2017_Day))

VPD_Con = summarySE(data=as.data.frame(Con_2017_Day2),measurevar = "VPD.Chamber..kPa.", groupvars="Month")
head(VPD_Con)
head(as.data.frame(Con_2017_Day))

#test2 = summaryJM(data=as.data.frame(Irr_2017_Day2),measurevars = c("corected.Trans","Anet"),renamevars = c("corrected.Trans","Anet"), groupvars=c("Month"))
#test2

#plot(test$mean,col="blue",type = 'l')
#points(test2$mean,col="red",type = 'l')


summary(Irr_2017_Day)
str(Irr_2017_Day)
pairs(Irr_2017_Day[,5:15])


summary(Irr_2017_Night)
str(Irr_2017)
pairs(Irr_2017[,5:15])

##EDA 2017###

summary(Irr2018)
str(Irr2018)
pairs(Irr2018[,5:15])

summary(Irr2018_Day)
str(Irr2018_Day)
pairs(Irr2018_Day[,5:15])


summary(Irr2018_Night)
str(Irr2018)
pairs(Irr2018[,5:15])

summary(Con_2017_Day)
str(Con_2017_Day)
pairs(Con_2017_Day[,5:15])

meltData <- melt(Irr_2017)
boxplot(data=meltData, value~variable)
p <- ggplot(meltData, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free")

meltData <- melt(Con2018)
boxplot(data=meltData, value~variable)
p <- ggplot(meltData, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free")

meltData_Irr2018 <- melt(Irr2018_Day)
boxplot(data=meltData_Irr2018, value~variable)
p <- ggplot(meltData_Irr2018, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free")

meltData_Con2018 <- melt(Con2018_Day)
boxplot(data=meltData_Con2018, value~variable)
p <- ggplot(meltData_Con2018, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free")

#make two plots next to each other
par(mfrow=c(1,2))
#make one plot
par(mfrow=c(1,1))

      #############boxplot data 2017#####################
#boxplot of monthly data- day Transpiration
boxplot(Irr_2017_Day$`Trans (mmol m-2 s-1)`~Irr_2017_Day$Month,col='blue',xlab= "Month",ylab= "Transpiration (mmol m-2 s-1)")
boxplot(Con_2017_Day$`corected Trans`~Con_2017_Day$Month,col='red',xlab= "Month",ylab= "Transpiration (mmol m-2 s-1)")
legend(5,4.7, legend=c("Irrigation", "Control"),
       col=c("blue", "red"), lty=1, cex=0.8,box.lty=0)


#boxplot of monthly data- Nighttime Transpiration
boxplot(Irr_2017_Night$Trans~Irr_2017_Night$Month,col='blue',xlab= "Month",ylab= "Night Transpiration (mmol m-2 s-1)")
boxplot(Con_2017_Night$Trans~Con_2017_Night$Month,col='red',xlab= "Month",ylab= "Night Transpiration (mmol m-2 s-1)")
legend(5,4.7, legend=c("Irrigation", "Control"),
       col=c("blue", "red"), lty=1, cex=0.8,box.lty=0)

#boxplot of monthly data- Photosynthesis
boxplot(Irr_2017_Day$Photo~Irr_2017_Day$Month,col='blue',xlab= "Month",ylab= "Anet (umol m-2 s-1)")
boxplot(Con_2017_Day$Photo~Con_2017_Day$Month,col='red',xlab= "Month",ylab= "Anet (umol m-2 s-1)")
legend(5,-15, legend=c("Irrigation", "Control"),
       col=c("blue", "red"), lty=1, cex=0.8,box.lty=0)

#boxplot of monthly data- nighttime respiration
boxplot(Irr_2017_Night$Photo~Irr_2017_Night$Month,col='blue',xlab= "Month",ylab= "Resp. (umol m-2 s-1)")
boxplot(Con_2017_Night$Photo~Con_2017_Night$Month,col='red',xlab= "Month",ylab= "Resp. (umol m-2 s-1)")
legend(5,-2, legend=c("Irrigation", "Control"),
       col=c("blue", "red"), lty=1, cex=0.8,box.lty=0)


boxplot(Irr_2017_Day$gbr~Irr_2017_Day$Month,col='blue')
boxplot(Con_2017_Day$gbr~Con_2017_Day$Month,col='red')
legend(5,4.7, legend=c("Irrigation", "Control"),
       col=c("blue", "red"), lty=1, cex=0.8,box.lty=0)

boxplot(Irr_2017_Night$Trans~Irr_2017_Night$`VPD Chamber`,col='blue')
boxplot(Con_2017_Day$Gbr~Con_2017_Day$Month,col='red')
legend(5,4.7, legend=c("Irrigation", "Control"),
       col=c("blue", "red"), lty=1, cex=0.8,box.lty=0)


      #############boxplot data 2018#####################

#boxplot of monthly data- Transpiration
#boxplot of monthly data- Nighttime Transpiration
boxplot(Irr2018_Night$Trans~Irr2018_Night$Month,col='blue',xlab= "Month",ylab= "Night Transpiration (mmol m-2 s-1)")
boxplot(Con2018_Night$Trans~Con2018_Night$Month,col='red',xlab= "Month",ylab= "Night Transpiration (mmol m-2 s-1)")
legend(5,4.7, legend=c("Irrigation", "Control"),
       col=c("blue", "red"), lty=1, cex=0.8,box.lty=0)

#boxplot of monthly data- Photosynthesis
boxplot(Irr2018_Day$Anet~Irr2018_Day$Month,col='blue',xlab= "Month",ylab= "Anet (umol m-2 s-1)")
boxplot(Con2018_Day$Photo~Con2018_Day$Month,col='red',xlab= "Month",ylab= "Anet (umol m-2 s-1)")
legend(5,-15, legend=c("Irrigation", "Control"),
       col=c("blue", "red"), lty=1, cex=0.8,box.lty=0)

#boxplot of monthly data- nighttime respiration
boxplot(Irr2018_Night$Photo~Irr2018_Night$Month,col='blue',xlab= "Month",ylab= "Resp. (umol m-2 s-1)")
boxplot(Con2018_Night$Photo~Con2018_Night$Month,col='red',xlab= "Month",ylab= "Resp. (umol m-2 s-1)")
legend(5,-2, legend=c("Irrigation", "Control"),
       col=c("blue", "red"), lty=1, cex=0.8,box.lty=0)


boxplot(Irr2018_Day$gbr~Irr2018_Day$Month,col='blue')
boxplot(Con2018_Day$gbr~Con2018_Day$Month,col='red')
legend(5,4.7, legend=c("Irrigation", "Control"),
       col=c("blue", "red"), lty=1, cex=0.8,box.lty=0)

boxplot(Irr2018_Night$Trans~Irr2018_Night$`VPD Chamber`,col='blue')
boxplot(Con2018_Day$Gbr~Con2018_Day$Month,col='red')
legend(5,4.7, legend=c("Irrigation", "Control"),
       col=c("blue", "red"), lty=1, cex=0.8,box.lty=0)

        #############clean data 2017#####################

#Irr_2017$GME = NULL
#Irr_2017$Plot = NULL
Irr_2017= Irr_2017[Irr_2017$`CO2 ambient`>380,]
Irr_2017=Irr_2017[Irr_2017$`H2O ambient`<30,]
Irr_2017=Irr_2017[Irr_2017$gbr>-0.1,]
Irr_2017=Irr_2017[Irr_2017$`VPD Chamber`>0,]
Irr_2017=Irr_2017[Irr_2017$`VPD ambient`>0,]

#Irr_2017=Irr_2017[Irr_2017$year<2018,]
Irr_2017_Day=Irr_2017[Irr_2017$Hour>5& Irr_2017$Hour<19,]

#identify_outliers(Irr_2017_Day$Hour)

Irr2018=Irr_2017[Irr_2017$year>2017,]
Irr2018_Day=Irr2018[Irr2018$Hour>5& Irr2018$Hour<19,]
summary(Irr2018_Day)
str(Irr2018_Day)
pairs(Irr2018_Day[,5:15])

Con_2017=Con_2017[Con_2017$Year<2018,]
Con_2017_Day=Con_2017[Con_2017$Hour>5& Con_2017$Hour<19,]
summary(Con_2017_Day)
str(Con_2017_Day)
pairs(Con_2017_Day[,5:15])


Con2018=Con_2017[Con_2017$Year>2017,]
Con2018_Day=Con2018[Con2018$Hour>5& Con2018$Hour<19,]
summary(Con2018_Day)
str(Con2018_Day)
pairs(Con2018_Day[,5:15])


Irr_2017_Day=Irr_2017[Irr_2017$Hour>5& Irr_2017$Hour<19,]
Irr_2017_Night=Irr_2017[Irr_2017$Hour<5 | Irr_2017$Hour>19,]
summary(Irr_2017_Day)
str(Irr_2017_Day)
#Anet_Irr=Irr_2017_Day$Photo*-1


#Con_2017$GME = NULL
#Con_2017$Plot = NULL
Con_2017= Con_2017[Con_2017$`CO2 ambient`>380,]
Con_2017=Con_2017[Con_2017$`H2O ambient`<30,]
Con_2017=Con_2017[Con_2017$gbr>-0.1 &Con_2017$gbr<1,]
Con_2017=Con_2017[Con_2017$`VPD Chamber (kPa)`>-0.01,]
Con_2017=Con_2017[Con_2017$`VPD ambient (kPa)`>-0.01,]


Con_2017_Day=Con_2017[Con_2017$Hour>5& Con_2017$Hour<19,]
Con_2017_Night=Con_2017[Con_2017$Hour<5| Con_2017$Hour>19,]

Anet_Con=Con_2017_Day$Photo*-1

            #############clean data 2018#####################

Irr2018$GME = NULL
Irr2018$Plot = NULL
Irr2018= Irr2018[Irr2018$`CO2 ambient`>380,]
Irr2018=Irr2018[Irr2018$`H2O ambient`<30,]
Irr2018=Irr2018[Irr2018$Gbr>-0.1,]
Irr2018=Irr2018[Irr2018$`VPD Chamber`>-0.01,]
Irr2018=Irr2018[Irr2018$`VPD ambient`>-0.01,]

Irr2018_Day=Irr2018[Irr2018$Hour>5& Irr2018$Hour<19,]
Irr2018_Night=Irr2018[Irr2018$Hour<5 | Irr2018$Hour>19,]

na.omit(Irr2018)

Con2018$GME = NULL
Con2018$Plot = NULL
Con2018= Con2018[Con2018$`CO2 ambient`>380,]
Con2018=Con2018[Con2018$`H2O ambient`<30,]
Con2018=Con2018[Con2018$Gbr>-0.1 &Con2018$Gbr<1,]
Con2018=Con2018[Con2018$`VPD Chamber`>-0.01,]
Con2018=Con2018[Con2018$`VPD ambient`>-0.01,]

Con2018_Day=Con2018_Day[Con2018_Day$Trans>0,]


Con2018_Day=Con2018[Con2018$Hour>5& Con2018$Hour<19,]
Con2018_Night=Con2018[Con2018$Hour<5| Con2018$Hour>19,]

#######Plots#######
#boxplot of monthly data- Photosynthesis
boxplot(Irr2018_Day$Photo~Irr2018_Day$Month,col='blue',xlab= "Month",ylab= "Anet (umol m-2 s-1)")
boxplot(Con2018_Day$Photo~Con2018_Day$Month,col='red',xlab= "Month",ylab= "Anet (umol m-2 s-1)")
legend(5,-15, legend=c("Irrigation", "Control"),
       col=c("blue", "red"), lty=1, cex=0.8,box.lty=0)

#calculate VPD and gbr##
SVP=(610.7*107.5*Con2018$`Chamber Temp`)/(237.3+Con2018$`Chamber Temp`)
VPD_new= (((100 - Con2018$`RH Air Vaisala`)/100)*SVP)/1000
plot(VPD_new,Con2018$`VPD ambient`,ylim = c(0,7))
points(VPD_new,Con2018$`VPD Chamber`,col='blue')


na.omit(Con_2017_Day$`VPD Chamber`)
summary(Con_2017_Day$`VPD Chamber`)
plot(Con_2017_Day$DOY,Con_2017_Day$`VPD Chamber`,type = 'p',col='red',xlab= "DOY",ylab="VPD (kPa)")
plot(Con_2017_Day$DOY,Con_2017_Day$Trans,type = 'p',col='blue')
plot(Con_2017_Day$DOY,Con_2017_Day$Photo,type = 'p',col='gray')
