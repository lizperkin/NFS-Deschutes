#Creating figures of Temp at different points in Deschutes River (OR) basin
#using data from USGS gauges.
#PGE SWW tower completed in 2010
#Data available at: https://maps.waterdata.usgs.gov/mapper/index.html

#Dependencies----
#library(googleVis)
#library(devtools)
# Hit an empty line in the Console to skip updates
library(dataRetrieval)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(chron)
library(zoo)
library(openxlsx)
library(readxl)

#MOODY GAUGE%%%%
#Load & Combine Data for Figures----

#Moody Pre-SWW Temp Data
mood.T.pre<-read_xlsx("Deschutes Temperatures_20220722.xlsx",
                      sheet = "Moody Pre SWW Temperatures")
#Add year, day-month columns
mood.T.pre$Yr <- format(as.Date(mood.T.pre$dateTime), "%Y")
mood.T.pre$md <- format(as.Date(mood.T.pre$dateTime), "%b-%d")
mood.T.pre$jdate <- yday(as.Date(mood.T.pre$dateTime))
mood72_81 <- subset(mood.T.pre, Yr >= 1972)
mood55 <- subset(mood.T.pre, Yr >= 1955)
mood55_58 <- subset(mood55, Yr < 1959)

#Create Mean Temp DF
mood72_81_day <- aggregate(MaxTemp ~ jdate, data = mood72_81, mean)
mood55_58_day <- aggregate(MaxTemp ~ jdate, data = mood55_58, mean)

#Moody Post-SWW Data
mood.T.post<-read_xlsx("Deschutes Temperatures_20220722.xlsx",
                  sheet = "Moody Post SWW Temperatures")

#Add year, day-month columns
mood.T.post$Yr <- format(as.Date(mood.T.post$dateTime), "%Y")
mood.T.post$md <- format(as.Date(mood.T.post$dateTime), "%b-%d")
mood.T.post$jdate <- yday(as.Date(mood.T.post$dateTime))
mood12_21 <- subset(mood.T.post, Yr >= 2012)

#Create Mean Temp DF
mood12_21_day <- aggregate(MaxTemp ~ jdate, data = mood12_21, mean)

#Add years to prep for combining
mood12_21_day$prepost <- "2012-2021"
mood72_81_day$prepost <- "1972-1981"

#combine datasets
moodprepost <- rbind(mood72_81_day,
                     mood12_21_day)

#Moody Graphs
moodpre.fig<-ggplot(mood72_81_day, 
                 aes(x = jdate,
                     y = MaxTemp))+ 
  geom_line(data = mood72_81_day, aes(size = 1.5, color = "1972-1981"))+
  scale_fill_manual(values = c("#64CCC9"))+
  theme_bw() +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(size = 13),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylab(expression(bold(Temperature~(degree~C))))+
  xlab("Julian Date")+
  theme (legend.title = element_blank(),
         legend.text = element_text(size = 13))+
  theme (legend.position = c (20.0, 5.0), legend.justification = c(0,1))

moodpre.fig

moodpost.fig<-ggplot(mood12_21_day, 
                    aes(x = jdate,
                        y = MaxTemp))+ 
  geom_line(data = mood12_21_day, aes(size = 1.5, color = "2012-2021"))+
  scale_fill_manual(values = c("#7E7F74"))+
  theme_bw() +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(size = 13),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylab(expression(bold(Temperature~(degree~C))))+
  xlab("Julian Date")+
  theme (legend.title = element_blank(),
         legend.text = element_text(size = 13))+
  theme (legend.position = c (20.0, 5.0), legend.justification = c(0,1))

moodpost.fig

mood.fig<-ggplot(mood12_21_day, 
                 aes(x = jdate,
                     y = MaxTemp))+ 
  geom_line(data = mood12_21_day, aes(size = 1.0, color = "2012-2021"))+
  geom_line(data = mood72_81_day, aes(size = 1.0, color = "1972-1981"))+
  scale_color_manual(values = c("#64CCC9", "#FF585D"))+
  theme_bw() +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(size = 13),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylab(expression(bold(Temperature~(degree~C))))+
  xlab("Day of Year")+
  theme (legend.title = element_blank(),
         legend.text = element_text(size = 13))+
  theme (legend.position = c (0.05, 0.95), legend.justification = c(0,1))

mood.fig

moodclim.fig<-ggplot(mood12_21_day, 
                 aes(x = jdate,
                     y = MaxTemp))+ 
  geom_line(data = mood12_21_day, aes(size = 1.0, color = "2012-2021"))+
  geom_line(data = mood72_81_day, aes(size = 1.0, color = "1972-1981"))+
  geom_line(data = mood55_58_day, aes(size = 1.0, color = "1955-1958"))+
  scale_color_manual(values = c("#A2E0DF","#64CCC9","#FF585D"))+
  theme_bw() +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(size = 13),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylab(expression(bold(Temperature~(degree~C))))+
  xlab("Day of Year")+
  theme (legend.title = element_blank(),
         legend.text = element_text(size = 13))+
  theme (legend.position = c (0.05, 0.95), legend.justification = c(0,1))

moodclim.fig


#MADRAS GAUGE%%%%-----
#Load & Combine Data for Figures

#Madras Pre-SWW Temp Data
mad.T.pre<-read_xlsx("Deschutes Temperatures_20220722.xlsx",
                     sheet = "Madras Pre SWW Temperatures")

#Add year, day-month columns
mad.T.pre$Yr <- format(as.Date(mad.T.pre$dateTime), "%Y")
mad.T.pre$md <- format(as.Date(mad.T.pre$dateTime), "%b-%d")
mad.T.pre$jdate <- yday(as.Date(mad.T.pre$dateTime))
mad55 <- subset(mad.T.pre, Yr >= 1955)
mad55_58 <- subset(mad55, Yr < 1959)
mad72 <- subset(mad.T.pre, Yr >= 1972)
mad72_81 <- subset(mad72, Yr < 1982)
mad00 <- subset(mad.T.pre, Yr >= 2000)
mad00_09 <- subset(mad00, Yr < 2010)

#Create Mean Temp DF
mad55_58_day <- aggregate(MaxTemp ~ jdate, data = mad55_58, mean)
mad72_81_day <- aggregate(MaxTemp ~ jdate, data = mad72_81, mean)
mad00_09_day <- aggregate(MaxTemp ~ jdate, data = mad00_09, mean)

#Madras Post-SWW Temp Data
mad.T.post<-read_xlsx("Deschutes Temperatures_20220722.xlsx",
                      sheet = "Madras Post SWW Temperatures")

#Add year, day-month columns
mad.T.post$Yr <- format(as.Date(mad.T.post$dateTime), "%Y")
mad.T.post$md <- format(as.Date(mad.T.post$dateTime), "%b-%d")
mad.T.post$jdate <- yday(as.Date(mad.T.post$dateTime))
mad12_21 <- subset(mad.T.post, Yr < 2022)
mad12_16 <- subset(mad.T.post, Yr < 2016)

#Create Mean Temp DF
mad12_21_day <- aggregate(MaxTemp ~ jdate, data = mad12_21, mean)
mad12_16_day <- aggregate(MaxTemp ~ jdate, data = mad12_16, mean)

#Add years to prep for combining
#mad12_21_day$prepost <- "2012-2021"
#mad72_81_day$prepost <- "1972-1981"
#mad00_09_day$prepost <- "2005-2009"

#combine datasets
#madprepost <- rbind(mad72_81_day,
#                     mad12_21_day)

#Madras Figure----
mad.fig<-ggplot(mad12_21_day, 
                 aes(x = jdate,
                     y = MaxTemp))+ 
  geom_line(data = mad12_21_day, aes(size = 1.0, color = "2012-2021"))+
  geom_line(data = mad72_81_day, aes(size = 1.0, color = "1972-1981"))+
  scale_color_manual(values = c("#64CCC9", "#FF585D"))+
  theme_bw() +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(size = 13),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylab(expression(bold(Temperature~(degree~C))))+
  xlab("Day of Year")+
  theme (legend.title = element_blank(),
         legend.text = element_text(size = 13))+
  theme (legend.position = c (0.05, 0.95), legend.justification = c(0,1))

mad.fig

madmod.fig<-ggplot(mad12_21_day, 
                aes(x = jdate,
                    y = MaxTemp))+ 
  geom_line(data = mad12_21_day, aes(size = 1.0, color = "2012-2021"))+
  geom_line(data = mad00_09_day, aes(size = 1.0, color = "2005-2009"))+
  scale_color_manual(values = c("#64CCC9", "#FF585D"))+
  theme_bw() +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(size = 13),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylab(expression(bold(Temperature~(degree~C))))+
  xlab("Day of Year")+
  theme (legend.title = element_blank(),
         legend.text = element_text(size = 13))+
  theme (legend.position = c (0.05, 0.95), legend.justification = c(0,1))

madmod.fig

madclim.fig<-ggplot(mad12_16_day, 
                   aes(x = jdate,
                       y = MaxTemp))+ 
  geom_line(data = mad12_16_day, aes(size = 1.0, color = "2012-2016"))+
  geom_line(data = mad00_09_day, aes(size = 1.0, color = "2005-2009"))+
  scale_color_manual(values = c("#64CCC9", "#FF585D"))+
  theme_bw() +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(size = 13),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylab(expression(bold(Temperature~(degree~C))))+
  xlab("Day of Year")+
  theme (legend.title = element_blank(),
         legend.text = element_text(size = 13))+
  theme (legend.position = c (0.05, 0.95), legend.justification = c(0,1))

madclim.fig

madpreclim.fig<-ggplot(mad55_58_day, 
                    aes(x = jdate,
                        y = MaxTemp))+ 
  geom_line(data = mad00_09_day, aes(size = 1.0, color = "2005-2009"))+
  geom_line(data = mad72_81_day, aes(size = 1.0, color = "1972-1981"))+
  geom_line(data = mad55_58_day, aes(size = 1.0, color = "1955-1958"))+
  scale_color_manual(values = c("#A2E0DF","#64CCC9", "#016B67"))+
  theme_bw() +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(size = 13),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylab(expression(bold(Temperature~(degree~C))))+
  xlab("Day of Year")+
  theme (legend.title = element_blank(),
         legend.text = element_text(size = 13))+
  theme (legend.position = c (0.05, 0.95), legend.justification = c(0,1))

madpreclim.fig

madmet.fig<-ggplot(mad55_58_day, 
                       aes(x = jdate,
                           y = MaxTemp))+ 
  geom_line(data = mad12_16_day, aes(size = 1.0, color = "2012-2016"))+
  geom_line(data = mad00_09_day, aes(size = 1.0, color = "2005-2009"))+
  geom_line(data = mad55_58_day, aes(size = 1.0, color = "1955-1958"))+
  scale_color_manual(values = c("#A2E0DF", "#64CCC9", "#FF585D"))+
  theme_bw() +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(size = 13),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylab(expression(bold(Temperature~(degree~C))))+
  xlab("Day of Year")+
  theme (legend.title = element_blank(),
         legend.text = element_text(size = 13))+
  theme (legend.position = c (0.05, 0.95), legend.justification = c(0,1))

madmet.fig
