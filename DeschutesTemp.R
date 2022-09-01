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

#Moody Temp Data Availability----
moodPostT <- readNWISdata(sites="14103000",
                      parameterCd="00010", 
                      startDate="2012-01-01",endDate="2022-01-01")

moodPreT <- readNWISdata(sites="14103000",
                      parameterCd="00010", 
                      startDate="1955-01-01",endDate="2010-01-01")

#Madras Temp Data----
madPostT <- readNWISdata(sites="14092500",
                          parameterCd="00010", 
                          startDate="2012-01-01",endDate="2022-01-01")

madPreT <- readNWISdata(sites="14092500",
                         parameterCd="00010", 
                         startDate="1955-01-01",endDate="2010-01-01")

#create output excel file
wb <- createWorkbook()
addWorksheet(wb, "Moody Post SWW Temperatures")
writeDataTable(wb, sheet = "Moody Post SWW Temperatures",
               x = moodPostT,
               withFilter = F, tableStyle = "none")

addWorksheet(wb, "Moody Pre SWW Temperatures")
writeDataTable(wb, sheet = "Moody Pre SWW Temperatures",
               x = moodPreT,
               withFilter = F, tableStyle = "none")

addWorksheet(wb, "Madras Post SWW Temperatures")
writeDataTable(wb, sheet = "Madras Post SWW Temperatures",
               x = madPostT,
               withFilter = F, tableStyle = "none")

addWorksheet(wb, "Madras Pre SWW Temperatures")
writeDataTable(wb, sheet = "Madras Pre SWW Temperatures",
               x = madPreT,
               withFilter = F, tableStyle = "none")

saveWorkbook(wb, paste0("Deschutes Temperatures_",
                        format(Sys.time(), "%Y%m%d"), ".xlsx"),
             overwrite = F)

#Madras Pre-SWW Temp Figs----
mad.T.pre<-read_xlsx("Deschutes Temperatures_20220722.xlsx",
                      sheet = "Madras Pre SWW Temperatures")

#Madras Post-SWW Temp Figs----
mad.T.post<-read_xlsx("Deschutes Temperatures_20220722.xlsx",
                     sheet = "Madras Post SWW Temperatures")

#Moody Pre-SWW Temp Figs----
mood.T.pre<-read_xlsx("Deschutes Temperatures_20220722.xlsx",
                  sheet = "Moody Pre SWW Temperatures")

#add month-year column
mood.T.pre$Month_Yr <- format(as.Date(mood.T.pre$dateTime), "%m-%Y")
mood.T.pre$Yr <- format(as.Date(mood.T.pre$dateTime), "%Y")

moody.preSWW <- ggplot(mood.T.pre, aes(x=Month_Yr))+ 
        geom_line (aes(y=MaxTemp), color="#64ccc9", size=2) +
        theme_bw() +
        #scale_y_continuous(limits = c(8,32))+
        theme(axis.line = element_line(color = "black"),
              axis.text = element_text(size = 13),
              axis.title.x = element_text(size = 13, face = "bold"),
              axis.title.y = element_text(size = 13, face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())+
        ylab(expression(bold(Temperature~(degree~C))))+
        xlab("Date")+
        geom_hline(yintercept=20, linetype="dashed", color = "black")+
        geom_hline(yintercept=16, linetype="dotted", color = "black")

moody.preSWW

#Pre-SWW Moody Year-by-year graphs-----
moody.1955 <- subset (mood.T.pre, Yr == 1955)
moody.55 <- ggplot(moody.1955, aes(x=Month_Yr))+ 
        geom_line (aes(y=MaxTemp), color="#64ccc9", size=2) +
        theme_bw() +
        theme(axis.line = element_line(color = "black"),
              axis.text = element_text(size = 13),
              axis.title.x = element_text(size = 13, face = "bold"),
              axis.title.y = element_text(size = 13, face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(angle = 90,vjust = 0.5))+
        ylab(expression(bold(Temperature~(degree~C))))+
        xlab("Date")+
        geom_hline(yintercept=20, linetype="dashed", color = "black")+
        geom_hline(yintercept=16, linetype="dotted", color = "black")

moody.55

moody.1958 <- subset (mood.T.pre, Yr == 1958)
moody.58 <- ggplot(moody.1958, aes(x=Month_Yr))+ 
        geom_line (aes(y=MaxTemp), color="#64ccc9", size=2) +
        theme_bw() +
        theme(axis.line = element_line(color = "black"),
              axis.text = element_text(size = 13),
              axis.title.x = element_text(size = 13, face = "bold"),
              axis.title.y = element_text(size = 13, face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(angle = 90,vjust = 0.5))+
        ylab(expression(bold(Temperature~(degree~C))))+
        xlab("Date")+
        geom_hline(yintercept=20, linetype="dashed", color = "black")+
        geom_hline(yintercept=16, linetype="dotted", color = "black")

moody.58

moody.1965 <- subset (mood.T.pre, Yr == 1965)
moody.65 <- ggplot(moody.1965, aes(x=Month_Yr))+ 
        geom_line (aes(y=MaxTemp), color="#64ccc9", size=2) +
        theme_bw() +
        theme(axis.line = element_line(color = "black"),
              axis.text = element_text(size = 13),
              axis.title.x = element_text(size = 13, face = "bold"),
              axis.title.y = element_text(size = 13, face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(angle = 90,vjust = 0.5))+
        ylab(expression(bold(Temperature~(degree~C))))+
        xlab("Date")+
        geom_hline(yintercept=20, linetype="dashed", color = "black")+
        geom_hline(yintercept=16, linetype="dotted", color = "black")

moody.65

moody.1970 <- subset (mood.T.pre, Yr == 1970)
moody.70 <- ggplot(moody.1970, aes(x=Month_Yr))+ 
        geom_line (aes(y=MaxTemp), color="#64ccc9", size=2) +
        theme_bw() +
        theme(axis.line = element_line(color = "black"),
              axis.text = element_text(size = 13),
              axis.title.x = element_text(size = 13, face = "bold"),
              axis.title.y = element_text(size = 13, face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(angle = 90,vjust = 0.5))+
        ylab(expression(bold(Temperature~(degree~C))))+
        xlab("Date")+
        geom_hline(yintercept=20, linetype="dashed", color = "black")+
        geom_hline(yintercept=16, linetype="dotted", color = "black")

moody.70

moody.1975 <- subset (mood.T.pre, Yr == 1975)
moody.75 <- ggplot(moody.1975, aes(x=Month_Yr))+ 
        geom_line (aes(y=MaxTemp), color="#64ccc9", size=2) +
        theme_bw() +
        theme(axis.line = element_line(color = "black"),
              axis.text = element_text(size = 13),
              axis.title.x = element_text(size = 13, face = "bold"),
              axis.title.y = element_text(size = 13, face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(angle = 90,vjust = 0.5))+
        ylab(expression(bold(Temperature~(degree~C))))+
        xlab("Date")+
        geom_hline(yintercept=20, linetype="dashed", color = "black")+
        geom_hline(yintercept=16, linetype="dotted", color = "black")

moody.75

moody.1980 <- subset (mood.T.pre, Yr == 1980)
moody.80 <- ggplot(moody.1980, aes(x=Month_Yr))+ 
        geom_line (aes(y=MaxTemp), color="#64ccc9", size=2) +
        theme_bw() +
        theme(axis.line = element_line(color = "black"),
              axis.text = element_text(size = 13),
              axis.title.x = element_text(size = 13, face = "bold"),
              axis.title.y = element_text(size = 13, face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(angle = 90,vjust = 0.5))+
        ylab(expression(bold(Temperature~(degree~C))))+
        xlab("Date")+
        geom_hline(yintercept=20, linetype="dashed", color = "black")+
        geom_hline(yintercept=16, linetype="dotted", color = "black")

moody.80

moody.1981 <- subset (mood.T.pre, Yr == 1981)
moody.81 <- ggplot(moody.1981, aes(x=Month_Yr))+ 
        geom_line (aes(y=MaxTemp), color="#64ccc9", size=2) +
        theme_bw() +
        theme(axis.line = element_line(color = "black"),
              axis.text = element_text(size = 13),
              axis.title.x = element_text(size = 13, face = "bold"),
              axis.title.y = element_text(size = 13, face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(angle = 90,vjust = 0.5))+
        ylab(expression(bold(Temperature~(degree~C))))+
        xlab("Date")+
        geom_hline(yintercept=20, linetype="dashed", color = "black")+
        geom_hline(yintercept=16, linetype="dotted", color = "black")

moody.81

#Post-SWW Moody Year-by-year graphs-----
mood.T<-read_xlsx("Deschutes Temperatures_20220722.xlsx",
                  sheet = "Moody Post SWW Temperatures")

#add month-year column
mood.T$Month_Yr <- format(as.Date(mood.T$dateTime), "%m-%Y")
mood.T$Yr <- format(as.Date(mood.T$dateTime), "%Y")

moody.2012 <- subset (mood.T, Yr == 2012)
moody.12 <- ggplot(moody.2012, aes(x=Month_Yr))+ 
        geom_line (aes(y=MaxTemp), color="#ff585d", size=2) +
        theme_bw() +
        theme(axis.line = element_line(color = "black"),
              axis.text = element_text(size = 13),
              axis.title.x = element_text(size = 13, face = "bold"),
              axis.title.y = element_text(size = 13, face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(angle = 90,vjust = 0.5))+
        ylab(expression(bold(Temperature~(degree~C))))+
        xlab("Date")+
        geom_hline(yintercept=20, linetype="dashed", color = "black")+
        geom_hline(yintercept=16, linetype="dotted", color = "black")

moody.12

moody.2016 <- subset (mood.T, Yr == 2016)
moody.16 <- ggplot(moody.2016, aes(x=Month_Yr))+ 
        geom_line (aes(y=MaxTemp), color="#ff585d", size=2) +
        theme_bw() +
        theme(axis.line = element_line(color = "black"),
              axis.text = element_text(size = 13),
              axis.title.x = element_text(size = 13, face = "bold"),
              axis.title.y = element_text(size = 13, face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(angle = 90,vjust = 0.5))+
        ylab(expression(bold(Temperature~(degree~C))))+
        xlab("Date")+
        geom_hline(yintercept=20, linetype="dashed", color = "black")+
        geom_hline(yintercept=16, linetype="dotted", color = "black")

moody.16

moody.2021 <- subset (mood.T, Yr == 2021)
moody.21 <- ggplot(moody.2021, aes(x=Month_Yr))+ 
        geom_line (aes(y=MaxTemp), color="#ff585d", size=2) +
        theme_bw() +
        theme(axis.line = element_line(color = "black"),
              axis.text = element_text(size = 13),
              axis.title.x = element_text(size = 13, face = "bold"),
              axis.title.y = element_text(size = 13, face = "bold"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(angle = 90,vjust = 0.5))+
        ylab(expression(bold(Temperature~(degree~C))))+
        xlab("Date")+
        geom_hline(yintercept=20, linetype="dashed", color = "black")+
        geom_hline(yintercept=16, linetype="dotted", color = "black")

moody.21

moody.2021.max <- sapply(moody.2021$MaxTemp, max)
moody.2021.max

#Temp figures%%%---------
#Have to be careful and check time periods downloaded to ensure there 
#are actually solid data available for entire date range

temp <- "00010"
#Loop is set to only plot sites with temp data starting by 1 Jan 2012.
#Madras Gauge
madras <- "14092500"

#Moody Gauge
#temp only available here starting 29 July 2011
moody <- "14103000"

#Metolius at Grandview Gauge
metol <- "14091500"

#Deschutes at Culver Gauge
udes <- "14076500"

#Crooked below Opal Springs Gauge
crook <- "14087400"

#Create list of gages for loop----
gage.list <- list(madras, moody, metol, udes, crook)
#Make sure this is identical to the list prior...probably a more clever way to do this.
gage.names <- list("madras", "moody", "metol", "udes", "crook")

for(i in seq_along(gage.list)) 
{  site <- gage.list[i]

#Rest of script - sub "site" for the gauge code in the functions#
#2012-14
temp_pre1 <- readNWISuv(site,temp,"2012-01-01","2012-12-31")
temp_pre1$year <- format(temp_pre1$dateTime, "%Y")
temp_pre1 <- subset(temp_pre1, year != "2013")
temp_pre1$jdate <- yday(as.Date(temp_pre1$dateTime))
pre1_day <- aggregate(X_00010_00000 ~ jdate, data = temp_pre1, mean)

temp_pre2 <- readNWISuv(site,temp,"2013-01-01","2013-12-31")
temp_pre2$year <- format(temp_pre2$dateTime, "%Y")
temp_pre2 <- subset(temp_pre2, year != "2014")
temp_pre2$jdate <- yday(as.Date(temp_pre2$dateTime))
pre2_day <- aggregate(X_00010_00000 ~ jdate, data = temp_pre2, mean)

temp_pre3 <- readNWISuv(site,temp,"2014-01-01","2014-12-31")
temp_pre3$jdate <- yday(as.Date(temp_pre3$dateTime))
temp_pre3$year <- format(temp_pre3$dateTime, "%Y")
temp_pre3 <- subset(temp_pre3, year != "2015")
pre3_day <- aggregate(X_00010_00000 ~ jdate, data = temp_pre3, mean)

df_list <- list(pre1_day, pre2_day, pre3_day)
pre_all <- Reduce(function(x, y) merge(x, y, by= "jdate"), df_list, accumulate=FALSE)

pre_temp_site <- cbind(pre_all, meantemp = rowMeans(pre_all[2:4]))

#temp, post site: 2018-20----
#temp_post1 <- readNWISuv(site,temp,"2018-01-01","2018-12-31")
#temp_post1$year <- format(temp_post1$dateTime, "%Y")
#temp_post1 <- subset(temp_post1, year != "2019")
#temp_post1$jdate <- yday(as.Date(temp_post1$dateTime))
#post1_day <- aggregate(X_00010_00000 ~ jdate, data = temp_post1, mean)

#temp_post2 <- readNWISuv(site,temp,"2019-01-01","2019-12-31")
#temp_post2$year <- format(temp_post2$dateTime, "%Y")
#temp_post2 <- subset(temp_post2, year != "2020")
#temp_post2$jdate <- yday(as.Date(temp_post2$dateTime))
#post2_day <- aggregate(X_00010_00000 ~ jdate, data = temp_post2, mean)

#temp_post3 <- readNWISuv(site,temp,"2020-01-01","2020-12-31")
#temp_post3$year <- format(temp_post3$dateTime, "%Y")
#temp_post3 <- subset(temp_post3, year != "2021")
#temp_post3$jdate <- yday(as.Date(temp_post3$dateTime))
#post3_day <- aggregate(X_00010_00000 ~ jdate, data = temp_post3, mean)

#temp, post site: 2019-21----
temp_post1 <- readNWISuv(site,temp,"2019-01-01","2019-12-31")
temp_post1$year <- format(temp_post1$dateTime, "%Y")
temp_post1 <- subset(temp_post1, year != "2020")
temp_post1$jdate <- yday(as.Date(temp_post1$dateTime))
post1_day <- aggregate(X_00010_00000 ~ jdate, data = temp_post1, mean)

temp_post2 <- readNWISuv(site,temp,"2020-01-01","2020-12-31")
temp_post2$year <- format(temp_post2$dateTime, "%Y")
temp_post2 <- subset(temp_post2, year != "2021")
temp_post2$jdate <- yday(as.Date(temp_post2$dateTime))
post2_day <- aggregate(X_00010_00000 ~ jdate, data = temp_post2, mean)

temp_post3 <- readNWISuv(site,temp,"2021-01-01","2021-12-31")
temp_post3$year <- format(temp_post3$dateTime, "%Y")
temp_post3 <- subset(temp_post3, year != "2022")
temp_post3$jdate <- yday(as.Date(temp_post3$dateTime))
post3_day <- aggregate(X_00010_00000 ~ jdate, data = temp_post3, mean)

df_list <- list(post1_day, post2_day, post3_day)
post_all <- Reduce(function(x, y) merge(x, y, by= "jdate"), df_list, accumulate=FALSE)

post_temp_site <- cbind(post_all, meantemp = rowMeans(post_all[2:4]))

#head(pre_temp_site)
#pre_date <- as.data.frame(month.day.year(pre_temp_site$jdate))
#pre_temp_site$monthday <- as.Date(with(pre_date, paste(month, day,sep="-")), "%m-%d")
#pre_temp_site$monthday <- as.Date(pre_temp_site$monthday)

#then just output all of the plots
jpeg(filename=paste("temp.avg", gage.names[i], ".jpg"), 
     width = 600, height = 600, units = "px", pointsize = 12,
     quality = 300)


print(ggplot(pre_temp_site, aes(jdate, meantemp))+ 
        #geom_line(data=pre_temp_site, aes(jdate, meantemp, color="2008-10")) +
        #geom_line(data=post_temp_site, aes(jdate, meantemp, color="2017-19"))+
        geom_line(data=pre_temp_site, aes(y=rollmean(meantemp, 20, na.pad=TRUE), 
                                          color="2012-14"), size=1.5, na.rm = TRUE) +
        geom_line(data=post_temp_site, aes(y=rollmean(meantemp, 20, na.pad=TRUE), 
                                           color="2018-20"),size=1.5,na.rm = TRUE) +
        theme_bw() +
        scale_y_continuous(limits = c(3,20))+
        scale_color_manual(values=c("#999999", "#000000"))+
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
        theme(legend.title=element_blank(),
              legend.text=element_text(size=13))+
        theme(legend.position = c(0.05, 0.95),legend.justification = c(0, 1)))

dev.off()
} 

#Madras-Moody Effect%%%-----
temp <- "00010"

#2007-2009


#2012-14
#Madras Gauge "14092500"----------
mad12 <- readNWISuv("14092500",temp,"2012-01-01","2012-12-31")
mad12$year <- format(mad12$dateTime, "%Y")
mad12 <- subset(mad12, year != "2013")
mad12$jdate <- yday(as.Date(mad12$dateTime))
mad12_day <- aggregate(X_00010_00000 ~ jdate, data = mad12, mean)

mad13 <- readNWISuv("14092500",temp,"2013-01-01","2013-12-31")
mad13$year <- format(mad13$dateTime, "%Y")
mad13 <- subset(mad13, year != "2014")
mad13$jdate <- yday(as.Date(mad13$dateTime))
mad13_day <- aggregate(X_00010_00000 ~ jdate, data = mad13, mean)

mad14 <- readNWISuv("14092500",temp,"2014-01-01","2014-12-31")
mad14$year <- format(mad14$dateTime, "%Y")
mad14 <- subset(mad14, year != "2015")
mad14$jdate <- yday(as.Date(mad14$dateTime))
mad14_day <- aggregate(X_00010_00000 ~ jdate, data = mad14, mean)

df_list <- list(mad12_day, mad13_day, mad14_day)
pre_mad <- Reduce(function(x, y) merge(x, y, by= "jdate"), df_list, accumulate=FALSE)

names(pre_mad) <- c("jdate" ,"2012","2013","2014") 

#pre_temp_mad <- cbind(pre_mad, meantemp = rowMeans(pre_mad[2:4]))

#tidy
rm(df_list,mad12,mad13,mad14,mad12_day,mad13_day,mad14_day)

#Moody Gauge "14103000"----------
mood12 <- readNWISuv("14103000",temp,"2012-01-01","2012-12-31")
mood12$year <- format(mood12$dateTime, "%Y")
mood12 <- subset(mood12, year != "2013")
mood12$jdate <- yday(as.Date(mood12$dateTime))
mood12_day <- aggregate(X_00010_00000 ~ jdate, data = mood12, mean)

mood13 <- readNWISuv("14103000",temp,"2013-01-01","2013-12-31")
mood13$year <- format(mood13$dateTime, "%Y")
mood13 <- subset(mood13, year != "2014")
mood13$jdate <- yday(as.Date(mood13$dateTime))
mood13_day <- aggregate(X_00010_00000 ~ jdate, data = mood13, mean)

mood14 <- readNWISuv("14103000",temp,"2014-01-01","2014-12-31")
mood14$year <- format(mood14$dateTime, "%Y")
mood14 <- subset(mood14, year != "2015")
mood14$jdate <- yday(as.Date(mood14$dateTime))
mood14_day <- aggregate(X_00010_00000 ~ jdate, data = mood14, mean)

df_list <- list(mood12_day, mood13_day, mood14_day)
pre_mood <- Reduce(function(x, y) merge(x, y, by= "jdate"), df_list, accumulate=FALSE)

names(pre_mood) <- c("jdate" ,"2012","2013","2014")

#pre_temp_mood <- cbind(pre_mood, meantemp = rowMeans(pre_mood[2:4]))

#tidy
rm(df_list,mood12,mood13,mood14,mood12_day,mood13_day,mood14_day)

#temp, post site: 2018-20
#Madras Gauge "14092500"----------
mad18 <- readNWISuv("14092500",temp,"2018-01-01","2018-12-31")
mad18$year <- format(mad18$dateTime, "%Y")
mad18 <- subset(mad18, year != "2019")
mad18$jdate <- yday(as.Date(mad18$dateTime))
mad18_day <- aggregate(X_00010_00000 ~ jdate, data = mad18, mean)

mad19 <- readNWISuv("14092500",temp,"2019-01-01","2019-12-31")
mad19$year <- format(mad19$dateTime, "%Y")
mad19 <- subset(mad19, year != "2020")
mad19$jdate <- yday(as.Date(mad19$dateTime))
mad19_day <- aggregate(X_00010_00000 ~ jdate, data = mad19, mean)

mad20 <- readNWISuv("14092500",temp,"2020-01-01","2020-12-31")
mad20$year <- format(mad20$dateTime, "%Y")
mad20 <- subset(mad20, year != "2021")
mad20$jdate <- yday(as.Date(mad20$dateTime))
mad20_day <- aggregate(X_00010_00000 ~ jdate, data = mad20, mean)

df_list <- list(mad18_day, mad19_day, mad20_day)
post_mad <- Reduce(function(x, y) merge(x, y, by= "jdate"), df_list, accumulate=FALSE)

names(post_mad) <- c("jdate" ,"2018","2019","2020") 

#post_temp_mad <- cbind(post_mad, meantemp = rowMeans(post_mad[2:4]))

#tidy
rm(df_list,mad18,mad19,mad20,mad18_day,mad19_day,mad20_day)

#Moody Gauge "14103000"----------
mood18 <- readNWISuv("14103000",temp,"2018-01-01","2018-12-31")
mood18$year <- format(mood18$dateTime, "%Y")
mood18 <- subset(mood18, year != "2019")
mood18$jdate <- yday(as.Date(mood18$dateTime))
mood18_day <- aggregate(X_00010_00000 ~ jdate, data = mood18, mean)

mood19 <- readNWISuv("14103000",temp,"2019-01-01","2019-12-31")
mood19$year <- format(mood19$dateTime, "%Y")
mood19 <- subset(mood19, year != "2020")
mood19$jdate <- yday(as.Date(mood19$dateTime))
mood19_day <- aggregate(X_00010_00000 ~ jdate, data = mood19, mean)

mood20 <- readNWISuv("14103000",temp,"2020-01-01","2020-12-31")
mood20$year <- format(mood20$dateTime, "%Y")
mood20 <- subset(mood20, year != "2021")
mood20$jdate <- yday(as.Date(mood20$dateTime))
mood20_day <- aggregate(X_00010_00000 ~ jdate, data = mood20, mean)

df_list <- list(mood18_day, mood19_day, mood20_day)
post_mood <- Reduce(function(x, y) merge(x, y, by= "jdate"), df_list, accumulate=FALSE)

names(post_mood) <- c("jdate" ,"2018","2019","2020")

#post_temp_mood <- cbind(post_mood, meantemp = rowMeans(post_mood[2:4]))

#tidy
rm(df_list,mood18,mood19,mood20,mood18_day,mood19_day,mood20_day)

#combine for analysis----
pre_mad$loc <- "madras"
post_mad$loc <- "madras"
pre_mood$loc <- "moody"
post_mood$loc <- "moody"

#melt from wide to long
pre_mad_long <- gather(pre_mad, year, tempC, "2012":"2014",factor_key = TRUE)
pre_mood_long <- gather(pre_mood, year, tempC, "2012":"2014",factor_key = TRUE)
post_mad_long <- gather(post_mad, year, tempC, "2018":"2020",factor_key = TRUE)
post_mood_long <- gather(post_mood, year, tempC, "2018":"2020",factor_key = TRUE)

#combine datasets
mad_mood <- rbind(pre_mad_long,
                  post_mad_long,
                  pre_mood_long,
                  post_mood_long)

#Plot------
#2012
madmood12<-ggplot(mad_mood [mad_mood$year == "2012",], 
                  aes(x = jdate,
                      y = tempC,
                      fill = loc))+ 
        geom_point(size = 2, shape = 21, color = "black")+
        scale_fill_manual(values = c("#64CCC9","#7E7F74"))+
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
        xlab("Julian Date")

madmood12+labs(fill = "Station")

ggsave(file=paste0("Madras Moody Temp 2012",
                   ".png"),
       madmood12+labs(fill = "Station"), width = 6.5,
       height = 4.5, units = "in", dpi = "600")

#2013
madmood20<-ggplot(mad_mood [mad_mood$year == "2020",], 
                  aes(x = jdate,
                      y = tempC,
                      fill = loc))+ 
        geom_point(size = 2, shape = 21, color = "black")+
        scale_fill_manual(values = c("#64CCC9","#7E7F74"))+
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
        xlab("Julian Date")

madmood20+labs(fill = "Station")

