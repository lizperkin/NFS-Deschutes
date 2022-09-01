#Creating figures of Temp at different points in McKenzie River (OR) basin
#using data from USGS gauges.
#SWW tower completed in 2004
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

#SF McKenzie Temp Data Availability----
sfMcKPostT <- readNWISdata(sites="14159500",
                          parameterCd="00010", 
                          startDate="2005-01-01",endDate="2022-01-01")

sfMcKPreT <- readNWISdata(sites="14159500",
                         parameterCd="00010", 
                         startDate="1956-01-01",endDate="2003-01-01")

#Vida Gauge McKenzie
vidaPostT <- readNWISdata(sites="14162500",
                           parameterCd="00010", 
                           startDate="2005-01-01",endDate="2022-01-01")

vidaPreT <- readNWISdata(sites="14162500",
                          parameterCd="00010", 
                          startDate="1962-01-01",endDate="2003-01-01")

#Harrisburg Gauge Willamette (climate change reference)
harrPostT <- readNWISdata(sites="14166000",
                          parameterCd="00010", 
                          startDate="2005-01-01",endDate="2022-01-01")

harrPreT <- readNWISdata(sites="14166000",
                         parameterCd="00010", 
                         startDate="1962-01-01",endDate="2003-01-01")

#create output excel file
wb <- createWorkbook()
addWorksheet(wb, "SF McK Post SWW Temperatures")
writeDataTable(wb, sheet = "SF McK Post SWW Temperatures",
               x = sfMcKPostT,
               withFilter = F, tableStyle = "none")

addWorksheet(wb, "SF McK Pre SWW Temperatures")
writeDataTable(wb, sheet = "SF McK Pre SWW Temperatures",
               x = sfMcKPreT,
               withFilter = F, tableStyle = "none")

addWorksheet(wb, "Vida Post SWW Temperatures")
writeDataTable(wb, sheet = "Vida Post SWW Temperatures",
               x = vidaPostT,
               withFilter = F, tableStyle = "none")

addWorksheet(wb, "Vida Pre SWW Temperatures")
writeDataTable(wb, sheet = "Vida Pre SWW Temperatures",
               x = vidaPreT,
               withFilter = F, tableStyle = "none")

addWorksheet(wb, "Harrisburg Post Temperatures")
writeDataTable(wb, sheet = "Harrisburg Post Temperatures",
               x = harrPostT,
               withFilter = F, tableStyle = "none")

addWorksheet(wb, "Harrisburg Pre Temperatures")
writeDataTable(wb, sheet = "Harrisburg Pre Temperatures",
               x = harrPreT,
               withFilter = F, tableStyle = "none")

saveWorkbook(wb, paste0("McKenzie-Willamette Temperatures_",
                        format(Sys.time(), "%Y%m%d"), ".xlsx"),
             overwrite = F)

#SF McK Pre-SWW Temp Figs----
sfMcKPre.T<-read_xlsx("McKenzie-Willamette Temperatures_20220722.xlsx",
                      sheet = "SF McK Pre SWW Temperatures")

#add month-year column
sfMcKPre.T$Month_Yr <- format(as.Date(sfMcKPre.T$dateTime), "%m-%Y")
sfMcKPre.T$Yr <- format(as.Date(sfMcKPre.T$dateTime), "%Y")

#Pre-SWW SF McKenzie Year-by-year graphs-----
sfMcK.1956 <- subset (sfMcKPre.T, Yr == 1956)
sfMcK.56 <- ggplot(sfMcK.1956, aes(x=Month_Yr))+ 
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

sfMcK.56

#SF McK Post-SWW Temp Figs----
sfMcKPost.T<-read_xlsx("McKenzie-Willamette Temperatures_20220722.xlsx",
                      sheet = "SF McK Post SWW Temperatures")

#add month-year column
sfMcKPost.T$Month_Yr <- format(as.Date(sfMcKPost.T$dateTime), "%m-%Y")
sfMcKPost.T$Yr <- format(as.Date(sfMcKPost.T$dateTime), "%Y")

#Post-SWW SF McKenzie Year-by-year graphs-----
sfMcK.2021 <- subset (sfMcKPost.T, Yr == 2021)
sfMcK.21 <- ggplot(sfMcK.2021, aes(x=Month_Yr))+ 
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

sfMcK.21

#Willamette Pre Temp Figs----
willPre.T<-read_xlsx("McKenzie-Willamette Temperatures_20220722.xlsx",
                      sheet = "Harrisburg Pre Temperatures")

#add month-year column
willPre.T$Month_Yr <- format(as.Date(willPre.T$dateTime), "%m-%Y")
willPre.T$Yr <- format(as.Date(willPre.T$dateTime), "%Y")

#Pre Willamette Year-by-year graphs-----
will.1965 <- subset (willPre.T, Yr == 1965)
will.65 <- ggplot(will.1965, aes(x=Month_Yr))+ 
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

will.65

will.1981 <- subset (willPre.T, Yr == 1981)
will.81 <- ggplot(will.1981, aes(x=Month_Yr))+ 
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

will.81

#Willamette Post Temp Figs----
willPost.T<-read_xlsx("McKenzie-Willamette Temperatures_20220722.xlsx",
                       sheet = "Harrisburg Post Temperatures")

#add month-year column
willPost.T$Month_Yr <- format(as.Date(willPost.T$dateTime), "%m-%Y")
willPost.T$Yr <- format(as.Date(willPost.T$dateTime), "%Y")

#Post Willamette Year-by-year graphs-----
will.2021 <- subset (willPost.T, Yr == 2021)
will.21 <- ggplot(will.2021, aes(x=Month_Yr))+ 
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

will.21
