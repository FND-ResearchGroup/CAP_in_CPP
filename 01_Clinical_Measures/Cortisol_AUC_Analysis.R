#------------------------- Cortisol Area-under-the-curve (AUC) analysis for CAPs in CPP Publication -------------------------#

# Salome Häuselmann (salome.haeuselmann@gmail.com), Winter 2025

# SETTING UP R ------------------------------------------------------------

# Clear the workplace (remove all data objects and variables)
rm(list = ls())


#Load packages: 
pacman::p_load(tidyverse, dplyr, xlsx, readxl, ggplot2, ggpubr, forcats, cowplot, 
               plyr, rstatix, reshape2, writexl, car, lawstat, multcomp, tidyr,
               gtsummary, gridExtra, gt,lmerTest, MuMIn, gg.gap, lme4, Gmisc, lubridate, 
               RColorBrewer, wesanderson, gridGraphics, plotly, maditr, psych, corrplot,openxlsx)


# Set working directory
DataPath = ""
setwd(DataPath) 

# Read in Excel file
Cort <- read_xlsx("")
Quest<- read_xlsx("")
numSubjects <- nrow(Cort) / 9
CRF <- read.xlsx("")
head(CRF)
CRF <- CRF %>%
  mutate(p_code = ifelse(nchar(p_code) == 2, paste0("P0", p_code), 
                     ifelse(nchar(p_code) == 3, paste0("P", p_code), p_code)))
CRF <- CRF %>% select(-X1)

# 1. Reorganize data --------------------------------------------------------
# Create new order of data frame with each subject in one row
CAR <- pivot_wider(data=Cort,
                   id_cols = c("p_code","group","delay_1","delay_2","delay_3","delay_4","delay_5","delay_6","delay_7","delay_8","delay_9"),
                   names_from = c("timepoint"),
                   values_from = "Cortisol")

for (timepoint in c("1", "2", "3", "4", "5", "6", "7", "8", "9")) {
  CAR[[paste0("TI_real_", timepoint)]] <- NA
}
for (i in 1:nrow(CAR)) {
  CAR$TI_real_1 [i] <- 0 + CAR$delay_1[i]
  CAR$TI_real_2[i] <- 15 + CAR$delay_2[i]
  CAR$TI_real_3[i] <- 15 + CAR$delay_3[i]
  CAR$TI_real_4[i] <- 15 + CAR$delay_4[i]
  CAR$TI_real_5[i] <- 15 + CAR$delay_5[i]
  CAR$TI_real_6[i] <- 0 + CAR$delay_6[i]
  CAR$TI_real_7[i] <- 60 + CAR$delay_7[i]
  CAR$TI_real_8[i] <- 60 + CAR$delay_8[i]
  CAR$TI_real_9[i] <- 60 + CAR$delay_9[i]
}
for (timepoint in c("1", "2", "3", "4", "5", "6", "7", "8", "9")) {
  CAR[[paste0("Time_real_", timepoint)]] <- NA
}
for (i in 1:nrow(CAR)) {
  CAR$Time_real_1 [i] <- CAR$TI_real_1[i]
  CAR$Time_real_2[i] <- CAR$TI_real_1[i] + CAR$TI_real_2[i]
  CAR$Time_real_3[i] <- CAR$TI_real_1[i] + CAR$TI_real_2[i] + CAR$TI_real_3[i]
  CAR$Time_real_4[i] <- CAR$TI_real_1[i] + CAR$TI_real_2[i] + CAR$TI_real_3[i] + CAR$TI_real_4[i]
  CAR$Time_real_5[i] <- CAR$TI_real_1[i] + CAR$TI_real_2[i] + CAR$TI_real_3[i] + CAR$TI_real_4[i] + CAR$TI_real_5[i]
  CAR$Time_real_6[i] <- CAR$TI_real_6[i]
  CAR$Time_real_7[i] <- CAR$TI_real_6[i] + CAR$TI_real_7[i]
  CAR$Time_real_8[i] <- CAR$TI_real_6[i] + CAR$TI_real_7[i] + CAR$TI_real_8[i]
  CAR$Time_real_9[i] <- CAR$TI_real_6[i] + CAR$TI_real_7[i] + CAR$TI_real_8[i] + CAR$TI_real_9[i]
}

#2. Calculate Diurnal Cortisol with ideal time steps (AUCg, AUCi, DCS, CAR) --------------------------------------------------------
##2.1. Area-under-the-curve with respect to ground (AUCg)  --------------------------------------------------------
###2.1.2 AUGg in the morning (AUGg_AM) - Post awakening corisol concentration PACC  --------------------------------------------------------
AUCg_AM <- numeric(numSubjects)
t1 <- 15  
t3 <- 60 
for (i in 1:numSubjects) {
  AUCg_AM[i] <- (((CAR[i, "wake-up"] + CAR[i, "15'"]) * t1 / 2) + ((CAR[i, "15'"] + CAR[i, "30'"]) * t1 / 2) + ((CAR[i, "30'"] + CAR[i, "45'"]) * t1 / 2)+ ((CAR[i, "45'"] + CAR[i, "60'"]) * t1 / 2))
}
CAR$AUCg_AM <- AUCg_AM

### 2.1.2 AUGg in the afternoon (AUGg_PM) - durinal basline corisol concentration DBCC  --------------------------------------------------------
AUCg_PM <- numeric(numSubjects)
t1 <- 15  
t3 <- 60 
for (i in 1:numSubjects) {
  AUCg_PM[i] <- (((CAR[i, "2 p.m."] + CAR[i, "3 p.m."]) * t3 / 2) + ((CAR[i, "3 p.m."] + CAR[i, "4 p.m."]) * t3 / 2) + ((CAR[i, "4 p.m."] + CAR[i, "5 p.m."]) * t3 / 2))
}
CAR$AUCg_PM <- AUCg_PM

## 2.2. Area under Curve with respect to increase (AUCi) (Just morning)  --------------------------------------------------------
AUCi <- numeric(numSubjects)
for (i in 1:numSubjects) {
  AUCi[i] <- AUCg_AM[i] - (CAR[i, "wake-up"] * (4 * t1))
}
CAR$AUCi <- AUCi


#3. Calculation Baseline Cortisol with Delays  --------------------------------------------------------
## 3.1. Area-under-the-curve with respect to ground (AUCg) with real time intervals (TI)/Delays  --------------------------------------------------------
### 3.1.2 AUGg in the morning (AUGg_AM_delay) - Post awakening corisol concentration PACC  --------------------------------------------------------
AUCg_AM_delay <- numeric(numSubjects)
for (i in 1:numSubjects) {
  AUCg_AM_delay[i] <- ((CAR[i, "wake-up"] + CAR[i, "15'"]) * ((CAR[i,"Time_real_2"]-CAR[i,"Time_real_1"]) / 2)) + ((CAR[i, "15'"] + CAR[i, "30'"]) * ((CAR[i,"Time_real_3"]-CAR[i,"Time_real_2"]) / 2)) + ((CAR[i, "30'"] + CAR[i, "45'"]) * ((CAR[i,"Time_real_4"]-CAR[i,"Time_real_3"]) / 2))+ ((CAR[i, "45'"] + CAR[i, "60'"]) * ((CAR[i,"Time_real_5"]-CAR[i,"Time_real_4"]) / 2))
}
CAR$AUCg_AM_delay <- AUCg_AM_delay

### 3.1.2 AUGg in the afternoon (AUGg_PM_delay) - durinal basline corisol concentration DBCC  --------------------------------------------------------
AUCg_PM_delay <- numeric(numSubjects)
for (i in 1:numSubjects) {
  AUCg_PM_delay[i] <- (((CAR[i, "2 p.m."] + CAR[i, "3 p.m."]) * ((CAR[i,"Time_real_7"]-CAR[i,"Time_real_6"]) / 2)) + ((CAR[i, "3 p.m."] + CAR[i, "4 p.m."]) * ((CAR[i,"Time_real_8"]-CAR[i,"Time_real_7"]) / 2)) + ((CAR[i, "4 p.m."] + CAR[i, "5 p.m."]) * ((CAR[i,"Time_real_9"]-CAR[i,"Time_real_8"]) / 2)))
}
CAR$AUCg_PM_delay <- AUCg_PM_delay

## 3.2. Area under Curve with respect to increase (AUCi_delay) (Just morning)  --------------------------------------------------------
AUCi_delay <- numeric(numSubjects)
for (i in 1:numSubjects) {
  AUCi_delay[i] <- AUCg_AM[i] - (CAR[i, "wake-up"] * ((CAR[i,"Time_real_2"]-CAR[i,"Time_real_1"])+(CAR[i,"Time_real_3"]-CAR[i,"Time_real_2"])+(CAR[i,"Time_real_4"]-CAR[i,"Time_real_3"])+(CAR[i,"Time_real_5"]-CAR[i,"Time_real_4"])))
}
CAR$AUCi_delay <- AUCi_delay

# 4. Remove Outliers  --------------------------------------------------------
## Find non-adherers
Exclude <- filter(CAR, delay_1 > 5 | delay_1 < -5) # if delay is > 5 min
print(Exclude[1]) #P070 has to be excluded
Exclude <- filter(CAR, delay_2 > 5 | delay_2 < -5) # if delay is > 5 min
print(Exclude[1])
Exclude <- filter(CAR, delay_3 > 5 | delay_3 < -5) # if delay is > 5 min
print(Exclude[1])
Exclude <- filter(CAR, delay_4 > 5 | delay_4 < -5) # if delay is > 5 min
print(Exclude[1])
Exclude <- filter(CAR, delay_5 > 5 | delay_5 < -5) # if delay is > 5 min
print(Exclude[1])
Exclude <- filter(CAR, delay_6 > 15 | delay_6 < -15) # if delay is > 15 min
print(Exclude[1])
Exclude <- filter(CAR, delay_7 > 15 | delay_7 < -15) # if delay is > 15 min
print(Exclude[1])
Exclude <- filter(CAR, delay_8 > 15 | delay_8 < -15) # if delay is > 15 min
print(Exclude[1])
Exclude <- filter(CAR, delay_9 > 15 | delay_9 < -15) # if delay is > 15 min
print(Exclude[1])

## Handling missing data

# 5. Statistics & Plotting #  --------------------------------------------------------

m <- ggplot(data = DCS, aes(x=group,y=AUCg_PM_delay,fill=group)) +
  stat_summary(fun = mean,geom = "bar")+ 
  stat_summary(fun.data = mean_ci, geom = "errorbar", width = 0.05) +
  scale_fill_manual(values = c("#c1d4dd", "#96989c"))+ 
  theme_classic()+
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  ggtitle("AUCg PM") + theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+  
  theme(axis.title.x=element_blank(),axis.text.y = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 18, face = "bold"), plot.title = element_text(hjust = 0.5))+ 
  stat_compare_means(label = "p.signif", method="wilcox.test", comparisons = list(c("HC", "CPP")), label.y = 300, label.x=1) + ylab("DBCC")
m


