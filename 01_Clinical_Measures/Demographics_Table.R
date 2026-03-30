#-------------------------Demographics for CAPs in CPP Publication-------------------------#

# Salome Häuselmann (salome.haeuselmann@gmail.com), Winter 2025

# SETTING UP R ------------------------------------------------------------

# Clear the workplace (remove all data objects and variables)
rm(list = ls())

#Load packages: 
pacman::p_load(tidyverse, dplyr, xlsx, readxl, ggplot2, ggpubr, forcats, cowplot, 
               plyr, rstatix, reshape2, writexl, car, lawstat, multcomp, tidyr,
               gtsummary, gridExtra, gt,lmerTest, MuMIn, gg.gap, lme4, Gmisc, lubridate, magick,dplyr)


#Set path
setwd("")


# 1. Load & Cleaning up  -------------------------------------------------------------

#Load data
CRF_total<-read_xlsx("")
names(CRF)

# 2. Statistics  -------------------------------------------------------------

## 2.1 Age  ------------------------

CRF_total %>%
  group_by(group)%>%
  get_summary_stats(age, type = "mean_sd")

# Inspect distribution
ggplot(CRF_total, aes(x = age, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Age distribution by group")

# Test normality (Shapiro-Wilk test) in each group
shapiro.test(CRF_total$age[CRF_total$group == "CPP"]) 
shapiro.test(CRF_total$age[CRF_total$group == "HC"]) 

# Check for variance equality (optional)
var.test(age ~ group, data = CRF_total)

# Choose the appropriate test
# If both groups are approximately normal and variances are equal -> t-test
# If variances differ -> Welch's t-test
# If non-normal -> Wilcoxon test

# Welch's t-test (recommended unless normality/variance assumptions clearly hold)
# t.test(age ~ group, data = CRF_total, var.equal = TRUE)
t.test(age ~ group, data = CRF_total, var.equal = FALSE)

## 2.2 Sex  ------------------------

table(CRF_total$group, CRF_total$gender)

# Run chi-square test
chisq.test(table(CRF_total$group, CRF_total$gender))
chisq.test(table(CRF_total$group, CRF_total$gender), correct = FALSE)

## 2.3 Smoker  ------------------------

table(CRF_total$group, CRF_total$smoke)
chisq.test(table(CRF_total$group, CRF_total$smoke))
chisq.test(table(CRF_total$group, CRF_total$smoke), correct = FALSE)

## 2.4 Menopause  ------------------------

table(CRF_total$group, CRF_total$menopause)
# Subset only female participants
CRF_female <- CRF_total[CRF_total$gender == 2, ]
table(CRF_female$group, CRF_female$menopause)
chisq.test(table(CRF_female$group, CRF_female$menopause))
chisq.test(table(CRF_female$group, CRF_female$menopause),correct = FALSE)
fisher.test(table(CRF_female$group, CRF_female$menopause)) # Fisher’s test is more reliable when expected counts are small

## 2.5 Menstrual Cycle  ------------------------
# Group-wise counts of menstrual cycle categories
table(CRF_female$group, CRF_female$menstrualcycle)

chisq.test(table(CRF_female$group, CRF_female$menstrualcycle))
chisq.test(table(CRF_female$group, CRF_female$menstrualcycle), correct = FALSE)
fisher.test(table(CRF_female$group, CRF_female$menstrualcycle))# Fisher’s test is more reliable when expected counts are small

## 2.6 Psychotropic Medication  ------------------------
table(CRF_total$group, CRF_total$psychMed)
chisq.test(table(CRF_total$group, CRF_total$psychMed))
chisq.test(table(CRF_total$group, CRF_total$psychMed), correct = FALSE)
fisher.test(table(CRF_total$group, CRF_total$psychMed))

## 2.6 Non-opoid analgesics Medication  ------------------------
table(CRF_total$group, CRF_total$pain_med)
chisq.test(table(CRF_total$group, CRF_total$pain_med))
chisq.test(table(CRF_total$group, CRF_total$pain_med), correct = FALSE)
fisher.test(table(CRF_total$group, CRF_total$pain_med))

## 2.7  Corticosteroids Medication  ------------------------
table(CRF_total$group, CRF_total$CortMed)
chisq.test(table(CRF_total$group, CRF_total$CortMed))
chisq.test(table(CRF_total$group, CRF_total$CortMed),correct = FALSE)
fisher.test(table(CRF_total$group, CRF_total$CortMed))

## 2.8  Hormonal contraception  ------------------------
table(CRF_female$group, CRF_female$SexMed)
chisq.test(table(CRF_female$group, CRF_female$SexMed))
chisq.test(table(CRF_female$group, CRF_female$SexMed),correct = FALSE)
fisher.test(table(CRF_female$group, CRF_female$SexMed))

## 2.9 BDI  ------------------------

CRF_total %>%
  group_by(group)%>%
  get_summary_stats(bdi, type = "mean_sd")

# Inspect distribution
ggplot(CRF_total, aes(x = bdi, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "bdi distribution by group")

# Test normality (Shapiro-Wilk test) in each group
shapiro.test(CRF_total$bdi[CRF_total$group == "CPP"]) 
shapiro.test(CRF_total$bdi[CRF_total$group == "HC"]) 

# Check for variance equality (optional)
var.test(bdi ~ group, data = CRF_total)

#  Wilcoxon rank-sum test (also known as the Mann–Whitney U test) is the non-parametric alternative to the t-test
wilcox.test(bdi ~ group, data = CRF_total, exact = FALSE)

## 3.0 STAI-I  ------------------------

CRF_total %>%
  group_by(group)%>%
  get_summary_stats(stai1, type = "mean_sd")

# Inspect distribution
ggplot(CRF_total, aes(x = stai1, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "bdi distribution by group")

# Test normality (Shapiro-Wilk test) in each group
shapiro.test(CRF_total$stai1[CRF_total$group == "CPP"]) 
shapiro.test(CRF_total$stai1[CRF_total$group == "HC"]) 

# Check for variance equality (optional)
var.test(stai1 ~ group, data = CRF_total)


# Welch's t-test (recommended unless normality/variance assumptions clearly hold)
t.test(stai1 ~ group, data = CRF_total, var.equal = FALSE) #Use the Welch’s two-sample t-test, which does not assume equal variances.

## 3.1 STAI-II  ------------------------

CRF_total %>%
  group_by(group)%>%
  get_summary_stats(stai2, type = "mean_sd")

# Inspect distribution
ggplot(CRF_total, aes(x = stai2, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "stai2 distribution by group")

# Test normality (Shapiro-Wilk test) in each group
shapiro.test(CRF_total$stai2[CRF_total$group == "CPP"]) 
shapiro.test(CRF_total$stai2[CRF_total$group == "HC"])

# Check for variance equality (optional)
var.test(stai2 ~ group, data = CRF_total)


# Welch's t-test (recommended unless normality/variance assumptions clearly hold)
t.test(stai2 ~ group, data = CRF_total, var.equal = FALSE) #Use the Welch’s two-sample t-test, which does not assume equal variances.

## 3.2 LSEQ  ------------------------

CRF_total %>%
  group_by(group)%>%
  get_summary_stats(LSEQ_mean, type = "mean_sd")

# Inspect distribution
ggplot(CRF_total, aes(x = LSEQ_mean, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "LSEQ_mean distribution by group")

# Test normality (Shapiro-Wilk test) in each group
shapiro.test(CRF_total$LSEQ_mean[CRF_total$group == "CPP"]) 
shapiro.test(CRF_total$LSEQ_mean[CRF_total$group == "HC"]) 

# Check for variance equality (optional)
var.test(LSEQ_mean ~ group, data = CRF_total)

# Welch's t-test (recommended unless normality/variance assumptions clearly hold)
t.test(LSEQ_mean ~ group, data = CRF_total, var.equal = FALSE) #Use the Welch’s two-sample t-test, which does not assume equal variances.


## 3.2 Symptome Duration  ------------------------

CRF_total %>%
  group_by(group)%>%
  get_summary_stats(symptom_duration, type = "mean_sd")

## 3.3 SSS  ------------------------

CRF_total %>%
  group_by(group)%>%
  get_summary_stats(sss, type = "mean_sd")


## 3.4 WPI  ------------------------

CRF_total %>%
  group_by(group)%>%
  get_summary_stats(wpi, type = "mean_sd")

## 3.5 Pain intensity (current)  ------------------------

CRF_total %>%
  group_by(group)%>%
  get_summary_stats(bpi_moment, type = "mean_sd")

## 3.6 BPI severity  ------------------------

CRF_total %>%
  group_by(group)%>%
  get_summary_stats(bpi_intensity, type = "mean_sd")

## 3.7 BPI inference  ------------------------

CRF_total %>%
  group_by(group)%>%
  get_summary_stats(bpi_consequences, type = "mean_sd")


## 3.8 Peg Algometry - ears  ------------------------

CRF_total %>%
  group_by(group)%>%
  get_summary_stats(algopeg_ear, type = "mean_sd")

# Inspect distribution
ggplot(CRF_total, aes(x = algopeg_ear, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "algopeg_ear distribution by group")

# Test normality (Shapiro-Wilk test) in each group
shapiro.test(CRF_total$algopeg_ear[CRF_total$group == "CPP"]) 
shapiro.test(CRF_total$algopeg_ear[CRF_total$group == "HC"]) 

# Check for variance equality (optional)
var.test(algopeg_ear ~ group, data = CRF_total)

#  Wilcoxon rank-sum test (also known as the Mann–Whitney U test) is the non-parametric alternative to the t-test
wilcox.test(algopeg_ear ~ group, data = CRF_total, exact = FALSE)

## 3.9 Peg Algometry - fingers  ------------------------

CRF_total %>%
  group_by(group)%>%
  get_summary_stats(algopeg_finger, type = "mean_sd")

# Inspect distribution
ggplot(CRF_total, aes(x = algopeg_finger, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "algopeg_finger distribution by group")

# Test normality (Shapiro-Wilk test) in each group
shapiro.test(CRF_total$algopeg_finger[CRF_total$group == "CPP"]) 
shapiro.test(CRF_total$algopeg_finger[CRF_total$group == "HC"]) 

# Check for variance equality (optional)
var.test(algopeg_finger ~ group, data = CRF_total)

#  Wilcoxon rank-sum test (also known as the Mann–Whitney U test) is the non-parametric alternative to the t-test
wilcox.test(algopeg_finger ~ group, data = CRF_total, exact = FALSE)

## 4.0 Peg Algometry - mean  ------------------------

CRF_total %>%
  group_by(group)%>%
  get_summary_stats(algopeg_mean, type = "mean_sd")

# Inspect distribution
ggplot(CRF_total, aes(x = algopeg_mean, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "algopeg_finger distribution by group")

# Test normality (Shapiro-Wilk test) in each group
shapiro.test(CRF_total$algopeg_mean[CRF_total$group == "CPP"]) 
shapiro.test(CRF_total$algopeg_mean[CRF_total$group == "HC"]) 

# Check for variance equality (optional)
var.test(algopeg_mean ~ group, data = CRF_total)

# Welch's t-test (recommended unless normality/variance assumptions clearly hold)
t.test(algopeg_mean ~ group, data = CRF_total, var.equal = FALSE) #Use the Welch’s two-sample t-test, which does not assume equal variances.

# Fit the linear model
model <- lm(algopeg_mean ~ group + age + gender + LSEQ_mean + acutestress  + sum_stai2_bdi + psychMed + pain_med, 
            data = CRF_total)

# View summary of results
summary(model)

## 4.1 CTQ - total  ------------------------

CRF_total %>%
  group_by(group)%>%
  get_summary_stats(ctq_total, type = "mean_sd")

# Inspect distribution
ggplot(CRF_total, aes(x = ctq_total, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "ctq_total distribution by group")

# Test normality (Shapiro-Wilk test) in each group
shapiro.test(CRF_total$ctq_total[CRF_total$group == "CPP"]) 
shapiro.test(CRF_total$ctq_total[CRF_total$group == "HC"]) 

# Check for variance equality (optional)
var.test(ctq_total ~ group, data = CRF_total)

#  Wilcoxon rank-sum test (also known as the Mann–Whitney U test) is the non-parametric alternative to the t-test
wilcox.test(ctq_total ~ group, data = CRF_total, exact = FALSE)

# 4.2 Acute Stress  ------------------------

CRF_total %>%
  group_by(group)%>%
  get_summary_stats(acutestress, type = "mean_sd")

# Inspect distribution
ggplot(CRF_total, aes(x = acutestress, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "acutestress distribution by group")

# Test normality (Shapiro-Wilk test) in each group
shapiro.test(CRF_total$acutestress[CRF_total$group == "CPP"]) 
shapiro.test(CRF_total$acutestress[CRF_total$group == "HC"]) 

# Check for variance equality (optional)
var.test(acutestress ~ group, data = CRF_total)

#  Wilcoxon rank-sum test (also known as the Mann–Whitney U test) is the non-parametric alternative to the t-test
wilcox.test(acutestress ~ group, data = CRF_total, exact = FALSE)

# 4.2 PSS  ------------------------

CRF_total %>%
  group_by(group)%>%
  get_summary_stats(pss, type = "mean_sd")

# Inspect distribution
ggplot(CRF_total, aes(x = pss, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "pss distribution by group")

# Test normality (Shapiro-Wilk test) in each group
shapiro.test(CRF_total$pss[CRF_total$group == "CPP"]) 
shapiro.test(CRF_total$pss[CRF_total$group == "HC"]) 

# Check for variance equality (optional)
var.test(pss ~ group, data = CRF_total)

#  Wilcoxon rank-sum test (also known as the Mann–Whitney U test) is the non-parametric alternative to the t-test
wilcox.test(pss ~ group, data = CRF_total, exact = FALSE)

# 4.3 Amylase  ------------------------

CRF_total %>%
  group_by(group)%>%
  get_summary_stats(amylase_mean, type = "mean_sd")

# Inspect distribution
ggplot(CRF_total, aes(x = amylase_mean, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "amylase_mean distribution by group")

# Test normality (Shapiro-Wilk test) in each group
shapiro.test(CRF_total$amylase_mean[CRF_total$group == "CPP"]) 
shapiro.test(CRF_total$amylase_mean[CRF_total$group == "HC"]) 

# Check for variance equality (optional)
var.test(amylase_mean ~ group, data = CRF_total)


#  Wilcoxon rank-sum test (also known as the Mann–Whitney U test) is the non-parametric alternative to the t-test
wilcox.test(amylase_mean ~ group, data = CRF_total, exact = FALSE)

# Fit the linear model
model <- lm(amylase_mean ~ group + age + gender + SexMed + menstrualcycle  + smoke, 
            data = CRF_total)
summary(model)
DCSAov2 <- aov(model)
summary(DCSAov2)
# View summary of results
summary(model)

# 4.3 Cortisol  ------------------------

CRF_total %>%
  group_by(group)%>%
  get_summary_stats(AUCi_delay, type = "mean_sd")

# Inspect distribution
ggplot(CRF_total, aes(x = AUCi_delay, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(title = "AUCi_delay distribution by group")

# Test normality (Shapiro-Wilk test) in each group
shapiro.test(CRF_total$AUCi_delay[CRF_total$group == "CPP"]) 
shapiro.test(CRF_total$AUCi_delay[CRF_total$group == "HC"]) 

# Check for variance equality (optional)
var.test(AUCi_delay ~ group, data = CRF_total)

#  Wilcoxon rank-sum test (also known as the Mann–Whitney U test) is the non-parametric alternative to the t-test
wilcox.test(AUCi_delay ~ group, data = CRF_total, exact = FALSE)

# Fit the linear model
model <- lm(AUCi_delay ~ group, 
            data = CRF_total)

# View summary of results
summary(model)

# Fit the linear model
model <- lm(AUCi_delay ~ group + age + gender + SexMed + menstrualcycle  + smoke, 
            data = CRF_total)

# View summary of results
summary(model)



