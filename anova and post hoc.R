library(readr)
library(ggplot2)
#install.packages("forecast")
library(forecast)

#install.packages("fpp2")
#library(fpp2)
library(dplyr)
library(lubridate)
library(quantmod)
library(tseries)
library(xts)
library(tidyverse)
library(tibble)
#install.packages("writexl")
library("writexl")
#library(readxl)


######################## MCAR 5 % ANOVA for mean, fore, reg amd MI ##############

AR <- c(46.83,73.07,128.6,52.58,91.50,65.97)  #mean
mean(AR)

Mf <- c(63.31,87.34,123.01,36.50,100.72,148.5) #fore
mean(Mf)

ARR <- c(42.55, 105.86, 30.18, 31.62, 56.93, 73.49) #reg
mean(ARR)

MfF <- c(171.0, 238.0, 87.33, 230.0, 229.7, 211.0) #MI
mean(MfF)

Values<-c(AR,Mf,ARR,MfF)
methods <-c(rep("Mean_imp",6),rep("Fore_imp",6),rep("Reg_imp",6),rep("Mult_imp",6))
mcar_5 <-cbind(Values,methods)
mcar_d5 <-data.frame(mcar_5)
mcar.aov <- aov(Values~methods, data = mcar_d5) #ANOVA TEST
summary(mcar.aov)
TukeyHSD(mcar.aov) #TukeyHSD test #MI was the worst


#10b MCAR 
AR_10 <- c(75.31,87.62,85.58,74.48,60.41,55.28) #mean
mean(AR_10)
Mf1 <- c(130.77,131.83,85.36,123.83,87.98,128.72) #fore
mean(Mf1)
ARR_10 <- c(58.55, 39.11, 40.05, 50.90, 58.62, 65.02) #reg
mean(ARR_10)
Mf11 <- c(279.8, 113.2, 157.17,  307.0, 162.00, 223.33) #Mult
mean(Mf11)


Values_10<-c(AR_10,Mf1,AR_10,Mf11)
methods_10 <-c(rep("Mean_imp",6),rep("Fore_imp",6),rep("Reg_imp",6),rep("Mult_imp",6))
mcar_10 <-cbind(Values_10,methods_10)
mcar_d10 <-data.frame(mcar_10)
mcar.aov_10 <- aov(Values_10~methods_10, data = mcar_d10) #ANOVA TEST
summary(mcar.aov_10)

TukeyHSD(mcar.aov_10) #TukeyHSD test 


####################################### end ###############################
#########################MAR 5% ##########################################
MAR_5 <- c(46.31, 101, 54.40, 63.30, 32.211 ,67.541) #mean
mean(MAR_5)
ARf_5 <- c(89.87, 111.08, 109.72, 124.4, 87.70, 76.50) #fore
mean(ARf_5)
MARR_5 <- c(43.60, 28.61, 46.97, 39.65, 55.33, 33.21) #reg
mean(MARR_5)
ARfF_5 <- c( 161.0, 153.3, 64.0, 109.7, 256.0, 179.3) #Mult
mean(ARfF_5)


Values_MAR5<-c(MAR_5,ARf_5,MARR_5,ARfF_5)
methods_MAR5 <-c(rep("Mean_imp",6),rep("Fore_imp",6),rep("Reg_imp",6),rep("Mult_imp",6))
mar_5 <-cbind(Values_MAR5,methods_MAR5)
mar_d5 <-data.frame(mar_5)
mar.aov_5 <- aov(Values_MAR5~methods_MAR5, data = mar_d5) #ANOVA TEST
summary(mar.aov_5)

TukeyHSD(mar.aov_5) #TukeyHSD test


#10%
MAR_10 <- c(37.95,44.02,48.68,44.80,57.35,47.42)# mean
mean(MAR_10)
ARf_10 <- c(108.79,97.29,97.13,85.38,90.27,95.02) #fore
mean(ARf_10)
MARR_10 <- c(50.57, 62.50, 58.60, 55.29, 54.27, 64.72) #reg
mean(MARR_10)
ARff_10 <- c(49.33, 155.67, 239.5, 191.8, 151.33, 110.83) #mult
mean(ARff_10)

Values_MAR10<-c(MAR_10,ARf_10,MARR_10,ARff_10)
methods_MAR10 <-c(rep("Mean_imp",6),rep("Fore_imp",6),rep("Reg_imp",6),rep("Mult_imp",6))
mar_10 <-cbind(Values_MAR10,methods_MAR10)
mar_d10 <-data.frame(mar_10)
mar.aov_10 <- aov(Values_MAR10~methods_MAR10, data = mar_d10) #ANOVA TEST
summary(mar.aov_10)

TukeyHSD(mar.aov_10) #TukeyHSD test

#######################END#####################################################
###############################MNAR 5 % and 10% ##############################
# 5%
NAR_5 <- c(134.2,168.8,165.7,150.2,149.7,154.2)#mean
mean(NAR_5)

NAR_55 <- c(105.94,123.80,190.50,36.14,85.07,102.26) #fore
mean(NAR_55)

NARR_555 <- c(60.99, 112.05, 115.20, 114.42, 91.63, 94.78)#reg
mean(NARR_555)


NAR_5555 <- c( 176.0, 174.3, 257.3, 212.3, 373.0, 256.3)#mult
mean(NAR_5555)

Values_MNAR5<-c(NAR_5,NAR_55,NARR_555,NAR_5555)
methods_MNAR5 <-c(rep("Mean_imp",6),rep("Fore_imp",6),rep("Reg_imp",6),rep("Mult_imp",6))
mnar_5 <-cbind(Values_MNAR5,methods_MNAR5)
mnar_d5 <-data.frame(mnar_5)
mnar.aov_5 <- aov(Values_MNAR5~methods_MNAR5, data = mnar_d5) #ANOVA TEST
summary(mnar.aov_5)

TukeyHSD(mnar.aov_5) #TukeyHSD test

# 10%
NAR_10 <- c(211.75,190.49,230.57,210.36,185.44,198.03)#mean
mean(NAR_10)

NAR_100 <- c(156.98,111.15,89.88,113.16,110.47,114.86)#fore
mean(NAR_100)

NARR_1000 <- c(75.25, 76.14, 79.31, 76.85, 77.68, 77.43)#reg
mean(NARR_1000)

NAR_10000 <- c(202.5, 336.2, 197.7, 336.7, 269.0, 211.67)# mult
mean(NAR_10000)

Values_MNAR10<-c(NAR_10,NAR_100,NARR_1000,NAR_10000)
methods_MNAR10 <-c(rep("Mean_imp",6),rep("Fore_imp",6),rep("Reg_imp",6),rep("Mult_imp",6))
mnar_10 <-cbind(Values_MNAR10,methods_MNAR10)
mnar_d10 <-data.frame(mnar_10)
mnar.aov_10 <- aov(Values_MNAR10~methods_MNAR10, data = mnar_d10) #ANOVA TEST
summary(mnar.aov_10)
TukeyHSD(mnar.aov_10) #TukeyHSD test