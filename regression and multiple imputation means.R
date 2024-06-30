library(readr)
library(ggplot2)
#install.packages("forecast")
library(forecast)

#install.packages("fpp2")
#library(fpp2)
library(dplyr)
library(lubridate)
#library(quantmod)
library(tseries)
library(xts)
library(tidyverse)
library(tibble)
#install.packages("writexl")
library("writexl")
#library(readxl)
############################ Regression Method ###############
####################### MCAR 5% ##############################
#mean for 5%
ARR <- c(42.55, 105.86, 30.18, 31.62, 56.93, 73.49)
MCAR_mean5<- mean(ARR)
MCAR_mean5

#median for 5%
AA <-  c(43.67, 76.12, 29.48, 32.72, 46.35, 26.01)
median_mean_5 <- mean(AA)
median_mean_5

#mean for 10 %
ARR_10 <- c(58.55, 39.11, 40.05, 50.90, 58.62, 65.02)
MCAR_mean10 <- mean(ARR_10)
MCAR_mean10
#median for 10%
BB <- c(55.59, 29.99, 43.92, 45.81, 39.57, 46.34)
median_mean_10 <- mean(BB)
median_mean_10
############################ END ################################
####################MAR 5 % and 10 %#############################
#mean for the means
MARR_5 <- c(43.60, 28.61, 46.97, 39.65, 55.33, 33.21)
MAR_mean5 <- mean(MARR_5)
MAR_mean5

#mean for median
CC <- c(46.86, 20.96, 54.31, 43.67, 54.31, 31.81)
meadian_mean_5 <- mean(CC)
meadian_mean_5

#mean for 10%
MARR_10 <- c(50.57, 62.50, 58.60, 55.29, 54.27, 64.72)
MAR_mean10 <- mean(MARR_10)
MAR_mean10
#median for 10
DD <- c(50.59, 65.15, 53.46, 50.58, 50.58, 65.22)
meadian_M_10 <- mean(DD)
meadian_M_10
########################### END ##################################
#mean for 5%
NARR_5 <- c(60.99, 112.05, 115.20, 114.42, 91.63, 94.78)
MNAR_mean_5 <- mean(NARR_5)
MNAR_mean_5
#median for 5%
EE <- c(79.85, 82.20, 89.29, 89.29, 79.85, 89.29)
median_M_5 <- mean(EE)
median_M_5
#10% mean
NARR_10 <- c(75.25, 76.14, 79.31, 76.85, 77.68, 77.43)
MNAR_mean_10 <- mean(NARR_10)
MNAR_mean_10
# 10% median
FFF <- c(81.02, 81.02,  81.02, 81.02, 81.02, 81.02)
median_MNAR_10 <- mean(FFF)
median_MNAR_10
########################################################################################################
############################### Multiple Imputation 5 and 10 #####################################
# MCAR 5%
#mean for 5
MfF <- c(171.0, 238.0, 87.33, 230.0, 229.7, 211.0)
MCARf_5 <- mean(MfF)
MCARf_5
#nedian 5%
AAA <-c(187.0, 259.0, 20.00, 260.0, 337.0, 260.0)
MCARf_med5 <- mean(AAA)
MCARf_med5
# MCAR mean 10 %
Mf11 <- c(279.8, 113.2, 157.17,  307.0, 162.00, 223.33)
MCARf_10 <- mean(Mf11)
MCARf_10
#median 10%
BBB <- c(299.5, 73.0, 58.00,  245.5, 156.50, 130.00)
MCARfmed_10<- mean(BBB)
MCARfmed_10
####################################### MAR 5 and 10%##################################
#mean
ARfF_5 <- c( 161.0, 153.3, 64.0, 109.7, 256.0, 179.3)
MARf_5 <- mean(ARfF_5)
MARf_5
#median
CCc <- c(28.0, 71.0, 28.0, 122.0, 319.0, 90.0)
MARfmed_5 <- mean(CCc)
MARfmed_5
#mean 10%
ARff_10 <- c(49.33, 155.67, 239.5, 191.8, 151.33, 110.83)
MARf_10 <- mean(ARff_10)
MARf_10
#median 10%
DDd  <- c(57.50, 62.50, 212.0, 51.0, 42.50, 76.50)
MARfmed_10 <- mean(DDd)
MARfmed_10
#############################MNAR 5% and 10%###############################################
#mean 5%
NAR_55 <- c( 176.0, 174.3, 257.3, 212.3, 373.0, 256.3)
MNARFMEAN_5 <- mean(NAR_55)
MNARFMEAN_5
#median 5%
EEe <- c(236.0, 187.0, 308.0, 169.0, 350.0, 244.0)
MNARFMEd_5<- mean(EEe)
MNARFMEd_5
#mean 5%
NAR_100 <- c(202.5, 336.2, 197.7, 336.7, 269.0, 211.67)
MNARFMEAN_10 <-mean(NAR_100)
MNARFMEAN_10
#median 10%
FFFf<- c(168.0, 380.0, 226.5, 369.5, 280.0, 180.50)
MNARFMEd_10 <- mean(FFFf)
MNARFMEd_10


####################### t-test ###########################################################
# MCAR regression method and Multiple imputation method 5 %
#t-test
t_test_result <- t.test(AAA, AA)
t_test_result

#MAR regression method and Multiple imputation method 5%

#t-test
t_test_result <- t.test(BBB,BB)
t_test_result

#MNAR regression method and Multiple imputation method 5%
#t-test
t_test_result <- t.test(CCc, CC)
t_test_result
###########################################################################################
# now for all the 10% 
# MCAR regression method and Multiple imputation method 10 %
t_test_result <- t.test(DDd, DD)
t_test_result

#MAR regression method and Multiple imputation method 10%
t_test_result <- t.test(EEe, EE)
t_test_result

#MNAR regression method and Multiple imputation method 10%
t_test_result <- t.test(FFFf, FFF)
t_test_result
