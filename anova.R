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


####################### MCAR 5% ##############################
#mean for 5%
# AR <- c(46.83,73.07,128.6,52.58,91.50,65.97)
# MCAR_mean5<- mean(AR)
# MCAR_mean5
# #median for 5%
# 
# # A <-  c(51.70,51.70,142.7,32.22,107,71.40)
# # median_mean_5 <- mean(A)
# # median_mean_5
# 
# #mean for 10 %
# AR_10 <- c(75.31,87.62,85.58,74.48,60.41,55.28)
# MCAR_mean10 <- mean(AR_10)
# MCAR_mean10
#median for 10%
# B <- c(80.89,61.35,86.95,72.85,47.60,41.55)
# median_mean_10 <- mean(B)
# median_mean_10
############################ END ################################
####################MAR 5 % and 10 %#############################
#mean for the means
# MAR_5 <- c(46.31,101,54.40,63.30,32.211,67.541)
# MAR_mean5 <- mean(MAR_5)
# MAR_mean5
#mean for median
# C <- c(41.67,93,65.30,86,41.67,41.11)
# meadian_mean_5 <- mean(C)
# meadian_mean_5

#mean for 10%
# MAR_10 <- c(37.95,44.02,48.68,44.80,57.35,47.42)
# MAR_mean10 <- mean(MAR_10)
# MAR_mean10
#median for 10
# D <- c(27.85,42,42,42,42,42)
# meadian_M_10 <- mean(D)
# meadian_M_10
########################### END ##################################
#mean for 5%
NAR_5 <- c(134.2,168.8,165.7,150.2,149.7,154.2)
MNAR_mean_5 <- mean(NAR_5)
MNAR_mean_5
#median for 5%
# E <- c(212.45,207.5,170,85.5,209,99.5)
# median_M_5 <- mean(E)
# median_M_5
#10% mean
# NAR_10 <- c(211.75,190.49,230.57,210.36,185.44,198.03)
# MNAR_mean_10 <- mean(NAR_10)
# MNAR_mean_10
# 10% median
# FF <- c(207,131.38,217.42,185,131.38,148)
# median_MNAR_10 <- mean(FF)
# median_MNAR_10
########################################################################################################
############################### Forecasting 5 and 10 #####################################

#mean for 5
# Mf <- c(63.31,87.34,123.01,36.50,100.72,148.5)
# MCARf_5 <- mean(Mf)
# MCARf_5
#median 5%
# AA <-c(38.25,122.09,143.51,24.03,129.66,146.9)
# MCARf_med5 <- mean(AA)
# MCARf_med5
# mean 10 %
# Mf1 <- c(130.77,131.83,85.36,123.83,87.98,128.72)
# MCARf_10 <- mean(Mf1)
# MCARf_10
#median 10%
# BB <- c(103.94,134.50,101.17,107.43,79.67,115.12)
# MCARfmed_10<- mean(BB)
# MCARfmed_10
####################################### MAR 5 and 10%##################################
#mean
# ARf_5 <- c(89.87,111.08,109,72,124.4,87.70,76.50)
# MARf_5 <- mean(ARf_5)
# MARf_5
#median
# CC <- c(128.88,110.6,122.14,122.1,122.13,100.57)
# MARfmed_5 <- mean(CC)
# MARfmed_5
#mean 10%
# ARf_10 <- c(108.79,97.29,97.13,85.38,90.27,95.02)
# MARf_10 <- mean(ARf_10)
# MARf_10
# #median 10%
# DD  <- c(116.36,110.14,109.68,105.58,105.58,105.58)
# MARfmed_10 <- mean(DD)
# MARfmed_10
#############################MNAR 5% and 10%###############################################
#mean 5%
# NAR_5 <- c(105.94,123.80,190.50,36.14,85.07,102.26)
# MNARFMEAN_5 <- mean(NAR_5)
# MNARFMEAN_5
#median 5%
# EE <- c(84.32,88.97,172.4,24.03,84.32,110.32)
# MNARFMEd_5<- mean(EE)
# MNARFMEd_5
#mean 5%
# NAR_10 <- c(156.98,111.15,89.88,113.16,110.47,114.86)
# MNARFMEAN_10 <-mean(NAR_10)
# MNARFMEAN_10
#median 10%
# FFF<- c(128.37,105.99,54.17,112.02,103.94,117.12)
# MNARFMEd_10 <- mean(FFF)
# MNARFMEd_10
# 
# ############################ Regression Method ###############
####################### MCAR 5% ##############################
#mean for 5%
# ARR <- c(42.55, 105.86, 30.18, 31.62, 56.93, 73.49)
# MCAR_mean5<- mean(ARR)
# MCAR_mean5

#median for 5%
# AA <-  c(43.67, 76.12, 29.48, 32.72, 46.35, 26.01)
# median_mean_5 <- mean(AA)
# median_mean_5

#mean for 10 %
# ARR_10 <- c(58.55, 39.11, 40.05, 50.90, 58.62, 65.02)
# MCAR_mean10 <- mean(ARR_10)
# MCAR_mean10
# #median for 10%
# BB <- c(55.59, 29.99, 43.92, 45.81, 39.57, 46.34)
# median_mean_10 <- mean(BB)
# median_mean_10
############################ END ################################
####################MAR 5 % and 10 %#############################
#mean for the means
# MARR_5 <- c(43.60, 28.61, 46.97, 39.65, 55.33, 33.21)
# MAR_mean5 <- mean(MARR_5)
# MAR_mean5

#mean for median
# CC <- c(46.86, 20.96, 54.31, 43.67, 54.31, 31.81)
# meadian_mean_5 <- mean(CC)
# meadian_mean_5

#mean for 10%
# MARR_10 <- c(50.57, 62.50, 58.60, 55.29, 54.27, 64.72)
# MAR_mean10 <- mean(MARR_10)
# MAR_mean10
#median for 10
# DD <- c(50.59, 65.15, 53.46, 50.58, 50.58, 65.22)
# meadian_M_10 <- mean(DD)
# meadian_M_10
########################### END ##################################
#mean for 5%
# NARR_5 <- c(60.99, 112.05, 115.20, 114.42, 91.63, 94.78)
# MNAR_mean_5 <- mean(NARR_5)
# MNAR_mean_5
# #median for 5%
# # EE <- c(79.85, 82.20, 89.29, 89.29, 79.85, 89.29)
# # median_M_5 <- mean(EE)
# # median_M_5
# #10% mean
# NARR_10 <- c(75.25, 76.14, 79.31, 76.85, 77.68, 77.43)
# MNAR_mean_10 <- mean(NARR_10)
# MNAR_mean_10
# 10% median
# FFF <- c(81.02, 81.02,  81.02, 81.02, 81.02, 81.02)
# median_MNAR_10 <- mean(FFF)
# median_MNAR_10
########################################################################################################
############################### Multiple Imputation 5 and 10 #####################################
# MCAR 5%
#mean for 5
# MfF <- c(171.0, 238.0, 87.33, 230.0, 229.7, 211.0)
# MCARf_5 <- mean(MfF)
# MCARf_5
# #nedian 5%
# # AAA <-c(187.0, 259.0, 20.00, 260.0, 337.0, 260.0)
# # MCARf_med5 <- mean(AAA)
# # MCARf_med5
# # MCAR mean 10 %
# Mf11 <- c(279.8, 113.2, 157.17,  307.0, 162.00, 223.33)
# MCARf_10 <- mean(Mf11)
# MCARf_10
#median 10%
# BBB <- c(299.5, 73.0, 58.00,  245.5, 156.50, 130.00)
# MCARfmed_10<- mean(BBB)
# MCARfmed_10
####################################### MAR 5 and 10%##################################
#mean
# ARfF_5 <- c( 161.0, 153.3, 64.0, 109.7, 256.0, 179.3)
# MARf_5 <- mean(ARfF_5)
# MARf_5
#median
# CCc <- c(28.0, 71.0, 28.0, 122.0, 319.0, 90.0)
# MARfmed_5 <- mean(CCc)
# MARfmed_5
#mean 10%
# ARff_10 <- c(49.33, 155.67, 239.5, 191.8, 151.33, 110.83)
# MARf_10 <- mean(ARff_10)
# MARf_10
#median 10%
# DDd  <- c(57.50, 62.50, 212.0, 51.0, 42.50, 76.50)
# MARfmed_10 <- mean(DDd)
# MARfmed_10
#############################MNAR 5% and 10%###############################################
#mean 5%
# NAR_55 <- c( 176.0, 174.3, 257.3, 212.3, 373.0, 256.3)
# MNARFMEAN_5 <- mean(NAR_55)
# MNARFMEAN_5
#median 5%
# EEe <- c(236.0, 187.0, 308.0, 169.0, 350.0, 244.0)
# MNARFMEd_5<- mean(EEe)
# MNARFMEd_5
#mean 5%
# NAR_100 <- c(202.5, 336.2, 197.7, 336.7, 269.0, 211.67)
# MNARFMEAN_10 <-mean(NAR_100)
# MNARFMEAN_10
#median 10%
# FFFf<- c(168.0, 380.0, 226.5, 369.5, 280.0, 180.50)
# MNARFMEd_10 <- mean(FFFf)
# MNARFMEd_10
