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
library(readxl)
#deaths <- read_excel("deaths.xlsx")
#deaths <- read_excel("/home/ntokozo/Documents/research/death.xlsx", sheet = "Data", col_types = c("numeric","text", "numeric"))
deaths <- read_excel("/home/ntokozo/Documents/research/Ntokozon.xlsx", sheet = "Sheet1", col_types = c("numeric","text", "numeric"))
deaths
#View(deaths)
deathsmean <- c(deaths$deaths_due_to_severe_malaria)
mean1<- mean(deathsmean)
 ##########################creating missingness MCAR 5% ###########################################################################################################
 # Create a copy of the original data frame to modify
 deaths_55 <- deaths
 View(deaths_55)
 # Define the range of rows to introduce missing values
 row_range <- 73:132
 View(row_range)
 # Calculate the number of rows to set as NA (5% of the total rows in the specified range)
 num_rows <- length(row_range)
 num_missing <- ceiling(num_rows * 0.05)
 
 # Randomly select the rows to set as NA
 set.seed(1)  # For reproducibility; you can remove this for actual randomness
 missing_rows <- sample(row_range, num_missing)
 
 # Set the selected rows' 'deaths_due_to_severe_malaria' column to NA
 deaths_55$deaths_due_to_severe_malaria[missing_rows] <- NA
 
 # Print the modified data frame
 print(deaths_55)
 View(deaths_55)  #  View data
 ####################Mean Imputation#############################
 # Identify rows with missing values
 missing_indices <- which(is.na(deaths_55$deaths_due_to_severe_malaria))
 
 deaths_55$month_index <- rep(1:12, length.out = nrow(deaths_55))
 deaths_55
 View(deaths_55)
 deaths_cutt <- deaths
 View(deaths_cutt)
 deaths_cutt$month_index <- rep(1:12, length.out = nrow(deaths_cutt))
 
 # Calculate the mean for each month where values are not missing
 mean_imputation <- deaths_55 %>%
   group_by(month_index) %>%
   summarise(mean_cases = mean(deaths_due_to_severe_malaria, na.rm = TRUE))
 
 # Join the original data with the mean imputation
 imputed_data <- deaths_55 %>%
   left_join(mean_imputation, by = "month_index") %>%
   mutate(deaths_due_to_severe_malaria = ifelse(is.na(deaths_due_to_severe_malaria), mean_cases, deaths_due_to_severe_malaria)) %>%
   select(-mean_cases)
 
 print(imputed_data)
 View(imputed_data)
 #differences <- list()
 differences <- numeric(length(missing_indices))
 for(i in missing_indices){differences[[as.character(i)]] <- abs(deaths_cutt$deaths_due_to_severe_malaria[i]-imputed_data$deaths_due_to_severe_malaria[i])}
 for (i in missing_indices){print(paste("differences for row",i,":" ,differences[[as.character(i)]]))}
 #print(differences)
 View(differences)
 ################################Ends here#########################################
 ###########################creating mean#########################################
 mean_value_MCAR_5 <- mean(differences[4:6])
 mean_value_MCAR_5
 ci <- t.test(differences[4:6])$conf.int
 summary(ci)
 ########################################### MCAR 10% ###############################################################
 # Create a copy of the original data frame to modify
 deaths
 deaths_10 <- deaths

 # Define the range of rows to introduce missing values
 row_range <- 73:132

 # Calculate the number of rows to set as NA (10% of the total rows in the specified range)
 num_rows_10 <- length(row_range)
 num_missing_10 <- ceiling(num_rows_10 * 0.1)

 # Randomly select the rows to set as NA
 set.seed(1)  # For reproducibility; you can remove this for actual randomness
 missing_rows_10 <- sample(row_range, num_missing_10)

 # Set the selected rows' 'deaths_due_to_severe_malaria' column to NA
 deaths_10$deaths_due_to_severe_malaria[missing_rows_10] <- NA

 # Print the modified data frame
 print(deaths_10)
 View(deaths_10)  # View data

 ####################Mean Imputation#############################
 # Identify rows with missing values
 deaths
 missing_rows_10 <- which(is.na(deaths_10$deaths_due_to_severe_malaria))

 deaths_10$month_index <- rep(1:12, length.out = nrow(deaths_10))
 deaths_10

 deaths_ori <- deaths
 deaths_ori$month_index <- rep(1:12, length.out = nrow(deaths_ori))
 View(deaths_ori)
 # Computing the mean for each month where values are not missing
 mean_imputation <- deaths_10 %>%
   group_by(month_index) %>%
   summarise(mean_cases = mean(deaths_due_to_severe_malaria, na.rm = TRUE))


 # Joining the original data with the mean imputation
 imputed_data_10 <- deaths_10 %>%
   left_join(mean_imputation, by = "month_index") %>%
   mutate(deaths_due_to_severe_malaria = ifelse(is.na(deaths_due_to_severe_malaria), mean_cases, deaths_due_to_severe_malaria)) %>%
   select(-mean_cases)

 print(imputed_data_10)
 View(imputed_data_10)

 diffmean <- numeric(length(missing_rows_10))
 diffmean

 for(i in missing_rows_10){diffmean[[as.character(i)]] <- print(abs(deaths_ori$deaths_due_to_severe_malaria[i]-imputed_data_10$deaths_due_to_severe_malaria[i]))}

 for (i in missing_rows_10){print(paste("differences for row",i,":" ,diffmean[[as.character(i)]]))}
 #print(differences)
 View(diffmean)

 mean_value <- mean(diffmean[7:12])
 mean_value
 ci <- t.test(diffmean[7:12])$conf.int
 summary(ci)

 #t-test
 t_test_result <- t.test(difforecast, diffmean[7:12])
 print(t_test_result)

 ##################################################################################################################################################
 ########################forecasting method 5% ######################################
 # Slice the first 10 rows
 deaths
 death_2<-deaths[1:72, ]
 #view(death_2)
 names(deaths)
 nrow(deaths)
 #deaths$Time <- seq(1,72)

 ### Temporary series analysis NU PF INT(Temporary series analysis NU PF INT)
 Timeseries_BI <-  ts(death_2$deaths_due_to_severe_malaria, start = c(2013,1), end= c(2018,12), frequency=12)
 Timeseries_BI
 #View(Timeseries_BI)


 Acf(Timeseries_BI, plot = TRUE)
 pacf(Timeseries_BI,plot = TRUE)

 adf.test(Timeseries_BI) #Augment Dickey-Fuller Test: Null is non-stationary and Alt is stationary
 kpss.test(Timeseries_BI) #KPSS: Null is Stationary and Alt is non stationary
 ndiffs(Timeseries_BI) #how many differences needed
 nsdiffs(Timeseries_BI) #how many seasonal differences needed

 MMM <- diff(Timeseries_BI, lag = 12,differences = 1)
 Acf(MMM)
 Pacf(MMM)

 fit_Timeseries_BI <- auto.arima(Timeseries_BI)
 fit_Timeseries_BI
 checkresiduals(fit_Timeseries_BI)

 fit3 <- Arima(Timeseries_BI, order=c(1,0,1), seasonal=c(1,1,1),include.constant = TRUE)
 fit3
 fit8 <- Arima(Timeseries_BI, order=c(1,0,1), seasonal=c(0,1,1),include.constant = TRUE)
 fit8

 fit4 <- Arima(Timeseries, order=c(2,0,1), seasonal=c(0,1,0))
 fit4

 fit5 <- Arima(Timeseries, order=c(2,0,2), seasonal=c(1,1,0))
 fit5

 # h = 10*12 because, forecast is for 10 years for all 12 months
 ffcast_fit_Timeseries_BI <-forecast(fit_Timeseries_BI, level=c(95), h=1*12)
 fore_values<-ffcast_fit_Timeseries_BI[["mean"]]
 #view(fore_values)
 ################################################################################
 combined_column <- c(death_2$deaths_due_to_severe_malaria, fore_values)
 df <- data.frame(combined_column = combined_column)
 df

 #View(combined_column)

 Timeseries_1 <-  ts(df$combined_column, start = c(2013,1), end= c(2019,12), frequency=12)
 Timeseries_1
 fit_Timeseries_1 <- auto.arima(Timeseries_1)
 fit_Timeseries_1
 ffcast_fit_Timeseries_1 <-forecast(fit_Timeseries_1, level=c(95), h=1*12)
 fore_values1<-ffcast_fit_Timeseries_1[["mean"]]
 #view(fore_values1)
 combined_column1 <- c(df$combined_column, fore_values1)
 df1 <- data.frame(combined_column1 = combined_column1)
 df1

 Timeseries_2 <-  ts(df1$combined_column1, start = c(2013,1), end= c(2020,12), frequency=12)
 Timeseries_2
 fit_Timeseries_2 <- auto.arima(Timeseries_2)
 fit_Timeseries_2
 ffcast_fit_Timeseries_2 <-forecast(fit_Timeseries_2, level=c(95), h=1*12)
 fore_values2<-ffcast_fit_Timeseries_2[["mean"]]
 #view(fore_values2)
 combined_column2 <- c(df1$combined_column1, fore_values2)
 df2 <- data.frame(combined_column2 = combined_column2)
 df2

 Timeseries_3 <-  ts(df2$combined_column2, start = c(2013,1), end= c(2021,12), frequency=12)
 Timeseries_3
 fit_Timeseries_3 <- auto.arima(Timeseries_3)
 fit_Timeseries_3
 ffcast_fit_Timeseries_3 <-forecast(fit_Timeseries_3, level=c(95), h=1*12)
 fore_values3<-ffcast_fit_Timeseries_3[["mean"]]
 #view(fore_values3)
 combined_column3 <- c(df2$combined_column2, fore_values3)
 df3 <- data.frame(combined_column3 = combined_column3)
 df3

 Timeseries_4 <-  ts(df3$combined_column3, start = c(2013,1), end= c(2022,12), frequency=12)
 Timeseries_4
 fit_Timeseries_4 <- auto.arima(Timeseries_4)
 fit_Timeseries_4
 ffcast_fit_Timeseries_4 <-forecast(fit_Timeseries_4, level=c(95), h=1*12)
 fore_values4<-ffcast_fit_Timeseries_4[["mean"]]
 #view(fore_values4)
 combined_column4 <- c(df3$combined_column3, fore_values4)
 dfinal <- data.frame(combined_column4 = combined_column4)
 dfinal

 names(dfinal)[names(dfinal) == "combined_column4"] <- "deaths_forecast_5years"
 View(dfinal)
 names(dfinal)[names(dfinal) == "deaths_due_to_severe_malaria"] <- "deaths_forecast_5years"
 View(dfinal)
 deaths
 deaths <- cbind(deaths,dfinal)

 # Create a copy of the original data frame to modify
 forecast_MCAR <-deaths[deaths$Time>72,]
 # Define the range of rows to introduce missing values
 row_range <- 1:60

 # Calculate the number of rows to set as NA (5% of the total rows in the specified range)
 num_rows <- length(row_range)
 num_missing_force <- ceiling(num_rows * 0.05)
 num_missing_force
 # Randomly select the rows to set as NA
 set.seed(1)  # For reproducibility; you can remove this for actual randomness
 missing_rows_force <- sample(row_range, num_missing_force)
 missing_rows_force
 # Set the selected rows' 'deaths_due_to_severe_malaria' column to NA
 forecast_MCAR$deaths_due_to_severe_malaria[missing_rows_force] <- NA

 # Print the modified data frame
 View(forecast_MCAR)  #  View data
 forecast_MCAR
 deaths_cut <- deaths[73:132,]

 forecast_MCAR <- forecast_MCAR %>%
   mutate(deaths_due_to_severe_malaria = ifelse(is.na(deaths_due_to_severe_malaria), deaths_forecast_5years, deaths_due_to_severe_malaria))

 # Print the imputed dataset
 print(forecast_MCAR)

 difore <- numeric(length(missing_rows_force))
 for(i in missing_rows_force){difore[[as.character(i)]] <- print(abs(deaths_cut$deaths_due_to_severe_malaria[i]-forecast_MCAR$deaths_due_to_severe_malaria[i]))}

 for (i in missing_rows_force){print(paste("differences for row",i,":" ,difore[[as.character(i)]]))}
 #print(differences)
 View(difore)

 mean_value_fore5 <- mean(difore[4:6])
 mean_value_fore5
 ci <- t.test(difore[4:6])$conf.int
 ci
 summary(ci)

 #t-test
 t_test_result <- t.test(difore[4:6], differences[4:6])
 t_test_result
 ################################################################ forecasting for 10% missing #############################################
 deaths
 forecast_MCAR_10 <-deaths[deaths$Time>72,]
 # Define the range of rows to introduce missing values
 row_range <- 1:60

 # Calculate the number of rows to set as NA (5% of the total rows in the specified range)
 num_rows <- length(row_range)
 num_missing_force_10 <- ceiling(num_rows * 0.1)
 num_missing_force_10
 # Randomly select the rows to set as NA
 set.seed(1)  # For reproducibility; you can remove this for actual randomness
 missing_rows_force_10 <- sample(row_range, num_missing_force_10)
 missing_rows_force_10
 # Set the selected rows' 'deaths_due_to_severe_malaria' column to NA
 forecast_MCAR_10$deaths_due_to_severe_malaria[missing_rows_force_10] <- NA

 # Print the modified data frame
 View(forecast_MCAR_10)  #  View data

 forecast_MCAR_10 <- forecast_MCAR_10 %>%
   mutate(deaths_due_to_severe_malaria = ifelse(is.na(deaths_due_to_severe_malaria), deaths_forecast_5years, deaths_due_to_severe_malaria))

 # Print the imputed dataset
 print(forecast_MCAR_10)

 diffore_10 <- numeric(length(missing_rows_force_10))

 diffore_10
 deaths_cuttt <- deaths[73:132,]
 for(i in missing_rows_force_10){diffore_10[[as.character(i)]] <- print(abs(deaths_cuttt$deaths_due_to_severe_malaria[i]-forecast_MCAR_10$deaths_due_to_severe_malaria[i]))}

 for (i in missing_rows_force_10){print(paste("differences for row",i,":" ,diffore_10[[as.character(i)]]))}
 #print(differences)
 View(diffore_10)

 mean_value_fore10 <- mean(diffore_10[7:12])
 mean_value_fore10
 ci <- t.test(diffore_10[7:12])$conf.int
 ci
 summary(ci)

 #t-test
 t_test_result <- t.test(diffore_10[7:12], diffmean[7:12])
 t_test_result


 ############################ forecasting ends here ###########################################
 ##############################################################################################################################################


 ##############################################################################################################################################


 ########################################################  Mean Imputation MAR 5%  ###################################################################

 # Assigning deaths to the new name
 deaths
 df_missing_MAR<-deaths[deaths$Time>72,]
 nje <- deaths#[deaths$Time>72,]
 nje$month_index <- rep(1:12, length.out = nrow(nje))
 df_missing_MAR$month_index <- rep(1:12, length.out = nrow(df_missing_MAR))
 df_missing_MAR

 deathsX5 <- deaths[deaths$Time<73,]
 deathsX5$month_index <- rep(1:12, length.out = nrow(deathsX5))

 View(df_missing_MAR)
 # Calculating the total number of missing values (5% of the entire data set)
 #number of rows of the data set
 nrow(df_missing_MAR)
 total_missing <- 0.05 * nrow(df_missing_MAR)
 total_missing
 # Calculating the number of missing values to be inserted in December (95% of total missing)
 missing_Décembre <- floor(0.95 * total_missing)
 missing_Décembre
 # Calculating the number of missing values to be inserted in non-December months
 missing_non_Décembre <- total_missing - missing_Décembre
 missing_non_Décembre
 # Set seed for reproducibility
 set.seed(1)

 december_indices <- grep("Décembre", df_missing_MAR$Months)
 #View(december_indices)

 non_december_indices <- setdiff(seq_len(nrow(df_missing_MAR)), december_indices)
 #View(non_december_indices)

 # Generating missing values in the deaths_due_to_severe_malaria column for december entries
 df_missing_MAR$deaths_due_to_severe_malaria[sample(december_indices, size = missing_Décembre, replace = FALSE)] <- NA

 # Generating missing values in the deaths_due_to_sefindvere_malaria column for non-december entries
 df_missing_MAR$deaths_due_to_severe_malaria[sample(non_december_indices, size = missing_non_Décembre, replace = FALSE)] <- NA

 df_missing_5 <- df_missing_MAR
 #print(df_missing_5)
 View(df_missing_5)
 #saving the data as an excel file
 #write.xlsx(df_missing_5, file= "df_missing_5.xlsx", sheetName = "Sheet1", append = TRUE)
 # Identify rows with missing values
 missing_indices_MAR <- which(is.na(df_missing_5$deaths_due_to_severe_malaria))
 # Calculate the mean for each month where values are not missing
 deathsX5$month_index <- rep(1:12, length.out = nrow(deathsX5))
 df_missing_5X <-rbind(deathsX5,df_missing_5)

 mean_imputation <- df_missing_5X %>%
   group_by(month_index) %>%
   summarise(mean_cases = mean(deaths_due_to_severe_malaria, na.rm = TRUE))

 # Join the original data with the mean imputation
 imputed_data_MAR <- df_missing_5X %>%
   left_join(mean_imputation, by = "month_index") %>%
   mutate(deaths_due_to_severe_malaria = ifelse(is.na(deaths_due_to_severe_malaria), mean_cases, deaths_due_to_severe_malaria)) %>%
   select(-mean_cases)

 #print(imputed_data_MAR)
 View(imputed_data_MAR)

 missing_indices_MAR <- which(is.na(df_missing_5X$deaths_due_to_severe_malaria))

 difmean_MAR <- numeric(length(df_missing_5X))

 for(i in missing_indices_MAR){difmean_MAR[[as.character(i)]] <- print(abs(nje$deaths_due_to_severe_malaria[i]-imputed_data_MAR$deaths_due_to_severe_malaria[i]))}

 for (i in missing_indices_MAR){print(paste("differences for row",i,":" ,difmean_MAR[[as.character(i)]]))}
 #print(differences)
 View(difmean_MAR)


 # Calculate the mean of the differences
 mean_value_MAR5 <- mean(difmean_MAR[5:7], na.rm = TRUE)
 mean_value_MAR5
 # Calculate the confidence interval
 ci <- t.test(difmean_MAR[5:7])$conf.int
 ci
 # Print the results
 print(paste("Mean of differences:", mean_value_MAR5))
 print(paste("Confidence interval of differences:", ci))
###############################################################################################################################################################
############################################### FORECASTING MAR 5%#############################################################################################

 death_2<-deaths[1:72, ]
 view(death_2)
 names(deaths)
 nrow(deaths)
 #deaths$Time <- seq(1,72)

 #deaths_BI<-deaths[deaths$Time<73,]
 #deaths_DI<-deaths[deaths$Time>87,]
 # creating missingness
 #deaths[73:84,3]<-NA
 #deaths$deaths_due_to_severe_malaria[deaths$deaths_due_to_severe_malaria>72 & deaths$deaths_due_to_severe_malaria<85]<-NA
 deaths

 ### Temporary series analysis NU PF INT(Temporary series analysis NU PF INT)
 Timeseries_BI <-  ts(death_2$deaths_due_to_severe_malaria, start = c(2013,1), end= c(2018,12), frequency=12)
 Timeseries_BI
 #View(Timeseries_BI)


 Acf(Timeseries_BI, plot = TRUE)
 pacf(Timeseries_BI,plot = TRUE)

 adf.test(Timeseries_BI) #Augment Dickey-Fuller Test: Null is non-stationary and Alt is stationary
 kpss.test(Timeseries_BI) #KPSS: Null is Stationary and Alt is non stationary
 ndiffs(Timeseries_BI) #how many differences needed
 nsdiffs(Timeseries_BI) #how many seasonal differences needed

 MMM <- diff(Timeseries_BI, lag = 12,differences = 1)
 Acf(MMM)
 Pacf(MMM)

 fit_Timeseries_BI <- auto.arima(Timeseries_BI)
 fit_Timeseries_BI
 checkresiduals(fit_Timeseries_BI)

 fit3 <- Arima(Timeseries_BI, order=c(1,0,1), seasonal=c(1,1,1),include.constant = TRUE)
 fit3
 fit8 <- Arima(Timeseries_BI, order=c(1,0,1), seasonal=c(0,1,1),include.constant = TRUE)
 fit8

 # h = 10*12 because, forecast is for 10 years for all 12 months
 ffcast_fit_Timeseries_BI <-forecast(fit_Timeseries_BI, level=c(95), h=1*12)

 fore_values<-ffcast_fit_Timeseries_BI[["mean"]]
 #view(fore_values)
 ################################################################################
 combined_column <- c(death_2$deaths_due_to_severe_malaria, fore_values)
 df <- data.frame(combined_column = combined_column)
 df
 #View(combined_column)
 Timeseries_1 <-  ts(df$combined_column, start = c(2013,1), end= c(2019,12), frequency=12)
 Timeseries_1
 fit_Timeseries_1 <- auto.arima(Timeseries_1)
 fit_Timeseries_1
 ffcast_fit_Timeseries_1 <-forecast(fit_Timeseries_1, level=c(95), h=1*12)
 fore_values1<-ffcast_fit_Timeseries_1[["mean"]]
 #view(fore_values1)
 combined_column1 <- c(df$combined_column, fore_values1)
 df1 <- data.frame(combined_column1 = combined_column1)
 df1

 Timeseries_2 <-  ts(df1$combined_column1, start = c(2013,1), end= c(2020,12), frequency=12)
 Timeseries_2
 fit_Timeseries_2 <- auto.arima(Timeseries_2)
 fit_Timeseries_2
 ffcast_fit_Timeseries_2 <-forecast(fit_Timeseries_2, level=c(95), h=1*12)
 fore_values2<-ffcast_fit_Timeseries_2[["mean"]]
 #view(fore_values2)
 combined_column2 <- c(df1$combined_column1, fore_values2)
 df2 <- data.frame(combined_column2 = combined_column2)
 df2

 Timeseries_3 <-  ts(df2$combined_column2, start = c(2013,1), end= c(2021,12), frequency=12)
 Timeseries_3
 fit_Timeseries_3 <- auto.arima(Timeseries_3)
 fit_Timeseries_3
 ffcast_fit_Timeseries_3 <-forecast(fit_Timeseries_3, level=c(95), h=1*12)
 fore_values3<-ffcast_fit_Timeseries_3[["mean"]]
 #view(fore_values3)
 combined_column3 <- c(df2$combined_column2, fore_values3)
 df3 <- data.frame(combined_column3 = combined_column3)
 df3

 Timeseries_4 <-  ts(df3$combined_column3, start = c(2013,1), end= c(2022,12), frequency=12)
 Timeseries_4
 fit_Timeseries_4 <- auto.arima(Timeseries_4)
 fit_Timeseries_4
 ffcast_fit_Timeseries_4 <-forecast(fit_Timeseries_4, level=c(95), h=1*12)
 fore_values4<-ffcast_fit_Timeseries_4[["mean"]]
 #view(fore_values4)
 combined_column4 <- c(df3$combined_column3, fore_values4)
 dfinal <- data.frame(combined_column4 = combined_column4)
 dfinal

 names(dfinal)[names(dfinal) == "combined_column4"] <- "deaths_forecast_5years"
 #View(dfinal)

 deaths5MAR <- cbind(deaths,dfinal)
##############MISINGNESS
 # Assigning deaths to the new name
 deaths5MAR
 df_missing_MAR<-deaths[deaths$Time>72,]
 nje <- deaths#[deaths$Time>72,]
 nje$month_index <- rep(1:12, length.out = nrow(nje))
 df_missing_MAR$month_index <- rep(1:12, length.out = nrow(df_missing_MAR))
 df_missing_MAR

 deathsX5 <- deaths[deaths$Time<73,]
 deathsX5$month_index <- rep(1:12, length.out = nrow(deathsX5))

 #now forecasting
 #View(df_missing_MAR)
 # Calculating the total number of missing values (5% of the entire data set)
 #number of rows of the data set
 nrow(df_missing_MAR)
 total_missing <- 0.05 * nrow(df_missing_MAR)
 total_missing
 # Calculating the number of missing values to be inserted in December (95% of total missing)
 missing_Décembre <- floor(0.95 * total_missing)
 missing_Décembre
 # Calculating the number of missing values to be inserted in non-December months
 missing_non_Décembre <- total_missing - missing_Décembre
 missing_non_Décembre
 # Set seed for reproducibility
 #set.seed(1)

 december_indices <- grep("Décembre", df_missing_MAR$Months)
 #View(december_indices)

 non_december_indices <- setdiff(seq_len(nrow(df_missing_MAR)), december_indices)
 #View(non_december_indices)

 # Generating missing values in the deaths_due_to_severe_malaria column for december entries
 df_missing_MAR$deaths_due_to_severe_malaria[sample(december_indices, size = missing_Décembre, replace = FALSE)] <- NA

 # Generating missing values in the deaths_due_to_sefindvere_malaria column for non-december entries
 df_missing_MAR$deaths_due_to_severe_malaria[sample(non_december_indices, size = missing_non_Décembre, replace = FALSE)] <- NA
 #df_missing_X5
 df_missing_5 <- df_missing_MAR
 #View(df_missing_5)
 #print(df_missing_5)
 #View(df_missing_5)
 # binding the columns
 df_missing_5X <-rbind(deathsX5,df_missing_5)
 missing_indices_MAR <- which(is.na(df_missing_5X$deaths_due_to_severe_malaria))


 #View(df_missing_5X)
 df_missing_fore_MAR_5 <- cbind(df_missing_5X,dfinal)
 #View(df_missing_fore_MAR_5)
 View(df_missing_5X)
 #imputation
 df_missing_fore_MAR_5 <- df_missing_fore_MAR_5 %>%
   mutate(deaths_due_to_severe_malaria = ifelse(is.na(deaths_due_to_severe_malaria), deaths_forecast_5years, deaths_due_to_severe_malaria))

 # Print the imputed dataset
 print(df_missing_fore_MAR_5)
 #View(nje)
 difore <- numeric(length(missing_indices_MAR))
 for(i in missing_indices_MAR){difore[[as.character(i)]] <- print(abs(nje$deaths_due_to_severe_malaria[i]-df_missing_fore_MAR_5$deaths_due_to_severe_malaria[i]))}

 for (i in missing_indices_MAR){print(paste("differences for row",i,":" ,difore[[as.character(i)]]))}
 #print(differences)
 View(difore)

 # mean_value_fore5 <- mean(difore[4:6])
 # mean_value_fore5
 # mean
 #  ci <- t.test(difore[4:6])$conf.int
 # ci
 # summary(ci)
 #
 # #t-test
 # t_test_result <- t.test(difore[4:6], difmean_MAR[5:7])
 # t_test_result
 # Calculate the confidence interval


 mean_value_fore5 <- mean(difore[4:6])
 mean_value_fore5
 meanMAR5 <- difore[4:6]
 summary(meanMAR5)
 # Calculate the confidence interval
 ci <- t.test(meanMAR5)$conf.int
 ci

 data_fore6 <- df_missing_fore_MAR_5
 write_xlsx(data_fore6, "/home/ntokozo/Documents/research/data_fore6.xlsx")
########################### forecasting ends here ################################################################################################################


##################################  Mean Imputation MAR 10% #########################################################################################
 # Assigning deaths to the new name
 deaths
 df_missing_MAR_10 <-deaths[deaths$Time>72,]
 nje_10 <- deaths#[deaths$Time>72,]
 nje_10$month_index <- rep(1:12, length.out = nrow(nje_10))
 df_missing_MAR_10$month_index <- rep(1:12, length.out = nrow(df_missing_MAR_10))
 df_missing_MAR_10
 #View(nje_10)
 #View(df_missing_MAR_10)
 # Calculating the total number of missing values (5% of the entire data set)
 #number of rows of the data set

 #
 # # Step 1: Calculate the number of rows in the dataframe
  num_rows <- nrow(df_missing_MAR_10)
  num_rows
 # # Step 2: Calculate the total number of missing values (10% of total rows)
  total_missing_10 <- 0.1 * num_rows
 #
 # # Step 3: Initialize missing values for December and non-December
  missing_December_10 <- 0
  missing_non_December_10 <- 0

  if (total_missing_10 >= 1) {
 #   # Step 4: Calculate missing values for December (95% of total)
    missing_December_10 <- floor(0.95 * total_missing_10)
 #
 #   # Step 5: Ensure at least one missing value is assigned to non-December months
    if (missing_December_10 == total_missing_10) {
      missing_December_10 <- total_missing_10 - 1
      missing_non_December_10 <- 1
    } else {
      missing_non_December_10 <- total_missing_10 - missing_December_10
    }
  } else {
    # If total_missing_10 is less than 1, assign it all to non-December months
    missing_non_December_10 <- total_missing_10
  }

  # Print the results for verification
  print(paste("Total rows:", num_rows))
  print(paste("Total missing values (10%):", total_missing_10))
  print(paste("Missing values for December (95%):", missing_December_10))
  print(paste("Missing values for non-December months:", missing_non_December_10))

 # nrow(df_missing_MAR_10)
 total_missing_10 <- 0.1 * nrow(df_missing_MAR_10)
 total_missing_10
 # # Calculating the number of missing values to be inserted in December (95% of total missing)
  missing_Décembre_10 <- floor(0.95 * total_missing_10)
  missing_Décembre_10
 # # Calculating the number of missing values to be inserted in non-December months
  missing_non_Décembre_10 <- total_missing_10 - missing_Décembre_10
  missing_non_Décembre_10
 # Set seed for reproducibility
 set.seed(1)

 #indices
 december_indices_10 <- grep("Décembre", df_missing_MAR_10$Months)
 #View(december_indices_10)

 non_december_indices_10 <- setdiff(seq_len(nrow(df_missing_MAR_10)), december_indices_10)
 #View(non_december_indices_10)

 # Generating missing values in the deaths_due_to_severe_malaria column for december entries
  df_missing_MAR_10$deaths_due_to_severe_malaria[sample(december_indices_10, size = missing_Décembre_10, replace = FALSE)] <- NA

 # Generating missing values in the deaths_due_to_sefindvere_malaria column for non-december entries
 df_missing_MAR_10$deaths_due_to_severe_malaria[sample(non_december_indices_10, size = missing_non_December_10, replace = FALSE)] <- NA
 deathsX <- deaths[deaths$Time<73,]
 deathsX$month_index <- rep(1:12, length.out = nrow(deathsX))

 df_missing_10 <- df_missing_MAR_10
 df_missing_10X <-rbind(deathsX,df_missing_10)
 View(df_missing_10X)
 #saving the data as an excel file
 #write.xlsx(df_missing_5, file= "df_missing_5.xlsx", sheetName = "Sheet1", append = TRUE)
 # Identify rows with missing values
 missing_indices_MAR_10 <- which(is.na(df_missing_10X$deaths_due_to_severe_malaria))

 # Calculate the mean for each month where values are not missing
  mean_imputation <- df_missing_10X %>%
    group_by(month_index) %>%
    summarise(mean_cases = mean(deaths_due_to_severe_malaria, na.rm = TRUE))

  # Join the original data with the mean imputation
  imputed_data_MAR_10 <- df_missing_10X %>%
    left_join(mean_imputation, by = "month_index") %>%
    mutate(deaths_due_to_severe_malaria = ifelse(is.na(deaths_due_to_severe_malaria), mean_cases, deaths_due_to_severe_malaria)) %>%
    select(-mean_cases)


 View(imputed_data_MAR_10)

 difmean_MAR_10 <- numeric(length(df_missing_10))
 #
 for(i in missing_indices_MAR_10){difmean_MAR_10[[as.character(i)]] <- print(abs(nje_10$deaths_due_to_severe_malaria[i]-imputed_data_MAR_10$deaths_due_to_severe_malaria[i]))}
 #
 for (i in missing_indices_MAR_10){print(paste("differences for row",i,":" ,difmean_MAR_10[[as.character(i)]]))}
 # #print(differences)
 View(difmean_MAR_10)
 View(nje_10)
 #
 # Calculate the mean of the differences
 mean_value_10_MAR <- mean(difmean_MAR_10[5:10], na.rm = TRUE)
 mean_value_10_MAR
 # Calculate the confidence interval
 ci <- t.test(difmean_MAR_10[5:10])$conf.int
 ci
 # Print the results
 print(paste("Mean of differences:", mean_value_10_MAR))
 print(paste("Confidence interval of differences:", ci))
##########################################################################################
#################################################### Forecasting MAR 10% ###############################################################################################
death_2<-deaths[1:72, ]
view(death_2)
names(deaths)
nrow(deaths)
#deaths$Time <- seq(1,72)

#deaths_BI<-deaths[deaths$Time<73,]
#deaths_DI<-deaths[deaths$Time>87,]
# creating missingness
#deaths[73:84,3]<-NA    
#deaths$deaths_due_to_severe_malaria[deaths$deaths_due_to_severe_malaria>72 & deaths$deaths_due_to_severe_malaria<85]<-NA
deaths

### Temporary series analysis NU PF INT(Temporary series analysis NU PF INT)
Timeseries_BI <-  ts(death_2$deaths_due_to_severe_malaria, start = c(2013,1), end= c(2018,12), frequency=12)
Timeseries_BI
#View(Timeseries_BI)


Acf(Timeseries_BI, plot = TRUE)
pacf(Timeseries_BI,plot = TRUE)

adf.test(Timeseries_BI) #Augment Dickey-Fuller Test: Null is non-stationary and Alt is stationary
kpss.test(Timeseries_BI) #KPSS: Null is Stationary and Alt is non stationary
ndiffs(Timeseries_BI) #how many differences needed
nsdiffs(Timeseries_BI) #how many seasonal differences needed

MMM <- diff(Timeseries_BI, lag = 12,differences = 1)
Acf(MMM)
Pacf(MMM)

fit_Timeseries_BI <- auto.arima(Timeseries_BI)
fit_Timeseries_BI
checkresiduals(fit_Timeseries_BI)

fit3 <- Arima(Timeseries_BI, order=c(1,0,1), seasonal=c(1,1,1),include.constant = TRUE)
fit3
fit8 <- Arima(Timeseries_BI, order=c(1,0,1), seasonal=c(0,1,1),include.constant = TRUE)
fit8

# h = 10*12 because, forecast is for 10 years for all 12 months
ffcast_fit_Timeseries_BI <-forecast(fit_Timeseries_BI, level=c(95), h=1*12)

fore_values<-ffcast_fit_Timeseries_BI[["mean"]]
#view(fore_values)
################################################################################
combined_column <- c(death_2$deaths_due_to_severe_malaria, fore_values)
df <- data.frame(combined_column = combined_column)
df
#View(combined_column)
Timeseries_1 <-  ts(df$combined_column, start = c(2013,1), end= c(2019,12), frequency=12)
Timeseries_1
fit_Timeseries_1 <- auto.arima(Timeseries_1)
fit_Timeseries_1
ffcast_fit_Timeseries_1 <-forecast(fit_Timeseries_1, level=c(95), h=1*12)
fore_values1<-ffcast_fit_Timeseries_1[["mean"]]
#view(fore_values1)
combined_column1 <- c(df$combined_column, fore_values1)
df1 <- data.frame(combined_column1 = combined_column1)
df1

Timeseries_2 <-  ts(df1$combined_column1, start = c(2013,1), end= c(2020,12), frequency=12)
Timeseries_2
fit_Timeseries_2 <- auto.arima(Timeseries_2)
fit_Timeseries_2
ffcast_fit_Timeseries_2 <-forecast(fit_Timeseries_2, level=c(95), h=1*12)
fore_values2<-ffcast_fit_Timeseries_2[["mean"]]
#view(fore_values2)
combined_column2 <- c(df1$combined_column1, fore_values2)
df2 <- data.frame(combined_column2 = combined_column2)
df2

Timeseries_3 <-  ts(df2$combined_column2, start = c(2013,1), end= c(2021,12), frequency=12)
Timeseries_3
fit_Timeseries_3 <- auto.arima(Timeseries_3)
fit_Timeseries_3
ffcast_fit_Timeseries_3 <-forecast(fit_Timeseries_3, level=c(95), h=1*12)
fore_values3<-ffcast_fit_Timeseries_3[["mean"]]
#view(fore_values3)
combined_column3 <- c(df2$combined_column2, fore_values3)
df3 <- data.frame(combined_column3 = combined_column3)
df3

Timeseries_4 <-  ts(df3$combined_column3, start = c(2013,1), end= c(2022,12), frequency=12)
Timeseries_4
fit_Timeseries_4 <- auto.arima(Timeseries_4)
fit_Timeseries_4
ffcast_fit_Timeseries_4 <-forecast(fit_Timeseries_4, level=c(95), h=1*12)
fore_values4<-ffcast_fit_Timeseries_4[["mean"]]
#view(fore_values4)
combined_column4 <- c(df3$combined_column3, fore_values4)
dfinal <- data.frame(combined_column4 = combined_column4)
dfinal

names(dfinal)[names(dfinal) == "combined_column4"] <- "deaths_forecast_5years"
#View(dfinal)
deaths
deaths_10_fore_MAR <- cbind(deaths,dfinal)


df_missing_MAR_fore_10 <-deaths[deaths$Time>72,]
nje_fore_10 <- deaths#[deaths$Time>72,]
nje_fore_10$month_index <- rep(1:12, length.out = nrow(nje_fore_10)) 
df_missing_MAR_fore_10$month_index <- rep(1:12, length.out = nrow(df_missing_MAR_fore_10))
df_missing_MAR_fore_10
#View(nje_10)
#View(df_missing_MAR_10)
# Calculating the total number of missing values (5% of the entire data set) 
#number of rows of the data set

# 
# # Step 1: Calculate the number of rows in the dataframe
num_rows <- nrow(df_missing_MAR_fore_10)
num_rows
# # Step 2: Calculate the total number of missing values (10% of total rows)
total_missing_fore_10 <- 0.1 * num_rows
#
# # Step 3: Initialize missing values for December and non-December
missing_December_10 <- 0
missing_non_December_10 <- 0

if (total_missing_fore_10 >= 1) {
  #   # Step 4: Calculate missing values for December (95% of total)
  missing_December_10 <- floor(0.95 * total_missing_fore_10)
  #   
  #   # Step 5: Ensure at least one missing value is assigned to non-December months
  if (missing_December_10 == total_missing_fore_10) {
    missing_December_10 <- total_missing_fore_10 - 1
    missing_non_December_10 <- 1
  } else {
    missing_non_December_10 <- total_missing_fore_10 - missing_December_10
  }
} else {
  # If total_missing_10 is less than 1, assign it all to non-December months
  missing_non_December_10 <- total_missing_fore_10
}

# Print the results for verification
print(paste("Total rows:", num_rows))
print(paste("Total missing values (10%):", total_missing_fore_10))
print(paste("Missing values for December (95%):", missing_December_10))
print(paste("Missing values for non-December months:", missing_non_December_10))

# nrow(df_missing_MAR_10)
total_missing_fore_10 <- 0.1 * nrow(df_missing_MAR_fore_10)
total_missing_fore_10
# # Calculating the number of missing values to be inserted in December (95% of total missing) 
missing_Décembre_10 <- floor(0.95 * total_missing_fore_10)
missing_Décembre_10
# # Calculating the number of missing values to be inserted in non-December months 
missing_non_Décembre_10 <- total_missing_fore_10 - missing_Décembre_10
missing_non_Décembre_10
# Set seed for reproducibility 
#set.seed(1) 

#indices
december_indices_10 <- grep("Décembre", df_missing_MAR_fore_10$Months)
#View(december_indices_10)

non_december_indices_10 <- setdiff(seq_len(nrow(df_missing_MAR_fore_10)), december_indices_10)
#View(non_december_indices_10)

# Generating missing values in the deaths_due_to_severe_malaria column for december entries 
df_missing_MAR_fore_10$deaths_due_to_severe_malaria[sample(december_indices_10, size = missing_Décembre_10, replace = FALSE)] <- NA

# Generating missing values in the deaths_due_to_sefindvere_malaria column for non-december entries 
df_missing_MAR_fore_10$deaths_due_to_severe_malaria[sample(non_december_indices_10, size = missing_non_December_10, replace = FALSE)] <- NA
deathsX <- deaths[deaths$Time<73,]
deathsX$month_index <- rep(1:12, length.out = nrow(deathsX))

df_missing_10 <- df_missing_MAR_fore_10
df_missing_10X <-rbind(deathsX,df_missing_MAR_fore_10)
#View(df_missing_10X)
df_missing_10XX <- cbind(df_missing_10X, dfinal)
#saving the data as an excel file
#write.xlsx(df_missing_5, file= "df_missing_5.xlsx", sheetName = "Sheet1", append = TRUE)
# Identify rows with missing values
missing_indices_MAR_fore_10 <- which(is.na(df_missing_10XX$deaths_due_to_severe_malaria))

df_missing_10XX <- df_missing_10XX %>%
  mutate(deaths_due_to_severe_malaria = ifelse(is.na(deaths_due_to_severe_malaria), deaths_forecast_5years, deaths_due_to_severe_malaria))

#View(df_missing_10XX)

difmean_fore_MAR_10 <- numeric(length(total_missing_fore_10))
#
for(i in missing_indices_MAR_fore_10){difmean_fore_MAR_10[[as.character(i)]] <- print(abs(nje_fore_10$deaths_due_to_severe_malaria[i]-df_missing_10XX$deaths_due_to_severe_malaria[i]))}
#
for (i in missing_indices_MAR_fore_10){print(paste("differences for row",i,":" ,difmean_fore_MAR_10[[as.character(i)]]))}
# #print(differences)
#View(difmean_fore_MAR_10)
#View(nje_fore_10)
# 
# Calculate the mean of the differences
mean_value_10_fore_MAR <- mean(difmean_fore_MAR_10[2:7], na.rm = TRUE)
mean_value_10_fore_MAR
m <- difmean_fore_MAR_10[2:7]
summary(m)
# Calculate the confidence interval
ci <- t.test(m)$conf.int
ci

data_fore_10_6 <- df_missing_10XX
write_xlsx(data_fore_10_6, "/home/ntokozo/Documents/research/data_fore_10_6.xlsx")
# Print the results

#t-test
# t_test_result <- t.test(difmean_fore_MAR_10[2:7], difmean_MAR_10[5:10])
# t_test_result

###########################################################################################################################################################################
############################################## 5 % MNAR #######################

deaths
death_2<-deaths[1:72, ]
#view(death_2)
names(deaths)
nrow(deaths)
### Temporary series analysis NU PF INT(Temporary series analysis NU PF INT)
Timeseries_BI <-  ts(death_2$deaths_due_to_severe_malaria, start = c(2013,1), end= c(2018,12), frequency=12)
Timeseries_BI
#View(Timeseries_BI)

Acf(Timeseries_BI, plot = TRUE)
pacf(Timeseries_BI,plot = TRUE)

adf.test(Timeseries_BI) #Augment Dickey-Fuller Test: Null is non-stationary and Alt is stationary
kpss.test(Timeseries_BI) #KPSS: Null is Stationary and Alt is non stationary
ndiffs(Timeseries_BI) #how many differences needed
nsdiffs(Timeseries_BI) #how many seasonal differences needed

MMM <- diff(Timeseries_BI, lag = 12,differences = 1)
Acf(MMM)
Pacf(MMM)

fit_Timeseries_BI <- auto.arima(Timeseries_BI)
fit_Timeseries_BI
checkresiduals(fit_Timeseries_BI)

fit3 <- Arima(Timeseries_BI, order=c(1,0,1), seasonal=c(1,1,1),include.constant = TRUE)
fit3
fit8 <- Arima(Timeseries_BI, order=c(1,0,1), seasonal=c(0,1,1),include.constant = TRUE)
fit8

# h = 10*12 because, forecast is for 10 years for all 12 months
ffcast_fit_Timeseries_BI <-forecast(fit_Timeseries_BI, level=c(95), h=1*12)

fore_values<-ffcast_fit_Timeseries_BI[["mean"]]
#view(fore_values)
################################################################################
combined_column <- c(death_2$deaths_due_to_severe_malaria, fore_values)
df <- data.frame(combined_column = combined_column)
df
#View(combined_column)
Timeseries_1 <-  ts(df$combined_column, start = c(2013,1), end= c(2019,12), frequency=12)
Timeseries_1
fit_Timeseries_1 <- auto.arima(Timeseries_1)
fit_Timeseries_1
ffcast_fit_Timeseries_1 <-forecast(fit_Timeseries_1, level=c(95), h=1*12)
fore_values1<-ffcast_fit_Timeseries_1[["mean"]]
#view(fore_values1)
combined_column1 <- c(df$combined_column, fore_values1)
df1 <- data.frame(combined_column1 = combined_column1)
df1

Timeseries_2 <-  ts(df1$combined_column1, start = c(2013,1), end= c(2020,12), frequency=12)
Timeseries_2
fit_Timeseries_2 <- auto.arima(Timeseries_2)
fit_Timeseries_2
ffcast_fit_Timeseries_2 <-forecast(fit_Timeseries_2, level=c(95), h=1*12)
fore_values2<-ffcast_fit_Timeseries_2[["mean"]]
#view(fore_values2)
combined_column2 <- c(df1$combined_column1, fore_values2)
df2 <- data.frame(combined_column2 = combined_column2)
df2

Timeseries_3 <-  ts(df2$combined_column2, start = c(2013,1), end= c(2021,12), frequency=12)
Timeseries_3
fit_Timeseries_3 <- auto.arima(Timeseries_3)
fit_Timeseries_3
ffcast_fit_Timeseries_3 <-forecast(fit_Timeseries_3, level=c(95), h=1*12)
fore_values3<-ffcast_fit_Timeseries_3[["mean"]]
#view(fore_values3)
combined_column3 <- c(df2$combined_column2, fore_values3)
df3 <- data.frame(combined_column3 = combined_column3)
df3

Timeseries_4 <-  ts(df3$combined_column3, start = c(2013,1), end= c(2022,12), frequency=12)
Timeseries_4
fit_Timeseries_4 <- auto.arima(Timeseries_4)
fit_Timeseries_4
ffcast_fit_Timeseries_4 <-forecast(fit_Timeseries_4, level=c(95), h=1*12)
fore_values4<-ffcast_fit_Timeseries_4[["mean"]]
#view(fore_values4)
combined_column4 <- c(df3$combined_column3, fore_values4)
dfinal <- data.frame(combined_column4 = combined_column4)
dfinal

names(dfinal)[names(dfinal) == "combined_column4"] <- "deaths_forecast_5years"
#View(dfinal)


############################ MNCR 5% ################################################
 df <- deaths
 
 # View the first few rows of the dataframe
 head(df)
 
 # Extract the year from the Months column
 df$year <- as.numeric(sub(".* ", "", df$Months))
 
 # Filter the data for the years 2013 to 2023
 df_filtered <- df %>%
   filter(year >= 2019 & year <= 2023)
 
 # Find the highest value for each year
 highest_values <- df_filtered %>%
   group_by(year) %>%
   summarise(max_deaths = max(deaths_due_to_severe_malaria, na.rm = TRUE))
 
 # Print the result336.7236 354.7924 314.1376
 print(highest_values)
 View(highest_values)
 ######################5%#######################################################
 
 # Create 5% missing values with 95% probability being the highest values
 #set.seed(1) # Set seed for reproducibility
 
 # Calculate the number of rows to introduce missing values
 n <- nrow(df_filtered)
 num_missing <- round(0.05 * n)
 
 # Extract the highest deaths for each year
 highest_deaths <- highest_values$max_deaths
 
 # Create a logical vector where highest deaths are TRUE
 is_highest <- df_filtered$deaths_due_to_severe_malaria %in% highest_deaths
 
 # Determine the number of missing values to insert in highest values (95% of total missing)
 num_missing_highest <- round(0.95 * num_missing)
 
 # Select rows with highest deaths
 highest_indices <- which(is_highest)
 
 # Randomly select indices for missing values among the highest deaths
 missing_highest_indices <- sample(highest_indices, size = num_missing_highest, replace = FALSE)
 
 # Select remaining missing values to be inserted randomly
 num_missing_remaining <- num_missing - length(missing_highest_indices)
 remaining_indices <- setdiff(1:n, highest_indices)
 
 # Randomly select indices for the remaining missing values
 missing_remaining_indices <- sample(remaining_indices, size = num_missing_remaining, replace = FALSE)
 
 # Combine indices
 missing_indices <- c(missing_highest_indices, missing_remaining_indices)
 
 # Introduce missing values
 df_filtered$deaths_due_to_severe_malaria[missing_indices] <- NA
 
 # Print the modified dataframe
 df_filtered_5 <- df_filtered
 print(df_filtered_5)
 View(df_filtered_5)

install.packages("xlsx")
library("xlsx")
write.xlsx(df_filtered_5, file= "dff_5.xlsx", sheetName = "Sheet1", append = TRUE)
########################imputation######################################################
 missing_indices_MNAR_5 <- which(is.na(df_filtered_5$deaths_due_to_severe_malaria))
 
 df_filtered_5$month_index <- rep(1:12, length.out = nrow(df_filtered_5))
 df_filtered_5
 
 # Calculate the mean for each month where values are not missing
 mean_imputation <- df_filtered_5 %>%
   group_by(month_index) %>%
   summarise(mean_cases = mean(deaths_due_to_severe_malaria, na.rm = TRUE))
 
 # Join the original data with the mean imputation
 imputed_data_MNAR_5 <- df_filtered_5 %>%
   left_join(mean_imputation, by = "month_index") %>%
   mutate(deaths_due_to_severe_malaria = ifelse(is.na(deaths_due_to_severe_malaria), mean_cases, deaths_due_to_severe_malaria)) %>%
   select(-mean_cases)
 
 print(imputed_data_MNAR_5)
 View(imputed_data_MNAR_5)
 
 diffmean_MNAR_5 <- numeric(length(missing_indices_MNAR_5))
 for(i in missing_indices_MNAR_5){diffmean_MNAR_5[[as.character(i)]] <- abs(imputed_data_MNAR_5$deaths_due_to_severe_malaria[i]-deaths$deaths_due_to_severe_malaria[i])}
 for (i in missing_indices_MNAR_5){print(paste("differences_MNAR for row",i,":" ,diffmean_MNAR_5[[as.character(i)]]))}
 #print(differences)
 View(diffmean_MNAR_5)
 # Calculate the mean of the differences
 
 mean_value_MNAR_5 <- mean(diffmean_MNAR_5[4:6], na.rm = TRUE)
 mean_value_MNAR_5
 # Calculate the confidence interval
 ci <- t.test(diffmean_MNAR_5[4:6])$conf.int
 ci
 # Print the results
 print(paste("Mean of differences:", mean_value_MNAR_5))
#######################################################################################
####################################### Forecasting MNAR 5% ############################
death_2<-deaths[1:72, ]
deaths
#view(death_2)
names(deaths)
nrow(deaths)
### Temporary series analysis NU PF INT(Temporary series analysis NU PF INT)
Timeseries_BI <-  ts(death_2$deaths_due_to_severe_malaria, start = c(2013,1), end= c(2018,12), frequency=12)
Timeseries_BI
#View(Timeseries_BI)

Acf(Timeseries_BI, plot = TRUE)
pacf(Timeseries_BI,plot = TRUE)

adf.test(Timeseries_BI) #Augment Dickey-Fuller Test: Null is non-stationary and Alt is stationary
kpss.test(Timeseries_BI) #KPSS: Null is Stationary and Alt is non stationary
ndiffs(Timeseries_BI) #how many differences needed
nsdiffs(Timeseries_BI) #how many seasonal differences needed

MMM <- diff(Timeseries_BI, lag = 12,differences = 1)
Acf(MMM)
Pacf(MMM)

fit_Timeseries_BI <- auto.arima(Timeseries_BI)
fit_Timeseries_BI
checkresiduals(fit_Timeseries_BI)

fit3 <- Arima(Timeseries_BI, order=c(1,0,1), seasonal=c(1,1,1),include.constant = TRUE)
fit3
fit8 <- Arima(Timeseries_BI, order=c(1,0,1), seasonal=c(0,1,1),include.constant = TRUE)
fit8

# h = 10*12 because, forecast is for 10 years for all 12 months
ffcast_fit_Timeseries_BI <-forecast(fit_Timeseries_BI, level=c(95), h=1*12)

fore_values<-ffcast_fit_Timeseries_BI[["mean"]]
#view(fore_values)
################################################################################
combined_column <- c(death_2$deaths_due_to_severe_malaria, fore_values)
df <- data.frame(combined_column = combined_column)
df
#View(combined_column)
Timeseries_1 <-  ts(df$combined_column, start = c(2013,1), end= c(2019,12), frequency=12)
Timeseries_1
fit_Timeseries_1 <- auto.arima(Timeseries_1)
fit_Timeseries_1
ffcast_fit_Timeseries_1 <-forecast(fit_Timeseries_1, level=c(95), h=1*12)
fore_values1<-ffcast_fit_Timeseries_1[["mean"]]
#view(fore_values1)
combined_column1 <- c(df$combined_column, fore_values1)
df1 <- data.frame(combined_column1 = combined_column1)
df1

Timeseries_2 <-  ts(df1$combined_column1, start = c(2013,1), end= c(2020,12), frequency=12)
Timeseries_2
fit_Timeseries_2 <- auto.arima(Timeseries_2)
fit_Timeseries_2
ffcast_fit_Timeseries_2 <-forecast(fit_Timeseries_2, level=c(95), h=1*12)
fore_values2<-ffcast_fit_Timeseries_2[["mean"]]
#view(fore_values2)
combined_column2 <- c(df1$combined_column1, fore_values2)
df2 <- data.frame(combined_column2 = combined_column2)
df2

Timeseries_3 <-  ts(df2$combined_column2, start = c(2013,1), end= c(2021,12), frequency=12)
Timeseries_3
fit_Timeseries_3 <- auto.arima(Timeseries_3)
fit_Timeseries_3
ffcast_fit_Timeseries_3 <-forecast(fit_Timeseries_3, level=c(95), h=1*12)
fore_values3<-ffcast_fit_Timeseries_3[["mean"]]
#view(fore_values3)
combined_column3 <- c(df2$combined_column2, fore_values3)
df3 <- data.frame(combined_column3 = combined_column3)
df3

Timeseries_4 <-  ts(df3$combined_column3, start = c(2013,1), end= c(2022,12), frequency=12)
Timeseries_4
fit_Timeseries_4 <- auto.arima(Timeseries_4)
fit_Timeseries_4
ffcast_fit_Timeseries_4 <-forecast(fit_Timeseries_4, level=c(95), h=1*12)
fore_values4<-ffcast_fit_Timeseries_4[["mean"]]
#view(fore_values4)
combined_column4 <- c(df3$combined_column3, fore_values4)
dfinal <- data.frame(combined_column4 = combined_column4)
names(dfinal)[names(dfinal) == "combined_column4"] <- "deaths_forecast_5years"
dfinal

#names(dfinal)[names(dfinal) == "combined_column4"] <- "deaths_forecast_5years"
View(dfinal)
 df <- deaths

 # View the first few rows of the dataframe
 head(df)

 # Extract the year from the Months column
 df$year <- as.numeric(sub(".* ", "", df$Months))

 # Filter the data for the years 2013 to 2023
 df_filtered_MNAR <- df %>%
   filter(year >= 2019 & year <= 2023)

 # Find the highest value for each year
 highest_values_MNAR <- df_filtered_MNAR %>%
   group_by(year) %>%
   summarise(max_deaths = max(deaths_due_to_severe_malaria, na.rm = TRUE))

 # Print the result336.7236 354.7924 314.1376
 print(highest_values_MNAR)
 #View(highest_values_MNAR)
 ######################MNAR_FORECASTING 5%#######################################################

 # Create 5% missing values with 95% probability being the highest values
 #set.seed(2) # Set seed for reproducibility

 # Calculate the number of rows to introduce missing values
 n <- nrow(df_filtered_MNAR)
 num_missing_MNAR <- round(0.05 * n)

 # Extract the highest deaths for each year
 highest_deaths_MNAR <- highest_values_MNAR$max_deaths

 # Create a logical vector where highest deaths are TRUE
 is_highest_MNAR <- df_filtered_MNAR$deaths_due_to_severe_malaria %in% highest_deaths_MNAR

 # Determine the number of missing values to insert in highest values (95% of total missing)
 num_missing_highest_MNAR <- floor(0.95 * num_missing_MNAR)

 # Select rows with highest deaths
 highest_indices_MNAR <- which(is_highest_MNAR)

 # Randomly select indices for missing values among the highest deaths
 missing_highest_indices_MNAR <- sample(highest_indices_MNAR, size = num_missing_highest_MNAR, replace = FALSE)

 # Select remaining missing values to be inserted randomly
 num_missing_remaining <- num_missing_MNAR - num_missing_highest_MNAR
 remaining_indices <- setdiff(1:n, highest_indices_MNAR)

 # Randomly select indices for the remaining missing values
 missing_remaining_indices <- sample(remaining_indices, size = num_missing_remaining, replace = FALSE)

 # Combine indices
 missing_indices_MNAR <- c(missing_highest_indices_MNAR, missing_remaining_indices)

 # Introduce missing values
 df_filtered_MNAR$deaths_due_to_severe_malaria[missing_indices_MNAR] <- NA

 # Print the modified dataframe
 #View(df_filtered_MNAR)
 deaths_MNAR_56 <- deaths[deaths$Time<73,]
 deaths_MNAR_56$year <- as.numeric(sub(".* ", "", deaths_MNAR_56$Months))
 deaths_MNAR_56$month_index <- rep(1:12, length.out = nrow(deaths_MNAR_56))
 df_filtered_MNAR$month_index <- rep(1:12, length.out = nrow(df_filtered_MNAR))

 deaths_MNAR_5 <- df_filtered_MNAR
 df_MNAR_55 <-rbind(deaths_MNAR_56,deaths_MNAR_5)
 #View(df_MNAR_55)
 df_MNAR_555 <- cbind(df_MNAR_55, dfinal)

 df_MNAR_555 <- df_MNAR_555 %>%
   mutate(deaths_due_to_severe_malaria = ifelse(is.na(deaths_due_to_severe_malaria), deaths_forecast_5years, deaths_due_to_severe_malaria))

 #View(df_MNAR_555)
 missing_indices_MNAR_55 <- which(is.na(df_MNAR_55$deaths_due_to_severe_malaria))
 missing_indices_MNAR_555 <- numeric(length(num_missing_MNAR))
 #
 for(i in missing_indices_MNAR_55){missing_indices_MNAR_555[[as.character(i)]] <- print(abs(df$deaths_due_to_severe_malaria[i]-df_MNAR_555$deaths_due_to_severe_malaria[i]))}
 #
 for (i in missing_indices_MNAR_55){print(paste("differences for row",i,":" ,missing_indices_MNAR_555[[as.character(i)]]))}
 # #print(differences)
 #View(missing_indices_MNAR_555)

 #
 # Calculate the mean of the differences
 mean_value_5_fore_MNAR <- mean(missing_indices_MNAR_555[2:4], na.rm = TRUE)
 mean_value_5_fore_MNAR
 # Calculate the confidence interval
 summary(missing_indices_MNAR_555[2:4])
 ci <- t.test(missing_indices_MNAR_555[2:4])$conf.int
 ci
 data_MNfore_10_6 <- df_MNAR_555
 write_xlsx(data_MNfore_10_6, "/home/ntokozo/Documents/research/data_MNfore_10_6.xlsx")


t-test
t_test_result <- t.test(missing_indices_MNAR_555[2:4], diffmean_MNAR_5[4:6])
t_test_result

print(paste("Confidence interval of differences:", ci))

#################################################### MNAR END 5% both mean and forecast ############################################################################################


####################################################################################################################################################################################

#################################################################MNAR 10% MEAN ########################################
 # View the first few rows of the dataframe
 df_10 <- deaths
 head(df_10)
 
 # Extract the year from the Months column
 df_10$year <- as.numeric(sub(".* ", "", df_10$Months))
 
 # Filter the data for the years 2013 to 2023
 df_filtered_MNAR_10 <- df_10 %>%
   filter(year >= 2019 & year <= 2023)
 
 # Find the highest value for each year
 highest_values_MNAR_10 <- df_filtered %>%
   group_by(year) %>%
   summarise(max_deaths = max(deaths_due_to_severe_malaria, na.rm = TRUE))
 
 # Print the result336.7236 354.7924 314.1376
 print(highest_values_MNAR_10)
 View(highest_values_MNAR_10)
 # Create 10% missing values with 95% probability being the highest values
 #set.seed(1) # Set seed for reproducibility
 
 # Calculate the number of rows to introduce missing values
 #n <- nrow(df_filtered_10)
 #num_missing_10 <- round(0.1 * n)
 num_missing_10 <- 0.1 * nrow(df_filtered_MNAR_10)
 #total_missing_fore_10 <- 0.1 * nrow(df_missing_MAR_fore_10)
 #total_missing_fore_10
 # Extract the highest deaths for each year
 highest_deaths_10 <- highest_values_MNAR_10$max_deaths
 
 # Create a logical vector where highest deaths are TRUE
 is_highest <- df_filtered_MNAR_10$deaths_due_to_severe_malaria %in% highest_deaths_10
 
 # Determine the number of missing values to insert in highest values (95% of total missing)
 num_missing_highest_10 <- round(0.95 * num_missing_10)
 
 # Select rows with highest deaths
 highest_indices_10 <- which(is_highest )
 
 # Randomly select indices for missing values among the highest deaths
 #missing_highest_indices <- sample(highest_indices, size = num_missing_highest, replace = FALSE)
 if (num_missing_highest_10 > length(highest_indices_10)) {
   num_missing_highest_10 <- length(highest_indices_10)
 }
 missing_highest_indices_10 <- sample(highest_indices_10, size = num_missing_highest_10, replace = FALSE)
 
 # Select remaining missing values to be inserted randomly
 num_missing_remaining_10 <- num_missing_10 - length(missing_highest_indices_10)
 remaining_indices_10 <- setdiff(1:n, highest_indices_10)
 
 # Randomly select indices for the remaining missing values
 missing_remaining_indices <- sample(remaining_indices_10, size = num_missing_remaining_10, replace = FALSE)
 
 # Combine indices
 missing_indices_10 <- c(missing_highest_indices_10, missing_remaining_indices)
 
 # Introduce missing values
 df_filtered_MNAR_10$deaths_due_to_severe_malaria[missing_indices_10] <- NA
 
 # Print the modified dataframe
 print(df_filtered_MNAR_10)
 View(df_filtered_MNAR_10)
 #########################imputation for 10% ######################################################
 missing_indices_MNAR_10 <- which(is.na(df_filtered_MNAR_10$deaths_due_to_severe_malaria))
 
 df_filtered_MNAR_10$month_index <- rep(1:12, length.out = nrow(df_filtered_MNAR_10))
 df_filtered_MNAR_10
 
 # Calculate the mean for each month where values are not missing
 mean_imputation <- df_filtered_MNAR_10 %>%
   group_by(month_index) %>%
   summarise(mean_cases = mean(deaths_due_to_severe_malaria, na.rm = TRUE))
 
 # Join the original data with the mean imputation
 imputed_data_MNAR_10 <- df_filtered_MNAR_10 %>%
   left_join(mean_imputation, by = "month_index") %>%
   mutate(deaths_due_to_severe_malaria = ifelse(is.na(deaths_due_to_severe_malaria), mean_cases, deaths_due_to_severe_malaria)) %>%
   select(-mean_cases)
 
 print(imputed_data_MNAR_10)
 View(imputed_data_MNAR_10)
 
 diffmean_MNAR_10 <- numeric(length(missing_indices_MNAR_10))
 for(i in missing_indices_MNAR_10){diffmean_MNAR_10[[as.character(i)]] <- abs(deaths$deaths_due_to_severe_malaria[i]-imputed_data_MNAR_10$deaths_due_to_severe_malaria[i])}
 for (i in missing_indices_MNAR_10){print(paste("differences_MNAR for row",i,":" ,diffmean_MNAR_10[[as.character(i)]]))}
 #print(differences)
 View(diffmean_MNAR_10)
 # Calculate the mean of the differences
 
 mean_value_MNAR_10 <- mean(diffmean_MNAR_10[7:12], na.rm = TRUE)
 mean_value_MNAR_10
 # Calculate the confidence interval
 ci <- t.test(diffmean_MNAR_10[7:12])$conf.int
 ci
 # Print the results
 print(paste("Mean of differences:", mean_value_MNAR_10))
##############################################################FORECASTING 10% ##########################################################
df_Fore_10 <- deaths
head(df_Fore_10)

# Extract the year from the Months column
df_Fore_10$year <- as.numeric(sub(".* ", "", df_Fore_10$Months))

# Filter the data for the years 2013 to 2023
df_filtered_MNAR_fore_10 <- df_Fore_10 %>%
  filter(year >= 2019 & year <= 2023)

# Find the highest value for each year
highest_values_MNAR_fore_10 <- df_filtered_MNAR_fore_10 %>%
  group_by(year) %>%
  summarise(max_deaths = max(df_filtered_MNAR_fore_10$deaths_due_to_severe_malaria, na.rm = TRUE))
#a <- max(df_filtered_MNAR_fore_10$deaths_due_to_severe_malaria, na.rm = TRUE)
# Print the result336.7236 354.7924 314.1376
print(highest_values_MNAR_fore_10)
#View(highest_values_MNAR_fore_10)
# Create 10% missing values with 95% probability being the highest values
#set.seed(1) # Set seed for reproducibility

# Calculate the number of rows to introduce missing values
#n <- nrow(df_filtered_10)
#num_missing_10 <- round(0.1 * n)
num_missing_fore_10 <- 0.1 * nrow(df_filtered_MNAR_fore_10)
#total_missing_fore_10 <- 0.1 * nrow(df_missing_MAR_fore_10)
#total_missing_fore_10
# Extract the highest deaths for each year
highest_deaths_fore_10 <- highest_values_MNAR_fore_10$max_deaths

# Create a logical vector where highest deaths are TRUE
is_highest <- df_filtered_MNAR_fore_10$deaths_due_to_severe_malaria %in% highest_deaths_fore_10

# Determine the number of missing values to insert in highest values (95% of total missing)
num_missing_highest_fore_10 <- round(0.95 * num_missing_fore_10)

# Select rows with highest deaths
highest_indices_fore_10 <- which(is_highest)

# Randomly select indices for missing values among the highest deaths
#missing_highest_indices <- sample(highest_indices, size = num_missing_highest, replace = FALSE)
if (num_missing_highest_fore_10 > length(highest_indices_fore_10)) {
  num_missing_highest_fore_10 <- length(highest_indices_fore_10)
}
missing_highest_indices_fore_10 <- sample(highest_indices_fore_10, size = num_missing_highest_fore_10, replace = FALSE)

# Select remaining missing values to be inserted randomly
num_missing_remaining_fore_10 <- num_missing_fore_10 - length(missing_highest_indices_fore_10)
remaining_indices_fore_10 <- setdiff(1:n, highest_indices_fore_10)

# Randomly select indices for the remaining missing values
missing_remaining_indices_fore_10 <- sample(remaining_indices_fore_10, size = num_missing_remaining_fore_10, replace = FALSE)

# Combine indices
missing_indices_fore_10 <- c(missing_highest_indices_fore_10, missing_remaining_indices_fore_10)

# Introduce missing values
df_filtered_MNAR_fore_10$deaths_due_to_severe_malaria[missing_indices_10] <- NA

# Print the modified dataframe
print(df_filtered_MNAR_fore_10)
View(df_filtered_MNAR_fore_10)

#f_filtered_MNAR <- df_filtered
print(df_filtered_MNAR_fore_10)
View(df_filtered_MNAR_fore_10)
df_fore_10 <- deaths[deaths$Time<73,]
df_fore_10$year <- as.numeric(sub(".* ", "", df_fore_10$Months))
df_fore_10$month_index <- rep(1:12, length.out = nrow(df_fore_10))
df_filtered_MNAR_fore_10$month_index <- rep(1:12, length.out = nrow(df_filtered_MNAR_fore_10)) 
#names(dfinal)[names(dfinal) == "combined_column4"] <- "deaths_forecast_5years"
df_fore_10m <- df_filtered_MNAR_fore_10
df_MNAR_f <-rbind(df_fore_10,df_fore_10m)
View(df_MNAR_f)
df_MNAR_ff <- cbind(df_MNAR_f, dfinal)
View(df_MNAR_ff)


df_MNAR_ff <- df_MNAR_ff %>%
  mutate(deaths_due_to_severe_malaria = ifelse(is.na(deaths_due_to_severe_malaria), deaths_forecast_5years, deaths_due_to_severe_malaria))

View(df_MNAR_ff)
missing_indices_MNAR_ff <- which(is.na(df_MNAR_f$deaths_due_to_severe_malaria))
missing_indices_MNAR_fff <- numeric(length(num_missing_MNAR))
#
for(i in missing_indices_MNAR_ff){missing_indices_MNAR_fff[[as.character(i)]] <- print(abs(df_Fore_10$deaths_due_to_severe_malaria[i]-df_MNAR_ff$deaths_due_to_severe_malaria[i]))}
#
for (i in missing_indices_MNAR_ff){print(paste("differences for row",i,":" ,missing_indices_MNAR_fff[[as.character(i)]]))}
# #print(differences)
#View(missing_indices_MNAR_fff)

# 
# Calculate the mean of the differences
mean_value_10_fore_MNAR <- mean(missing_indices_MNAR_fff[2:7], na.rm = TRUE)
mean_value_10_fore_MNAR
# Calculate the confidence interval
ci <- t.test(missing_indices_MNAR_fff[2:7])$conf.int
ci
summary(missing_indices_MNAR_fff[2:7])
data_MNARfore_10_1 <- df_missing_10XX
write_xlsx(data_MNARfore_10_1, "/home/ntokozo/Documents/research/data_MNARfore_10_1.xlsx")
# Print the results
# write_xlsx(data_MNfore_10_6, "/home/ntokozo/Documents/research/data_MNfore_10_6.xlsx")
# Print the results
##print(paste("Mean of differences:", mean_value_10_fore_MNAR))
#print(paste("Confidence interval of differences:", ci))

#t-test
#t_test_result <- t.test(missing_indices_MNAR_fff[2:7], diffmean_MNAR_10[7:12])
#t_test_result

#print(paste("Confidence interval of differences:", ci))
