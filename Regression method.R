#install.packages("dynlm")
library(dynlm)
library(readr)
library(ggplot2)
#install.packages("forecast")
library(forecast)

#install.packages("fpp2")
#library(fpp2)
#install.packages("dplyr")
library(dplyr)
library(lubridate)
#library(quantmod)
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
#deaths$deaths_due_to_severe_malaria
glimpse(deaths)
#str(deaths)
severe_malaria_cases <- read_excel("severe_malaria_cases.xlsx")
#View(severe_malaria_cases)
mal_case <- cbind(deaths, severe_malaria_cases$Case_of_severe_malaria_confirmed)  
names(mal_case)[names(mal_case) == "severe_malaria_cases$Case_of_severe_malaria_confirmed"] <- "severe_cases"

fit <- auto.arima(mal_case$deaths_due_to_severe_malaria, xreg = mal_case$severe_cases)
  
 # Generate fitted values
fitted_values <- fitted(fit)
 #View(fitted_values)
  # Create a data frame for plotting
deaths_plot <- deaths %>%
 mutate(Fitted = fitted_values)
 #View(deaths_plot)
  # Plotting using ggplot2
# ggplot(deaths_plot, aes(x =  mal_case$severe_cases)) +
#     geom_line(aes(y =deaths_due_to_severe_malaria ), color = 'blue', size = 1) +  # Original data
#     geom_line(aes(y = Fitted), color = 'red', linetype = "dashed", size = 1) +   # Fitted values
#     xlab("Year") +
#     ylab("Deaths due to Severe Malaria") +
#     ggtitle("Deaths due to Severe Malaria with ARIMA Fit") +
#     theme_minimal()
#cheking residuals
checkresiduals(fit)  
summary(fit)  
# 
# #creating missing values
# #View(deaths)
# deaths
# deaths_RM_5 <- deaths
# 
# # Define the range of rows to introduce missing values
# row_range <- 73:132
# 
# # Calculate the number of rows to set as NA (5% of the total rows in the specified range)
# num_rows <- length(row_range)
# num_missing_RM <- ceiling(num_rows * 0.05)
# num_missing_RM
# 
# # Randomly select the rows to set as NA
# #set.seed(2)  # For reproducibility; you can remove this for actual randomness
# missing_rows_RM <- sample(row_range, num_missing_RM)
# missing_rows_RM
# # Set the selected rows' 'deaths_due_to_severe_malaria' column to NA
# deaths_RM_5$deaths_due_to_severe_malaria[missing_rows_RM] <- NA
# 
# # Print the modified data frame
# print(deaths_RM_5)
# #View(deaths_RM_5)  #  View data
# #binding the missing data and the fitted values
# data_final5 <- cbind(deaths_RM_5, fitted_values)
# #View(data_final5)
# #imputting missing values 
# 
# data_final5 <- data_final5 %>%
#     mutate(deaths_due_to_severe_malaria = ifelse(is.na(deaths_due_to_severe_malaria),fitted_values, deaths_due_to_severe_malaria))
# 
#   # Print the imputed dataset
#   print(data_final5)
# 
#   diffReg_5 <- numeric(length(missing_rows_RM))
#   for(i in missing_rows_RM){diffReg_5[[as.character(i)]] <- print(abs(deaths$deaths_due_to_severe_malaria[i]-data_final5$deaths_due_to_severe_malaria[i]))}
# 
#   for (i in missing_rows_RM){print(paste("differences for row",i,":" ,diffReg_5[[as.character(i)]]))}
#   #print(differences)
#  # View(diffReg_5)
# 
# mean_value_reg5 <- mean(diffReg_5[4:6])
# mean_value_reg5
# reg_5 <- diffReg_5[4:6]
# summary(reg_5)
# ci <- t.test(reg_5)$conf.int
# ci
# data_new6 <- data_final5
# write_xlsx(data_new6, "/home/ntokozo/Documents/research/data_new6.xlsx") 
###################################### MCAR 10 % ###########################
# deaths
# deaths_reg10 <- deaths
# 
# # Define the range of rows to introduce missing values
# row_range <- 73:132
# 
# # Calculate the number of rows to set as NA (10% of the total rows in the specified range)
# num_rows_reg10 <- length(row_range)
# num_missing_reg10 <- ceiling(num_rows_reg10 * 0.1)
# 
# # Randomly select the rows to set as NA
# #set.seed(1)  # For reproducibility; you can remove this for actual randomness
# missing_rows_reg10 <- sample(row_range, num_missing_reg10)
# 
# # Set the selected rows' 'deaths_due_to_severe_malaria' column to NA
# deaths_reg10$deaths_due_to_severe_malaria[missing_rows_reg10] <- NA
# 
# # Print the modified data frame
# print(deaths_reg10)
# #View(deaths_reg10)  # View data
# 
# data_final5 <- cbind(deaths_reg10, fitted_values)
# #View(data_final5)
# #imputting missing values 
# 
# data_final5 <- data_final5 %>%
#   mutate(deaths_due_to_severe_malaria = ifelse(is.na(deaths_due_to_severe_malaria),fitted_values, deaths_due_to_severe_malaria))
# 
# # Print the imputed dataset
# print(data_final5)
# 
# diffReg_10 <- numeric(length(missing_rows_reg10))
# for(i in missing_rows_reg10){diffReg_10[[as.character(i)]] <- print(abs(deaths$deaths_due_to_severe_malaria[i]-data_final5$deaths_due_to_severe_malaria[i]))}
# 
# for (i in missing_rows_reg10){print(paste("differences for row",i,":" ,diffReg_10[[as.character(i)]]))}
# #print(differences)
#  #View(diffReg_10)
# 
# mean_value_reg10 <- mean(diffReg_10[7:12])
# mean_value_reg10
# reg_10 <- diffReg_10[7:12]
# summary(reg_10)
# ci <- t.test(reg_10)$conf.int
# ci
# data_newreg10_6 <- data_final5
# write_xlsx(data_newreg10_6, "/home/ntokozo/Documents/research/data_newreg10_6.xlsx") 
# ############################################################################
# 
# ############################## MAR 5% ######################################
# deaths
# df_missing_MAR<-deaths[deaths$Time>72,]
# nje <- deaths#[deaths$Time>72,]
# nje$month_index <- rep(1:12, length.out = nrow(nje))
# df_missing_MAR$month_index <- rep(1:12, length.out = nrow(df_missing_MAR))
# df_missing_MAR
# 
# deathsX5 <- deaths[deaths$Time<73,]
# deathsX5$month_index <- rep(1:12, length.out = nrow(deathsX5))
# 
# #View(df_missing_MAR)
# # Calculating the total number of missing values (5% of the entire data set)
# #number of rows of the data set
# nrow(df_missing_MAR)
# total_missing <- 0.05 * nrow(df_missing_MAR)
# total_missing
# # Calculating the number of missing values to be inserted in December (95% of total missing)
# missing_Décembre <- floor(0.95 * total_missing)
# missing_Décembre
# # Calculating the number of missing values to be inserted in non-December months
# missing_non_Décembre <- total_missing - missing_Décembre
# missing_non_Décembre
# # Set seed for reproducibility
# #set.seed(1)
# 
# december_indices <- grep("Décembre", df_missing_MAR$Months)
# #View(december_indices)
# 
# non_december_indices <- setdiff(seq_len(nrow(df_missing_MAR)), december_indices)
# #View(non_december_indices)
# 
# # Generating missing values in the deaths_due_to_severe_malaria column for december entries
# df_missing_MAR$deaths_due_to_severe_malaria[sample(december_indices, size = missing_Décembre, replace = FALSE)] <- NA
# 
# # Generating missing values in the deaths_due_to_sefindvere_malaria column for non-december entries
# df_missing_MAR$deaths_due_to_severe_malaria[sample(non_december_indices, size = missing_non_Décembre, replace = FALSE)] <- NA
# 
# df_missing_5 <- df_missing_MAR
# #print(df_missing_5)
# #View(df_missing_5)
# #saving the data as an excel file
# #write.xlsx(df_missing_5, file= "df_missing_5.xlsx", sheetName = "Sheet1", append = TRUE)
# # Identify rows with missing values
# missing_indices_MAR <- which(is.na(df_missing_5$deaths_due_to_severe_malaria))
# # Calculate the mean for each month where values are not missing
# deathsX5$month_index <- rep(1:12, length.out = nrow(deathsX5))
# df_missing_5X <-rbind(deathsX5,df_missing_5)
# #View(df_missing_5X)
# missing_indices_MAR <- which(is.na(df_missing_5X$deaths_due_to_severe_malaria))
# #imputation
# #data_final5 <- cbind(deaths_reg10, fitted_values)
# #View(data_final5)
# #imputting missing values 
# data_final5 <- cbind(df_missing_5X, fitted_values)
# #View(data_final5)
# #View(nje)
# data_final5 <- data_final5 %>%
#   mutate(deaths_due_to_severe_malaria = ifelse(is.na(deaths_due_to_severe_malaria),fitted_values, deaths_due_to_severe_malaria))
# #View(data_final5)
# # differences
# difmean_MAR <- numeric(length(missing_indices_MAR))
# 
# for(i in missing_indices_MAR){difmean_MAR[[as.character(i)]] <- print(abs(deaths$deaths_due_to_severe_malaria[i]-data_final5$deaths_due_to_severe_malaria[i]))}
# 
# for (i in missing_indices_MAR){print(paste("differences for row",i,":" ,difmean_MAR[[as.character(i)]]))}
# #print(differences)
# #View(difmean_MAR)
# 
# # Calculate the mean of the differences
# mean_value_MAR5 <- mean(difmean_MAR[4:6], na.rm = TRUE)
# mean_value_MAR5
# mm5 <-difmean_MAR[4:6]
# summary(mm5)
# # Calculate the confidence interval
# ci <- t.test(mm5)$conf.int
# ci
# data_newregMAR5_6 <- data_final5
# write_xlsx(data_newregMAR5_6, "/home/ntokozo/Documents/research/data_newregMAR5_6.xlsx") 
# #####################################################################################
# deaths
# df_missing_MAR_10 <-deaths[deaths$Time>72,]
# nje_10 <- deaths#[deaths$Time>72,]
# nje_10$month_index <- rep(1:12, length.out = nrow(nje_10))
# df_missing_MAR_10$month_index <- rep(1:12, length.out = nrow(df_missing_MAR_10))
# df_missing_MAR_10
# #View(nje_10)
# #View(df_missing_MAR_10)
# # Calculating the total number of missing values (5% of the entire data set)
# #number of rows of the data set
# 
# #
# # # Step 1: Calculate the number of rows in the dataframe
#  num_rows <- nrow(df_missing_MAR_10)
#  num_rows
# # # Step 2: Calculate the total number of missing values (10% of total rows)
#  total_missing_10 <- 0.1 * num_rows
# #
# # # Step 3: Initialize missing values for December and non-December
#  missing_December_10 <- 0
#  missing_non_December_10 <- 0
# 
#  if (total_missing_10 >= 1) {
# #   # Step 4: Calculate missing values for December (95% of total)
#    missing_December_10 <- floor(0.95 * total_missing_10)
# #
# #   # Step 5: Ensure at least one missing value is assigned to non-December months
#    if (missing_December_10 == total_missing_10) {
#      missing_December_10 <- total_missing_10 - 1
#      missing_non_December_10 <- 1
#    } else {
#      missing_non_December_10 <- total_missing_10 - missing_December_10
#    }
#  } else {
#    # If total_missing_10 is less than 1, assign it all to non-December months
#    missing_non_December_10 <- total_missing_10
#  }
# 
#  # Print the results for verification
#  print(paste("Total rows:", num_rows))
#  print(paste("Total missing values (10%):", total_missing_10))
#  print(paste("Missing values for December (95%):", missing_December_10))
#  print(paste("Missing values for non-December months:", missing_non_December_10))
# 
# # nrow(df_missing_MAR_10)
# total_missing_10 <- 0.1 * nrow(df_missing_MAR_10)
# total_missing_10
# # # Calculating the number of missing values to be inserted in December (95% of total missing)
#  missing_Décembre_10 <- floor(0.95 * total_missing_10)
#  missing_Décembre_10
# # # Calculating the number of missing values to be inserted in non-December months
#  missing_non_Décembre_10 <- total_missing_10 - missing_Décembre_10
#  missing_non_Décembre_10
# # Set seed for reproducibility
# #set.seed(1)
# 
# #indices
# december_indices_10 <- grep("Décembre", df_missing_MAR_10$Months)
# #View(december_indices_10)
# 
# non_december_indices_10 <- setdiff(seq_len(nrow(df_missing_MAR_10)), december_indices_10)
# #View(non_december_indices_10)
# 
# # Generating missing values in the deaths_due_to_severe_malaria column for december entries
#  df_missing_MAR_10$deaths_due_to_severe_malaria[sample(december_indices_10, size = missing_Décembre_10, replace = FALSE)] <- NA
# 
# # Generating missing values in the deaths_due_to_sefindvere_malaria column for non-december entries
# df_missing_MAR_10$deaths_due_to_severe_malaria[sample(non_december_indices_10, size = missing_non_December_10, replace = FALSE)] <- NA
# deathsX <- deaths[deaths$Time<73,]
# deathsX$month_index <- rep(1:12, length.out = nrow(deathsX))
# 
# df_missing_10 <- df_missing_MAR_10
# df_missing_10X <-rbind(deathsX,df_missing_10)
# #View(df_missing_10X)
# 
# # Identify rows with missing values
# missing_indices_MAR_10 <- which(is.na(df_missing_10X$deaths_due_to_severe_malaria))
# 
# data_final5 <- cbind(df_missing_10X, fitted_values)
# #View(data_final5)
# #View(nje)
# data_final5 <- data_final5 %>%
#   mutate(deaths_due_to_severe_malaria = ifelse(is.na(deaths_due_to_severe_malaria),fitted_values, deaths_due_to_severe_malaria))
# #View(data_final5)
# # differences
# difmean_MAR <- numeric(length(missing_indices_MAR_10))
# 
# for(i in missing_indices_MAR_10){difmean_MAR[[as.character(i)]] <- print(abs(deaths$deaths_due_to_severe_malaria[i]-data_final5$deaths_due_to_severe_malaria[i]))}
# 
# for (i in missing_indices_MAR_10){print(paste("differences for row",i,":" ,difmean_MAR[[as.character(i)]]))}
# #print(differences)
# 
# difmean_MAR_10 <- numeric(length(df_missing_10))
# #
# for(i in missing_indices_MAR_10){difmean_MAR_10[[as.character(i)]] <- print(abs(nje_10$deaths_due_to_severe_malaria[i]-data_final5$deaths_due_to_severe_malaria[i]))}
# #
# for (i in missing_indices_MAR_10){print(paste("differences for row",i,":" ,difmean_MAR_10[[as.character(i)]]))}
# # #print(differences)
# #View(difmean_MAR_10)
# #View(nje_10)
# #
# # Calculate the mean of the differences
# mean_value_10_MAR <- mean(difmean_MAR_10[5:10], na.rm = TRUE)
# mean_value_10_MAR
# mean10 <- difmean_MAR_10[5:10]
# summary(mean10)
# # Calculate the confidence interval
# ci <- t.test(mean10)$conf.int
# ci
# data_newregMAR10_6 <- data_final5
# write_xlsx(data_newregMAR10_6, "/home/ntokozo/Documents/research/data_newregMAR10_6.xlsx") 

##########################################################################################
########################### MNAR 5% ###################################################

# Create 5% missing values with 95% probability being the highest values
#set.seed(1) # Set seed for reproducibility\
# deaths
# df <- deaths
# #
# # View the first few rows of the dataframe
# head(df)
# 
# # Extract the year from the Months column
# df$year <- as.numeric(sub(".* ", "", df$Months))
# 
# # Filter the data for the years 2013 to 2023
# df_filtered_MNAR <- df %>%
#   filter(year >= 2019 & year <= 2023)
# 
# # Find the highest value for each year
# highest_values_MNAR <- df_filtered_MNAR %>%
#   group_by(year) %>%
#   summarise(max_deaths = max(deaths_due_to_severe_malaria, na.rm = TRUE))
# 
# # Print the result336.7236 354.7924 314.1376
# print(highest_values_MNAR)
# #View(highest_values_MNAR)
# # Calculate the number of rows to introduce missing values
# n <- nrow(df_filtered_MNAR)
# num_missing <- round(0.05 * n)
# 
# # Extract the highest deaths for each year
# highest_deaths <- highest_values_MNAR$max_deaths
# 
# # Create a logical vector where highest deaths are TRUE
# is_highest <- df_filtered_MNAR$deaths_due_to_severe_malaria %in% highest_deaths
# 
# # Determine the number of missing values to insert in highest values (95% of total missing)
# num_missing_highest <- round(0.95 * num_missing)
# 
# # Select rows with highest deaths
# highest_indices <- which(is_highest)
# 
# # Randomly select indices for missing values among the highest deaths
# missing_highest_indices <- sample(highest_indices, size = num_missing_highest, replace = FALSE)
# 
# # Select remaining missing values to be inserted randomly
# num_missing_remaining <- num_missing - length(missing_highest_indices)
# remaining_indices <- setdiff(1:n, highest_indices)
# 
# # Randomly select indices for the remaining missing values
# missing_remaining_indices <- sample(remaining_indices, size = num_missing_remaining, replace = FALSE)
# 
# # Combine indices
# missing_indices <- c(missing_highest_indices, missing_remaining_indices)
# 
# # Introduce missing values
# df_filtered_MNAR$deaths_due_to_severe_malaria[missing_indices] <- NA
# 
# # Print the modified dataframe
# df_filtered_5 <- df_filtered_MNAR
# print(df_filtered_5)
# #View(df_filtered_5)
# 
# 
# # 
# df_missing_10 <- df_filtered_5
# deaths_11 <- deaths[deaths$Time<73,]
# deaths_11$year <- rep(1:12, length.out = nrow(deaths_11))
# df_missing_100 <-rbind( deaths_11,df_missing_10)
# #View(df_missing_100)
# 
# 
# # Identify rows with missing values
#  missing_indices_MNAR_10 <- which(is.na(df_missing_100$deaths_due_to_severe_malaria))
#  
# data_final5 <- cbind(df_missing_100, fitted_values)
# #View(data_final5)
# 
# data_final5 <- data_final5 %>%
#   mutate(deaths_due_to_severe_malaria = ifelse(is.na(deaths_due_to_severe_malaria),fitted_values, deaths_due_to_severe_malaria))
# #View(data_final5)
# # differences
# difmean_MNAR5 <- numeric(length(missing_indices_MNAR_10))
# 
# for(i in missing_indices_MNAR_10){difmean_MNAR5[[as.character(i)]] <- print(abs(deaths$deaths_due_to_severe_malaria[i]-data_final5$deaths_due_to_severe_malaria[i]))}
# 
# for (i in missing_indices_MNAR_10){print(paste("differences for row",i,":" ,difmean_MNAR5[[as.character(i)]]))}
# #print(differences)
# 
# 
# # #print(differences)
# #View(difmean_MAR_10)
# #View(nje_10)
# #
# # Calculate the mean of the differences
# mean_value_5_MNAR <- mean(difmean_MNAR5[4:6], na.rm = TRUE)
# mean_value_5_MNAR
# meanM <- difmean_MNAR5[4:6]
# summary(meanM)
# # Calculate the confidence interval
# ci <- t.test(meanM)$conf.int
# ci
# data_newregMNAR5_6 <- data_final5
# write_xlsx(data_newregMNAR5_6, "/home/ntokozo/Documents/research/data_newregMNAR5_6.xlsx") 

############################ MNAR 10% ######################################################################################
# View the first few rows of the dataframe
deaths
df_10 <- deaths
head(df_10)

# Extract the year from the Months column
df_10$year <- as.numeric(sub(".* ", "", df_10$Months))

# Filter the data for the years 2013 to 2023
df_filtered_MNAR_10 <- df_10 %>%
  filter(year >= 2019 & year <= 2023)

# Find the highest value for each year
highest_values_MNAR_10 <- df_filtered_MNAR_10 %>%
  group_by(year) %>%
  summarise(max_deaths = max(deaths_due_to_severe_malaria, na.rm = TRUE))

# Print the result336.7236 354.7924 314.1376
print(highest_values_MNAR_10)
#View(highest_values_MNAR_10)
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
#View(df_filtered_MNAR_10)

df_missing_101 <- df_filtered_MNAR_10
deaths_112 <- deaths[deaths$Time<73,]
deaths_112$year <- rep(1:12, length.out = nrow(deaths_112))
df_missing_1001 <-rbind( deaths_112,df_missing_101)
#View(df_missing_100)


# Identify rows with missing values
missing_indices_MNAR_102 <- which(is.na(df_missing_1001$deaths_due_to_severe_malaria))

data_final5 <- cbind(df_missing_1001, fitted_values)


data_final5 <- data_final5 %>%
  mutate(deaths_due_to_severe_malaria = ifelse(is.na(deaths_due_to_severe_malaria),fitted_values, deaths_due_to_severe_malaria))
#View(data_final5)
# differences
difmean_MNAR10 <- numeric(length(missing_indices_MNAR_102))

for(i in missing_indices_MNAR_102){difmean_MNAR10[[as.character(i)]] <- print(abs(deaths$deaths_due_to_severe_malaria[i]-data_final5$deaths_due_to_severe_malaria[i]))}

for (i in missing_indices_MNAR_102){print(paste("differences for row",i,":" ,difmean_MNAR10[[as.character(i)]]))}
#print(differences)


# #print(differences)
#View(difmean_MAR_10)
#View(nje_10)
#
# Calculate the mean of the differences
mean_value_10_MNAR <- mean(difmean_MNAR10[7:12], na.rm = TRUE)
mean_value_10_MNAR
meanMM <- difmean_MNAR10[7:12]
summary(meanMM)
# Calculate the confidence interval
ci <- t.test(meanMM)$conf.int
ci
data_newregMNAR10_6 <- data_final5
write_xlsx(data_newregMNAR10_6, "/home/ntokozo/Documents/research/data_newregMNAR10_6.xlsx") 

