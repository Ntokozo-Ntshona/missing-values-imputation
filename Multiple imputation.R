# Install and load necessary package
install.packages("mice")
library(mice)
library(readr)
#library(ggplot2)
#install.packages("forecast")
#library(forecast)
#install.packages("fpp2")
#library(fpp2)
library(dplyr)
library(lubridate)
#library(quantmod)
#library(tseries)
library(xts)
library(tidyverse)
library(tibble)
#install.packages("writexl")
library("writexl")
library(readxl)
install.packages("VIM")
library(VIM)




deaths <- read_excel("/home/ntokozo/Documents/research/Ntokozon.xlsx", sheet = "Sheet1", col_types = c("numeric","text", "numeric"))
#View(deaths)
deaths 
#data missing 

#Create a copy of the original data frame to modify
deaths_55 <- deaths
#View(deaths_55)
# Define the range of rows to introduce missing values
row_range <- 73:132
#View(row_range)
# Calculate the number of rows to set as NA (5% of the total rows in the specified range)
num_rows <- length(row_range)
num_missing <- ceiling(num_rows * 0.05)

# Randomly select the rows to set as NA
#set.seed(3)  # For reproducibility; you can remove this for actual randomness
missing_rows <- sample(row_range, num_missing)

# Set the selected rows' 'deaths_due_to_severe_malaria' column to NA
deaths_55$deaths_due_to_severe_malaria[missing_rows] <- NA

# Print the modified data frame
print(deaths_55)
#View(deaths_55)  #  View data
# imputed_data <- mice(deaths_55, m = 5, method = 'pmm', maxit = 50, seed = 500)
# 
# # Check the imputed data
# complete_data <- complete(imputed_data, 1)
#View(complete_data)
# Perform analysis on imputed datasets
#fit <- with(imputed_data, lm(your_model_formula))
#summary(pool(fit))
par(mfrow=c(1,1))
par(mar=c(5, 4, 4, 2) + 0.1)

aggr(deaths_55, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(deaths_55), cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))

#aggr(deaths_55)
#mice

deaths_imputed <- mice(deaths_55)
#check the structure of the imputed data 
attributes(deaths_imputed)
# how to get the complete data
deaths_comp <- complete(deaths_imputed)
#aggregate(deaths_comp)
par(mfrow = c(2,2))
boxplot(deaths$deaths_due_to_severe_malaria, main = "Data with no missing ")
boxplot(deaths_comp$deaths_due_to_severe_malaria, main = "imputed data set")

plot(density(deaths$deaths_due_to_severe_malaria, na.rm = TRUE), main= "eaths", col = "black")
lines(density(deaths_comp$deaths_due_to_severe_malaria, na.rm = TRUE), col = "red", lty=3)
legend( x = "topright", legend=c("Original Dataset", "Imputed Dataset"),  
       fill = c("black","red") 
)
differences <- numeric(length(missing_rows))
for(i in missing_rows){differences[[as.character(i)]] <- abs(deaths$deaths_due_to_severe_malaria[i]-deaths_comp$deaths_due_to_severe_malaria[i])}
for (i in missing_rows){print(paste("differences for row",i,":" ,differences[[as.character(i)]]))}
print(differences)
# View(differences)

mean_MCAR_MI_5 <- mean(differences[4:6], na.rm = TRUE)
mean_MCAR_MI_5
mi <- differences[4:6]
summary(mi)
# Calculate the confidence interval
ci <- t.test(mi)$conf.int
ci

data_MI_5_6 <- deaths_comp
write_xlsx(data_MI_5_6, "/home/ntokozo/Documents/research/data_MI_5_6.xlsx")

############################### MCAR 10 % #################################################
# Create a copy of the original data frame to modify
deaths
deaths_10 <- deaths

# Define the range of rows to introduce missing values
row_range <- 73:132

# Calculate the number of rows to set as NA (10% of the total rows in the specified range)
num_rows_10 <- length(row_range)
num_missing_10 <- ceiling(num_rows_10 * 0.1)

# Randomly select the rows to set as NA
#set.seed(1)  # For reproducibility; you can remove this for actual randomness
missing_rows_10 <- sample(row_range, num_missing_10)

# Set the selected rows' 'deaths_due_to_severe_malaria' column to NA
deaths_10$deaths_due_to_severe_malaria[missing_rows_10] <- NA

# Print the modified data frame
print(deaths_10)
#View(deaths_10)  # View data

par(mfrow=c(1,1))
par(mar=c(5, 4, 4, 2) + 0.1)

aggr(deaths_10, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(deaths_55), cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))

#aggr(deaths_55)
#mice

deaths_imputed10 <- mice(deaths_10)
#check the structure of the imputed data 
attributes(deaths_imputed10)
# how to get the complete data
deaths_comp10 <- complete(deaths_imputed10)
#aggregate(deaths_comp)
par(mfrow = c(2,2))
boxplot(deaths$deaths_due_to_severe_malaria, main = "Data with no missing ")
boxplot(deaths_comp10$deaths_due_to_severe_malaria, main = "imputed data set")

plot(density(deaths$deaths_due_to_severe_malaria, na.rm = TRUE), main= "deaths", col = "black")
lines(density(deaths_comp10$deaths_due_to_severe_malaria, na.rm = TRUE), col = "red", lty=3)
legend( x = "topright", legend=c("Original_Data", "Imputed_Data"),  
        fill = c("black","red") 
)
differences10 <- numeric(length(missing_rows_10))
for(i in missing_rows_10){differences10[[as.character(i)]] <- abs(deaths$deaths_due_to_severe_malaria[i]-deaths_comp10$deaths_due_to_severe_malaria[i])}
for (i in missing_rows_10){print(paste("differences for row",i,":" ,differences10[[as.character(i)]]))}
print(differences10)
# View(differences)

mean_MCAR_MI_10 <- mean(differences10[7:12], na.rm = TRUE)
mean_MCAR_MI_10
mi <- differences10[7:12]
summary(mi)
# Calculate the confidence interval
ci <- t.test(mi)$conf.int
ci

data_MI_10_6 <- deaths_comp
write_xlsx(data_MI_10_6, "/home/ntokozo/Documents/research/data_MI_10_6.xlsx")
################################################### The End ###################################################

#################################################### MAR 5% ##################################################

 #Assigning deaths to the new name
 deaths
 df_missing_MAR<-deaths[deaths$Time>72,]
 nje <- deaths#[deaths$Time>72,]
 nje$month_index <- rep(1:12, length.out = nrow(nje))
 df_missing_MAR$month_index <- rep(1:12, length.out = nrow(df_missing_MAR))
 df_missing_MAR
 
 deathsX5 <- deaths[deaths$Time<73,]
 deathsX5$month_index <- rep(1:12, length.out = nrow(deathsX5))
 
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
 
 df_missing_5 <- df_missing_MAR
 #print(df_missing_5)
 #View(df_missing_5)
 #saving the data as an excel file
 #write.xlsx(df_missing_5, file= "df_missing_5.xlsx", sheetName = "Sheet1", append = TRUE)
 # Identify rows with missing values
 missing_indices_MAR <- which(is.na(df_missing_5$deaths_due_to_severe_malaria))
 # Calculate the mean for each month where values are not missing
 deathsX5$month_index <- rep(1:12, length.out = nrow(deathsX5))
 df_missing_5X <-rbind(deathsX5,df_missing_5)
 #View(df_missing_5X)
 
 
 
 par(mfrow=c(1,1))
 par(mar=c(5, 4, 4, 2) + 0.1)
 
 aggr(df_missing_5X, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(df_missing_5X), cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))
 
 #aggr(deaths_55)
 #mice
 deaths_imputedMAR5 <- mice(df_missing_5X)
 #check the structure of the imputed data 
 attributes(deaths_imputedMAR5)
 # how to get the complete data
 deaths_compMAR5 <- complete(deaths_imputedMAR5)
 #aggregate(deaths_comp)
 par(mfrow = c(2,2))
 boxplot(deaths$deaths_due_to_severe_malaria, main = "Data with no missing ")
 boxplot(deaths_compMAR5$deaths_due_to_severe_malaria, main = "imputed data set")
 
 plot(density(deaths$deaths_due_to_severe_malaria, na.rm = TRUE), main= "deaths", col = "black")
 lines(density(deaths_compMAR5$deaths_due_to_severe_malaria, na.rm = TRUE), col = "red", lty=3)
 legend( x = "topright", legend=c("Original_Data", "Imputed_Data"),  
         fill = c("black","red") 
 )
 
 missing_indices_MAR <- which(is.na(df_missing_5X$deaths_due_to_severe_malaria))
 
 difmean_MAR <- numeric(length(df_missing_5X))
 
 for(i in missing_indices_MAR){difmean_MAR[[as.character(i)]] <- print(abs(nje$deaths_due_to_severe_malaria[i]-deaths_compMAR5$deaths_due_to_severe_malaria[i]))}
 
 for (i in missing_indices_MAR){print(paste("differences for row",i,":" ,difmean_MAR[[as.character(i)]]))}
 #print(differences)
 #View(difmean_MAR)
 
 
 # Calculate the mean of the differences
 mean_value_MAR5 <- mean(difmean_MAR[5:7], na.rm = TRUE)
 mean_value_MAR5
 # Calculate the confidence interval
 MAR5<- difmean_MAR[5:7]
 summary(MAR5)
 ci <- t.test(MAR5)$conf.int
 ci
 
 data_MAR_5_6 <- deaths_comp
 write_xlsx(data_MAR_5_6, "/home/ntokozo/Documents/research/data_MAR_5_6.xlsx")
########################################## END ##############################################

######################### MAR 10 % #############################################
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
#set.seed(1)

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
#View(df_missing_10X)
#saving the data as an excel file
#write.xlsx(df_missing_5, file= "df_missing_5.xlsx", sheetName = "Sheet1", append = TRUE)
# Identify rows with missing values
missing_indices_MAR_10 <- which(is.na(df_missing_10X$deaths_due_to_severe_malaria))




par(mfrow=c(1,1))
par(mar=c(5, 4, 4, 2) + 0.1)

aggr(df_missing_10X, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(df_missing_10X), cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))

#aggr(deaths_55)
#mice
#View(df_missing_10X)
deaths_imputedMAR10 <- mice(df_missing_10X)
#View(deaths_imputedMAR10)
#check the structure of the imputed data
attributes(deaths_imputedMAR10)
# how to get the complete data
deaths_compMAR10 <- complete(deaths_imputedMAR10)
#aggregate(deaths_comp)
par(mfrow = c(2,2))
boxplot(deaths$deaths_due_to_severe_malaria, main = "Data with no missing ")
boxplot(deaths_compMAR5$deaths_due_to_severe_malaria, main = "imputed data set")

plot(density(deaths$deaths_due_to_severe_malaria, na.rm = TRUE), main= "deaths", col = "black")
lines(density(deaths_compMAR5$deaths_due_to_severe_malaria, na.rm = TRUE), col = "red", lty=3)
legend( x = "topright", legend=c("Original_Data", "Imputed_Data"),
        fill = c("black","red")
)




difmean_MAR_10 <- numeric(length(missing_indices_MAR_10))
#
for(i in missing_indices_MAR_10){difmean_MAR_10[[as.character(i)]] <- print(abs(nje_10$deaths_due_to_severe_malaria[i]-deaths_compMAR10$deaths_due_to_severe_malaria[i]))}
#
for (i in missing_indices_MAR_10){print(paste("differences for row",i,":" ,difmean_MAR_10[[as.character(i)]]))}
# #print(differences)
#View(difmean_MAR_10)
#View(nje_10)
#
# Calculate the mean of the differences
mean_value_10_MAR <- mean(difmean_MAR_10[5:10], na.rm = TRUE)
mean_value_10_MAR
MAR10 <- difmean_MAR_10[5:10]
summary(MAR10)
# Calculate the confidence interval
ci <- t.test(MAR10)$conf.int
ci
data_MAR_10_6 <- deaths_compMAR10
write_xlsx(data_MAR_10_6, "/home/ntokozo/Documents/research/data_MAR_10_6.xlsx")
#####################################################################################

################################## MNAR 5% #####################################
deaths
 df <- deaths
#
# # View the first few rows of the dataframe
 head(df)
#
# # Extract the year from the Months column
 df$year <- as.numeric(sub(".* ", "", df$Months))
#
# # Filter the data for the years 2013 to 2023
df_filtered <- df %>%
  filter(year >= 2019 & year <= 2023)
#
# # Find the highest value for each year
highest_values <- df_filtered %>%
  group_by(year) %>%
  summarise(max_deaths = max(deaths_due_to_severe_malaria, na.rm = TRUE))
#
# # Print the result336.7236 354.7924 314.1376
 print(highest_values)
 #View(highest_values)
##
#
# # Create 5% missing values with 95% probability being the highest values
# #set.seed(1) # Set seed for reproducibility
#
# # Calculate the number of rows to introduce missing values
 n <- nrow(df_filtered)
 num_missing <- round(0.05 * n)
#
# # Extract the highest deaths for each year
 highest_deaths <- highest_values$max_deaths
#
# # Create a logical vector where highest deaths are TRUE
 is_highest <- df_filtered$deaths_due_to_severe_malaria %in% highest_deaths

# # Determine the number of missing values to insert in highest values (95% of total missing)
 num_missing_highest <- round(0.95 * num_missing)
#
# # Select rows with highest deaths
highest_indices <- which(is_highest)
#
# # Randomly select indices for missing values among the highest deaths
 missing_highest_indices <- sample(highest_indices, size = num_missing_highest, replace = FALSE)
#
# # Select remaining missing values to be inserted randomly
 num_missing_remaining <- num_missing - length(missing_highest_indices)
remaining_indices <- setdiff(1:n, highest_indices)
#
# # Randomly select indices for the remaining missing values
 missing_remaining_indices <- sample(remaining_indices, size = num_missing_remaining, replace = FALSE)
#
# # Combine indices
 missing_indices <- c(missing_highest_indices, missing_remaining_indices)
#
# # Introduce missing values
df_filtered$deaths_due_to_severe_malaria[missing_indices] <- NA
#
# # Print the modified dataframe
 df_filtered_5 <- df_filtered
 print(df_filtered_5)
 #slicing the data from 1 to 72
 deaths
 deaths_cut <- deaths[1:72, ]
 deaths_cut$year <- as.numeric(sub(".* ", "", deaths_cut$Months))
 df_filteredd <- deaths_cut %>%
   filter(year >= 2013 & year <= 2018)
 #View(df_filteredd)
 new_data5 <- rbind(deaths_cut, df_filtered_5)
 #View(new_data5)
 missing_indices_MNAR <- which(is.na(new_data5$deaths_due_to_severe_malaria))
 #View(new_data5)
#multiple imputation
 par(mfrow=c(1,1))
 par(mar=c(5, 4, 4, 2) + 0.1)

 aggr(new_data5, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(new_data5), cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))

 #aggr(deaths_55)
 #mice
 deaths_imputedMNAR5 <- mice(new_data5)
 #check the structure of the imputed data
 attributes(deaths_imputedMNAR5)
 # how to get the complete data
 missing_indices_MNAR_5 <- which(is.na(new_data5$deaths_due_to_severe_malaria))
 # 
 deaths_compMNAR5 <- complete(deaths_imputedMNAR5)
 #View(deaths_compMNAR5)
 #aggregate(deaths_comp)
 par(mfrow = c(2,2))
 
 boxplot(deaths$deaths_due_to_severe_malaria, main = "Data with no missing ")
 boxplot(deaths_compMNAR5$deaths_due_to_severe_malaria, main = "imputed data set")

 plot(density(deaths$deaths_due_to_severe_malaria, na.rm = FALSE), main= "deaths", col = "black")
 lines(density(deaths_compMNAR5$deaths_due_to_severe_malaria, na.rm = TRUE), col = "red", lty=3)
 legend( x = "topright", legend=c("Original_Data", "Imputed_Data"),
         fill = c("black","red")
 )



 difmean_MNAR <- numeric(length(missing_indices_MNAR))

 for(i in missing_indices_MNAR){difmean_MNAR[[as.character(i)]] <- print(abs(deaths$deaths_due_to_severe_malaria[i]-deaths_compMNAR5$deaths_due_to_severe_malaria[i]))}

 for (i in missing_indices_MNAR){print(paste("differences for row",i,":" ,difmean_MNAR[[as.character(i)]]))}
 #print(differences)
 #View(difmean_MNAR)
 mean_value_5_MNAR <- mean(difmean_MNAR[4:6], na.rm = TRUE)
 mean_value_5_MNAR
 MNAR5 <- difmean_MNAR[4:6]
 summary(MNAR5)
 # Calculate the confidence interval
 ci <- t.test(MNAR5)$conf.int
 ci
library("xlsx")
 data_MNAR_5_6 <- deaths_compMNAR5
write.xlsx(data_MNAR_5_6, file= "dff_56.xlsx", sheetName = "Sheet1", append = TRUE)

#################################### MNAR 10% ##################################

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
highest_values_MNAR_10 <- df_filtered %>%
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


deaths
deaths_cutt <- deaths[1:72, ]
deaths_cutt$year <- as.numeric(sub(".* ", "", deaths_cutt$Months))
df_filteredd <- deaths_cutt %>%
  filter(year >= 2013 & year <= 2018)
#View(df_filteredd)
new_data10 <- rbind(deaths_cutt, df_filtered_MNAR_10)
#View(new_data10)
missing_indices_MNAR <- which(is.na(new_data10$deaths_due_to_severe_malaria))


par(mfrow=c(1,1))
par(mar=c(5, 4, 4, 2) + 0.1)

aggr(new_data10, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(new_data10), cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))

#aggr(deaths_55)
#mice
#View(df_missing_10X)
deaths_imputedMNAR10 <- mice(new_data10)
#View(deaths_imputedMAR10)
#check the structure of the imputed data
attributes(deaths_imputedMNAR10)
# how to get the complete data
deaths_compMNAR10 <- complete(deaths_imputedMNAR10)
View(deaths_compMNAR10)
#aggregate(deaths_comp)
par(mfrow = c(2,2))
boxplot(deaths$deaths_due_to_severe_malaria, main = "Data with no missing ")
boxplot(deaths_compMNAR10$deaths_due_to_severe_malaria, main = "imputed data set")

plot(density(deaths$deaths_due_to_severe_malaria, na.rm = TRUE), main= "deaths", col = "black")
lines(density(deaths_compMNAR10$deaths_due_to_severe_malaria, na.rm = TRUE), col = "red", lty=3)
legend( x = "topright", legend=c("Original_Data", "Imputed_Data"),
        fill = c("black","red")
)




difmean_MNAR_10 <- numeric(length(missing_indices_MNAR))
#
for(i in missing_indices_MNAR){difmean_MNAR_10[[as.character(i)]] <- print(abs(nje_10$deaths_due_to_severe_malaria[i]-deaths_compMNAR10$deaths_due_to_severe_malaria[i]))}
#
for (i in missing_indices_MNAR){print(paste("differences for row",i,":" ,difmean_MNAR_10[[as.character(i)]]))}
print(difmean_MNAR_10)
#View(difmean_MAR_10)
#View(nje_10)
#
# Calculate the mean of the differences
mean_value_10_MNAR <- mean(difmean_MNAR_10[7:12], na.rm = TRUE)
mean_value_10_MNAR
MNAR10 <- difmean_MNAR_10[7:12]
summary(MNAR10)
# Calculate the confidence interval
ci <- t.test(MNAR10)$conf.int
ci
data_MNAR_10_4 <- deaths_compMAR10
write_xlsx(data_MNAR_10_4, "/home/ntokozo/Documents/research/data_MNAR_10_4.xlsx")
