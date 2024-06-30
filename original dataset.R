library(readr)
library(ggplot2)
#install.packages(forecast)
#library(forecast)

#install.packages(fpp2)
library(fpp2)
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

#deaths <- read_excel("/home/ntokozo/Documents/research/death.xlsx", sheet = "Data", col_types = c("numeric","text", "numeric"))
deaths <- read_excel("/home/ntokozo/Documents/research/Ntokozon.xlsx", sheet = "Sheet1", col_types = c("numeric","text", "numeric"))
View(deaths)

###########################extracting and calculating the sample size###########

#extracting the the deaths_due_due_to_malaria

deaths_due_to_severe_malaria <- death_2$deaths_due_to_severe_malaria

##calculating the sample size

# Extracting the numeric column
deaths_due_to_severe_malaria <- death_2$deaths_due_to_severe_malaria
view(deaths_due_to_severe_malaria)
# Population standard deviation
pop_stdev <- sd(deaths_due_to_severe_malaria)

#  margin of error
margin_of_error <- 5  # Replace with your desired margin of error

# Z critical value for 95% confidence level
z_critical <- qnorm(0.975)

# Calculate the sample size
sample_size <- (z_critical * pop_stdev / margin_of_error) ^ 2
sample_size <- ceiling(sample_size)  # Round up to the nearest whole number

print("Calculated sample size:")
print(sample_size)

