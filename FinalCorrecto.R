# Load necessary libraries
library(dplyr)

# Assuming DBHE.csv is the file name of the dataset and it's located in the working directory
Data2 <- read.csv("DBHE.csv")

# Function to classify Burnout risk
classify_burnout_risk <- function(SCORE_MBI_S1, SCORE_MBI_S2, SCORE_MBI_S3N) {
  classify_variable <- function(value, low_limit, mod_lower, mod_upper, high_limit) {
    if (value <= low_limit) {
      return("Low")
    } else if (value >= mod_lower & value <= mod_upper) {
      return("Moderate")
    } else if (value >= high_limit) {
      return("High")
    }
  }
  
  SCORE_MBI_S1_Risk <- sapply(SCORE_MBI_S1, classify_variable, low_limit = 18, mod_lower = 19, mod_upper = 26, high_limit = 27)
  SCORE_MBI_S2_Risk <- sapply(SCORE_MBI_S2, classify_variable, low_limit = 5, mod_lower = 6, mod_upper = 9, high_limit = 10)
  SCORE_MBI_S3N_Risk <- sapply(SCORE_MBI_S3N, classify_variable, low_limit = 33, mod_lower = 34, mod_upper = 39, high_limit = 40)
  
  Burnout_Risk <- ifelse(SCORE_MBI_S1_Risk == "High" & SCORE_MBI_S2_Risk == "High" & SCORE_MBI_S3N_Risk == "Low", "High risk of Burnout",
                         ifelse(SCORE_MBI_S1_Risk == "Moderate" & SCORE_MBI_S2_Risk == "Moderate" & SCORE_MBI_S3N_Risk == "Moderate", "Moderate risk of Burnout",
                                ifelse(SCORE_MBI_S1_Risk == "Low" & SCORE_MBI_S2_Risk == "Low" & SCORE_MBI_S3N_Risk == "High", "Low risk of Burnout", "Unclassified")))
  
  return(Burnout_Risk)
}

# Function to classify RES levels
RESNiveles <- function(var1) {
  rango <- numeric()
  m <- 0
  for (i in 1:length(var1)) {
    if (var1[i] <= 5.7) {
      m <- m + 1
      rango[m] <- 1
    }
    if (var1[i] > 5.7 & var1[i] <= 7.5) {
      m <- m + 1
      rango[m] <- 2
    }
    if (var1[i] > 7.5 & var1[i] <= 8.3) {
      m <- m + 1
      rango[m] <- 3
    }
    if (var1[i] > 8.3 & var1[i] <= 9.2) {
      m <- m + 1
      rango[m] <- 4
    }
    if (var1[i] > 9.2) {
      m <- m + 1
      rango[m] <- 5
    }
  }
  return(rango)
}

# Function to classify PWB levels
PWBNiveles <- function(var1) {
  rango <- numeric()
  m <- 0
  for (i in 1:length(var1)) {
    if (var1[i] <= 168) {
      m <- m + 1
      rango[m] <- 1
    }
    if (var1[i] > 168 & var1[i] <= 336) {
      m <- m + 1
      rango[m] <- 2
    }
    if (var1[i] > 336 & var1[i] <= 504) {
      m <- m + 1
      rango[m] <- 3
    }
  }
  return(rango)
}

# Creating SCORE_MBI_S3N
Data2 <- Data2 %>%
  mutate(SCORE_MBI_S3N = MBI4_REVERSE + MBI7_REVERSE + MBI9_REVERSE + MBI12_REVERSE + 
           MBI17_REVERSE + MBI18_REVERSE + MBI19_REVERSE + MBI21_REVERSE)

# Creating the classification columns
Data2 <- Data2 %>%
  mutate(Burnout_Risk = classify_burnout_risk(SCORE_MBI_S1, SCORE_MBI_S2, SCORE_MBI_S3N),
         PWB_Level = PWBNiveles(TOTALSCORE_PWB),
         RES_Level = RESNiveles(TOTALSCORE_RES))

# Creating the classificationSET data frame
classificationSET <- Data2 %>%
  select(Burnout_Risk, PWB_Level, RES_Level)

# Saving the data frame to a CSV file
write.csv(classificationSET, "classificationSET.csv", row.names = FALSE)

# Print the first few rows of classificationSET to verify
head(classificationSET)
