# Load necessary library
library(dplyr)

# Load the dataset
dbhe_data <- read.csv("DBHE.csv",sep=",")

# Calculate SCORE_MBI_S3N
dbhe_data$SCORE_MBI_S3N <- dbhe_data$MBI4_REVERSE + dbhe_data$MBI7_REVERSE + dbhe_data$MBI9_REVERSE + 
  dbhe_data$MBI12_REVERSE + dbhe_data$MBI17_REVERSE + dbhe_data$MBI18_REVERSE + 
  dbhe_data$MBI19_REVERSE + dbhe_data$MBI21_REVERSE

# Define the classify_burnout_risk function
classify_burnout_risk <- function(SCORE_MBI_S1, SCORE_MBI_S2, SCORE_MBI_S3N) {
  classify_variable <- function(value, low_limit, mod_lower, mod_upper, high_limit) {
    if (is.na(value)) {
      return("Unclassified")
    } else if (value <= low_limit) {
      return("Low")
    } else if (value >= mod_lower & value <= mod_upper) {
      return("Moderate")
    } else if (value >= high_limit) {
      return("High")
    } else {
      return("Unclassified")
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

# Apply the classify_burnout_risk function
dbhe_data$Burnout_Risk <- classify_burnout_risk(dbhe_data$SCORE_MBI_S1_STD, dbhe_data$SCORE_MBI_S2_STD, dbhe_data$SCORE_MBI_S3N)

# Define the RESNiveles function
RESNiveles <- function(var1) {
  rango <- numeric()
  for (i in 1:length(var1)) {
    if (var1[i] <= 5.7) {
      rango[i] <- 1
    } else if (var1[i] > 5.7 & var1[i] <= 7.5) {
      rango[i] <- 2
    } else if (var1[i] > 7.5 & var1[i] <= 8.3) {
      rango[i] <- 3
    } else if (var1[i] > 8.3 & var1[i] <= 9.2) {
      rango[i] <- 4
    } else if (var1[i] > 9.2) {
      rango[i] <- 5
    }
  }
  return(rango)
}

# Define the PWBNiveles function
PWBNiveles <- function(var1) {
  rango <- numeric()
  for (i in 1:length(var1)) {
    if (var1[i] <= 168) {
      rango[i] <- 1
    } else if (var1[i] > 168 & var1[i] <= 336) {
      rango[i] <- 2
    } else if (var1[i] > 336 & var1[i] <= 504) {
      rango[i] <- 3
    }
  }
  return(rango)
}

# Calculate PWB and RES levels
dbhe_data$RES_Level <- RESNiveles(dbhe_data$TOTALSCORE_RES_STD)
dbhe_data$PWB_Level <- PWBNiveles(dbhe_data$TOTALSCORE_PWB)

# Create the classificationSET data frame
classificationSET <- dbhe_data %>% select(Burnout_Risk, PWB_Level, RES_Level)

# Save the classificationSET data frame to a CSV file
write.csv(classificationSET, "classificationSET.csv", row.names = FALSE)
