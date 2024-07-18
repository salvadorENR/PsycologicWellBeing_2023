library(gridExtra)
library(grid)
library(ggplot2)

SCORE_MBI_S3N = MBI4_REVERSE + MBI7_REVERSE + MBI9_REVERSE + MBI12_REVERSE +
  MBI17_REVERSE + MBI18_REVERSE + MBI19_REVERSE + MBI21_REVERSE

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
  
  # Create a data frame to store the results
  results <- data.frame(SCORE_MBI_S1, SCORE_MBI_S2, SCORE_MBI_S3N, SCORE_MBI_S1_Risk, SCORE_MBI_S2_Risk, SCORE_MBI_S3N_Risk, Burnout_Risk)
  
  # Create a table of the results
  table_results <- table(results$Burnout_Risk)
  
  # Calculate percentages
  total <- sum(table_results)
  percentages <- round((table_results / total) * 100, 2)
  
  # Create a data frame for the table with check.names = FALSE to prevent dots in column names
  table_df <- data.frame(
    `Burnout risk level` = factor(names(table_results), levels = c("Low risk of Burnout", "Moderate risk of Burnout", "High risk of Burnout", "Unclassified")),
    `Number of teachers` = as.integer(table_results),
    `Percentage of teachers` = paste0(percentages, "%"),
    check.names = FALSE
  )
  
  # Print the table
  print(table_df)
  
  # Save the table as an image
  table_plot <- tableGrob(table_df, rows = NULL)
  ggsave("burnout_risk_table.png", plot = table_plot, width = 8, height = 4, units = "in", dpi = 300)
  
  return(results)
}

# Example usage:
# Assuming your variables are already attached to the environment
results <- classify_burnout_risk(SCORE_MBI_S1, SCORE_MBI_S2, SCORE_MBI_S3N)









library(gridExtra)
library(grid)
library(ggplot2)

SCORE_MBI_S3N = MBI4_REVERSE + MBI7_REVERSE + MBI9_REVERSE + MBI12_REVERSE +
  MBI17_REVERSE + MBI18_REVERSE + MBI19_REVERSE + MBI21_REVERSE

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
  
  # Create a data frame to store the results
  results <- data.frame(SCORE_MBI_S1, SCORE_MBI_S2, SCORE_MBI_S3N, SCORE_MBI_S1_Risk, SCORE_MBI_S2_Risk, SCORE_MBI_S3N_Risk, Burnout_Risk)
  
  # Create a table of the results
  table_results <- table(results$Burnout_Risk)
  
  # Calculate percentages
  total <- sum(table_results)
  percentages <- round((table_results / total) * 100, 2)
  
  # Create a data frame for the table with check.names = FALSE to prevent dots in column names
  table_df <- data.frame(
    `Burnout risk level` = factor(names(table_results), levels = c("Low risk of Burnout", "Moderate risk of Burnout", "High risk of Burnout", "Unclassified")),
    `Number of teachers` = as.integer(table_results),
    `Percentage of teachers` = paste0(percentages, "%"),
    check.names = FALSE
  )
  
  # Print the table
  print(table_df)
  
  return(results)
}

# Example usage:
# Assuming your variables are already attached to the environment
results <- classify_burnout_risk(SCORE_MBI_S1, SCORE_MBI_S2, SCORE_MBI_S3N)