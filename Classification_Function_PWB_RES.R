library(gridExtra)
library(grid)
library(ggplot2)

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

classify_pwb_res <- function(TOTALSCORE_PWB, TOTALSCORE_RES_STD) {
  PWB_Risk <- PWBNiveles(TOTALSCORE_PWB)
  RES_Risk <- RESNiveles(TOTALSCORE_RES_STD)
  
  PWB_Risk <- factor(PWB_Risk, levels = 1:3, labels = c("Nivel 1", "Nivel 2", "Nivel 3"))
  RES_Risk <- factor(RES_Risk, levels = 1:5, labels = c("Nivel 1", "Nivel 2", "Nivel 3", "Nivel 4", "Nivel 5"))
  
  PWB_Risk[is.na(PWB_Risk)] <- "Unclassified"
  RES_Risk[is.na(RES_Risk)] <- "Unclassified"
  
  # Create a data frame to store the results
  results <- data.frame(TOTALSCORE_PWB, TOTALSCORE_RES_STD, PWB_Risk, RES_Risk)
  
  # Create tables of the results
  table_pwb <- table(results$PWB_Risk)
  table_res <- table(results$RES_Risk)
  
  # Calculate percentages
  total_pwb <- sum(table_pwb)
  total_res <- sum(table_res)
  percentages_pwb <- round((table_pwb / total_pwb) * 100, 2)
  percentages_res <- round((table_res / total_res) * 100, 2)
  
  # Create data frames for the tables
  table_pwb_df <- data.frame(
    `PWB level` = factor(names(table_pwb), levels = c("Nivel 1", "Nivel 2", "Nivel 3", "Unclassified")),
    `Number of teachers` = as.integer(table_pwb),
    `Percentage of teachers` = paste0(percentages_pwb, "%"),
    check.names = FALSE
  )
  
  table_res_df <- data.frame(
    `RES level` = factor(names(table_res), levels = c("Nivel 1", "Nivel 2", "Nivel 3", "Nivel 4", "Nivel 5", "Unclassified")),
    `Number of teachers` = as.integer(table_res),
    `Percentage of teachers` = paste0(percentages_res, "%"),
    check.names = FALSE
  )
  
  # Print the tables
  print(table_pwb_df)
  print(table_res_df)
  
  # Save the tables as images
  table_pwb_plot <- tableGrob(table_pwb_df, rows = NULL)
  table_res_plot <- tableGrob(table_res_df, rows = NULL)
  ggsave("pwb_risk_table.png", plot = table_pwb_plot, width = 8, height = 4, units = "in", dpi = 300)
  ggsave("res_risk_table.png", plot = table_res_plot, width = 8, height = 4, units = "in", dpi = 300)
  
  return(results)
}

# Example usage:
# Assuming your variables are already attached to the environment
results <- classify_pwb_res(TOTALSCORE_PWB, TOTALSCORE_RES_STD)
