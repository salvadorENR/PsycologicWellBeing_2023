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
  
  results <- data.frame(SCORE_MBI_S1, SCORE_MBI_S2, SCORE_MBI_S3N, SCORE_MBI_S1_Risk, SCORE_MBI_S2_Risk, SCORE_MBI_S3N_Risk, Burnout_Risk)
  
  return(results)
}

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
  
  results <- data.frame(TOTALSCORE_PWB, TOTALSCORE_RES_STD, PWB_Risk, RES_Risk)
  
  return(results)
}

# Example usage:
# Assuming your variables are already attached to the environment
burnout_results <- classify_burnout_risk(SCORE_MBI_S1, SCORE_MBI_S2, SCORE_MBI_S3N)
pwb_res_results <- classify_pwb_res(TOTALSCORE_PWB, TOTALSCORE_RES_STD)

# Combine the results into a single data frame
combined_results <- cbind(burnout_results, pwb_res_results)

# Filter the results for each burnout risk level
low_burnout <- subset(combined_results, Burnout_Risk == "Low risk of Burnout")
moderate_burnout <- subset(combined_results, Burnout_Risk == "Moderate risk of Burnout")
high_burnout <- subset(combined_results, Burnout_Risk == "High risk of Burnout")

# Function to create and save table images
create_and_save_table <- function(data, risk_level) {
  table_df <- data.frame(
    `Teacher ID` = 1:nrow(data),
    `PWB Level` = data$PWB_Risk,
    `RES Level` = data$RES_Risk,
    check.names = FALSE
  )
  
  table_plot <- tableGrob(table_df, rows = NULL)
  ggsave(paste0("table_", risk_level, "_burnout.png"), plot = table_plot, width = 8, height = 4, units = "in", dpi = 300)
  
  print(table_df)
}

# Create and save tables for each burnout risk level
create_and_save_table(low_burnout, "low")
create_and_save_table(moderate_burnout, "moderate")
create_and_save_table(high_burnout, "high")

