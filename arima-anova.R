library(ggplot2)
library(forecast)
library(dplyr)
library(car)

set.seed(123)

student_data <- data.frame(
  Social_Media_Usage = sample(1:20, 1000, replace = TRUE),
  GPA = c(
    rnorm(300, 3.5, 0.3),
    rnorm(400, 3.0, 0.4),
    rnorm(300, 2.5, 0.4)
  )
)

student_data$Usage_Category <- cut(student_data$Social_Media_Usage,
                                   breaks = c(0, 5, 10, Inf),
                                   labels = c("Low", "Moderate", "High"))

time_series_data <- ts(student_data$GPA, frequency = 12)

manual_anova <- function(data, response, factor) {
  overall_mean <- mean(data[[response]])
  group_means <- tapply(data[[response]], data[[factor]], mean)
  group_sizes <- table(data[[factor]])
  
  SST <- sum((data[[response]] - overall_mean) ^ 2)
  SSB <- sum(group_sizes * (group_means - overall_mean) ^ 2)
  SSW <- SST - SSB
  
  df_between <- length(group_means) - 1
  df_within <- nrow(data) - length(group_means)
  
  MSB <- SSB / df_between
  MSW <- SSW / df_within
  
  F_value <- MSB / MSW
  
  cat("\n--- Manual ANOVA Results ---\n")
  cat("Total Sum of Squares (SST):", SST, "\n")
  cat("Between-Group Sum of Squares (SSB):", SSB, "\n")
  cat("Within-Group Sum of Squares (SSW):", SSW, "\n")
  cat("Degrees of Freedom (Between):", df_between, "\n")
  cat("Degrees of Freedom (Within):", df_within, "\n")
  cat("Mean Square Between (MSB):", MSB, "\n")
  cat("Mean Square Within (MSW):", MSW, "\n")
  cat("F-statistic:", F_value, "\n")
  
  p_value <- pf(F_value, df_between, df_within, lower.tail = FALSE)
  cat("p-value:", p_value, "\n")
  
  return(p_value)
}

menu_function <- function() {
  repeat {
    cat("\n--- MENU ---\n")
    cat("1. Summary Statistics\n")
    cat("2. Manual ANOVA Analysis\n")
    cat("3. Hypothesis Testing\n")
    cat("4. Plot Options (ARIMA Model Forecast)\n")
    cat("5. Plot Options (Bar, Boxplot, Histogram, Time Series)\n")
    cat("6. Exit\n")
    
    choice <- as.integer(readline(prompt = "Enter your choice (1-6): "))
    
    if (choice == 1) {
      cat("\n--- Summary Statistics ---\n")
      print(summary(student_data))
      
    } else if (choice == 2) {
      cat("\n--- Manual ANOVA Analysis ---\n")
      p_value <- manual_anova(student_data, response = "GPA", factor = "Usage_Category")
      
      if (p_value < 0.05) {
        cat("Reject the null hypothesis. There is a significant effect of social media usage on GPA.\n")
      } else {
        cat("Fail to reject the null hypothesis. There is no significant effect of social media usage on GPA.\n")
      }
      
    } else if (choice == 3) {
      cat("\n--- Hypothesis Testing ---\n")
      cat("1. Testing if there is a significant effect of social media usage on GPA.\n")
      cat("2. Testing if there is a difference in GPA based on high and low usage.\n")
      cat("3. Testing if moderate usage leads to better GPA compared to high usage.\n")
      
      hypothesis_choice <- as.integer(readline(prompt = "Enter your hypothesis choice (1-3): "))
      
      if (hypothesis_choice == 1) {
        p_value <- manual_anova(student_data, response = "GPA", factor = "Usage_Category")
        if (p_value < 0.05) {
          cat("Reject the null hypothesis. There is a significant effect of social media usage on GPA.\n")
        } else {
          cat("Fail to reject the null hypothesis. There is no significant effect of social media usage on GPA.\n")
        }
      } else if (hypothesis_choice == 2) {
        high_usage <- student_data$GPA[student_data$Usage_Category == "High"]
        low_usage <- student_data$GPA[student_data$Usage_Category == "Low"]
        t_test_result <- t.test(high_usage, low_usage)
        cat("t-test results for High vs Low Usage:\n")
        print(t_test_result)
        
        if (t_test_result$p.value < 0.05) {
          cat("Reject the null hypothesis. There is a significant difference in GPA between high and low usage.\n")
        } else {
          cat("Fail to reject the null hypothesis. There is no significant difference in GPA between high and low usage.\n")
        }
      } else if (hypothesis_choice == 3) {
        moderate_usage <- student_data$GPA[student_data$Usage_Category == "Moderate"]
        high_usage <- student_data$GPA[student_data$Usage_Category == "High"]
        t_test_result <- t.test(moderate_usage, high_usage)
        cat("t-test results for Moderate vs High Usage:\n")
        print(t_test_result)
        
        if (t_test_result$p.value < 0.05) {
          cat("Reject the null hypothesis. Moderate usage leads to better GPA compared to high usage.\n")
        } else {
          cat("Fail to reject the null hypothesis. Moderate usage does not lead to better GPA compared to high usage.\n")
        }
      } else {
        cat("Invalid choice. Try again.\n")
      }
      
    } else if (choice == 4) {
      cat("\n--- ARIMA Model Forecast ---\n")
      plot(time_series_data, main = "Time Series of GPA", xlab = "Time (Months)", ylab = "GPA", col = "blue")
      
      arima_model <- auto.arima(time_series_data)
      print(summary(arima_model))
      
      forecasted_gpa <- forecast(arima_model, h = 12)
      plot(forecasted_gpa, main = "GPA Forecast for Next 12 Periods", col = "red")
      
    } else if (choice == 5) {
      cat("\n--- Plot Options ---\n")
      cat("1. Bar Plot\n")
      cat("2. Boxplot\n")
      cat("3. Histogram\n")
      cat("4. Time Series Plot\n")
      
      plot_choice <- as.integer(readline(prompt = "Enter your plot choice (1-4): "))
      
      if (plot_choice == 1) {
        bar_plot <- ggplot(student_data, aes(x = Usage_Category, fill = Usage_Category)) +
          geom_bar() +
          theme_minimal() +
          ggtitle("Bar Plot of Social Media Usage Categories") +
          xlab("Social Media Usage Category") +
          ylab("Count")
        print(bar_plot)
        
      } else if (plot_choice == 2) {
        boxplot <- ggplot(student_data, aes(x = Usage_Category, y = GPA, fill = Usage_Category)) +
          geom_boxplot() +
          theme_minimal() +
          ggtitle("Boxplot of GPA by Social Media Usage") +
          xlab("Social Media Usage Category") +
          ylab("GPA")
        print(boxplot)
        
      } else if (plot_choice == 3) {
        histogram <- ggplot(student_data, aes(x = GPA)) +
          geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
          theme_minimal() +
          ggtitle("Histogram of GPA") +
          xlab("GPA") +
          ylab("Frequency")
        print(histogram)
        
      } else if (plot_choice == 4) {
        plot(time_series_data, main = "Time Series of GPA", xlab = "Time (Months)", ylab = "GPA", col = "blue")
        
      } else {
        cat("Invalid plot choice. Try again.\n")
      }
      
    } else if (choice == 6) {
      cat("Exiting the program.\n")
      break
      
    } else {
      cat("Invalid choice. Try again.\n")
    }
  }
}

menu_function()
