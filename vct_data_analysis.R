# The following script is using Valorant Champions Tour International 2024 data
# The data will examine whether player average first kills per round predicts player average total kills per round

# Install and load jsonlite package to work with JSON file in R
install.packages("jsonlite")
library(jsonlite)

# Install and load sjPlot for regression table visualization
install.packages("sjPlot")
library(sjPlot)

# Load ggplot, dplyr, moderndive for data visualization, data wrangling, and statistical analyses
library(ggplot2)
library(dplyr)
library(moderndive)

# Load data set into R
vct_data <- fromJSON("Put path variable here", flatten = TRUE)

# Convert variables of interest to numeric in order to run analyses
vct_data$playerStatistics.first_kills_per_round <- as.numeric(vct_data$playerStatistics.first_kills_per_round)
vct_data$playerStatistics.kills_per_round <- as.numeric(vct_data$playerStatistics.kills_per_round)
class(vct_data$playerStatistics.first_kills_per_round)
class(vct_data$playerStatistics.kills_per_round)

# Data visualization
  # Distribution of player average first kill per round via histogram
  ggplot(data = vct_data, mapping = aes(x = playerStatistics.first_kills_per_round, fill = region)) +
    geom_histogram(binwidth = 0.015, position = "identity", alpha = 0.7) +
    labs(
      title = "Distribution of Player Average First Kill Per Round",
      x = "Average First Kills Per Round",
      y = "Count"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
  
  # Distribution of player average total kills per round via histogram
  ggplot(data = vct_data, mapping = aes(x = playerStatistics.kills_per_round, fill = region)) +
    geom_histogram(binwidth = 0.03, position = "identity", alpha = 0.7) +
    labs(title = "Distribution of Player Average Total Kills Per Round", x = "Average Total Kills Per Round") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
  
  # Check the directional of the relationship between player average first kill per round and player average total kills per round via       scatter plot
  ggplot(data = vct_data, mapping = aes(x = playerStatistics.first_kills_per_round, y = playerStatistics.kills_per_round)) +
    geom_point(aes(color = region)) +
    geom_smooth(method = "lm", formula = y ~ x) +
    labs(title = "Average First Kills Per Round versus Average Total Kills Per Round",
         x = "Average First Kills Per Round",
         y = "Average Total Kills Per Round") +
    theme_minimal() 

  
  
# Data Wrangling and Descriptive Statistics 
  # Run summary statistics by region
  vct_region_summarystats <- vct_data %>%
    group_by(region) %>%
    summarize(
      mean_first = mean(playerStatistics.first_kills_per_round, na.rm = TRUE),
      median_first = median(playerStatistics.first_kills_per_round, na.rm = TRUE),
      stdv_first = sd(playerStatistics.first_kills_per_round, na.rm = TRUE),
      mean_total = mean(playerStatistics.kills_per_round, na.rm = TRUE),
      median_total = median(playerStatistics.kills_per_round, na.rm = TRUE),
      stdv_total = sd(playerStatistics.kills_per_round, na.rm = TRUE)
    )
  vct_region_summarystats

# Correlation between average first kills and average total kills
  cor(vct_data$playerStatistics.first_kills_per_round, vct_data$playerStatistics.kills_per_round, use = "complete.obs")
  
# Regression Model
  # Fit regression model
  vct_model <- lm(playerStatistics.kills_per_round ~ playerStatistics.first_kills_per_round, data = vct_data)
  summary(vct_model)
  # Get regression table
  get_regression_table(vct_model)
  # Fit regression model with sjPlot
  tab_model(vct_model)