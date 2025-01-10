# Project Objective

The following script is using Valorant Champions Tour International 2024
Data. Valorant is a first person shooter (FPS) game where two teams of
five players each compete in a series of rounds to win. Each player or
agent has a set of abilities, and agents are split up into the following
four roles: Duelists, Sentinels, Initiators, and Controllers. In this
project, I am examining the relationship between player average first
kills per round and player average total kills per round. I am
interested in answering whether player average first kills per round
predicts player average total kills per round. I have also displayed the
data to showcase the region for each player since I was interested to
see if there are any differences by regions.

Here is the source of the original dataset:
<https://www.kaggle.com/datasets/sauurabhkr/valorant-champions-tour-2024>.

# Install and Load Packages

``` r
# Install and load jsonlite package to work with JSON file in R
install.packages("jsonlite")
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/l6/ttg8xpk13cl4szr__v161xjc0000gn/T//Rtmp6JTOa7/downloaded_packages

``` r
library(jsonlite)

# Install and load sjPlot for regression table visualization
install.packages("sjPlot")
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/l6/ttg8xpk13cl4szr__v161xjc0000gn/T//Rtmp6JTOa7/downloaded_packages

``` r
library(sjPlot)

# Load ggplot, dplyr, moderndive for data visualization, data wrangling, and statistical analyses
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(moderndive)
```

# Load in dataset

``` r
  vct_data <- fromJSON("Your path here /vct-international.json", flatten = TRUE)
```

# Convert variables of interest to numeric in order to run analyses

``` r
vct_data$playerStatistics.first_kills_per_round <- as.numeric(vct_data$playerStatistics.first_kills_per_round)
vct_data$playerStatistics.kills_per_round <- as.numeric(vct_data$playerStatistics.kills_per_round)
class(vct_data$playerStatistics.first_kills_per_round)
```

    ## [1] "numeric"

``` r
class(vct_data$playerStatistics.kills_per_round)
```

    ## [1] "numeric"

# Data Visualization

``` r
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
```

    ## Warning: Removed 11 rows containing non-finite outside the scale range
    ## (`stat_bin()`).
![Figure_1](https://github.com/user-attachments/assets/0916b74e-4b4a-403c-92d0-93da1d0847a7)

``` r
  # Distribution of player average total kills per round via histogram
  ggplot(data = vct_data, mapping = aes(x = playerStatistics.kills_per_round, fill = region)) +
    geom_histogram(binwidth = 0.03, position = "identity", alpha = 0.7) +
    labs(title = "Distribution of Player Average Total Kills Per Round", x = "Average Total Kills Per Round") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set1")
```

![Figure_2](https://github.com/user-attachments/assets/c97fe6cb-e0ac-43de-ac7d-5ff810867638)


``` r
  # Check the directional of the relationship between player average first kill per round and player average total kills per round via       scatter plot
  ggplot(data = vct_data, mapping = aes(x = playerStatistics.first_kills_per_round, y = playerStatistics.kills_per_round)) +
    geom_point(aes(color = region)) +
    geom_smooth(method = "lm", formula = y ~ x) +
    labs(title = "Average First Kills Per Round versus Average Total Kills Per Round",
         x = "Average First Kills Per Round",
         y = "Average Total Kills Per Round") +
    theme_minimal() 
```

    ## Warning: Removed 11 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 11 rows containing missing values or values outside the scale range
    ## (`geom_point()`).
    
![Figure_3](https://github.com/user-attachments/assets/bf4c7653-1373-4727-bdbf-be71b32d046b)



# Distribution of Player Average First Kills Per Round Histogram Interpretation

This histogram displays average first kills per round on the x-axis and
the amount of players that scored that average on the y-axis. The
histogram has a key where each region is associated with a color. Here,
we see that most players tend to have an average of 0.05 to 0.10 average
first kills per round. The graph displays a few bars on both the left
and right of the graph indicating there are outliers that either score
lower average first kills per round or score higher average first kills
per round than the majority of the sample. The histogram is right-skewed
indicating most players score lower average first kills per round.

# Distribution of Player Average Total Kills Per Round Histogram Interpretation

This histogram displays average total kills per round on the x-axis and
the amount of players that scored that average on the y-axis. The
histogram has a key where each region is associated with a color. Here,
we see that most players tend to have an average of 0.5 to 0.8 average
total kills per round. The graph displays a few bars on left of the
graph indicating there are outliers that score lower average total kills
per round than the majority of the sample. The histogram is roughly
symmetrical.

# Average First Kills Per Round versus Average Total Kills Per Round Scatterplot Interpretation

This scatterplot displays the relationship between average first kills
per round and the average total kills per round. There is a moderately
positive relationship between the variables indicating that an increase
in average first kills per round is associated with an increase in
average total kills per round. The graph also displays each player based
off of their region.

# Data Wrangling and Descriptive Statistics

``` r
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
```

    ## # A tibble: 4 × 7
    ##   region   mean_first median_first stdv_first mean_total median_total stdv_total
    ##   <chr>         <dbl>        <dbl>      <dbl>      <dbl>        <dbl>      <dbl>
    ## 1 Americas     0.0971         0.08     0.0451      0.685        0.68      0.0767
    ## 2 China        0.0970         0.08     0.0492      0.669        0.665     0.0982
    ## 3 EMEA         0.0980         0.08     0.0416      0.680        0.69      0.0822
    ## 4 Pacific      0.0968         0.08     0.0447      0.676        0.68      0.0853

# Interpretation of Descriptive Statistics

The following descriptive statistics were computed for average first
kills per round and average total kills per round for each region: Mean,
Median, and Standard Deviation. Each region had the same median and very
similar means and standard deviations. The region with the highest
average first kills per round was EMEA. Interestingly, there was some
variation in the mean average total kills per round, with Americas
having the highest average total kills per round.

# Correlation between average first kills and average total kills

``` r
cor(vct_data$playerStatistics.first_kills_per_round, vct_data$playerStatistics.kills_per_round, use = "complete.obs")
```

    ## [1] 0.6735414

This correlation is positive and of moderate strength. The correlation
tells us that as average first kills per round increases, average total
kills per round also increases.

# Regression Model

``` r
# Fit regression model
vct_model <- lm(playerStatistics.kills_per_round ~ playerStatistics.first_kills_per_round, data = vct_data)
  summary(vct_model)
```

    ## 
    ## Call:
    ## lm(formula = playerStatistics.kills_per_round ~ playerStatistics.first_kills_per_round, 
    ##     data = vct_data)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.164769 -0.042654  0.003403  0.044659  0.176773 
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                             0.56574    0.01039   54.48   <2e-16 ***
    ## playerStatistics.first_kills_per_round  1.21145    0.09696   12.49   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0598 on 188 degrees of freedom
    ##   (11 observations deleted due to missingness)
    ## Multiple R-squared:  0.4537, Adjusted R-squared:  0.4508 
    ## F-statistic: 156.1 on 1 and 188 DF,  p-value: < 2.2e-16

``` r
# Get regression table
  get_regression_table(vct_model)
```

    ## # A tibble: 2 × 7
    ##   term                    estimate std_error statistic p_value lower_ci upper_ci
    ##   <chr>                      <dbl>     <dbl>     <dbl>   <dbl>    <dbl>    <dbl>
    ## 1 intercept                  0.566     0.01       54.5       0    0.545    0.586
    ## 2 playerStatistics.first…    1.21      0.097      12.5       0    1.02     1.40

# Equation for regression model

average total kills per round = 0.566 + 1.211 \* average first kill per
round

# Table for regression model

``` r
# Fit regression model
tab_model(vct_model)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
player Statistics kills<br>per round
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Estimates
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
p
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
(Intercept)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.57
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.55 – 0.59
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
playerStatistics first<br>kills per round
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.21
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.02 – 1.40
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">
190
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
R<sup>2</sup> / R<sup>2</sup> adjusted
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.454 / 0.451
</td>
</tr>
</table>

# Interpretation of regression model

For every increase of 1 unit in average first kills per round, there is
an associated increase of, on average, 1.211 units of average total
kills per round. Approximately 45.1% of the variability in average total
kills per round can be explained by average first kills per round. An
F-statistic of 156.1 indicates there is a strong relationship between
average first kills per round and average total kills per round. Given
the value of the F-statistic and p-value of \<2.2e-16, we can conclude
that the model is statistically significant

# Final interpretation of the data

Average first kills per round predicts the average total kills per round
for players in the Valorant Champions Tour Interntional (2024).
Specifically, an increase in average first kills per round is associated
with an increase in average total kills per round. Future studies should
examine whether the type of agent a player selects (e.g., duelists)
moderates this relationship.
