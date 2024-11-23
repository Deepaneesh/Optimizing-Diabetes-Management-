# Load necessary libraries######
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)

# Parameters
set.seed(123) # For reproducibility
n_patients <- 100 # Number of patients per group
weeks <- 8 # Number of weeks

# Generate initial glucose levels (assuming a normal distribution)
initial_glucose <- rnorm(n_patients * 3, mean = 185, sd = 10)

# Create a data frame to hold the data############
data <- data.frame(
  patient_id = rep(1:(n_patients * 3), each = weeks + 1),
  group = rep(c('Metformin', 'Glipizide', 'Sitagliptin'), each = (n_patients * (weeks + 1))),
  week = rep(0:weeks, n_patients * 3),
  glucose_level = NA
)
View(data)

# Assign initial glucose levels
data <- data %>%
  group_by(patient_id) %>%
  mutate(glucose_level = if_else(week == 0, initial_glucose[patient_id], NA_real_))

# Define the weekly reduction pattern for each group
reduction_pattern <- function(glucose, week, group) {
  reduction <- switch(group,
                      "Metformin" = rnorm(1, mean = 5, sd = 2),
                      "Glipizide" = rnorm(1, mean = 7, sd = 2.5),
                      "Sitagliptin" = rnorm(1, mean = 6, sd = 2.2)
  )
  glucose - reduction
}
# Generate glucose levels week by week
for (i in 1:nrow(data)) {
  if (data$week[i] > 0) {
    data$glucose_level[i] <- reduction_pattern(data$glucose_level[i - 1], data$week[i], data$group[i])
  }
}

# View the generated data##########################################################
head(data)
write.csv(data,file = "nova nordisk 1.csv")
##effectiveness###################################################################

effectiveness <- data %>%
  filter(week > 0) %>%
  group_by(group, week) %>%
  summarize(mean_glucose = mean(glucose_level, na.rm = TRUE), .groups = 'drop') %>%
  group_by(group) %>%
  summarize(total_reduction = mean(mean_glucose[1]) - mean(mean_glucose), .groups = 'drop')

# View effectiveness################################################################
print(effectiveness)

# Plot efficiency ############################################################
ggplot(data, aes(x = week, y = glucose_level, color = group)) +
  geom_line(aes(group = patient_id), alpha = 0.2) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  labs(title = "Synthetic Blood Glucose Levels Over Time", x = "Week", y = "Glucose Level") +
  theme_minimal()

# training data &testing data##################
train_weeks <- 0:6
test_weeks <- 6:8
glipizide_data <- data %>%
  filter(group == "Glipizide") %>%
  group_by(week) %>%
  summarize(mean_glucose = mean(glucose_level, na.rm = TRUE))
glipizide_data

train_data <- glipizide_data %>%
  filter(week %in% train_weeks)

test_data <- glipizide_data %>%
  filter(week %in% test_weeks)

# Fit ARIMA model to the training data
glipizide_ts <- ts(train_data$mean_glucose, start = 1, frequency = 1)
fit <- auto.arima(glipizide_ts)

# Forecast for the test period
glipizide_forecast <- forecast(fit, h = length(test_weeks))

# Calculate forecast accuracy metrics
actual <- test_data$mean_glucose
predicted <- glipizide_forecast$mean

mae <- mean(abs(actual - predicted))
mse <- mean((actual - predicted)^2)
rmse <- sqrt(mse)

accuracy_metrics <- data.frame(
  MAE = mae,
  MSE = mse,
  RMSE = rmse
)

# Print accuracy ##########
print(accuracy_metrics)

# Plot the actual and forecasted glucose levels
combined_data <- data.frame(
  week = c(train_data$week, test_data$week),
  glucose_level = c(train_data$mean_glucose, actual),
  type = c(rep("Train", length(train_data$mean_glucose)), rep("Actual", length(test_data$mean_glucose)))
)

forecast_data <- data.frame(
  week = test_data$week,
  glucose_level = predicted,
  type = rep("Forecast", length(predicted))
)

all_data <- rbind(combined_data, forecast_data)
##plot accuracy ##############################################
ggplot(all_data, aes(x = week, y = glucose_level, color = type)) +
  geom_line(size = 1.5) +
  labs(title = "Actual vs Forecasted Glucose Levels for Glipizide",
       x = "Week",
       y = "Glucose Level") +
  theme_minimal()

###arima diagram################

glipizide_ts <- ts(glipizide_data$mean_glucose, start =0, frequency = 1)
fit <- auto.arima(glipizide_ts)

# Forecast future glucose levels
forecast_period <- 10# Number of weeks to forecast into the future
glipizide_forecast <- forecast(fit, h = forecast_period)

# arima forecasted value #################
print(glipizide_forecast)

# Plot the forecast
autoplot(glipizide_forecast) +
  labs(title = "Forecasted Glucose Levels for Glipizide",
       x = "Week",
       y = "Glucose Level") +
  theme_minimal()

# Determine the week when glucose levels reach normal range
normal_range_upper <- 120
normal_range_lower <-80

forecasted_weeks <- data.frame(week = seq(weeks + 1, weeks + forecast_period),
                               forecasted_glucose = as.numeric(glipizide_forecast$mean))

time_to_normal <- forecasted_weeks %>%
  filter(forecasted_glucose <= normal_range_upper & forecasted_glucose >= normal_range_lower) %>%
  summarize(time_to_normal = min(week))

# Print the time to normal range##################33
print(time_to_normal)

combined_data <- data.frame(
  week = c(glipizide_data$week, (max(glipizide_data$week) + 1):(max(glipizide_data$week) + forecast_period)),
  glucose_level = c(glipizide_data$mean_glucose, as.numeric(glipizide_forecast$mean)),
  type = c(rep("Actual", length(glipizide_data$mean_glucose)), rep("Forecast", forecast_period))
)
combined_data
week1=combined_data$week|>as.numeric()
week12=c(week1[1:15],9)
week12
glucose_level1=combined_data$glucose_level|>as.numeric()
glucose_level12=c(glucose_level1[1:15],glucose_level1[10])
glucose_level12
type1=combined_data$type|>as.factor()
Type=c(type1[1:15],type1[9])
combined_data1=data.frame(week12,glucose_level12,Type)
combined_data1

# Plot acutal and forecasted value #################
ggplot(combined_data1, aes(x = week12, y = glucose_level12, color = Type)) +
  geom_line(size = 1) +
  labs(title = "Forecasted Glucose Levels for Glipizide",
       x = "Week",
       y = "Glucose Level") +theme_minimal()+geom_vline(xintercept = 9,color="blue")+geom_hline(yintercept = 120,linetype="dotted",size=1,color="black")+
  geom_hline(yintercept = 80,linetype="dotted",size=1,color="black")+
  annotate(geom="text",label="80",x=-0.5,y=83,colour="black")+annotate(geom="text",label="120",x=-0.5,y=123,colour="black")

