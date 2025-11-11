###################################
########## HOMEWORK 10 ############
## Maddie Thall & Sophia Mummert ##
###################################

library(readxl)
library(ggplot2)
library(ggfortify)

### OBJECTIVE 1

lemur_df = read_xlsx("Cantwell_et_al_Lemurs_Task_Interrelationships.xlsx")


## variables
# x = NovelMean: Average amount of time spent in the proximity area across novel object trials in seconds, including no-approach responses
# y = TotalManip: Total amount of time spent in contact with the box during the persistence task in seconds

ggplot(lemur_df, aes(x = NovelMean, y = TotalManip)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Object Manipulation Time vs. Time in Proximity Area",
       x = "Average Time in Proximity Area (sec)",
       y = "Total Manipulation Time (sec)") +
  theme_minimal()


autoplot(lm(TotalManip ~ NovelMean, data = lemur_df))
# Residuals vs Fitted (checking linearity and homoscedasity): there is slight non-linearity because the blue trend line curves downward slightly 
# and the spread may be greater than expected
# Normal Q-Q (checking for normal distribution): the points mostly follow the diagonal line, indicating that the residuals are approximately normally distributed,
# although there are some deviations at the tails
# Residual vs Leverage (checking for correlation between residuals): there are a few points with higher leverage that may affect the overall normality


model = lm(TotalManip ~ NovelMean, data = lemur_df)
median_x = median(lemur_df$NovelMean, na.rm = TRUE) # 12.2075
p95_x = quantile(lemur_df$NovelMean, 0.95, na.rm = TRUE) # 37.814 
new_values = data.frame(NovelMean = c(median_x, p95_x))
predictions = predict(model, newdata = new_values, interval = "prediction")
predictions

# At the median (12.2075), our model predicts a Y value of 25.8
# At the 95th percentile (37.814), our model predicts a Y value of 40.9
# The predictions interval differ (-17.92 to 69.52 at median, -4.1 to 85.9 at 95th percentile)
# The interval for the 95th percentile is wider, indicating more uncertainty in the prediction


## OBJECTIVE 2

set.seed(123)
X = runif(100, min = 0, max = 10)
beta_0 = 2
beta_1 = 3
error = rlnorm(100, meanlog = 0, sdlog = 0.5)
Y = beta_0 + beta_1 * X + error
sim_df = data.frame(X = X, Y = Y)

# plot original regression
ggplot(sim_df, aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Simulated Lognormal Data") +
  theme_minimal()

# creating a data frame to store results
n_sim = 15
coef_results = data.frame(
  sim = integer(n_sim),
  true_intercept = numeric(n_sim),
  true_slope = numeric(n_sim),
  est_intercept = numeric(n_sim),
  est_slope = numeric(n_sim)
)

# creating a list for prediction intervals
pred_intervals = vector("list", n_sim)

# running simulations
for (i in 1:n_sim) {
  X2 = runif(100, min = 0, max = 10)
  beta2_0 = runif(1, 0, 5)
  beta2_1 = runif(1, 1, 4)
  error2 = rlnorm(100, meanlog = 0, sdlog = 0.5)
  Y2 = beta2_0 + beta2_1 * X2 + error2
  sim_model = lm(Y2 ~ X2)
  coef_results[i, ] = c(i, beta2_0, beta2_1, coef(sim_model)[1], coef(sim_model)[2])
  new_data = data.frame(X = X2)
  pred_int = predict(sim_model, 
                     newdata = new_data, 
                     interval = "prediction", 
                     level = 0.95)
  pred_intervals[[i]] = data.frame(X = X2, Y = Y2, pred_int)
}

# viewing coefficient results
head(pred_intervals[[1]])   

# comparing estimated and true values
coef_results$slope_error = coef_results$est_slope - coef_results$true_slope
coef_results$intercept_error = coef_results$est_intercept - coef_results$true_intercept

summary(coef_results$slope_error)
summary(coef_results$intercept_error)
# Slope: estimates are fairly accurate, with mean error close to 0 (0.05), so a slight upward bias
# Intercept: estimates are less accurate, with a larger mean error (1.11), so a consistent upward bias  

fraction_95 = sapply(pred_intervals, function(df) {
  within = df$Y >= df$lwr & df$Y <= df$upr
  mean(within)
})
coef_results$fraction_95 = fraction_95
summary(coef_results$fraction_95)
# On average, about 95.33% of the observed Y values fall within the 95% prediction intervals!
# The whole range is between 93% and 97%, so a fairly limited variability
# meaning that our model's predictions intervals are pretty accurate!
# Even with a non-normal error, the linear model is robust enough!




