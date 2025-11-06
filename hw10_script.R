###################################
########## HOMEWORK 10 ############
## Maddie Thall & Sophia Mummert ##
###################################

library(readxl)
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
n = 100
x = runif(n, 0, 50)
beta0 = 5
beta1 = 0.8
sigma_true = beta0 + beta1 * x
sd_log = 0.6
sigma_log = 0
eps_log = rlnorm(n, meanlog = sigma_log, sdlog = sd_log)
center = exp(sigma_log + (sd_log^2) / 2)
eps_lognorm_centered = eps_log - center
y_lognorm = sigma_true + eps_lognorm_centered
model_lognorm = lm(y_lognorm ~ x)
summary(model_lognorm)


