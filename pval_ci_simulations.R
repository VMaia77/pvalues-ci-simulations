library(ggplot2)

# True value of the parameter to create simulations
true_slope <- 0.1

# Create predictor variable: external temperature
external_temp <- seq(from = 0, to = 10000, by = 10)

# Create response variable: internal temperature
internal_temp <- (true_slope * (external_temp + 1000)) + rnorm(n = length(external_temp), 0, 1300)

# Plot
plot(internal_temp ~ external_temp, ylab = "Internal Temperature (ºC)", xlab = "External Temperature (ºC)")

# Linear model
regression_model <- summary(lm(internal_temp ~ external_temp))
regression_model  # observe that the effect is very close to the true_slope

# Combine variables into a data frame
data_frame_temp <- cbind.data.frame(external_temp, internal_temp)
summary(data_frame_temp)

# P-value will be calculated with 1000 iterations
iterations <- seq(1, 1000)

# Object for the calculated slope values
random_slopes <- cbind.data.frame(slopes = NA)

set.seed(123)  # Reproducibility

# For each iteration, shuffle the values of y and x
for (i in 1:length(iterations)) {
  shuffled_y = data_frame_temp[sample(1:nrow(data_frame_temp), replace = TRUE), ]$internal_temp
  shuffled_x = data_frame_temp[sample(1:nrow(data_frame_temp), replace = TRUE), ]$external_temp

  # Each row of random_slopes receives the value of b1 (slope) from a regression based on shuffled data
  random_slopes[i, 1] = coef(lm(shuffled_y ~ shuffled_x))[2]  # Model with shuffled x and y
}

head(random_slopes)
summary(random_slopes)

hist(random_slopes$slopes, main = "Null distribution of slopes", xlab = "Slope values")
abline(v = coef(lm(internal_temp ~ external_temp))[2], col = "red")

mean_slopes <- mean(random_slopes[, 1])
mean_slopes  # Mean of the null distribution of slopes
sd_slopes <- sd(random_slopes[, 1])
sd_slopes  # Standard deviation of the null distribution of slopes

pdf_value <- dnorm(coef(regression_model)[2], mean = mean_slopes, sd = sd_slopes)
pdf_value  # If it is very small, the estimated value by the regression is unlikely in the null distribution

pdf_null <- dnorm(0, mean = mean_slopes, sd = sd_slopes)
pdf_null

# Compare the two
pdf_value
pdf_null

# The probability density value to obtain 0 in the null distribution is much larger
# than obtaining the slope value calculated by the regression
# Remember that they are relative probabilities and can only be compared to each other

# # P-value

# It is the probability (area under the curve) in this null distribution of obtaining a value greater or equal a given computed statistic.
 
# Area under the curve is calculated by integrals. The function is the probability density function of
# the normal distribution (dnorm)
# Mean and sd determine the parameters of this normal distribution (dnorm).
# We will use the mean and sd parameters of the null distribution of parameters, since
# we are interested in knowing the probability of obtaining the parameters
# of the regression when the null hypothesis is true.

# In this case, we are calculating the probability (area under the curve) of obtaining a
# parameter equal (lower) or greater (upper) than that estimated by the regression
print(integrate(dnorm, mean = mean_slopes, sd = sd_slopes, lower = coef(regression_model)[2], upper = Inf))
# upper = infinity

# The pnorm function calculates this probability without needing integrals.
# The values are very similar
print(pnorm(coef(regression_model)[2], mean = mean_slopes, sd = sd_slopes, lower.tail = FALSE))
# lower.tail = FALSE means that it will be equal to or greater than coef(regression_model)[2]

# Here we calculate the probability of obtaining a parameter equal to or smaller than the
# parameter estimated by the regression
print(integrate(dnorm, mean = mean_slopes, sd = sd_slopes, lower = -Inf, upper = coef(regression_model)[2]))

print(pnorm(coef(regression_model)[2], mean = mean_slopes, sd = sd_slopes, lower.tail = TRUE))

# The first case, equal to or greater, is a one-sided test to the left, and the second case,
# equal to or smaller, is a one-sided test to the left

# To know the probability of obtaining a parameter greater or smaller
# (i.e., different from 0 on the right OR left)...
# than the estimated by the regression under the null hypothesis:
# add the probabilities, as it can be greater OR smaller
# but one of them must be inverted.
# I inverted the parameter that calculates the lower tail since the estimated parameter was positive

print(pnorm(coef(regression_model)[2], mean = mean_slopes, sd = sd_slopes, lower.tail = FALSE) + 
    pnorm(-coef(regression_model)[2], mean = mean_slopes, sd = sd_slopes, lower.tail = TRUE))

# Assuming that the value of slope estimated by the regression is 0.5,
# calculate the probability (area under the curve)
# of estimating this value when the null hypothesis is true
# the probabilities are what is to the right of the red line for one-sided tests to the right
# what is to the left of the red line for one-sided tests to the left
# and the sum of these areas for two-sided tests

plot(density(random_slopes[, 1]), main = "Null distribution", xlab = "Slopes")
abline(v = 0.05, col = "red")
abline(v = -0.05, col = "red")

# In summary: the p-value is the area under the curve (probability) of a null distribution
# the area is calculated according to the parameters estimated by the test (in this case, the regression)
# in this way, we calculate the probality of obtaining the test parameters when the null hypothesis is true

# If the slope value estimated by the regression is close to 0, it will have a high probability of occurring in the null distribution


# # Confidence interval
# It can be said to be the opposite of the p-value

# Function to calculate the slope in a regression from successive...
# random (with replacement) samplings of data (randomizations)
# there is no shuffling, that is, the relationships, if they exist, are maintained

f <- function () {
    fit <- lm(internal_temp ~ external_temp, data = data_frame_temp,
             subset = sample(nrow(data_frame_temp), 600, replace = TRUE))  # Resampling 600 observations
    coef(fit)
}

# Data frame with the parameters calculated in each iteration
z <- t(replicate(1000, f()))  # 1000 iterations
head(z)
dim(z)

par(mfrow = c(1, 2))
hist(z[, 1], main = "Intercept", xlab = "Intercept")
hist(z[, 2], main = "Slope", xlab = "Slope")

dev.off()

hist(z[, 2], main = "Slope", xlab = "Slope")

# Mean and standard deviation of the slopes in the distribution of estimates
mean_slopes_ci <<- mean(z[, 2])
sd_slopes_ci <<- sd(z[, 2])  # Standard error

mean_slopes_ci

pdf_val_ci <- dnorm(0, mean = mean_slopes_ci, sd = sd_slopes_ci)
pdf_val_ci

pdf_null_ci <- dnorm(true_slope, mean = mean_slopes_ci, sd = sd_slopes_ci)
pdf_null_ci

# Observe that the likelihood of observing 0 is very small compared to...
# ... the probability of observing the true parameter value

# What is the probability (area under the curve)...
# ... of the parameter being less than zero in the distribution of estimated parameters?

print(integrate(dnorm, mean = mean_slopes_ci, sd = sd_slopes_ci, lower = -Inf, upper = 0))

print(pnorm(0, mean = mean_slopes_ci, sd = sd_slopes_ci, lower.tail = TRUE))

# What is the probability (area under the curve)...
# ... of the parameter being greater than zero in the distribution of estimated parameters?

print(integrate(dnorm, mean = mean_slopes_ci, sd = sd_slopes_ci, lower = 0, upper = Inf))

print(pnorm(0, mean = mean_slopes_ci, sd = sd_slopes_ci, lower.tail = FALSE))

# the 95% range is obtained by the 0.025 (lower bound) and 0.975 (upper bound) percentiles values
# so, if zero is contained in the 95% range we don't reject the null hypothesis, 
# otherwise, the computed CI is 95% confident that the statistic is different from 0 (i.e. no effetct - null hypothesis)

# therefore, with the resamplings we obtained a distribution of paremeters values and using threshold values and percentiles we can obtain
# the range of 95% values, which are the lower and upper bounds of the CI. 
# Note that the confidence level can by any (90%, 99% etc).


df_hist <- rbind.data.frame(cbind.data.frame(slopes = z[, 2], type = "CI"),
                           cbind.data.frame(slopes = random_slopes$slopes, type = "pval"))

ggplot(df_hist) + theme_bw() +
  geom_density(aes(x = slopes, fill = type), alpha = 0.5) +
  labs(x = "Parameter Values", y = "Density", fill = "Distribution") +
  scale_fill_brewer(palette = "Dark2", labels = c("Estimates", "Null"))
