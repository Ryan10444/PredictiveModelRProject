csv_file_path <- file.choose() # Select File Location of csv file in system’s file explorer
data <- read.csv(csv_file_path) # Uses the selected file as the data
exam <- as.numeric(as.character(data$exam_score)) # Convert exam_score column to numeric, since the file stores the data as text.

# Remove any missing values, our csv has none, but I included this for proof of concept
exam_clean <- exam[!is.na(exam)] #stores clean data in a vector which is used in calculations later

# Checks for any invalid exam scores, such as scores below 0 or above 100, csv has none, again included for proof of concept
defective_index <- which(exam_clean < 0 | exam_clean > 100)
defective_data <- exam_clean[defective_index]
defective_data #Vector of all defective exam scores

# Detects outliers using IQR
Q1 <- quantile(exam_clean, 0.25)
Q3 <- quantile(exam_clean, 0.75)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value
outlier_index <- which(exam_clean < lower_bound | exam_clean > upper_bound)
outliers <- exam_clean[outlier_index]
outliers #Vector of all outliers

# Creates histogram and a density plot
hist(
  exam_clean,
  breaks = 20,
  freq = FALSE,
  main = "Histogram of Exam Scores", #title of histogram
  xlab = "Exam Score" #x axis name
  #default name is for ylab is density, which should be fine
)
lines(density(exam_clean))

# Calculates sample mean, variance, and confidence interval
n <- length(exam_clean) #n is sample size
xbar <- mean(exam_clean)
var <- var(exam_clean)
std_dev <- sqrt(var)
std_err <- std_dev / sqrt(n)
z_score <- 1.96 # 1.96 is z score for a 95% confidence interval
ci_lower <- xbar - z_score * std_err
ci_upper <- xbar + z_score * std_err
xbar #sample mean
var #varience
ci_lower #Lowest logical mean
ci_upper #Highest logical mean

### Parametric Modeling (Normal, Exponential, Gamma)

# Method of Moments (MM)

# Normal MM parameters (just the sample mean and sd)
normal_mean_mm <- xbar
normal_sd_mm <- std_dev

# Exponential MM parameter (rate = 1/mean)
exp_rate_mm <- 1 / xbar

# Gamma MM parameters
# shape = mean^2 / variance
# scale = variance / mean
gamma_shape_mm <- (xbar^2) / var
gamma_scale_mm <- var / xbar

normal_mean_mm
normal_sd_mm
exp_rate_mm
gamma_shape_mm
gamma_scale_mm

# Maximum Likelihood Estimation (MLE)

# Normal MLE:
# mu_hat = sample mean
# sigma^2_hat = sum (x - mean)^2 / n   (note: divide by n, not n-1)
normal_mean_mle <- mean(exam_clean)
normal_var_mle <- sum((exam_clean - normal_mean_mle)^2) / n
normal_sd_mle <- sqrt(normal_var_mle)

normal_mean_mle
normal_sd_mle

# Exponential MLE:
# For exponential, MM and MLE give the same rate
exp_rate_mle <- 1 / mean(exam_clean)
exp_rate_mle

# Gamma MLE:
# We will do a simple grid search over possible shape values.
# For each shape, the MLE scale is mean / shape.

gamma_loglik <- function(shape_value) {
  scale_value <- mean(exam_clean) / shape_value
  # log-likelihood for Gamma(shape, scale)
  ll <- (shape_value - 1) * sum(log(exam_clean)) -
    sum(exam_clean) / scale_value -
    n * (shape_value * log(scale_value) + lgamma(shape_value))
  return(ll)
}

# Search for shape around the MM value
shape_start <- max(0.1, gamma_shape_mm / 2)
shape_end <- gamma_shape_mm * 2
shape_grid <- seq(shape_start, shape_end, length.out = 200)

loglik_values <- numeric(length(shape_grid))
for (i in 1:length(shape_grid)) {
  loglik_values[i] <- gamma_loglik(shape_grid[i])
}

best_index <- which.max(loglik_values)
gamma_shape_mle <- shape_grid[best_index]
gamma_scale_mle <- mean(exam_clean) / gamma_shape_mle

gamma_shape_mle
gamma_scale_mle

# ----------------- Standard Errors and 95% Confidence Intervals -----------------

z_value <- 1.96 # for 95% CI

## Normal mean (using sd_mle / sqrt(n))
se_normal_mean_mle <- normal_sd_mle / sqrt(n)
ci_normal_mean_lower <- normal_mean_mle - z_value * se_normal_mean_mle
ci_normal_mean_upper <- normal_mean_mle + z_value * se_normal_mean_mle

se_normal_mean_mle
ci_normal_mean_lower
ci_normal_mean_upper

## Normal variance / standard deviation (using chi-square approximation)
chi_lower <- qchisq(0.975, df = n)
chi_upper <- qchisq(0.025, df = n)
ci_normal_var_lower <- (n * normal_var_mle) / chi_lower
ci_normal_var_upper <- (n * normal_var_mle) / chi_upper
ci_normal_sd_lower <- sqrt(ci_normal_var_lower)
ci_normal_sd_upper <- sqrt(ci_normal_var_upper)

ci_normal_var_lower
ci_normal_var_upper
ci_normal_sd_lower
ci_normal_sd_upper

## Exponential rate (large-sample approximation: SE ≈ rate / sqrt(n))
se_exp_rate_mle <- exp_rate_mle / sqrt(n)
ci_exp_rate_lower <- exp_rate_mle - z_value * se_exp_rate_mle
ci_exp_rate_upper <- exp_rate_mle + z_value * se_exp_rate_mle

se_exp_rate_mle
ci_exp_rate_lower
ci_exp_rate_upper

## Gamma shape and scale:
## We use a simple parametric bootstrap to get SEs and CIs.

set.seed(123) # for reproducibility
B <- 500       # number of bootstrap samples
gamma_shape_boot <- numeric(B)
gamma_scale_boot <- numeric(B)

for (b in 1:B) {
  sim_sample <- rgamma(n, shape = gamma_shape_mle, scale = gamma_scale_mle)
  sim_mean <- mean(sim_sample)
  # compute variance by hand so we don't rely on var()
  sim_var <- sum((sim_sample - sim_mean)^2) / (length(sim_sample) - 1)
  gamma_shape_boot[b] <- (sim_mean^2) / sim_var
  gamma_scale_boot[b] <- sim_var / sim_mean
}

se_gamma_shape <- sd(gamma_shape_boot)
se_gamma_scale <- sd(gamma_scale_boot)

ci_gamma_shape_lower <- gamma_shape_mle - z_value * se_gamma_shape
ci_gamma_shape_upper <- gamma_shape_mle + z_value * se_gamma_shape

ci_gamma_scale_lower <- gamma_scale_mle - z_value * se_gamma_scale
ci_gamma_scale_upper <- gamma_scale_mle + z_value * se_gamma_scale

se_gamma_shape
ci_gamma_shape_lower
ci_gamma_shape_upper
se_gamma_scale
ci_gamma_scale_lower
ci_gamma_scale_upper

### Monte Carlo Simulation

# First, estimate Normal, Exponential, and Gamma parameters
# Using simple formulas (Method of Moments)

# Normal distribution parameters
normal_mean <- xbar
normal_sd <- std_dev

# Exponential parameter (rate = 1/mean)
exp_rate <- 1 / xbar

# Gamma distribution parameters
# Method of Moments:
# shape = mean^2 / variance
# scale = variance / mean
gamma_shape <- (xbar^2) / var
gamma_scale <- var / xbar

# Function to run simulation and return means
run_simulation <- function(dist_name, reps) {
  sim_means <- numeric(reps) # empty vector for storing means
  
  for (i in 1:reps) {
    if (dist_name == "normal") {
      sim_data <- rnorm(n, mean = normal_mean, sd = normal_sd)
    }
    if (dist_name == "exponential") {
      sim_data <- rexp(n, rate = exp_rate)
    }
    if (dist_name == "gamma") {
      sim_data <- rgamma(n, shape = gamma_shape, scale = gamma_scale)
    }
    
    sim_means[i] <- mean(sim_data)
  }
  
  return(sim_means)
}

# Run simulations for 100, 1,000, and 10,000 repetitions
normal_100 <- run_simulation("normal", 100)
normal_1000 <- run_simulation("normal", 1000)
normal_10000 <- run_simulation("normal", 10000)

exp_100 <- run_simulation("exponential", 100)
exp_1000 <- run_simulation("exponential", 1000)
exp_10000 <- run_simulation("exponential", 10000)

gamma_100 <- run_simulation("gamma", 100)
gamma_1000 <- run_simulation("gamma", 1000)
gamma_10000 <- run_simulation("gamma", 10000)

# Compare simulated means to real mean
mean(normal_100); mean(normal_1000); mean(normal_10000)
mean(exp_100); mean(exp_1000); mean(exp_10000)
mean(gamma_100); mean(gamma_1000); mean(gamma_10000)

# Check variance of simulation means
var(normal_100); var(normal_1000); var(normal_10000)
var(exp_100); var(exp_1000); var(exp_10000)
var(gamma_100); var(gamma_1000); var(gamma_10000)

### Model Comparison

# KS Tests for each distribution
ks_normal <- ks.test(exam_clean, pnorm, mean = normal_mean, sd = normal_sd)
ks_exponential <- ks.test(exam_clean, pexp, rate = exp_rate)
ks_gamma <- ks.test(exam_clean, pgamma, shape = gamma_shape, scale = gamma_scale)

ks_normal
ks_exponential
ks_gamma

# Q-Q plots for each distribution
par(mfrow = c(1,3))

# Normal QQ plot
qqnorm(exam_clean, main="Normal Q-Q Plot")
qqline(exam_clean)

# Exponential QQ plot
exp_theoretical <- qexp(ppoints(n), rate = exp_rate)
qqplot(exp_theoretical, exam_clean, main="Exponential Q-Q Plot")
abline(0,1)

# Gamma QQ plot
gamma_theoretical <- qgamma(ppoints(n), shape = gamma_shape, scale = gamma_scale)
qqplot(gamma_theoretical, exam_clean, main="Gamma Q-Q Plot")
abline(0,1)

par(mfrow = c(1,1)) # reset layout

csv_dir <- dirname(csv_file_path) #Gets the directory of the original .csv file
output_file_path <- file.path(csv_dir, "DataList.txt") #Sets the output location for cleaned data
