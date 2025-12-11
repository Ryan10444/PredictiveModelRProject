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
