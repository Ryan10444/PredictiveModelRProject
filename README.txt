Exam Score Analysis README
This R program performs a statistical analysis on a set of student exam scores, including data cleaning, important statistics, parameter estimation using Method of Moments (MM) and Maximum Likelihood Estimation (MLE), Monte Carlo simulation, and a model comparison for Normal, Exponential, and Gamma distributions.
How to Run the Program
You must have R installed on your system. Use of RStudio will be best as your environment, since this is where the program was designed, meaning the chance of execution errors will be minimal.
Step 1: Download the data
Download the CSV data file (student_exam_scores.csv) from https://www.kaggle.com/datasets/saadaliyaseen/analyzing-student-academic-trends, and save it somewhere easily accessible on your system.
Step 2: Install Dependencies & Execute the Script
Prior to running the code, run install.packages("gridExtra") in the console. Then, either download the code via the R file or copy and paste all of the code into RStudio. If copy and pasting, make sure to get every line of code to prevent runtime errors.
Step 3: Interactive File Selection
As soon as the script begins running, file explorer (or similar based on operating system) will open.
Navigate to the location of your CSV file student_exam_scores.csv.
Select the file and click Open.
Step 4: Viewing the Graphs
The script will generate two main graphs groups in the RStudio Plots window:
A histogram with a density curve.
Three Q-Q plots (Normal, Exponential, Gamma) arranged side-by-side for visual model comparison.
To navigate between these graphs, there are arrows at the top of the RStudio Plots window, which allows the user to switch between the histogram and the three Q-Q plots.
To view the Pearson correlation matrix, predictor box plots, and predictor scatter plots, PNGs are saved in your working directory.
Step 5: Save Data
At the end of execution, the program will automatically save a data summary of all calculated statistics, parameters (MM and MLE), confidence intervals, and KS test results to a file named “DataList.txt” This file will be saved in the same folder as the CSV file you selected in Step 3.                   
END OF README
