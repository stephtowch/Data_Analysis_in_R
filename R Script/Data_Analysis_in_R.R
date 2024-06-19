#Data Analysis in R - Descriptive, Correlation and Regression Analysis

#set working directory
setwd("C:/Users/Steph/Documents/Data_Analysis_in_R_Example")

#load the tidyverse package
#install.packages("tidyverse")
#tidyverse_packages() 
library(tidyverse)

#read in the data
data <- read_csv("data.csv")

#review structure of data 
str(data) #all columns are numerical data

#count the number of participants
nrow(data)

#Identify rows with missing values for each variable
columns_to_check <- c(
  grep("^BWAS_1", names(data)),
  grep("^BWAS_2", names(data)),
  grep("^BWAS_3", names(data)),
  grep("^BWAS_4", names(data)),
  grep("^BWAS_5", names(data)),
  grep("^BWAS_6", names(data)),
  grep("^BWAS_7", names(data)),
  grep("^PC_1", names(data)),
  grep("^PC_2", names(data)),
  grep("^PC_3", names(data)),
  grep("^PC_4", names(data)),
  grep("^PC_5", names(data)),
  grep("^PC_6", names(data)),
  grep("^PC_7", names(data)),
  grep("^PC_8", names(data)),
  grep("^PC_9", names(data)),
  grep("^PC_10", names(data)),
  grep("^PC_11", names(data)),
  grep("^PC_12", names(data)),
  grep("^JSS_1", names(data)),
  grep("^JSS_2", names(data)),
  grep("^JSS_3", names(data)),
  grep("^JSS_4", names(data)),
  grep("^JSS_5", names(data)),
  grep("^JSS_6", names(data)),
  grep("^JSS_7", names(data)),
  grep("^BBI_1", names(data)),
  grep("^BBI_2", names(data)),
  grep("^BBI_3", names(data)),
  grep("^BBI_4", names(data)),
  grep("^BBI_5", names(data)),
  grep("^BBI_6", names(data)),
  grep("^BBI_7", names(data)),
  grep("^BBI_8", names(data)),
  grep("^BBI_9", names(data)
  )
)

#OR USE THIS SHORTENED CODE

#Define column prefixes to check
#prefixes_to_check <- c("BWAS_", "PC_", "JSS_", "BBI_")

# Identify columns with the specified prefixes
#columns_to_check <- unlist(lapply(prefixes_to_check, function(prefix) {
#  grep(paste0("^", prefix), names(data))
#}))

# Select columns to check for missing values
#data_to_check <- data[, columns_to_check]

# Count the number of complete cases
count_complete_cases <- sum(complete.cases(data[, columns_to_check]))
print(paste("Number of complete cases:", count_complete_cases)) #507 complete cases

# Identify rows with missing values
rows_with_missing <- which(!complete.cases(data[, columns_to_check]))
print(paste("Number of missing cases:", length(rows_with_missing))) #8 missing cases 

# Save the data with missing cases
data_with_missing <- data[rows_with_missing, ]
write_csv(data_with_missing, "Data_with_missing_cases.csv") #inspect this file for missing cases showing 8 missing cases in JSS_1, JSS_2, JSS_5, JSS_7 columns

# Save the data with complete cases
data_complete <- data[complete.cases(data[, columns_to_check]), ]
write_csv(data_complete, "Data_complete_cases.csv")

#visualise the data using ggplot before imputing the data. Review JSS_1, JSS_2, JSS_5, JSS_7
ggplot(data, aes(JSS_1)) +
  geom_histogram(color = "#000000", fill = "#0099F8", binwidth = 0.5) +
  ggtitle("Variable distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

#So, why is this important? It's a good idea to compare variable distribution before and after imputation. 
#You don't want the distribution to change significantly, and a histogram is a good way to check that.

#Impute Missing Values with row means of the variable JSS

# Load the package
library(dplyr)

# Impute missing values with row-wise mean
impute_with_row_mean <- function(data, columns_to_impute, all_columns) {
  data <- data %>%
    mutate(mean_JSS = rowMeans(select(., all_of(all_columns)), na.rm = TRUE))
  
  for (col in columns_to_impute) {
    data <- data %>%
      mutate(!!sym(col) := ifelse(is.na(.[[col]]), mean_JSS, .[[col]]))
  }
  
  data <- data %>% select(-mean_JSS)  # Remove the temporary mean column
  return(data)
}

# Define the columns to impute and all related columns
columns_to_impute <- c("JSS_1", "JSS_2", "JSS_5", "JSS_7") #These columns contained missing values
all_columns <- paste0("JSS_", 1:7)

# Impute missing values
data <- impute_with_row_mean(data, columns_to_impute, all_columns)

# Verify the imputation
summary(data[, columns_to_impute])

#visualise the data using ggplot after imputing the data. Review JSS_1, JSS_2, JSS_5, JSS_7
ggplot(data, aes(JSS_1)) +
  geom_histogram(color = "#000000", fill = "#228B22", binwidth = 0.5) +
  ggtitle("Variable distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

# Write the modified dataset to a CSV file
write_csv(data, "Imputed_Data.csv")

#Count rows (partcipants)
nrow(data)


#Visualise the imputed data in a table summary per variable showing the means and SD for each item (Example = The BWAS scale)

# Load libraries
library(gtsummary)
library(gt)

# Read in the data
data <- read_csv("Imputed_Data.csv")

# Define column names for Wellbeing
BWAS_columns <- c(
  "BWAS_1",
  "BWAS_2",
  "BWAS_3",
  "BWAS_4",
  "BWAS_5",
  "BWAS_6",
  "BWAS_7"
)


# Create a summary table using tbl_summary with overall statistics
BWAS_tbl <- 
  tbl_summary(
    data = select(data, all_of(BWAS_columns)),
    missing = "no",
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = all_continuous() ~ 2,
    type = list(BWAS_1 ~ "continuous", 
                BWAS_2 ~ "continuous", 
                BWAS_3 ~ "continuous", 
                BWAS_4 ~ "continuous", 
                BWAS_5 ~ "continuous",
                BWAS_6 ~ "continuous",
                BWAS_7 ~ "continuous"),
    label = list(BWAS_1 ~ "Thought of how you could free up more time to work?", 
                 BWAS_2 ~ "Spent much more time working than initially intended?", 
                 BWAS_3 ~ "Worked in order to reduce feelings of guilt, anxiety, helplessness and depression?", 
                 BWAS_4 ~ "Been told by others to cut down on work without listening to them?", 
                 BWAS_5 ~ "Become stressed if you have been prohibited from working?",
                 BWAS_6 ~ "Deprioritized hobbies, leisure activities, and exercise because of your work?",
                 BWAS_7 ~ "Worked so much that it has negatively influenced your health?")
  )%>%
  modify_header(label ~ "**Work Addiction**", stat_0 = "**N = 515**")

# Print the tbl_summary
print(BWAS_tbl)

# Convert tbl_summary to a tibble
BWAS_table <- BWAS_tbl %>%
  as_tibble()

#Saving the table in Excel or HTML format

# Specify the path where you want to save the table as Excel (xlsx)
output_path_excel <- "C:/Users/Steph/Documents/Data_Analysis_in_R_Example/BWAS_tbl.xlsx"

# Save the table as an Excel file using writexl package
BWAS_table %>%
  writexl::write_xlsx(output_path_excel)

# Convert tbl_summary to an object
BWAS_tbl <- as_gt(BWAS_tbl)

# Specify the path where you want to save the table as HTML
output_path <- "C:/Users/Steph/Documents/Data_Analysis_in_R_Example/BWAS_tbl.html"

# Save the table as an HTML file
gtsave(BWAS_tbl, file = output_path)

# Visualise the data per variable using ggplot (Example = shows a barchart and the means for each item of the BWAS scale)

library(tidyverse)
library(psych)

# Define custom labels for Career_Satisfaction columns
BWAS_labels <- c(
  "Thought of how you could free up more time to work?", 
  "Spent much more time working than initially intended?",
  "Worked in order to reduce feelings of guilt, anxiety, helplessness and depression?", 
  "Been told by others to cut down on work without listening to them?", 
  "Become stressed if you have been prohibited from working?",
  "Deprioritized hobbies, leisure activities, and exercise because of your work?",
  "Worked so much that it has negatively influenced your health?"
)

# Select relevant columns for BWAS and calculate mean
BWAS_columns <- select(data, starts_with("BWAS_"))
means <- colMeans(BWAS_columns, na.rm = TRUE)

# Create a data frame for plotting
df <- data.frame(Question = BWAS_labels, Mean = means)

# Create the plot using ggplot2
BWAS_plot <- ggplot(df, aes(x = Mean, y = Question)) +
  geom_bar(stat = "identity", fill = "darkorange", width = 0.8) +
  labs(title = "Mean Work Addiction Scores",
       x = "Mean Score", y = "Work Addiction Question") +
  theme_classic() +
  theme(axis.text.y = element_text(hjust = 0)) +
  xlim(0, 5) +  # Set x-axis limits
  geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = NA, color = "black", size = 0.5) +  # Add rectangle around plot area
  theme(panel.grid.major.x = element_blank(), aspect.ratio = 0.5)

# View plot
print(BWAS_plot)

# Specify the path to save the plot as PDF
output_path <- "C:/Users/Steph/Documents/Data_Analysis_in_R_Example/BWAS_ggplot.pdf"

# Save the plot as a PDF file
ggsave(filename = output_path, plot = BWAS_plot, width = 8, height = 6)

#Descriptive statistics  (n, mean, standard deviation, median, min, max, range, skew, kurtosis, standard error)
# Load the necessary libraries
library(tidyverse)
library(psych)

# Read in the data if you haven't loaded it already or run these descriptive statistics retrospectively with the final dataset
#data <- read_csv("Final_Data.csv") 

# select the columns you need descriptive statistics
subsetdata <- data %>%
  select(starts_with("BWAS_"), starts_with("BBI_"), starts_with("JSS_"), starts_with("PC_"))

# Get descriptive statistics
describe(subsetdata)

# Save the descriptive statistics on a csv file
write_csv(subsetdata,"descriptive_stats.csv")

#Creating scale scores in R (BWAS, PC, JJS, BBI variables)

#first calculate the Cronbach alpha for each scale to assess the internal reliability consistency using the Psych package
#Note: The general rule of thumb is that a Cronbach’s alpha of .70 and above is good, .80 and above is better, and .90 and above is best.

#load the package
library(psych)

# Select relevant columns for the Work Addiction scale
BWAS_columns <- select(data, starts_with("BWAS_"))

# Calculate Cronbach's alpha for the BWAS
cronbach_alpha_BWAS <- alpha(BWAS_columns, check.keys = TRUE)
print(cronbach_alpha_BWAS)

# Select relevant columns for the Psychological Capital scale
PC_columns <- select(data, starts_with("PC_"))

# Calculate Cronbach's alpha for the PC
cronbach_alpha_PC <- alpha(PC_columns, check.keys = TRUE)
print(cronbach_alpha_PC)

# Select relevant columns for the Job Satisfaction scale
JSS_columns <- select(data, starts_with("JSS_"))

# Calculate Cronbach's alpha for the JSS
cronbach_alpha_JSS <- alpha(JSS_columns, check.keys = TRUE)
print(cronbach_alpha_JSS)

# Select relevant columns for the Burnout scale
BBI_columns <- select(data, starts_with("BBI_"))

# Calculate Cronbach's alpha for the JSS
cronbach_alpha_BBI <- alpha(BBI_columns, check.keys = TRUE)
print(cronbach_alpha_BBI)

#Creating total scores per variable (BWAS, PC, JJS, BBI variables) and adding a column in the dataset
data$Total_BWAS <- rowMeans(BWAS_columns)
data$Total_PC <- rowMeans(PC_columns)
data$Total_JSS <- rowMeans(JSS_columns)
data$Total_BBI <- rowMeans(BBI_columns)

# Write the modified dataset to a CSV file
write_csv(data, "data_with_total_scores.csv")

#Visualise the data with boxplots to view potential outliers

#read in the data
data <- read_csv("data_with_total_scores.csv")

# Create a boxplot for Total_BWAS 
ggplot(data, aes(x = "", y = Total_BWAS)) +
  geom_boxplot(fill = "darkorange", color = "black") +
  labs(title = "Boxplot of Total BWAS Scores",
       x = "", y = "Total BWAS Score") +
  theme_minimal()

# Create a boxplot for Total_PC 
ggplot(data, aes(x = "", y = Total_PC)) +
  geom_boxplot(fill = "darkorange", color = "black") +
  labs(title = "Boxplot of Total PC Scores",
       x = "", y = "Total PC Score") +
  theme_minimal()

# Create a boxplot for Total_JSS
ggplot(data, aes(x = "", y = Total_JSS)) +
  geom_boxplot(fill = "darkorange", color = "black") +
  labs(title = "Boxplot of Total JSS Scores",
       x = "", y = "Total JSS Score") +
  theme_minimal()

# Create a boxplot for Total_BBI
ggplot(data, aes(x = "", y = Total_BBI)) +
  geom_boxplot(fill = "darkorange", color = "black") +
  labs(title = "Boxplot of Total BBI Scores",
       x = "", y = "Total BBI Score") +
  theme_minimal()

#Or create a panel of boxplots for the four variables

#read in the data
data <- read_csv("data_with_total_scores.csv")

# Define the variables for boxplots
variables <- c("Total_BWAS", "Total_PC", "Total_JSS", "Total_BBI")

# Melt the data into long format suitable for ggplot2
data_long <- data %>%
  select(all_of(variables)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Score")

# Create a boxplot with facets
ggplot(data_long, aes(x = Variable, y = Score)) +
  geom_boxplot(fill = "darkorange", color = "black") +
  labs(title = "Boxplots of Total Scores",
       x = "Variable", y = "Total Score") +
  theme_minimal() +
  facet_wrap(~ Variable, scales = "free")


#Check for univariate outliers using the tidyverse package
library(tidyverse)

#read in the data
data <- read_csv("data_with_total_scores.csv")

# Columns to calculate z-scores for
columns <- c("Total_BWAS", "Total_PC", "Total_JSS", "Total_BBI")

# Function to calculate z-score
z_score <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Calculate z-scores and filter outliers
df_filtered <- data %>%
  mutate(across(all_of(columns), ~z_score(.), .names = "z_{.col}")) %>%
  filter(if_any(starts_with("z_"), ~abs(.) > 3.29))

# View the filtered dataframe
print(df_filtered)

# Write the filtered dataframe to a CSV file
write_csv(df_filtered, "univariate_outliers.csv")

#univariate outliers can be removed from the dataset prior to correlation and regression analysis
#saved as Final_Data.csv

#Visualise the data as a Q-Q Plot to check for a normal distribution per variable

# Save the Q-Q plot as a PDF file
pdf(file = "BWAS_QQPlot.pdf")

qqnorm(
  data$Total_BWAS, 
  main = "Q-Q Plot of Total Work Addiction", 
  xlab = "Theoretical Quantiles", 
  ylab = "Sample Quantiles")

# Add a reference line to the Q-Q plot
qqline(data$Total_BWAS)

# Close the PDF file
dev.off()

#Correlation Analysis

#Creating a scatterplot to review the linear relationships between an IV and a DV
data %>% 
  ggplot(aes(x = Total_BWAS, y = Total_BBI)) + # Note: x is the IV and y is the DV
  geom_jitter(alpha = .1) +
  geom_smooth(method = "lm")

#Creating a scatterplot to review the linear relationships between an IV and a DV
data %>% 
  ggplot(aes(x = Total_PC, y = Total_BBI)) + # Note: x is the IV and y is the DV
  geom_jitter(alpha = .1) +
  geom_smooth(method = "lm")

#Creating a scatterplot to review the linear relationships between an IV and a DV
data %>% 
  ggplot(aes(x = Total_JSS, y = Total_BBI)) + # Note: x is the IV and y is the DV
  geom_jitter(alpha = .1) +
  geom_smooth(method = "lm")

#Correlation Analysis using apaTables package

# Load the package
library(apaTables)

# Read in the data
data <- read_csv("Final_Data.csv")

# Generate APA-style correlation matrix table directly from the data
apa_table <- apa.cor.table(data[, c("Total_BWAS","Total_PC", "Total_JSS", "Total_BBI")], 
                           filename = "correlation_matrix.doc", 
                           table.number = 1)

# To find the Word doc, go to files in RStudio and look for the correlation_matrix.doc file and open

# Print the table
print(apa_table)


#Correlation Analysis using GGally package

#install.packages("GGally")
library(GGally)
data %>% 
  select(c("Total_BWAS","Total_PC", "Total_JSS", "Total_BBI")) %>%
  ggpairs(lower = list(continuous=wrap("points", position="jitter", alpha = .1)))

#Or calculate correlations between pairs of variables(Pearson's product moment correlation)
cor.test(data$Total_BWAS, data$Total_BBI, method = "pearson")
cor.test(data$Total_JSS, data$Total_BBI, method = "pearson")
cor.test(data$Total_PC, data$Total_BBI, method = "pearson")
cor.test(data$Total_BWAS, data$Total_PC, method = "pearson")
cor.test(data$Total_BWAS, data$Total_JSS, method = "pearson")
cor.test(data$Total_JSS, data$Total_PC, method = "pearson")
#can change method = "spearman" or to "kendall"

#Simple linear regression model with one IV (Total_BWAS) and one DV (Total_BBI)
model1 <- lm(Total_BBI ~ Total_BWAS, data = data)
summary(model1)

#Multiple linear regression model with three IVs (Total_BWAS, Total_PC, Total_JSS) and one DV (Total_BBI)
model2 <- lm(Total_BBI ~ Total_BWAS + Total_PC + Total_JSS, data = data)
summary(model2)

#Hierarchical linear regression run the two models above and compare model 1 and model 2 with ANOVA
# Compare the two models
anova(model1, model2)


