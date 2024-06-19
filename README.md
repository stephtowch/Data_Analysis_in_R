# Data_Analysis_in_R  <img src= "https://i.giphy.com/media/v1.Y2lkPTc5MGI3NjExbWhuZW81M2JxOWY4b3M0NHdrNnNhbnpkcjI0amZjMzc0Y3libWN0MSZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9cw/8Lb8TPY7l2T9Mc1QIW/giphy.gif" width="60">

[![](https://img.shields.io/badge/Language-R-blue)](http://cran.r-project.org/)

<sub>*Last updated 2024-06-19.*</sub>

This GitHub repository contains R code for conducting a data analysis, specifically tailored for psychologists.

It will detail how to import, tidy, visualise, transform, and model a dataset using R through a worked example <img src="https://media.giphy.com/media/WUlplcMpOCEmTGBtBW/giphy.gif" width="30">

Key steps include:
- Importing and tidying data
- Imputing missing data where applicable
- Visualising data through histograms, bar charts, scatterplots, boxplots, and Q-Q plots
- Viewing descriptive statistics to gain insights into data distributions
- Transforming data, including univariate outlier analysis
- Utilising Cronbach's Alpha to assess internal consistency reliability
- Constructing a correlation data matrix to examine relationships between variables
- Modelling data using techniques such as simple linear regression, multiple linear regression, and hierarchical linear regression

These steps are essential for understanding and interpreting data, ensuring robust analysis and informed decision-making. Let's delve into each stage to uncover valuable insights from our dataset.

## :telescope: Overview

The repository is organised into the following sections:

- **[Slides](/Slides/Data_Analysis_in_R.pptx)**: Accompanying slides with some hints and tips on conducting data analysis in R.
- **[R Script](/RScript/Data_Analysis_in_R.R)**: The R script for importing, tidying, visualising, transforming, and modelling a dataset using `tidyverse`, `psych`, `apaTables`, `ggally`, `gt`, and `gtsummary` packages.
- **[Data](/Data/data.csv)**: The dataset used in this worked example contains an anonyomised data set collected in 2023 with demographic, work addiction, job satisfaction, psychological capital and burnout variables.
<img src='logo/Hex.png' align="right" height="139" />

## :scroll: Notes

This repository assumes basic competence in R (installing and loading packages, etc) and is therefore not a complete beginners guide. It contains only materials relating to *Data Analysis in R*. So the focus will be generally on the application and not on the theory.  

## :hammer_and_wrench: Setup

To run the code, you will need:

1. The dataset in the data folder.
2. A fresh installation of [**`R`**](https://cran.r-project.org/) (preferably version 4.3.2 or above).
3. [RStudio IDE](https://www.rstudio.com/products/rstudio/download/) (optional but recommended).
4. Install the required packages by running:

   ```R
   # in alphabetical order:
   pkgs <- c(
     "tidyverse", "psych", "apaTables", "ggally", "gt", "gtsummary"
   )

   install.packages(pkgs, repos = c("https://easystats.r-universe.dev", getOption("repos")))

<details>
<summary>
<i>Package Versions</i>
</summary>
   
Run on Windows 11 x64 (build 22621), with R version 4.3.2.

The packages used here:

- `tidyverse` 2.0.0 (*CRAN*)
- `psych` 2.3.12 (*CRAN*)
- `apaTables` 2.0.8 (*CRAN*)
- `ggally` 2.2.1 (*CRAN*)
- `gt` 0.10.1 (*CRAN*)
- `gtsummary` 1.7.2 (*CRAN*)

</details>

Feel free to adjust this based on your preferences and specific details about your code and setup.
