---
title: "CS5801 Coursework Resit Template Proforma"
author: "2385539"
date: "`r format(Sys.time(), '%d %B, %Y')`"
output:
  pdf_document: default
  html_notebook: default
version: 1
---

# 0. Instructions 

*1. Remove the (italicised) guidance text but keep the section headings.*  
*2. Add as many chunks of R code as required.*  
*3. Where relevant summarise your answer to a section at the top of the section then add descriptions of your analysis plans and explanations of your code and findings. Please be detailed and where you have made choices explain the rationale for them. Avoid including any generic definitions.*  
*4. Write your report using RMarkdown.  For guidance see a [helpful blog](https://www.dataquest.io/blog/r-markdown-guide-cheatsheet/#tve-jump-17333da0719) or use the R Markdown cheatsheet which can be accessed from within RStudio by selecting `Help > Cheatsheets > R Markdown Cheat Sheet`.*  
*5. Your report should be clearly and professionally presented with appropriate use of cited external sources You report should respect the word counts in each section. (5 marks)*  
*6. It should also be easy to understand, with well-documented code following the principles of literate programming. (5 marks)*


```{r}
library(DescTools)
library(tm)
library(rvest)
library(tm.plugin.factiva)
library(tidyverse)
library(modeest)
library(dplyr)
library(ggplot2)
library(aqp)
library(soilDB)
library(corrplot)
library(reshape2)
library(modelr)
library(validate)
library(Hmisc)
library(tidyr)
```


# 1. Organise and clean the data

## 1.1 Subset the data into the specific dataset allocated
 
*A description of the data set provided, its contents and which subset you should select is documented in the assessment brief at CS5801-Assessment Brief Template 2023-24.pdf*  

*Use R code to correctly select the subset of data allocated. (5 marks)*  

```{r}
# Only change the value for SID 
# Assign your student id into the variable SID, for example:
SID <- 2358839                  # This is an example, replace 2101234 with your actual ID
SIDoffset <- (SID %% 40) + 1    # Your SID mod 40 + 1

load("rental-analysis-data.Rda")
# Now subset the rentals data set
# Pick every 40th observation starting from your offset
# Put into your data frame named mydf (you can rename it)
mydf <- rentals.analysis[seq(from=SIDoffset,to=nrow(rentals.analysis),by=40),]
View(mydf)
```


## 1.2 Data quality analysis plan
 
*Provide a description of a comprehensive plan to assess the quality of this data. Include all variables/columns (5 marks) from the data set. (Max 200 words)*

```{r}
#Data Quality analysis plan
#check-for and identify any problems, and then determine and document the appropriate reponse.
#(chapter 5.3 of Modern Data book )


#This is a rental property dataset.
#The aim of the analysis is to model rental property prices.

#Assumptions:


#2. using 'str()' function get all the information on the dimensions of the dataframe i.e. how many observations and variables, the names of all variables, and the datatype of all variables. Ensure all variables are of the right data type in context, and make note of those which are incorrect.
#3. Using the 'head()' function display the first 6 records and all variables of the subset. 
#4. Check that constraints are adhered to: 
i) Using the constraint rule 'SID mod 40 + 1' calculate an expected value for the first row name. Check that the one in the dataframe matches the expected value. If not then make note.
ii) the observations have a difference of 50 unique ID's between them
#4. Identify all NA or missing records using is.na() function
#5. Check for outliers (i.e. price too high, year far ahead in the future, invalid engine size, low mileage)
#5. Ensure the data is valid in context i.e. they are all used cars.
#using '' to check that variables under 'mileage' are all greater than 500.
()

ls()
use ls() to generate a list of all functions in the R environment
```

## 1.3 Data quality analysis findings

*Provide a full implementation of your data quality plan from (1.2) Include all variables/columns from the data set and  (5 marks).(Max 100 words)*

```{r}
str(mydf)
#Observations: There are 13 variables and 211 observations.
#The variables 'instant_booking_fee','cleaning_fee',are down as char but should be of data-type logical
#(author, year)

head(mydf)
#First 6 observation Unique ID's: 371, 691, 1161, 1430, 1843, 2162

expectedvalue<- (2358839 %% 40) + 1
print(expectedvalue)

difference<- (961-371)
print(difference)

#Expected value doesnt match real value, and difference does not match constraint rule of 40.






```
 
## 1.4 Data cleaning  
 
*List and explain all the data quality issues found in 1.3 (5 marks)  NB even if no data quality issues are identified you should still check and report. Justify and document the way you have addressed each of the issues found (if any) (5 marks). (Max 200 words)*

```{r}
newdf <- na.omit(mydf)
head(newdf)
```


#(Ramzi W. N., 2024)

# 2. Exploratory Data Analysis (EDA)

## 2.1 EDA plan

*Outline a suitable plan to explore, describe and visualise your data. This plan should include appropriate summary statistics (uni- and multi-variate) and visualisations. The plan should also outline how you plan to explore the data with respect to the dependent variables  (5 marks) (Max 200 words)*  

```{r}
#The plan is as follows:


#1. Use summary() function to determine the mean, median, quartiles, variance, and standard dev. of the numerical variables,
#as well as the frequency count and mode of categorical variables using the table() and mode() (after installing the DescTools package). Because table() and mode() work on vectors and not dataframes; use the sapply() function to apply them to each column of the dataframe. (Kabacoff R.I, 2022)
#the correlation coefficients will be calculated using the rcorr() function in the Hmisc package which includes significance levels.
#(Mount J., 2019)

#2. visualise the distribution, relationships, and interactions of and between variables in the dataset. 
#hist() function generates histograms 
#barplot() function generates bar charts 
#boxplot() function generates bar charts (Kabacoff R.I, 2022)

#plot() function generates scatterplots
#cor() function generates a correlation matrix
#pheatmap() function (which can be used after installing pheatmap package generates heatmaps for correlation matrices. (Peng R.D, 2016),

#ggpairs() function (from the GGally package) is used to generare pair plots. (Mount J. & Zumel, 2019)


#3. #Using the IQR method outlined by Kabacoff, I will first install the datasets package, then find the first and third quartile difference using the quantile() function. 
#Any values that fall 1.5 times the IQR above the first quartile or below the third quartile is an outlier/anomaly which could affect the distribution of the dependent variables. (Kabacoff R.I, 2022)
```

## 2.2 EDA execution   

*Undertake the data exploration plan outlined in 2.1 (5 marks) (Max 100 words)*

```{r}

#the correlation coefficients will be calculated using the rcorr() function in the Hmisc package which includes significance levels.
#(Mount J., 2019)

#1.1
summary(newdf)

frequency_count <- sapply(newdf, table)
print(frequency_count)

mode_results <- sapply(newdf, Mode)
print(mode_results)

#1.2
rcorrmatrix <- rcorr(as.matrix(newdf))
print(rcorrmatrix)
  


  


#2. visualise the distribution, relationships, and interactions of and between variables in the dataset. 
#hist() function generates histograms 
#barplot() function generates bar charts 
#boxplot() function generates bar charts (Kabacoff R.I, 2022)

#plot() function generates scatterplots
#cor() function generates a correlation matrix
#pheatmap() function (which can be used after installing pheatmap package generates heatmaps for correlation matrices. (Peng R.D, 2016),

#ggpairs() function (from the GGally package) is used to generare pair plots. (Mount J. & Zumel, 2019)


#3. #Using the IQR method outlined by Kabacoff, I will first install the datasets package, then find the first and third quartile difference using the quantile() function. 
#Any values that fall 1.5 times the IQR above the first quartile or below the third quartile is an outlier/anomaly which could affect the distribution of the dependent variables. (Kabacoff R.I, 2022)
```


## 2.3 EDA summary of results

*Provide a concise summary of your findings (5 marks) (Max 300 words)*

## 2.4 Additional insights and issues

*Highlight potential further issues or insights uncovered in 2.2.  This might include follow up to findings from your initial EDA.  We accept that the boundary between 2.2 and 2.3 is somewhat arbitrary so use your judgement and maximise good structure and readability. (5 marks) (Max 200 words)*


# 3. Modelling

## 3.1 Explain your analysis plan

*The aim of the analysis is to model rental prices. Outline and justify an analysis plan (don't include or repeat the data cleaning and EDA plan) to address the aim that incorporates/references any findings from the data cleaning (1.4) and EDA (2.3, 2.4)  (5 marks). (Max 200 words)*

## 3.2 Build a model for rental price
*Use R to build a suitable model for rental prices on your data (dependent variable is price) (5 marks). (Max 100 words)*  
*NB Submissions where suitable models do not have good fit due to the nature of the data will not be penalised.*  


## 3.3 Critique model using relevant diagnostics

*Offer an interpretation of the model, goodness of fit and graphical diagnostics (5 marks) for the model built in 3.2. Explain any potential weaknesses (5 marks). (Max 200 words)*


## 3.4 Suggest and implement improvements to your model

*Based on the findings in 3.2 and 3.3 articulate and include one possible alternative approach to address the model weaknesses articulated in 3.3. Explain which model (from the ones in 3.2 and 3.4) you propose and why (5 marks). (Max 200 words)*


# 4. Modelling another dependent variable

## 4.1 Model the likelihood of a property having a strict cancellation policy (using the cancellation_policy_ind variable provided).

*The aim of the analysis is to model whether a property has a strict cancellation policy or not. (i.e., involving the binary target attribute).* 
*Provide a plan of analysis based on relevant EDA for this attribute. Execute the plan and address any weaknesses of the model and explore methods to improve it (10 marks).* 
*Justify and propose one model. Describe, explain and critique it (10 marks).*
*(Max 500 words)*
*NB Submissions where suitable models do not have good fit due to the nature of the data will not be penalised.* 

# References  

*Add any references here including references to use of GenAI. NB You can either do this manually or automatically with a `.bib` file (which then must be submitted along with your `.Rmd` file).  See the RMarkdown [documentation](https://bookdown.org/yihui/rmarkdown-cookbook/bibliography.html) for guidance.*   

# Kabacoff, R.I. (2022) R in Action, Third Edition. Shelter Island: Manning Publications. (Chapter 4)
# Crawley, M.J. (2015) Statistics: an introduction using R. 2nd ed. Chichester: Wiley. (Chapter 3)
# Mount, J. and Zumel, N. (2019) Practical Data Science with R. 2nd ed. Shelter Island: Manning Publications. (Chapter 6)
# Kabacoff, R.I. (2022) R in Action, Third Edition. Shelter Island: Manning Publications. (Chapter 7)
# Peng, R.D. and Matsui, E. (2016) The Art of Data Science: A Guide for Anyone Who Works with Data. Leanpub. (Chapter 5)
# Mount, J. and Zumel, N. (2019) Practical Data Science with R. 2nd ed. Shelter Island: Manning Publications. (Chapter 7)
# Crawley, M.J. (2015) Statistics: an introduction using R. 2nd ed. Chichester: Wiley. (Chapter 4)
# Peng, R.D. and Matsui, E. (2016) The Art of Data Science: A Guide for Anyone Who Works with Data. Leanpub. (Chapter 3)
# Kabacoff, R.I. (2022) R in Action, Third Edition. Shelter Island: Manning Publications. (Chapter 8)
# Mount, J. and Zumel, N. (2019) Practical Data Science with R. 2nd ed. Shelter Island: Manning Publications. (Chapter 10)
# Kabacoff, R.I. (2022). R in Action, Third Edition. Shelter Island: Manning Publications. (Chapter 4).
# Ramzi W. Nahhas (2024) An Introduction to R for Research (Chapter 4.3)

 