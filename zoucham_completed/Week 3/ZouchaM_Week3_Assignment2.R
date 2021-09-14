# Assignment: ASSIGNMENT 3.2
# Name: Zoucha, Michael
# Date: 2021-09-13

## Load the ggplot2 package
library(ggplot2)
library(pastecs)
theme_set(theme_minimal())

## Set the working directory to the root of your DSC 520 directory
setwd("/Users/Shared/OneDrive - Bellevue University/DSC 520 - Statistics for Data Science/mkzoucha_dsc520/dsc520")

## Load the `data/r4ds/heights.csv` to
acs_df <- read.csv("data/acs-14-1yr-s0201.csv")

## Please provide the output from the following functions: str(); nrow(); ncol()
str(acs_df)
nrow(acs_df)
ncol(acs_df)

##Create a Histogram of the HSDegree variable using the ggplot2 package.
##Set a bin size for the Histogram.
##Include a Title and appropriate X/Y axis labels on your Histogram Plot
ggplot(acs_df, aes(HSDegree)) + geom_histogram(bins = 12) + 
  ggtitle("Distribution of High School Graduation Rates") + 
  xlab("% of Population w/ High School Degree") + ylab("Number of Counties")

##Create a Probability Plot of the HSDegree variable.
ggplot(acs_df, aes(HSDegree)) + geom_density(aes(HSDegree))

##Now that you have looked at this data visually for normality, you will now 
##quantify normality with numbers using the stat.desc() function. 
##Include a screen capture of the results produced.
stat.desc(acs_df)
stat.desc(acs_df)