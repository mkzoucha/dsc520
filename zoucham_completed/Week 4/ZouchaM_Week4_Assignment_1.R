# Assignment: Week 4 Assignment 1
# Name: Zoucha, Michael
# Date: 2021-09-19

## Load the ggplot2 package
library(ggplot2)
theme_set(theme_minimal())

## Load scores.csv to data frame
scores_df <- read.csv("data/scores.csv")

str(scores_df)
nrow(scores_df)
ncol(scores_df)

## 1. Each observational unit is a score of the class, the count, and the section
## 2. Categorical - Section
##    Quantitative - Count and Score

library(dplyr)

## 3.
sports_scores_df <- filter(scores_df, scores_df$Section == "Sports")
regular_scores_df <- filter(scores_df, scores_df$Section == "Regular")

## Add column to find total score in order to calculate mean score
sports_scores_df$total_score = sports_scores_df$Count * sports_scores_df$Score
regular_scores_df$total_score = regular_scores_df$Count * regular_scores_df$Score


sports_mean <- sum(sports_scores_df$total_score)/sum(sports_scores_df$Count)
regular_mean <- sum(regular_scores_df$total_score)/sum(regular_scores_df$Count)

sports_mean
regular_mean

## 4.
ggplot(scores_df, aes(fill=Section, y=Count, x=Score)) +
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Number of Students per Score by Section") + 
  xlab("Score") + ylab("Number of Students")


##   a. The regular seems to have higher scores because the distribution is skewed
##      further left than the sports scores. The sports scores are also slightly
##      skewed to the left, but not as far.
##   b. No, some sports section students scored higher than the regular section students
##      and some regular section students scored higher than the sports section students. 
##      Students in the regular class have the tendency of scoring
##      higher than the students in the sports section.
##   c. Whether or not the student played sports would influence the score distribution.