# Assignment: Week 5 Assignment 
# Name: Zoucha, Michael
# Date: 2021-10-02

## Load the ggplot2 package
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(rlang)
theme_set(theme_minimal())

## Load housing data to data frame
housing_df <- read_excel("data/week-7-housing.xlsx")
colnames(housing_df)[2] <- "sale_price"

## 1. 6 different dplyr functions
## a. GroupBy / b. Summarize
##    Sale price per zip code
group_df_zip_mean <- dplyr::group_by(housing_df, zip5) %>% dplyr::summarize(avg_price = mean(sale_price))
group_df_zip_mean

## c. Mutate
##    Add new column price per square foot
mutated_df <- dplyr::mutate(housing_df, price_per_sq_ft = (square_feet_total_living / sale_price))

## c. Filter
##    Filter out null city values
filtered_df <- dplyr::filter(housing_df, !is.na(ctyname))

## d. Select
##    select just address information
selected_df <- dplyr::select(housing_df, addr_full:lat)

## e. Arrange
##    Arrange the rows by sale price, largest first
arranged_df <- dplyr::arrange(housing_df, desc(sale_price))

## 2. Use 2 functions from the purr package
## a. Map
##    Find the average property price using map()
select(housing_df, sale_price) %>% map(mean)

## b. Keep / Unique
##    Get unique city names
city_name <- unlist(select(housing_df, ctyname))
data.frame(unique(keep(city_name, ~ !is.na(.x))))

## 3. Use cbind and rbind on the dataset
## a. cbind
##    Add age of property column
cbind_df <- cbind(housing_df, 2021-housing_df$year_built)

## b. rbind
##    Recombine filtered data frames
filtered_df2 <- dplyr::filter(housing_df, is.na(ctyname))
rbind_df <- rbind(filtered_df, filtered_df2)

## 4. Split a string, then concatenate the results back together
string <- "Zoucha, Michael"
last_name <- str_split(string, ", ")[[1]][1]
first_name <- str_split(string, ", ")[[1]][2]
full_name <- paste(first_name, last_name, sep=" ")