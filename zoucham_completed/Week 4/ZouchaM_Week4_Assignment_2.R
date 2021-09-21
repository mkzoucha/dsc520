# Assignment: Week 4 Assignment 2
# Name: Zoucha, Michael
# Date: 2021-09-19

## Load the ggplot2 package
library(ggplot2)
library(readxl)
library(plyr)
library(Rcpp)
theme_set(theme_minimal())

## Load housing data to data frame
housing_df <- read_excel("data/week-7-housing.xlsx")

## a.
average_sale_price <- apply(housing_df[2], 2, mean)
average_sale_price
colnames(housing_df)[2] <- "sale_price"

## b.
average_rooms <- mean(housing_df$bedrooms)
average_full_bath <- mean(housing_df$bath_full_count)
average_half_bath <- mean(housing_df$bath_half_count)
average_three_qtr_bath <- mean(housing_df$bath_3qtr_count)

average_rooms
average_full_bath
average_half_bath
average_three_qtr_bath

## c.
housing_df <- ddply(housing_df, .(addr_full), transform, price_per_sqft=sale_price/square_feet_total_living)

## d.
## Lot Size vs Price
ggplot(housing_df, aes(x=sq_ft_lot, y=sale_price/100000)) + geom_point() + 
  ggtitle("Distribution of Sale Price") + 
  xlab("Lot Size (sq ft)") + ylab("Sale Price")

## House Size vs Price
ggplot(housing_df, aes(x=square_feet_total_living, y=sale_price/100000)) + geom_point() + 
  ggtitle("Distribution of Sale Price") + 
  xlab("House Size (sq ft)") + ylab("Sale Price")

## Sale Price Distribution
ggplot(housing_df, aes(sale_price)) + geom_histogram()

## Living Space Distribution
ggplot(housing_df, aes(square_feet_total_living)) + geom_histogram()

## Lot Size Distribution
ggplot(housing_df, aes(sq_ft_lot)) + geom_histogram()

## Price per sq ft Distribution
ggplot(housing_df, aes(price_per_sqft)) + geom_histogram()

## e. There are multiple outliers when comparing variables together (such as small houses/lots that are significantly
##    more expensive or large houses/lots that are significantly cheaper) as well as individual variables, including lot size
##    house size. The most extreme outlier I found was in price_per_sq_ft

## f.
## Created price_per_sqft above
## Binary variable in_city
housing_df <- ddply(housing_df, .(addr_full), transform, in_city=ifelse(is.na(ctyname), 0, 1))
## Building age
housing_df <- ddply(housing_df, .(addr_full), transform, age=2021-year_built)