# Assignment: ASSIGNMENT 4
# Name: Smith, David
# Date: 2023-07-02

## Load the ggplot2 package
library(ggplot2)
theme_set(theme_minimal())

## Set the working directory to the root of your DSC 520 directory
setwd("F:\\GitLab-Projects\\Bellevue\\SMITH-DSC520")


## Scores Scenario ##

## Load the `"data/scores.csv` to
scores_df <- read.csv("data/scores.csv")
summary(scores_df)

library(pastecs)
options(scipen=100)
options(digits=2)

regular_df <- subset(scores_df,scores_df$Section =="Regular")
ggplot(regular_df,aes(x=Score,y=Count)) + geom_point() + ggtitle("Regular Section: Scores vs. Count") + xlab("Scores") + ylab("Student Counts")

sports_df <- subset(scores_df,scores_df$Section =="Sports")
ggplot(sports_df,aes(x=Score,y=Count)) + geom_point() + ggtitle("Sports Section: Scores vs. Count") + xlab("Scores") + ylab("Student Counts")

stat.desc(sports_df$Score)
stat.desc(regular_df$Score)

stat.desc(sports_df$Count)
stat.desc(regular_df$Count)

shapiro.test(sports_df$Score)
shapiro.test(sports_df$Count)

shapiro.test(regular_df$Score)
shapiro.test(regular_df$Count)

library(moments)
kurtosis(regular_df$Score)
kurtosis(sports_df$Score)

jarque.test(regular_df$Score)
jarque.test(sports_df$Score)


## Analysis for Sports and Regular section scores:
## 1. The Regular section scored more points than the Sports Section based on the stat.desc() output for
##    sum. The standard deviation from the mean is also less.
##
## 2. Not every student in one section scored more than every student in the other section. Statistical
##    tendency in this context means based on the mean, the higher scores and lower number of students
##    could have an impact on the end result.
##
## 3. I'm guessing on this one, but I do think the total class sizes would be appropriate.The number can
##    be calculated but having it provided would increase confidence.


## Housing Scenario ##

library(readxl)
## Load the `"data/week-7-housing.xlsx` to
housing_df <- read_excel("data/week-7-housing.xlsx")
summary(housing_df)

## Use the apply function on a variable in the dataset
apply(housing_df[c('Sale Price', 'square_feet_total_living')], 2, mean)

## Identify any records where the zipcode is "NA"
zip_na <- sum(is.na(housing_df$zip5))
zip_na

## There are 0 records where zip5 is "NA". 

## Identify any records where the ctyname is "NA"
city_na <- sum(is.na(housing_df$ctyname))
city_na

## There are 6,078 where the city name field is NA but the zip5 is populated.

## Calculate the sum Sale Price associated with the rows where ctyname is 'NA' by zip5
## Use the aggregate function on a variable in my dataset.
## Split some data: Fixing the ctyname entries that are "NA"

cty_na_df <- subset(housing_df,is.na(housing_df$ctyname))
sum_sale_price <- aggregate((cty_na_df$`Sale Price`),list(cty_na_df$zip5),FUN=sum)

## Updating the 6,078 entries with the postal city name field.
mutate(cty_na_df,ctyname=postalctyn)

## Merging the updates back into housing_df
library(plyr)
housing2_df <- list(
  a = data.frame(x = housing_df),
  b = data.frame(x = cty_na_df)
)

housing_all_df <- join_all(housing2_df)

                    
zip5_ft <- ddply(housing_df,.(zip5),summarize,living_ft=mean(square_feet_total_living))
plot(living_ft ~ zip5, data = zip5_ft)


## There's one outlier with regards to the square_feet_total_living space. The same property is listed
## 3 times for 13,540 sq ft. This is the same property that has a sale price of $130,000 instead of
## $230,00.

library(pastecs)
options(scipen=100)
options(digits=2)

stat.desc(housing_df$square_feet_total_living)
shapiro.test(housing_df$square_feet_total_living)

## The p-value is less than .05 which is indicative of the data distribution not being normal. 

library(moments)
kurtosis(housing_df$square_feet_total_living)
jarque.test(housing_df$square_feet_total_living)


                    
## Created at least 2 new variables to remove the space out the existing Sale Date and Sale Price
## variables.
mutate(housing_df,sale_price=`Sale Price`)
mutate(housing_df,sale_date=`Sale Date`)

