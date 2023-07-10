# Assignment: ASSIGNMENT 5
# Name: Smith, David
# Date: 2023-07-09

## Load the necessary packages for the script and set the default options as necessary.

# ** ggplot2 **
library(ggplot2)
theme_set(theme_minimal())

# ** pastecs **
library(pastecs)
options(scipen=100)
options(digits=2)

# ** moments **
library(moments)

# ** readxl **
library(readxl)

# ** dplyr **
library(dplyr)

# ** purr **
library(purrr)

# ** scales **
library(scales)

# ** tidyr **
library(tidyr)

# ** stringr **
library(stringr)


## Set the working directory to the root of your DSC 520 directory
setwd("C:\\GitLab-Projects\\Bellevue\\SMITH-DSC520")

## Load the `"data/week-7-housing.xlsx` dataset and get the initial details
housing_df <- read_excel("data/week-7-housing.xlsx")
summary(housing_df)

## Identify the baseline distribution information before any modifications.
## The post-modification validation may or may not be the same.

stat.desc(housing_df$square_feet_total_living)
# shapiro.test(housing_df$square_feet_total_living)
kurtosis(housing_df$square_feet_total_living)
jarque.test(housing_df$square_feet_total_living)

# Using the dplyr package, use the 6 different operations to analyze/transform the data - *GroupBy,
# *Summarize, *Mutate, *Filter, Select, and Arrange – Remember this isn’t just modifying data, you are
# learning about your data also – so play around and start to understand your dataset in more detail

# Use group_by and summarize to get the mean Sale Price by zip code.
dollar_format(prefix = "$", suffix = "", largest_with_cents = 100000000,big.mark = ",", negative_parens = TRUE)

housing_mean_by_zip5 <- housing_df %>%
  group_by(zip5) %>%
  summarize(Avg_Sale_Price = dollar(mean(`Sale Price`)))

# Use mutate to replace the NA values in ctyname column with the corresponding postalctyname values.
# The postalctyname contains the correct values, but without any NA occurrences in the column.
cty_na_df <- filter(housing_df,is.na(housing_df$ctyname))
head(cty_na_df)
cty_na_df <- cty_na_df %>% mutate(ctyname = ifelse (is.na(ctyname),postalctyn,ctyname))
head(cty_na_df)

# Filter rows with duplicate values. I know of the 3 rows with 13,540 square feet, but there
# may be others.
housing_df_dupes <- housing_df %>%
  add_count(square_feet_total_living,zip5) %>%
  filter(n>1) %>%
  distinct(zip5)
nrow(housing_df_dupes)

# Select, and Arrange – Remember this isn’t just modifying data
housing_nmbr_df <- housing_df %>% select_if(is.numeric)
housing_nmbr_df

# Using the purrr package – perform 2 functions on your dataset.  You could use zip_n, keep, discard,
# compact, etc.

housing_pluck <- housing_df %>% pluck(1)
head(housing_pluck)

housing_df %>% map_df(~(data.frame(n_distinct = n_distinct(.x),class = class(.x))),.id="variable")

# Using the rbind function to add the updated rows from the cty_na_df data frame.
housing_new = rbind(housing_df,cty_na_df)
nrow(housing_new)

# Currently both the NA and updated ctyname records exist in the dataframe, so we 
# have to drop the NA rows now.
housing_drop <- housing_new %>% drop_na(ctyname)
nrow(housing_drop)

# Use the cbind function to create a data frame with sale_price and sale_date without the space in the variable name.
sale_price = c(housing_df$`Sale Price`)
sale_date = c(housing_df$`Sale Date`)
housing_add_cols_df <- cbind(housing_df,sale_price,sale_date)

# Select, and Arrange – Remember this isn’t just modifying data
arrange(housing_add_cols_df,desc(sale_price))

# Split a string, then concatenate the results back together
addr_fullsplit <- str_split(string=housing_df$addr_full,pattern = " ")
addr_full_conc <- paste(addr_fullsplit,seperator=" ")

