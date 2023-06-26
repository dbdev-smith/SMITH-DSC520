# Assignment: American Community Survey
# Name: Smith, David
# Date: 2023-06-25

## Load the ggplot2 package
library(ggplot2)
theme_set(theme_minimal())

## Set the working directory to the root of your DSC 520 directory
setwd("F:\\GitLab-Projects\\Bellevue\\SMITH-DSC520")

## Load the `data/acs-14-1yr-s0201.csv` to
acs_df <- read.csv("data/acs-14-1yr-s0201.csv")

# What are the elements in your data (including the categories and data types)?
summary(acs_df)

# Please provide the output from the following functions: str(); nrow(); ncol()
str(acs_df)
nrow(acs_df)
ncol(acs_df)

## Create a histogram of the `HSDegree` variable using `geom_histogram()`
## Use 10 bins
hist1_HSDegree <- ggplot(acs_df, aes(HSDegree)) + geom_histogram(bins=10)+ ggtitle("High School Graduates") + xlab("High School Degree (%)") + ylab("Number of Counties")
hist1_HSDegree

# hist2_HSDegree <- hist1_HSDegree + stat_function(data = acs_df, fun = dnorm, args = list(mean = mean(acs_df$HSDegree, na.rm=TRUE), sd = sd(acs_df$HSDegree, na.rm=TRUE)),colour ="red")
hist2_HSDegree <- ggplot(acs_df, aes(x = log10(HSDegree),y = after_stat(density)))   +  geom_histogram(alpha =0.5)   +  geom_density(color = "red", linewidth = 1)
hist2_HSDegree


## Create a Probability Plot of the HSDegree variable.
prob_HSDegree <- ggplot(acs_df, aes(sample=HSDegree)) + stat_qq() + stat_qq_line() + labs(title = "HSDegree Normal Probabiolity Plot")
prob_HSDegree

# Now that you have looked at this data visually for normality, you will now quantify normality with numbers using the stat.desc() function.
library(pastecs)
options(scipen=100)
options(digits=2)
stat.desc(acs_df$HSDegree)

library(moments)
skew_HSDegree <- skewness(acs_df$HSDegree)
skew_HSDegree

kurtosis_HSDegree <- kurtosis(acs_df$HSDegree)
kurtosis_HSDegree

test_HSDegree <- jarque.test(acs_df$HSDegree)
test_HSDegree

