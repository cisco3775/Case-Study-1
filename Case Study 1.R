library(dplyr)
library(repmis)
library(ggplot2)
library(scales)
setwd("C:/Users/Ariel/Desktop/01 DATA SCIENCE/DDS/DATA File")

#read in csv file "getdata_data_EDSTATS_Country"
stats.data <- read.csv("getdata_data_EDSTATS_Country.csv")

#read in csv file "getdata_data_GDP" skipping the first three rows
gdp.data <- read.csv("getdata_data_GDP.csv", skip=3, header=TRUE)

##Tidying stats.data file
#deleting the columns in stats.data that are not used in the analysis
stats.data <- stats.data[-c(2,4:31)]
#number of missing values in the Income Group column of stats.data
sum(stats.data$Income.Group=="")   
#deleting the rows with missing values in then Income Group
stats.data <- stats.data[!(stats.data$Income.Group==""),]


##Tidying gdp.data file
#deleting columns in gdp.data not used in the analysis
gdp.data <- gdp.data[-c(3,6:10)]
#deleting blank rows not used in the analysis
gdp.data <- gdp.data[-c(1, 192:327), ]
#renaming the column headers in gdp.data
gdp.data <- rename(gdp.data, CountryCode = X, Country = Economy, GDP.in.Millions.USD = US.dollars.)
#convering Ranking column from factor to numeric 
gdp.data$Ranking <- as.numeric(as.character(gdp.data$Ranking))
#removing the commas in the GDP.in.Millions.USD column
gdp.data$GDP.in.Millions.USD <- gsub(",", "", gdp.data$GDP.in.Millions.USD)
#convering GDP.in.Millions.USD column from character to numeric
gdp.data$GDP.in.Millions.USD <- as.numeric(as.character(gdp.data$GDP.in.Millions.USD))


#merging stats.data and gdp.data by CountryCode column
gdp.stats.merged = merge(stats.data, gdp.data, by="CountryCode")

#sorting gdp.stats.merged in ascending order by GDP
gdp.stats.merged <- gdp.stats.merged[order(gdp.stats.merged$GDP.in.Millions.USD),]

#grouping gdp.stats.merged into groups by Income.Group
by_Income.Group <- group_by(gdp.stats.merged, Income.Group)
#obtaining the average GDP ranking of each group
summarise(by_Income.Group,mean = mean(Ranking))


#setup ggplot with data frame gdp.stats.merged
g <- ggplot(gdp.stats.merged, aes(Country, GDP.in.Millions.USD))
#adding color by Income.Group + converting from scientific notation to commas + listing the country names vertically on x-axis + decreasing font size on x-axis
g + geom_point(aes(color = Income.Group)) + scale_y_continuous(labels = comma) + theme(axis.text.x = element_text(angle = 90, size=1, hjust=1)) 


#summary statistics for GDP by income groups 
summarise(by_Income.Group,mean = mean(GDP.in.Millions.USD), standard.dev = sd(GDP.in.Millions.USD), 
          variance=var(GDP.in.Millions.USD), min=min(GDP.in.Millions.USD), max=max(GDP.in.Millions.USD), 
          median=median(GDP.in.Millions.USD))

#cutting Ranking into 5 quantile groups
Quantile <- cut(gdp.stats.merged$Ranking, breaks=5,labels = FALSE, include.lowest = TRUE, right = FALSE)
#creating new object with a new column, Quantile
quantile.gdp.stats.merged <- mutate(gdp.stats.merged, Quantile)
#converting Quantile from integer to numeric
quantile.gdp.stats.merged$Quantile <- as.numeric(as.integer(quantile.gdp.stats.merged$Quantile))
#making a table Quantile vs. Income.Group
table(quantile.gdp.stats.merged$Quantile, quantile.gdp.stats.merged$Income.Group)



