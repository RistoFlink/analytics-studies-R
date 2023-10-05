rm(list=ls()) #clear global environment
dev.off() #clear plots

library(ggplot2)
#good to load ggplot2 BEFORE dplyr - due to masking some functions?
install.packages("dplyr")
library(dplyr)
install.packages("corrplot")
library(corrplot)

#let's work with two data sets
products = read.csv("FAER_lectures_Mac.csv", header = TRUE, sep = ";")
products = products[,1:5]

str(products)

details = read.csv("Details_Mac.csv", header = TRUE, sep = ";")
str(details)

#let's combine the data sets

my_data = inner_join(products, details)
str(my_data)
any(is.na(my_data))

#1. removing missing values
any(is.na(details))

details = details[complete.cases(details),]

#2. impute missing values
#index into those hourly wages that contain NA and which are assembled in Finland
#assign into those a mean of the other hourly wages in FI while ignoring NAs in calculations
my_data$Hourly.Wage[is.na(my_data$Hourly.Wage) & my_data$Assembly == "Finland"] =
 mean(my_data$Hourly.Wage[my_data$Assembly == "Finland"], na.rm = TRUE)
#same process but just for China
my_data$Hourly.Wage[is.na(my_data$Hourly.Wage) & my_data$Assembly == "China"] =
  mean(my_data$Hourly.Wage[my_data$Assembly == "China"], na.rm = TRUE)

#let's use dplyr

select(my_data, Product)
select(my_data, Cost.Parts, Cost)


#the pipe operator %>%

my_data %>% select(Product)

#filter function of dplyr
filter(my_data, Department == "Smart Home")
#could combine functions like this
select(filter(my_data, Department == "Smart Home"), Product, Department)

my_data %>% filter(Department == "Smart Home") %>% select(Product, Department)

#arrange function
arrange(my_data, Weekly.Units)
arrange(my_data, desc(Weekly.Units))


arrange(select(filter(my_data, Department == "Smart Home"), Product, Department, Weekly.Units), desc(Weekly.Units))

my_data %>%
  filter(Department == "Smart Home") %>%
  select(Product, Department, Weekly.Units) %>%
  arrange(desc(Weekly.Units))

#summarize function
summarize(my_data, avg_CostParts = mean(Cost.Parts))

#average cost for products in the Personal Products department
my_data %>%
  filter(Revenue > 500 & Department == "Personal Products") %>%
  summarize(avgCost = mean(Cost.Parts))
  
my_data %>%
  summarize(avgCost = mean(Cost.Parts))

#group-by function
my_data %>%
  group_by(Department) %>%
  summarize(avgCost = mean(Cost.Parts))

my_data %>%
  group_by(Parts, Assembly) %>%
  summarize(avgCost = mean(Cost.Parts))

#mutate function
mutate(my_data, Profit.Margin = (Revenue - Cost) / Revenue)

my_data %>%
  mutate(Profit.Margin = (Revenue - Cost) / Revenue) %>%
  group_by(Department) %>%
  summarize(avgMargin = mean(Profit.Margin), stdMargin = sd(Profit.Margin))

#correlations
my_numdata = my_data[, c(2, 3, 4, 8, 9)]

cor(my_numdata)

#visualizing correlations with corrplot
corrplot(cor(my_numdata), "number")
corrplot(cor(my_numdata), "ellipse")

#weekly exercise 4 stuff..
is.na(details)
!is.na(details)
details[!is.na(details)]
details[complete.cases(details),]
na.remove(details)

x <- c(2, 3, 4, 5, 6, NA, 4, 7, NA)
median(x, na.rm = TRUE)
median(x[!is.na(x)])

my_data %>% filter(Cost.Parts > 100) %>% group_by(Department) %>% summarise(avgRevenue = mean(Cost))

