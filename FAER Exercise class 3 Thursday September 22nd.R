rm(list=ls())

people <- read.csv("people_Mac.csv", header = TRUE, sep = ";")
str(people)


#simple plots
plot(people$savings, people$income,
     main = "Savings vs income",
     xlab = "Savings",
     ylab = "Income",
     pch = 16,
     col = "#c2ea53",
     panel.first = grid()
     )
points(mean(people$savings), mean(people$income), pch = 16, col = "red")

#histogram
hist(people$income, main = "Histogram of the income", xlab = "Income", col = "#5e23c2", panel.first = grid())

#boxplot
boxplot(people$income, main = "Boxplot of income", ylab = "Income")

boxplot(people$income[people$education == "None"],
        people$income[people$education == "High School"],
        people$income[people$education == "University"],
        main = "Boxplot of income", ylab = "Income",
        names = c("None", "High School", "University"),
        xlab = "Education",
        outline = TRUE)

# ggplot2
install.packages("ggplot2") #installing packages
library(ggplot2) #open the package for usage
#stopped the video at 50 minutes and 24 seconds

data("diamonds") #loading a dataset that came with ggplot
str(diamonds)

#let's use ggplot to make plots
#making a histogram for the continuous values
ggplot(diamonds, aes(x = price)) + 
  geom_histogram()

ggplot(diamonds, aes(x = carat)) + 
  geom_histogram()
#making bar plots for categorical values
ggplot(diamonds, aes(x = cut)) + 
  geom_bar()

ggplot(diamonds, aes(x = clarity)) + 
  geom_bar()

#let's look at relationships between variables
ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar()
dev.off() #removes plots

ggplot(diamonds, aes(x = price, y = carat)) +
  geom_point()

ggplot(diamonds, aes(x = price, y = clarity)) +
  geom_point() +
  geom_jitter()

ggplot(diamonds, aes(x = price, y = carat, color = clarity)) +
  geom_point()

#let's find a suitable diamond for our friend
ggplot(diamonds[diamonds$price < 2000,], aes(x = price, y = carat, color = clarity)) +
  geom_point()

ggplot(diamonds[diamonds$price < 2000 & diamonds$carat >= 0.6,], aes(x = price, y = carat, color = clarity)) +
  geom_point()

ggplot(diamonds[diamonds$price < 2000 & diamonds$carat >= 0.6 & diamonds$clarity > "VVS2",], aes(x = price, y = carat, color = clarity)) +
  geom_point()

ggplot(diamonds[diamonds$price < 2000 & diamonds$carat >= 0.6 & diamonds$clarity > "VVS2",], aes(x = price, y = carat, color = clarity)) +
  geom_point() +
  facet_wrap(~cut) +
  ggtitle("Diamond options") + 
  xlab("Price in USD") +
  ylab("Size in carat")

ggplot(diamonds[diamonds$carat > 2,], aes(x = price, y = carat)) +
  geom_point()

ggplot(diamonds, aes(x = clarity)) + 
  geom_bar() +
  geom_boxp

ggplot(diamonds[diamonds$price <= 2000 & diamonds$carat > 1,], aes(x = clarity)) +
  geom_bar()
str(diamonds)

par(mfrow = c(2, 2))
ggsave("test_plot.png")

ggplot(diamonds, aes(x = price, y = carat)) +
  geom_boxplot(outlier.shape = NA)
