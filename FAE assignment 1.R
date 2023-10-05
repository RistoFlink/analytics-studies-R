df <- matrix(1:150, nrow = 10, ncol = 15, byrow = TRUE)
df[c(5,7,9),c(10,11,12)]

Debt <- 30000
yearCounter <- 0

while (Debt > 6000) {
  yearCounter = yearCounter + 1
  print(yearCounter)
  Debt = Debt * 0.83
  print(Debt)
}

mf <- function(a, b, c){
  result = a * b + c
  print(result)
}

mf(6, 4, 12)
mf(b = 4, a = 6, c = 12)


ggplot(diamonds, aes(x=carat, y=price)) + 
  geom_point() + 
  facet_wrap( ~ cut, ncol=3)+
  labs(title="Scatterplot", x="Carat", y="Price") 

ggplot(diamonds, aes(x=price, y=carat, color=cut)) + 
  geom_point() + 
  facet_wrap(~cut, ncol=3)+
  labs(title="Scatterplot", x="Price", y="Carat")


ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
  
  geom_point() + 
  
  facet_wrap( cut, ncol=3)+
  
  labs(title="Scatterplot", x="Carat", y="Price") 



ggplot(diamonds, aes(x=carat, y=price, color=cut)) +
  
  geom_point() + facet_wrap( ~ cut, ncol=3)+
  
  labs(title="Scatterplot", x="Carat", y="Price")

ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
  
  geom_density() + 
  
  facet_wrap( ~ cut, ncol=4)+
  
  labs(title="Scatterplot", x="Carat", y="Price") 

companyData = read.csv("dataCompany_Mac.csv", header = TRUE, sep = ";")
str(companyData)
any(is.na(companyData))
str(companyData$Product)
unique(companyData$Product)
which.max(companyData$Price)
companyData$Product[which.max(companyData$Price)]
ggplot(companyData, aes(x = Week, y = Price)) +
  geom_point () + facet_wrap(~Product, ncol = 5)
library("dplyr")

newData <- companyData %>% mutate(Marketing.Spending = case_when(
  Marketing.Expense < 5000 ~ "Low",
  Marketing.Expense >= 5000 & Marketing.Expense < 20000 ~ "Medium",
  Marketing.Expense >= 20000 ~ "High"
))
str(newData)
newData %>% count(Product[Marketing.Spending == "Medium"])


ggplot(newData, aes(x = Week, y = Marketing.Spending)) +
  geom_point() + facet_wrap(~Product, ncol = 5)

newData %>% 
  group_by(Product) %>%
  summarise(avg = mean(Marketing.Expense))
newData$Product[newData$Marketing.Expense]

ggplot(newData, aes(x = Week, y = Marketing.Expense)) + 
  geom_point() + facet_wrap(~Product, ncol = 5)

library("corrplot")
corrplot(cor(x = newData$Marketing.Expense, y = newData$Price, "ellipse"))

linRegModel = lm(Marketing.Expense~Price, data = newData)
summary(linRegModel)

ggplot(newData, aes(x = Price, y = Marketing.Expense)) +
  geom_point() + facet_wrap(~Product, ncol = 5)

cor.test(x = newData$Price, y = newData$Marketing.Expense)
cor.test(x = newData$Marketing.Expense, y = newData$Price)
cor.test(x = newData$Marketing.Expense, y = newData$Weekly.Units)

linRegModel = lm(Weekly.Units~Price+Cost+Marketing.Expense+Online.Views+Online.Purchases+Online.Rating, data = companyData)
summary(linRegModel)
corrplot(cor(companyData[, c(3, 4, 5, 6, 8, 9, 10, 11)]), "number")

newestData <- companyData %>% mutate(Online.Conversion = Online.Purchases / Online.Views)
ggplot(newestData, aes(x = Week, y = Online.Conversion)) +
  geom_point() + facet_wrap(~Product, ncol = 5)

ggplot(newestData, aes(x = Week, y = Price)) + 
  geom_point() + facet_wrap(~Product, ncol = 5)

ggplot(newestData, aes(x = Week, y = Online.Conversion)) +
  geom_point()
#part 3/C
rm(list=ls())
data("economics")
str(economics)
any(is.na(economics))
any(is.null(economics))
which(complete.cases(economics))
ggplot(economics, aes(x = date)) +
  geom_line(aes(y = psavert), color = "red") +
  geom_line(aes(y = uempmed), color = "blue")
  
newE <- economics %>% mutate(pop_levels = case_when(
  pop < 224896 ~ "Low",
  pop >= 224896 & pop <= 290291 ~ "Medium",
  pop > 290291 ~ "High"
))
ggplot(newE, aes(x = pop_levels)) +
  geom_bar()
newE %>% count(pop_levels == "High")  
ggplot(newE, aes(x = pop_levels, y = uempmed)) +
  geom_boxplot() +
  coord_flip()
corrplot(cor(economics[, 2:6]), "number")
ggplot(newE, aes(y = psavert, x = uempmed, color = pop_levels)) +
  geom_point() + facet_wrap(~pop_levels)

par(mfrow=c(3,1)) 

# loop through the grouping variable
for(i in unique(newE$pop_levels)) {
  corrplot(cor(newE[newE$pop_levels==i, -match("pop_levels", names(newE))]))
}

corrplot(cor(economics[, 2:6]), "number")
corrplot(newE(economics[, 2:6]))
lowPop = newE %>% filter(pop_levels == "Low")
medPop = newE %>% filter(pop_levels == "Medium")
hiPop = newE %>% filter(pop_levels == "High")
corrplot(filter(newE$pop_levels == "Low")[, 2:6])

newE %>%
  filter(newE$pop_levels == "Low") %>%
  corrplot(newE())

par(mfrow=c(3,1))
corrplot(cor(lowPop[, 2:6]), "number")
corrplot(cor(medPop[, 2:6]), "number")
corrplot(cor(hiPop[, 2:6]), "number")

popBefore = economics %>% filter(date <= 1999-12-31)
popAfter = economics %>% filter(date >= 2000-01-01)
popDifference = summarise(popAfter, avg = mean(pop)) - summarise(popBefore, avg2 = mean(pop))














