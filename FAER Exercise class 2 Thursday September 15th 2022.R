rm(list=ls()) #clearing the global environment - good to do when starting a new file

#sorting a vector
my_numbers <- c(10, 60, 20, 50, 30, 40)
my_people <- c("Eero", "Anna", "Floora", "Charlot", "Dimitri", "Bonni")


sort(my_numbers)
#if you want to permanently store the sort, assign it to the old value or create a new variable and assign it
sort(my_people, decreasing = TRUE)
help(sort)
sort(-my_numbers) #turn the numbers negative and sorts them in a new way with -60 being smallest
order(my_numbers)
order(-my_numbers)

#determining and finding the minimum and maximum value
max(my_numbers)
#finding in what index was the largest number
which.max(my_numbers)
#what person ran the longest amount
my_people[which.max(my_numbers)]
max(my_people)
#finding in what index was the lowest number
min(my_numbers)
#what person ran the least
my_people[which.min(my_numbers)]
min(my_people)

#let's make our first user-defined functions
divideby4 <- function(x){
  result <- x / 4
  return(result)
}

divideby4(12)
divideby4(333)
divideby4(c(12, 6, 3, 1, 33))
divideby4(3:11)

divideby4(my_numbers)

#default values in user-defined functions

divideby <- function(x, y = 4){
  result = x / y
  return(result)
}

divideby(12, 3)
divideby(my_numbers, 5.25)

div_result <- divideby(20, 5)

# learning about loops
# for-loop
for(i in 1:6){
  print(i^2)
}
#using a for loop to index a vector
for(i in 1:6){
  print(my_numbers[i])
}

my_money <- 1000
for(i in 1:12){
  my_money = my_money * 1.01 + 50
  print(my_money)
}

#pre-processing
#computationally faster to define a storage in the beginning instead of always checking if it is long enough before adding things
resultvec <- vector(mode = "numeric", 12)
my_money <- 1000
for(i in 1:12){
  my_money = my_money * 1.01 + 50
  print(my_money)
  resultvec[i] = my_money
}

#while-loop - run a certain command as long as a condition is true
years <- 0
my_investment <- 500
while(my_investment <= 1500){
  my_investment = my_investment * 1.05
  print(my_investment)
  year = year + 1
  print()
}

#if else statements
distrun <- 8

if(distrun < 5){
  print("Didn't run enough!")
}else if(distrun > 10){
  print("Ran too much!")
}else{
  print("Ran enough!")
}

# let's load our data
my_data = read.csv("FAER_lectures_Mac.csv", header = TRUE, sep = ";")

my_data[order(-my_data$Revenue),]
my_data[which.max(my_data$Revenue),]
my_data[which.min(my_data$Revenue),]

my_profit_margin <- function(revenue, cost){
  profit_margin <- (revenue - cost) / revenue
  return(profit_margin)
}
my_data$Margin <- my_profit_margin(my_data$Revenue, my_data$Cost)

my_data[order(-my_data$Margin),]

#weekly quiz numero dos
for(i in c(3:8)){
  fvalue = 5 + i^2
  print(result)
}

Y <- c(3, 5, -2, 7, -11, -4)
ifelse(Y>-4, "Normal", "Abnormal")
ifelse(Y<6, "Abnormal", "Abnormal")
ifelse(Y>0, "Normal", "Abnormal")
ifelse(Y>-5 | Y < 6, "Normal", "Abnormal")
ifelse(Y>-3 & Y < 6, "Normal", "Abnormal")
ifelse(Y>-3 | Y < 6, "Normal", "Abnormal")
ifelse(Y>-3 & Y < 6, "ABnormal", "Normal")









