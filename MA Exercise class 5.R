rm(list = ls())
dev.off()

install.packages("recommenderlab")
library(recommenderlab)
library(ggplot2)
library(dplyr)

#first example - simple rating matrix
mat = matrix(c(4, NA, NA, 5, 1, NA, NA, 5, NA, 5, NA, NA, 4, NA,
               NA, NA, NA, 2, 4, 4, NA, 2, NA, NA, 1, NA, 5, NA), byrow = TRUE, nrow = 4)

rownames(mat) = paste("user", 1:4, sep = "")
colnames(mat) = paste("item", 1:7, sep = "")

#basic analysis of the data
barplot(rowMeans(mat, na.rm = TRUE), ylab = "Average rating", main = "Average user rating")
barplot(colMeans(mat, na.rm = TRUE), ylab = "Average rating", main = "Average item rating")

#converting the data to a ratingmatrix (real-valued)
r = as(mat, "realRatingMatrix")

getRatings(r) #ratings as just vector
getRatingMatrix(r)

#Mean-centered data
rnorm = normalize(r, method = "center", row = TRUE)

getRatingMatrix(rnorm)

avgr = rowMeans(mat, na.rm = TRUE)

#illustrating the differences
image(r, main = "Raw ratings")
image(rnorm, main = "Mean-centered ratings")

#calculating cosine similarity
similarity(r, method = "cosine")
similarity(rnorm, method = "cosine")

#our simple user-based collaborative filter
ourCF = function(r, user, item, k = 2, sw = TRUE){
  #only consider rows of neighbours that have ratings for the item
  rrated = r[as.logical(hasRating(r[, item])), ]
  
  #calculate similarity-values
  simr = similarity(r[user, ], rrated, method = "cosine")
  
  #determine the similar user and ratings
  simusers = order(simr, decreasing = TRUE)[1:k]
  simratings = getRatings(rrated[simusers, item])
  simvalues = simr[simusers]
  
  #calculate user-based CF rating
  if(sw){
    myrating = weighted.mean(simratings, simvalues)
  } else{
    myrating = mean(simratings)
  }
  #returning the output
  return(myrating)
}

#testing our function
ourCF(r, user = 1, item = 6, k = 2, sw = FALSE)
ourCF(r, user = 1, item = 6, k = 2, sw = TRUE)

#returning back from mean-centered by adding the user average
ourCF(rnorm, user = 1, item = 6, k = 2, sw = FALSE) + avgr[1]
ourCF(rnorm, user = 1, item = 6, k = 2, sw = TRUE) + avgr[1]

#recommenderlab function for user-based collaborative filter UBCF
recommenderRegistry$get_entry("UBCF", datatype = "realRatingmatrix")

#setting up the model parameters
model_params = list(method = "cosine", nn = 2+1, sample = FALSE, normalize = "center", weighted = FALSE)

#using the UBCF
modelUB = Recommender(r, "UBCF", parameter = model_params)

#apply model to missing ratings
predUB = predict(modelUB, r, type = "ratings")

getRatingMatrix(predUB)

#alternative way
listUB = as(predUB, "list")
listUB$`0`

#using the item-based collaborative filter IBCF
recommenderRegistry$get_entry("IBCF", datatype = "realRatingMatrix")

#setting up the model parameters
model_params = list(method = "cosine", k = 2+1, normalize = "center")

#running the IBCF
modelIB = Recommender(r, "IBCF", parameter = model_params)

#making the predictions
predIB = predict(modelIB, r, type = "ratings")

#getting the output
getRatingMatrix(predIB)

#SECOND EXAMPLE - BIGGER DATASET
rm(list = ls())
data("Jester5k")
set.seed(1234)
r = sample(Jester5k, 1000)

rmat = as(r, "matrix")

#loading ProductTypes
products = read.csv("ProductTypes_Mac.csv", header = TRUE, sep = ";")
str(products)

str(rmat)
min(rmat, na.rm = TRUE)
max(rmat, na.rm = TRUE)
barplot(rowMeans(rmat, na.rm = TRUE), ylab = "Average rating", main = "Average user rating")
barplot(colMeans(rmat, na.rm = TRUE), ylab = "Average rating", main = "Average item rating")

products$AvgRating = colMeans(rmat, na.rm = TRUE)

ggplot(products, aes(x = Product, y = AvgRating, fill = Type)) +
  geom_bar(stat = "identity") +
  ggtitle("Average product ratings") +
  theme(axis.text.x = element_text(angle = 90))

typeavg = products %>%
  group_by(Type) %>%
  summarise(Avg = mean(AvgRating))

ggplot(typeavg, aes(x = Type, y = Avg, fill = Type)) +
  geom_bar(stat = "identity") +
  ggtitle("Average rating by item type")

#building the recommender system
model_params = list(method = "cosine", nn = 10+1, sample = FALSE, normalize = "center", weighted = TRUE)

#using the UBCF
modelUB = Recommender(r, "UBCF", parameter = model_params)

#apply model to missing ratings
predUB = predict(modelUB, r, type = "ratings")

getRatingMatrix(predUB["u20648", ])
rownames(predUB)
getRatingMatrix(predUB)

resultmat = as(getRatingMatrix(predUB["u20648",]), "matrix")
#order(resultmat, decreasing = TRUE)[1:3]

colnames(predUB)[order(resultmat, decreasing = TRUE)[1:3]]

#getting the top rated items instead of the ratings
predUB = predict(modelUB, r, type = "topN", n = 3)
topitems = as(predUB, "list")
topitems$`0`
topitems = as.data.frame(unlist(topitems))
topitems = as.data.frame(table(topitems))

colnames(topitems) = c("Product", "Frequency")

#combining this information with the data frame
productsagg = merge(products, topitems, by = "Product")

#making some visualizations
#which products are frequently recommended
ggplot(productsagg, aes(x = Product, y = Frequency, fill = Type)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequency of recommendations") +
  theme(axis.text.x = element_text(angle = 90))
  
productsagg %>%
  arrange(Frequency)

#comparing the average ratings with average frequencies
productsagg %>%
  filter(AvgRating >= 0) %>%
  ggplot(aes(x = AvgRating, y = Frequency, col = Type, label = Product)) +
  geom_point(size = 5) +
  geom_text(hjust = 0, nudge_x = 0.05) +
  ggtitle("Average rating and frequency of recommendations")

#how many products are recommended by category
productsagg %>%
  filter(AvgRating >= 0) %>%
  group_by(Type) %>%
  summarise(n(), Avg = mean(AvgRating, na.rm = TRUE))



