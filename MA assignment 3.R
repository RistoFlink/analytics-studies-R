rm(list = ls())
library(ggplot2)
library(recommenderlab)
library(dplyr)

mat = read.csv("rating_assignment3_Mac_2023.csv", header = TRUE, sep = ";")
mat = as(mat, "matrix")
str(mat)
head(mat)

r = as(mat, "realRatingMatrix")
#using the recommenderlab package
recommenderRegistry$get_entries() #seeing what methods exist
recommenderRegistry$get_entry("SVDF", datatype = "realRatingMatrix")

rnorm = normalize(r, method = "center", row = TRUE)
getRatingMatrix(rnorm)
avgr = rowMeans(mat, na.rm = TRUE)

image(r, main = "Raw ratings")
image(rnorm, main = "Mean-centered ratings")

#calculating cosine similarity
similarity(r, method = "cosine")
similarity(rnorm, method = "cosine")

#user-based collaborative filter
#Q8
model_params = list(method = "cosine", nn = 3+1, sample = FALSE, normalize = "center", weighted = FALSE)
#using the UBCF
modelUB = Recommender(r, "UBCF", parameter = model_params)

#apply model to missing ratings
predUB = predict(modelUB, r, type = "ratings")

getRatingMatrix(predUB)

listUB = as(predUB, "list")
listUB$`1`

#Q9
model_params = list(method = "cosine", nn = 4+1, sample = FALSE, normalize = "center", weighted = TRUE)
#using the UBCF
modelUB = Recommender(r, "UBCF", parameter = model_params)

#apply model to missing ratings
predUB = predict(modelUB, r, type = "ratings")

getRatingMatrix(predUB)

listUB = as(predUB, "list")
listUB$`5`

#Q10
model_params = list(method = "cosine", nn = 4+1, sample = FALSE, normalize = "center", weighted = FALSE)
#using the UBCF
modelUB = Recommender(r, "UBCF", parameter = model_params)

#apply model to missing ratings
predUB = predict(modelUB, r, type = "ratings")

getRatingMatrix(predUB)

listUB = as(predUB, "list")
listUB$`1`

#Q11
model_params = list(method = "cosine", nn = 2+1, sample = FALSE, normalize = "center", weighted = FALSE)
#using the UBCF
modelUB = Recommender(r, "UBCF", parameter = model_params)

#apply model to missing ratings
predUB = predict(modelUB, r, type = "ratings")

getRatingMatrix(predUB)

listUB = as(predUB, "list")
listUB$`3`


getRatingMatrix(r)


