rm(list = ls())
library(ggplot2)
library(recommenderlab)
library(dplyr)

#simple example of a latent factor
mat = matrix(c(0, 0, 0, 1, 1, 1,
               0, 0, 0, 1, 1, 1,
               0, 0, 0, 1, 1, 1,
               -1, -1, -1, 1, 1, 1,
               1, 1, 1, -1, -1, -1,
               1, 1, 1, -1, 1, -1,
               1, 1, 1, -1, -1, -1), byrow = TRUE, nrow = 7)

rownames(mat) = paste("user", 1:7, sep = "")
colnames(mat) = paste("item", 1:6, sep = "")

r = as(mat, "realRatingMatrix")

#using the recommenderlab package
recommenderRegistry$get_entries() #seeing what methods exist
recommenderRegistry$get_entry("SVDF", datatype = "realRatingMatrix")

#setting the parameters for the model
model_params = list(k = 2, gamma = 0, lambda = 0.1, normalize = "center") #lambda = step size

model_0 = Recommender(r, "SVDF", parameter = model_params)

Qmat = model_0@model[["svd"]][["U"]] #using @ to access a model
Pmat = model_0@model[["svd"]][["V"]]

round(Qmat, 1)
round(Pmat, 1)

round(Qmat %*%t(Pmat), 4)

#using the MovieLense dataset
rm(list = ls())
data("MovieLense")

r  = MovieLense

MovieLense@data@Dimnames
 
#initial analysis
rmat = as(getRatingMatrix(r), "matrix")
rmat = data.frame(rmat)
str(rmat)
rmat[rmat == 0] = NA

#calculating the user ratings and other additional columns
MovieLenseUser$avgr = round(rowMeans(rmat, na.rm = TRUE), 2)

#average user ratings
ggplot(MovieLenseUser, aes(x = avgr)) +
  geom_histogram(fill = "grey", colour = "black") +
  ggtitle("Average user ratings")

#ratings per sex
MovieLenseUser %>%
  group_by(sex) %>%
  summarize(avgs = mean(avgr)) %>%
  ggplot(aes(x = sex, y = avgs, fill = sex)) +
  geom_bar(stat = "identity") +
  ggtitle("Average rating by sex")

#ratings per occupation
Moviesum = MovieLenseUser %>%
  group_by(occupation,) %>%
  summarize(avgo = mean(avgr), avga = mean(age))

ggplot(Moviesum, aes(x = occupation, y = avgo, fill = occupation)) +
  geom_bar(stat = "identity") +
  ggtitle("Average rating by occuption") +
  theme(axis.text.x = element_text(angle = 90))

#ratings per occupation and age
ggplot(Moviesum, aes(x = avga, y = avgo, col = occupation, label = occupation)) +
  geom_point(size = 5) +
  geom_text(hjust = 0, nudge_x = 1)

#analyzing the movie meta data
str(MovieLenseMeta)

#calculating the average rating
MovieLenseMeta$avgr = round(colMeans(rmat, na.rm = TRUE), 2)

ggplot(MovieLenseMeta, aes(x = avgr)) +
  geom_histogram(fill = "grey", colour = "black") +
  ggtitle("Average movie rating")

#aberage movie ratings over time
ggplot(MovieLenseMeta, aes(x = year, y = avgr)) +
  geom_point() +
  ggtitle("Average movie ratings over time")

#which movies were rated the most
MovieLenseMeta %>%
  select(title, year, avgr) %>%
  group_by(year) %>%
  summarize(n())

#cross-validation
e = evaluationScheme(r, method = "split", train = 0.9, given = 10, k = 1)
min(rowCounts(r))

#looking at the data from the split
getData(e, "train")
getData(e, "known") #known part of the test data
getData(e, "unknown") #not known part of the test data to predict

#model parameters for the latent factor model
model_params = list(k = 2, gamma = 0.015, lambda = 0.001, normalize = "center")

model_0 = Recommender(getData(e, "train"), "SVDF", parameter = model_params)

model_1 = Recommender(getData(e, "train"), "UBCF", param = list(normalize = "center", method = "cosine"))

model_2 = Recommender(getData(e, "train"), "IBCF", param = list(normalize = "center", method = "cosine"))

#short look at the latent concepts for users and items
Qmat = model_0@model[["svd"]][["U"]] #using @ to access a model
Pmat = model_0@model[["svd"]][["V"]]

#make the predictions using the models we traineds
pred_0 = predict(model_0, getData(e, "known"), type = "ratings")
pred_1 = predict(model_1, getData(e, "known"), type = "ratings")
pred_2 = predict(model_2, getData(e, "known"), type = "ratings")

#calculating selected error measures
error = rbind(LatFac = calcPredictionAccuracy(pred_0, getData(e, "unknown")),
              UBCF = calcPredictionAccuracy(pred_1, getData(e, "unknown")),
              IBCF = calcPredictionAccuracy(pred_2, getData(e, "unknown")))

#plotting the comparison of methods
barplot(error, main = "Comparison of recommended systems", beside = TRUE, xlab = "Metrics", ylab = "Error",
        col = c("pink", "darkorange", "green"))
legend("topleft", legend = c("Latent factor", "UCBF", "ICBF"), fill = c("pink", "darkorange", "green"), bty = "n")










