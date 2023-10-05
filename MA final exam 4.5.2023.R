library(ggplot2)
library(dplyr)
install.packages("corrplot")
library(corrplot)
install.packages("patchwork")
library(patchwork)
library(tidyr)
library(igraph)
library(igraphdata)
install.packages("ChannelAttribution")
library(ChannelAttribution)
install.packages("markovchain")
library(markovchain)
library(reshape2)
install.packages("NbClust")
library(NbClust)
library(purrr)
library(scales)
library(recommenderlab)
library(tm)
library(wordcloud)
library(tidytext)
library(ldatuning)
library(topicmodels)

rm(list = ls())
matrix_int = matrix(
  c(0, 1, 0, 0, 0,
    0, 0, 0, 1, 1,
    1, 0, 0, 0, 1,
    0, 0, 1, 0, 1,
    0, 0, 1, 1, 0),
  nrow=5, ncol = 5, byrow = TRUE)

graph_int = graph.adjacency(matrix_int, mode = "directed")
V(graph_int)$label = c("Y", "A", "M")
tkplot(graph_int, vertex.color = "green")

dIn = degree(graph_int, mode = "in")
dOut = degree(graph_int, mode = "out")

Mmat = t(matrix_int) / matrix(dOut, nrow = 5, ncol = 5, byrow = TRUE)

r = matrix(c(1/5, 1/5, 1/5, 1/5, 1/5), nrow = 5, ncol = 1)
rall  = r #save all r values over time
rchange = 1 #initial value for the change
n = 1
#run loop while differences between old and new r values is larger than 1%
while(rchange > 0.01){
  #new probablities
  r = Mmat %*% r #%*% is matrix multiplication
  #saving the new r
  rall = cbind(rall, r)
  #update differences between the old and the new r values
  rchange = max(abs(rall[, n] - r))
  #update the loop counter
  n = n + 1
}


#QUESTION 8
df_path <- data.frame(read.csv("Exam_Attrib1_mac.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE))
#preparing the data by converting it into an appropriate format
df_path = df_path %>% 
  arrange(client_id) %>%
  group_by(client_id) %>%
  summarise(path = paste(path, collapse = " > ")) %>%
  ungroup()
df_path[1, ]
#first markov
Mmdl = markov_model(df_path, var_path = "path", var_conv = "conv", var_value = "conv_val", order = 1, out_more = TRUE)
#second markov
Mmdl = markov_model(df_path, var_path = "path", var_conv = "conv", var_value = "conv_val", order = 2, out_more = TRUE)

#question 11
initdata = read.csv("data1_MAC.csv", header = TRUE, sep = ";")
str(initdata)
summary(initdata)

ggplot(initdata, aes(x = BrandLoyalty, y = PriceSensitivity)) +
  geom_point()

newdata = initdata[, 6:7]

data = data.frame(apply(newdata, 2, rescale, to = c(0, 1)))
kmeansmdl = kmeans(data, centers = 2, nstart = 100)
#utilizing the Elbow method to determine the optimal amount of clusters
tot_within_ss = map_dbl(1:10, function(k){
  kmeansmdl = kmeans(data, centers = k, nstart = 25)
  kmeansmdl$tot.withinss
})
#converting the variable to a data frame in order to use ggplot2
tot_within_ss = data.frame(tot_within_ss)
str(tot_within_ss)

ggplot(tot_within_ss, aes(x = 1:10, y = tot_within_ss)) +
  geom_line() +
  ggtitle("Elbow method")

set.seed(5)
kmeansmdl = kmeans(data, centers = 3, nstart = 100)
initdata$cluster = factor(kmeansmdl$cluster)
str(initdata)

initdata %>%
  group_by(cluster) %>%
  summarise_all(c("Avg" = mean, "Std" = sd))

sent1 = "IF YOU FIND IT HARD TO LAUGH AT YOURSELF, I WOULD BE HAPPY TO DO IT FOR YOU."
sent2 = "I REFUSE TO ANSWER THAT QUESTION ON THE GROUNDS THAT I DON’T KNOW THE ANSWER."

clean_corpus = function(corpus) {
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, content_transformer(tolower))
  return(corpus)
}
sent1 = removePunctuation(sent1)
sent2 = removePunctuation(sent2)

sent1 = tolower(sent1)
sent2 = tolower(sent2)

sent1 = stripWhitespace(sent1)
sent2 = stripWhitespace(sent2)

