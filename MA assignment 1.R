library(ggplot2)
library(tidyr)
library(igraph)
library(igraphdata)

#only for mac
library(tcltk)

rm(list = ls())
my_variable = c(1, 3, 1, 2, 3, 2, 2, 2)
my_data = data.frame(my_variable)

library(ggplot2)
ggplot(my_data, aes(x=my_variable))+
  geom_bar()

#Q13
matrix_int = matrix(
  c(0, 1, 0, 0, 0,
    0, 0, 0, 0, 1,
    1, 1, 0, 0, 0,
    0, 0, 1, 0, 0,
    1, 0, 0, 1, 0),
  nrow=5, ncol = 5, byrow = TRUE)


graph_int = graph.adjacency(matrix_int, mode = "directed")
#V(graph_int)$label = c("Y", "A", "M")
#tkplot(graph_int, vertex.color = "green")

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

#Q14
matrix_int = matrix(
  c(0, 0, 1, 0, 0,
    1, 0, 0, 0, 0,
    0, 0, 0, 0, 1,
    0, 1, 0, 0, 1,
    0, 1, 1, 0, 0),
  nrow=5, ncol = 5, byrow = TRUE)


graph_int = graph.adjacency(matrix_int, mode = "directed")
#V(graph_int)$label = c("Y", "A", "M")
#tkplot(graph_int, vertex.color = "green")

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

#Q15
matrix_int = matrix(
  c(0, 0, 0, 0, 1,
    1, 0, 0, 1, 0,
    0, 1, 0, 0, 0,
    0, 0, 1, 0, 0,
    1, 1, 0, 1, 0),
  nrow=5, ncol = 5, byrow = TRUE)


graph_int = graph.adjacency(matrix_int, mode = "directed")
#V(graph_int)$label = c("Y", "A", "M")
#tkplot(graph_int, vertex.color = "green")

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

#Q16
#loading the data
data = read.csv("movies_Mac.csv", header = TRUE, sep = ";")
matrix_int = data.matrix(data, rownames.force = NA)
graph_int = graph.adjacency(matrix_int, mode = "directed")
#V(graph_int)$label = c("Y", "A", "M")
#tkplot(graph_int, vertex.color = "green")

dIn = degree(graph_int, mode = "in")
dOut = degree(graph_int, mode = "out")

Mmat = t(matrix_int) / matrix(dOut, nrow = 9, ncol = 9, byrow = TRUE)
r = matrix(c(1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9), nrow = 9, ncol = 1)


beta = 0.9
matrix_teleport = matrix(
  c(0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0,
    1/2, 1/2, 1/2, 1/2, 1/2, 1/2, 1/2, 1/2, 1/2,
    0, 0, 0, 0, 0, 0, 0, 0, 0,
    1/2, 1/2, 1/2, 1/2, 1/2, 1/2, 1/2, 1/2, 1/2),
  nrow = 9, ncol = 9, byrow = TRUE)
Amat = beta * Mmat + (1 - beta) * matrix_teleport


rall  = r #save all r values over time
rchange = 1 #initial value for the change
n = 1
#run loop while differences between old and new r values is larger than 1%
while(rchange > 0.01){
  #new probablities
  r = Amat %*% r #%*% is matrix multiplication
  #saving the new r
  rall = cbind(rall, r)
  #update differences between the old and the new r values
  rchange = max(abs(rall[, n] - r))
  #update the loop counter
  n = n + 1
}