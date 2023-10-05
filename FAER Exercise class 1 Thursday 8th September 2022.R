#simple calculation

5/7
4*3
5<3
92>5
3==3
4!=3

#how to define and assign variables

a <- 5
b <- 7
a + b
a - b
#using just = is fine too, whitespace doesn't matter

#let's make our first vector
myvector = c(1, 3, 4, 6, 5, 2)
myvector
myvector2 = 1:6
class(myvector)

#let's make a character vector
mywords =c("Risto", "Hannah", "Jude")
välitystuotetiimi = c("Anu", "Ari", "Lotta", "Petri", "Risto", "Teemu")
class(mywords)

#let's make a matrix

mymatrix = matrix(1:12, nrow = 3, ncol = 4, byrow =  TRUE)
mymtx = matrix(1:24, nrow = 4, ncol = 6, byrow =  TRUE)
mymtx2 = matrix(1:24, nrow = 6, ncol = 4, byrow =  TRUE)
class(mymatrix)

#let's make a dataframe
mydataframe = data.frame("Place" = myvector, "Name" = välitystuotetiimi)
str(mydataframe)

colnames(mydataframe)

help(str)
?str


#positional indexing
myvector
myvector[3] #the third element
myvector[-3] #everything but the third element
myvector[1:3]
myvector[c(1, 2, 4)]

mymatrix[3, ]
mymatrix[, 4]
mymatrix[, 5] #does not exist
mymatrix[3,c(3, 4)]
mymatrix[3, 3:4]
mymatrix[c(1, 2), c(3, 4)]
mymatrix[3, c(1, 4)]
#name indexing
mydataframe$Place
mydataframe$Name[5]
mydataframe[, 2]

mydataframe["Place"]

#logical indexing
myvector[myvector <= 3]
myvector[myvector > 3 & myvector < 6]
myvector[myvector < 3 | myvector > 6]
mydataframe$Place < 3
mymatrix[mymatrix[, 4] > 5, 4]
mymatrix[, 4] >= 8

mydataframe$Name != "Risto"
mydataframe$Name[mydataframe$Place <= 3]
mydataframe[mydataframe$Place <= 3, ]

#loading data from a data set

rm(list=ls()) #remove variables from the global environment

my_data = read.csv("FAER_lectures_Mac.csv", header = TRUE, sep = ";")
str(my_data)

my_data$Product
my_data$Weekly.Units
my_data$Product[my_data$Weekly.Units > 200]
my_data[my_data$Department == "Smart Home", ]
my_data$Revenue[my_data$Department == "Smart Home"]


mean(my_data$Revenue[my_data$Department == "Smart Home"])
mean(my_data$Revenue[my_data$Department == "Personal Products"])

min(my_data$Revenue[my_data$Department == "Smart Home"])
max(my_data$Revenue[my_data$Department == "Smart Home"])

my_data[my_data$Revenue > 500,]
my_data[my_data$Revenue > 500 & my_data$Department == "Smart Home",]


#moodle quiz tests
my_data[, 1:3]
my_data[c(1, 2, 3), ]
my_data[1:3, ]
my_data(1:3, )
x <- c(3)
x = c(3)
y = c(3)

my_data[my_data$Revenue >= 250, ]
my_data[my_data$Revenue >= 250, 3]
my_data[my_data$Revenue >= 250]
my_data$Revenue[my_data$Revenue >= 250, "Revenue"]
my_data$Revenue[my_data$Revenue >= 250]
my_data$Revenue[Product == "Laptop" | Product == "Tablet"]
my_data$Revenue[my_data$Product == "Laptop" | my_data$Product == "Tablet"]
my_data[my_data$Product == "Laptop" | my_data$Product == "Tablet", 3]

mymtx[4,6]
mymtx[c(4,6),]
mymtx[,c(4,6)]
mymtx[1,6, c(4)]
mymtx2[c(1, 6), 4]
mymtx2[1:6, 4]


