library(markovchain)
library(matlab)
library(Rcpp)
library(dplyr)
library(ggplot2)
library(wesanderson)
library(reshape2)
library(ChannelAttribution)

rm(list = ls())
#loading the attribution data
df_path <- data.frame(read.csv("Attribution_assign_path_Mac.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE))

#loading the data forconversions and sales
df_conv <- data.frame(read.csv("Attribution_assign_conv_Mac.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE))
head(df_path)
head(df_conv)
str(df_path)
str(df_conv)

df_path = df_path %>% 
  arrange(client_id, date) %>%
  group_by(client_id) %>%
  summarise(path = paste(channel, collapse = " > ")) %>%
  ungroup()

df_path[1, ]

#merging our path and conversion data
df_attrib = merge(df_path, df_conv, by = "client_id")
str(df_attrib)

#making our first attribution model
Hmdl = heuristic_models(df_attrib, var_path = "path", var_conv = "conv", var_value = "conv_val")

#making the Markov chain model
Mmdl = markov_model(df_attrib, var_path = "path", var_conv = "conv", var_value = "conv_val", order = 1, out_more = TRUE)
Mmdl

#merging the results and compare them
results1 = merge(Hmdl, Mmdl$result, by = "channel_name")
head(results1)


vars_to_keep_c = c("channel_name", "first_touch_conversions",
                   "last_touch_conversions", "linear_touch_conversions", "total_conversions")
vars_to_keep_v = c("channel_name", "first_touch_value",
                   "last_touch_value", "linear_touch_value", "total_conversion_value")

results1_conv = results1[vars_to_keep_c]
results1_val = results1[vars_to_keep_v]

#making the conversions plot
conversions = melt(results1_conv, id = "channel_name")
str(conversions)

po_conv = ggplot(conversions, aes(x = channel_name, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Attributed conversions") +
  labs(x = "Channel names", y = "Conversions") +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "Attribution model",
                      labels = c("First touch", "Last touch", "Linear touch", "1st order Markov"))

po_conv

value = melt(results1_val, id = "channel_name")

po_val = ggplot(value, aes(x = channel_name, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Attributed value") +
  labs(x = "Channel names", y = "Value in Eur") +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "Attribution model",
                      labels = c("First touch", "Last touch", "Linear touch", "1st order Markov"))

#Q15
library(NbClust)
library(scales)
library(purrr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(corrplot)

initdata = data.frame(read.csv("customer_Mac.csv", header = TRUE, sep = ";"))
str(initdata)
max(round(cor(initdata), digits = 2))
corrplot(cor(initdata), method = "number", type = "lower")

max(initdata$CREDIT_LIMIT)
min(initdata$BALANCE)
initdata %>%
  arrange(BALANCE)

#find the row with the smallest balance
min_balance_row = which.min(initdata$BALANCE)
min_balance_rows = which(initdata$BALANCE == min(initdata$BALANCE))
#get the CREDIT_LIMIT value from that row
credit_limit = initdata$CREDIT_LIMIT[min_balance_row]

# Print the result
cat("The CREDIT_LIMIT of the individual with the smallest BALANCE is:", credit_limit)
initdata[1481, ]

#balance of the individual with the smallest overall_purchases
min_purchase_rows = which(initdata$Overall_PURCHASES == min(initdata$Overall_PURCHASES))
min_purchase_row = which.min(initdata$Overall_PURCHASES)
balance = initdata$BALANCE[min_purchase_row]
initdata[2, ]

#questions 19 and 20 - scaling etc
#question 19 - 3 clusters, nstart 1, variables 1, 5, 8
data = data.frame(apply(initdata, 2, rescale, to = c(0, 1)))
summary(data)

distmat = dist(data, method = "euclidean")
set.seed(5)
#selecting only variables 1, 5, 8
data = data[c(1, 5, 8)]
str(data)
mdl = kmeans(data, centers = 3, nstart = 1)
mdl$cluster
mdl$centers
mdl$withinss
mdl$tot.withinss

initdata$cluster = mdl$cluster

analysed_data = initdata[c(1, 5, 8, 9)] %>%
  group_by(cluster) %>%
  summarise_all(c("Avg" = mean, "Std" = sd))
View(analysed_data)
initdata %>%
  count(cluster)

#question 20 3 centers nstart1 variables 1, 2, 5
initdata = data.frame(read.csv("customer_Mac.csv", header = TRUE, sep = ";"))
data = data.frame(apply(initdata, 2, rescale, to = c(0, 1)))
summary(data)

distmat = dist(data, method = "euclidean")
set.seed(5)
#selecting only variables 1, 2, 5
data = data[c(1, 2, 5)]
str(data)
mdl = kmeans(data, centers = 3, nstart = 1)
mdl$cluster
mdl$centers
mdl$withinss
mdl$tot.withinss

initdata$cluster = mdl$cluster

analysed_data = initdata[c(1, 2, 5, 9)] %>%
  group_by(cluster) %>%
  summarise_all(c("Avg" = mean, "Std" = sd))
View(analysed_data)
initdata %>%
  count(cluster)
