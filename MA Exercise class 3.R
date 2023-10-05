install.packages("markovchain", dependencies=TRUE)
install.packages("matlab")
install.packages("Rcpp")
install.packages("wesanderson")
install.packages("reshape2")
install.packages("ChannelAttribution")
library(markovchain)
library(matlab)
library(Rcpp)
library(dplyr)
library(ggplot2)
library(wesanderson)
library(reshape2)
library(ChannelAttribution)


#clear environment
rm(list=ls())

#loading the attribution data
df_path <- data.frame(read.csv("Attribution_exercise_path_MAC.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE))

#loading the data forconversions and sales
df_conv <- data.frame(read.csv("Attribution_exercise_conv_MAC.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE))
head(df_path)
head(df_conv)
str(df_path)
str(df_conv)

#preparing the data by converting it into an appropriate format
df_path = df_path %>% 
  arrange(client_id, date) %>%
  group_by(client_id) %>%
  summarise(path = paste(channel, collapse = " > ")) %>%
  ungroup()


#looking at the first customer and their path
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

#preparing to make a barplot
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

po_val

#working with the survey data
survey = data.frame(read.csv("Survey_attribution_Mac.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE))
str(survey)

#Q3_0_GROUP: Need
#Q18_0_GROUP: Search
#Q19_0_GROUP: Choice

survey$Need = gsub(",", " > ", survey$Q3_0_GROUP)
survey$Search = gsub(",", " > ", survey$Q18_0_GROUP)
survey$Choice = gsub(",", " > ", survey$Q19_0_GROUP)

#creating the full conversion path
survey$path = do.call(paste, c(survey[, c("Need", "Search", "Choice")], sep = " > "))

survey$path[1] #issue with >  >
survey$path = gsub(">  >", " > ", survey$path)

#adding value and converesion information
survey$value = survey$Q14_1
survey$convert = rep(1, dim(survey)[1])

#only keeping relevant information for attribution modeling
df_attrib = survey[c("Need", "Search", "Choice", "path", "value", "convert")]
str(attrib)


#making our first attribution model
Hmdl = heuristic_models(df_attrib, var_path = "path", var_conv = "convert", var_value = "value")

#making the Markov chain model
Mmdl = markov_model(df_attrib, var_path = "path", var_conv = "convert", var_value = "value", order = 1, out_more = TRUE)
Mmdl

#analysing the transition matrix
plot_transition = Mmdl$transition_matrix
str(plot_transition)

#channel names from results
levels = as.character(Mmdl$result$channel_name)
plot_transition[1, ]

#replace numbers with channel names

plot_transition$channel_from[plot_transition$channel_from == "1"] = levels[1]

for (i in 1:length(levels)){
  plot_transition$channel_from[plot_transition$channel_from == i] = levels[i]
  plot_transition$channel_to[plot_transition$channel_to == i] = levels[i]
}

#making the heat map of the transition probabilities
cols = wes_palette("Zissou1", 100, type = "continuous")
po_heat = ggplot(plot_transition, aes(x = channel_from, y = channel_to, fill = transition_probability)) +
  geom_tile() + 
  labs(x = "Channel to", y = "Channel from", fill = "Transition Probability") +
  ggtitle("Transition heat map") +
  geom_text(aes(label = round(transition_probability, 2))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_fill_gradientn(colours = cols)









