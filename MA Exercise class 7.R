install.packages("tm")
install.packages("wordcloud")
install.packages("tidytext")
install.packages("textdata")
install.packages("ldatuning")
install.packages("topicmodels")
install.packages("wesanderson")
#loaded all the required packages by clicking them on.. got lazy :)

rm(list = ls())

#import the texts
#different OS have differences in how they deal with encoding (UTF-8 etc.)
hubspot = read.csv("HubSpot_tweets_MAC.csv", sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")
str(hubspot)
hubspot$text[4]

hubspot = read.csv("HubSpot_tweets_MAC.csv", sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
hubspot = read.csv("HubSpot_tweets_MAC.csv", sep = ";", stringsAsFactors = FALSE, encoding = "automatic")
hubspot = read.csv("HubSpot_tweets_MAC.csv", sep = ";", stringsAsFactors = FALSE, encoding = "MacRoman")
hubspot = read.csv("HubSpot_tweets_MAC.csv", sep = ";", stringsAsFactors = FALSE, encoding = "UTF-16")
hubspot = read.csv("HubSpot_tweets_MAC.csv", sep = ";", stringsAsFactors = FALSE, encoding = "x-mac-roman")

#could not get the Mac-version to work so tried it with the regular file
hubspot = read.csv("HubSpot_tweets.csv", sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
marketo = read.csv("Marketo_tweets.csv", sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
str(marketo)

#pre-processing the text - creating the corpus
hubspot.vec = VectorSource(hubspot$text)
str(hubspot.vec)

textcorpus = VCorpus(hubspot.vec)
inspect(textcorpus)
textcorpus[[82]]$content #accessing specific objects

#creating a custom function for pre-processing
clean_corpus = function(corpus) {
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, content_transformer(stemDocument))
  corpus = tm_map(corpus, removeWords, c(stopwords("en")))
  return(corpus)
}
stopwords("fi")
textcorpus2 = clean_corpus(textcorpus)
#comparing the two different corpora
textcorpus[[4]]$content
textcorpus2[[4]]$content

#simple wordcloud
set.seed(512)
wordcloud(textcorpus2, max.words = 100, colors ="darkcyan", random.order = FALSE)

#starting over in order to compare Marketo and Hubspot
hubspot.vec = paste(hubspot$text, collapse = " ")
marketo.vec = paste(marketo$text, collapse = " ")

both = c(hubspot.vec, marketo.vec)
str(both)

comparison_corpus = VCorpus(VectorSource(both))
inspect(comparison_corpus)
comparison_corpus$meta

comparison_corpus = clean_corpus(comparison_corpus)

#creating a term document matrix
comparison_tdm = TermDocumentMatrix(comparison_corpus)
inspect(comparison_tdm)

#storing this as a matrix
ctdmm = as.matrix(comparison_tdm)
colnames(ctdmm) = c("Hubspot", "Marketo")
head(ctdmm)

#commonality cloud
set.seed(512)
commonality.cloud(ctdmm, random.order = FALSE, max.words = 100, colors = wes_palette("Zissou1", n = 5, type = "discrete"))

#comparison cloud
set.seed(512)
comparison.cloud(ctdmm, title.size = 3, max.words = 100)

#getting the sentiments
lex = get_sentiments("afinn")
lex2 = get_sentiments("bing")
lex3 = get_sentiments("nrc")

head(ctdmm)
#creating a tibble and matching column names with lex
comparison_sa = tibble(word  = row.names(ctdmm), hubspot_frq = ctdmm[, 1], marketo_frq = ctdmm[, 2])
str(comparison_sa)

#combining comparison_sa with lex
#could be done in steps
comparison_afinn = inner_join(comparison_sa, lex, by = "word")

#using pipe operators instead %>%
comparison_afinn = comparison_sa %>%
  inner_join(lex, by = "word") %>%
  mutate(sentiment_hub = hubspot_frq * value, sentiment_mar = marketo_frq * value)

hist(comparison_afinn$sentiment_hub)
hist(comparison_afinn$sentiment_mar)

#calculating the overall sentiments
sean = comparison_afinn %>%
  summarise(overall_hub = sum(sentiment_hub) / sum(hubspot_frq), overall_mar = sum(sentiment_mar) / sum(marketo_frq))

#plotting the results
seanplot = sean %>%
  gather(firm, value, overall_hub:overall_mar)

p = ggplot(seanplot, aes(x = firm, y = value)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  ggtitle("Overall sentiments") +
  scale_x_discrete(labels = c("Hubspot", "Marketo"))

#topic modeling
#document term matrix is needed
cdtm = DocumentTermMatrix(comparison_corpus)
inspect(cdtm)

#estimating the number of topics
how_many = FindTopicsNumber(
  cdtm,
  topics = seq(from = 2, to =  10, by = 1),
  metrics = c("CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "VEM",
  control = list(seed = 1234),
  mc.cores = 2L,
  verbose = TRUE,
)

FindTopicsNumber_plot(how_many)

#proceeding with 3 topics
lda_topics = LDA(cdtm, k = 3, control = list(seed = 1234))
str(lda_topics)

#starting with the betas
ttp = tidy(lda_topics, matrix = "beta")

lda_top_terms = ttp %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

lda_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = term, y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  ggtitle("Word topic probablities")

#gammas
dtp = tidy(lda_topics, matrix = "gamma")

