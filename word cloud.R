# The downloaded binary packages are /var/folders/rt/rcrh7rrx0gvb4f59vwfj383c0000gn/T//RtmpRkda09/downloaded_packages

library("tm")  # for text mining
library("SnowballC")  # for text stemming
library("wordcloud")  # word-cloud generator 
library("RColorBrewer")  # color palettes
library("RDSTK")
library("readr")
library("twitteR")
library("qdap")
library("syuzhet")
library(ggplot2)
library(dplyr)

options(max.print = 1e+05)

par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))

# Read the text file

text <- readLines(file.choose())

# Load the data as a corpus

docs <- Corpus(VectorSource(text))

# Text cleaning : Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords("russian"))
# Remove your own stop word specify your stopwords as a character vector docs <-
# tm_map(docs, removeWords, c('???')) Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming docs <- tm_map(docs, stemDocument)

inspect(docs)

# Build a term-document matrix

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

head(d, 100)

# Generate the Word cloud

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, scale = c(5, 0.5), min.freq = 1, max.words = 300, 
  random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))


set.seed(1234)
wordcloud(words = d$word, scale = c(5, 0.5), freq = d$freq, min.freq = 1, max.words = 300, 
  random.order = FALSE, rot.per = 0.1, colors = "black")

`?`(wordcloud)
# find words that occur at least ___ times :

findFreqTerms(dtm, lowfreq = 10)

# analyze the association between frequent terms :

findAssocs(dtm, terms = "fluid", corlimit = 0.3)

`?`(findAssocs)

# The frequency table of words

head(d, 100)

barplot(d[1:20, ]$freq, las = 2, names.arg = d[1:20, ]$word, col = "lightblue", main = "Most frequent words", 
  ylab = "Word frequencies")


`?`(Corpus)
`?`(`?`(syuzhet))
######## text analytisc using the Syuzhet package

article <- read_file(file.choose())

# article <- readLines(file.choose())

View(article)

### polarity and sentiment ###

g_scores <- get_nrc_sentiment(article)

g_scores

g_polarity <- g_scores[1, 9:10]
g_polarity
g_sentiment <- g_scores[1, 1:8]
g_sentiment

## visualize polarity and sentiment
par(mfrow = c(1, 1), mar = c(3, 3, 3, 3))

class(g_polarity)

g_polarity <- data.matrix(g_polarity, rownames.force = TRUE)
g_polarity
barplot(g_polarity)

g_sentiment <- data.matrix(g_sentiment, rownames.force = TRUE)
barplot(g_sentiment)
