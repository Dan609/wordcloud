#### Text analysis

options(max.print=100000)

library(tm)  # for text mining
library(SnowballC) # for text stemming
library(wordcloud) # word-cloud generator 
library(RColorBrewer) # color palettes
library(RDSTK)
library(readr)
library(twitteR)
library(qdap)
library(syuzhet)
library(ggplot2)
library(dplyr)
library(pdftools)
library(devtools)
library(tcltk)

# Read the text from txt file
text <- readLines(file.choose())
# Read the text from pdf file

text <- pdf_text(file.choose())

# first page text
# cat(text[1])

# Load the data as a corpus
docs <- Corpus(VectorSource(text))

# Text cleaning :
# Remove your own stop word, specify your stopwords as a character vector
# docs <- tm_map(docs, removeWords, c("???"))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords("russian"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

# Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

head(d, 10)

# The frequency table of words
par(mfrow=c(1,1), mar=c(10,5,1,1))
barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

### polarity and sentiment ###
par(mfrow=c(3,1), mar=c(5,10,1,1))
scores <- get_nrc_sentiment(text)
polarity <- scores[1,9:10]
polarity
sentiment <- scores[1,1:8]
sentiment
polarity <- data.matrix(polarity, rownames.force = TRUE)
barplot(polarity)
sentiment <- data.matrix(sentiment, rownames.force = TRUE)
barplot(sentiment)
sentences <- get_sentences(text)
sentiment_vector <- get_sentiment(sentences, method = "syuzhet")
summary(sentiment_vector)
boxplot(sentiment_vector)

##What was the most positive?
max(sentiment_vector)
sentence_sentiment <- data.frame(sentences, sentiment_vector)
View(sentence_sentiment)
which.max(sentence_sentiment$sentiment_vector)
which.min(sentence_sentiment$sentiment_vector)
most_positive <- sentence_sentiment[which.max(sentence_sentiment$sentiment_vector),]
most_positive
most_pnegative <- sentence_sentiment[which.min(sentence_sentiment$sentiment_vector),]
most_pnegative

# Generate the Word clouds
# wordcloud(text, colors = c("blue", "green"))

par(mfrow=c(1,1), mar=c(1,1,1,1))

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, 
          scale=c(5,1),
          min.freq = 1, max.words=50, 
          random.order=FALSE, rot.per=0.1,
          colors=brewer.pal(8, "Dark2"))

# find words that occur at least 20 times :

findFreqTerms(dtm, lowfreq = 20)

#  analyze the association between frequent terms :

findAssocs(dtm, terms = "scaffold", corlimit = 0.5)










## Fading cloud

wordcloud(words = d$word, freq = d$freq, 
          scale=c(4,0.7),
          min.freq = 1, max.words=50, 
          random.order=FALSE, rot.per=0.1, 
          colors=brewer.pal(9,"BuGn"),
          main = file_name)

## Big cloud

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, 
          scale=c(3,.7),
          min.freq = 1, max.words=500, 
          random.order=FALSE, rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))

