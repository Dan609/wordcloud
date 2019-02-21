#### text analysis

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

####### Directory scan Mac solution  #tkchooseDirectory(),

file_list <- list.files(path = "/Users/danilabobkov/Dropbox/bks/bio/Age",
                        pattern = "pdf",
                        all.files = FALSE,
                        full.names = TRUE, recursive = TRUE,
                        ignore.case = FALSE, include.dirs = FALSE,
                        no.. = FALSE)
file_list

for (file_name in file_list) {

  text <- pdf_text(file_name)

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

  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq,
            scale=c(4,0.8),
            min.freq = 1, max.words=100,
            random.order=FALSE, rot.per=0.1,
            colors=brewer.pal(8, "Dark2"))
}
