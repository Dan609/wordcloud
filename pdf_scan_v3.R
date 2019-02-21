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

####### START

file_list <- list.files(path = , choose.dir(default = "",
                                            caption = "Select folder"),
                        pattern = "pdf",
                        all.files = FALSE,
                        full.names = TRUE, recursive = TRUE,
                        ignore.case = FALSE, include.dirs = FALSE,
                        no.. = FALSE)


file_list
summary = file.path(dirname(file_list[1]), "summary.txt")
for (file_name in file_list) {

  text <- pdf_text(file_name)

  # Load the data as a corpus
  docs <- Corpus(VectorSource(text))

  # Text cleaning :
  # Remove your own stop word, specify your stopwords as a character vector
  # docs <- tm_map(docs, removeWords, c("word1", "word2"))
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

  write(file_name, file = summary, append = TRUE)
  print(file_name)
  write("............", file = summary, append = TRUE)
  write.table(head(d, 5), file = summary, append = TRUE)
  write("............", file = summary, append = TRUE)
  write.table(findAssocs(dtm, terms = "calcium", corlimit = 0.5), file = summary,
              append = TRUE)
  write("............", file = summary, append = TRUE)

  par(mfrow=c(1,1), mar=c(1,1,1,1))

  set.seed(1234)
  png(paste(file_name, "png", sep="."))
  wordcloud(words = d$word, freq = d$freq,
            scale=c(4,0.8),
            min.freq = 1, max.words=200,
            random.order=FALSE, rot.per=0.1,
            colors=brewer.pal(8, "Dark2"),
            vfont=NULL)
  dev.off()
}

#### END ####
# If "Error in plot.new() : figure margins too large" :
# repeat dev.off() from console until "null device"

# find words that occur at least 20 times :

findFreqTerms(dtm, lowfreq = 200)

#  analyze the association between frequent terms :

findAssocs(dtm, terms = "arrhythmia", corlimit = 0.5)
findAssocs(dtm, terms = "oscillations", corlimit = 0.5)
findAssocs(dtm, terms = "frequency", corlimit = 0.5)
findAssocs(dtm, terms = "spectrum", corlimit = 0.5)
findAssocs(dtm, terms = "necrosis", corlimit = 0.5)
findAssocs(dtm, terms = "ischaemia", corlimit = 0.5)

# Mac
file_list <- list.files(path = "/Users/danilabobkov/Dropbox/bks/bio/Ca waves",
                        pattern = "pdf",
                        all.files = FALSE,
                        full.names = TRUE, recursive = TRUE,
                        ignore.case = FALSE, include.dirs = FALSE,
                        no.. = FALSE)
