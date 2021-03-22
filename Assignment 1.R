# Good website: https://rpubs.com/williamsurles/316682

# Set options
options(stringsAsFactors=F)
Sys.setlocale('LC_ALL','C')

# Loading libraries
library(stringi)
library(stringr)
library(qdap)
library(lubridate)
library(tm)
library(ggplot2)
library(wordcloud)
library(jsonlite)
library(dplyr)
library(tokenizers)
library(tidytext)
library(SnowballC)
library(corpus)
library(plotrix)


# Creating data set
text.df<-OnTheSnow_SkiAreaReviews
reviews<-data.frame(ID=seq(1:nrow(text.df)),text=text.df$`Review Text`, stars=text.df$`Review Star Rating (out of 5)`)
View(reviews)


# Select "good" and "bad" reviews
good.vec <- reviews$text[reviews[,3]>3]
bad.vec <- reviews$text[reviews[,3]<4]

# Return NA instead of tolower error
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e) # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y) 
  }

# Cleaning vectors
custom.stopwords<-c(stopwords('english'),'will','just','can','get','one')

clean.vec<-function(text.vec){
  text.vec <- tryTolower(text.vec)
  text.vec <- removeWords(text.vec, custom.stopwords)
  text.vec <- removePunctuation(text.vec)
  text.vec <- stripWhitespace(text.vec)
  text.vec <- removeNumbers(text.vec)
  return(text.vec)
}

good.vec <- clean.vec(good.vec)
bad.vec <- clean.vec(bad.vec)

# Creating corpus and TermDocumentMatrix
good.vec <- paste(good.vec, collapse=" ")
bad.vec <- paste(bad.vec, collapse=" ")
all.rev <- c(good.vec, bad.vec)
corpus <- VCorpus(VectorSource(all.rev))
tdm <- TermDocumentMatrix(corpus)
tdm.m <- as.matrix(tdm)
colnames(tdm.m) = c("Good reviews","Bad reviews")

# Term frequencies
term.freq <- rowSums(tdm.m)
freq.df <- data.frame(word=names(term.freq),frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2],decreasing=T),]
freq.df$word<-factor(freq.df$word,
                     levels=unique(as.character(freq.df$word)))
ggplot(freq.df[1:20,], aes(x=word,
                           y=frequency))+geom_bar(stat="identity",
                                                  fill='darkred')+
  coord_flip()+
  geom_text(aes(label=frequency),
            colour="white",hjust=1.25, size=5.0)

# Adjust stopwords based on term frequencies see line 43

# Comparison cloud
comparison.cloud(tdm.m,max.words=100,random.order=FALSE)

# Common words
common.words <- subset(tdm.m, tdm.m[,1]>0 & tdm.m[,2]>0)
View(common.words)

difference <- abs(common.words[, 1] - common.words[, 2])
common.words <- cbind(common.words, difference)
common.words <- common.words[order(common.words[, 3],
                                   decreasing = TRUE), ]
top25.df <- data.frame(x = common.words[1:25, 1],y =
                         common.words[1:25, 2],
                       labels = rownames(common.words[1:25, ]))

# Plotting common words with biggest differences in frequencies between good and bad revies
pyramid.plot(top25.df$x, top25.df$y,
             labels = top25.df$labels,
             gap = 18, top.labels = c("Good reviews",
                                      "Words", "Bad reviews"),
             main = "Words in Common", laxlab = NULL,
             raxlab = NULL, unit = NULL)

# Remove sparse words?
# tdm.m2 <- removeSparseTerms(tdm.m, sparse = 0.975)
