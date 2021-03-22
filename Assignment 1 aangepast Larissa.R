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
library(mgsub)
library(syuzhet)
library(ggrepel)
library(quanteda)
library(smacof)
library(factoextra)

# Creating data set
text.df <-OnTheSnow_SkiAreaReviews
reviews <- data.frame(ID=seq(1:nrow(text.df)),
                      text=text.df$Review.Text, 
                      stars=text.df$Review.Star.Rating..out.of.5.) #ik moest deze aanpassen om hem te laten werken

# Select "good" and "bad" reviews
good.vec <- reviews$text[reviews[,3]>3] #doe je dit niet pas na het schoonmaken van de data?
bad.vec <- reviews$text[reviews[,3]<4] 

#__________________STAGE ONE: cleaning of the data, removing punctuation and if needed, stemming______________________________ 
#Data must be stored in 2 versions:
  #Version 1: The dataset without stemming and including punctuation (needed for stage three)
  #Version 2: The dataset as a complete fully cleaned word vector representation.

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

#Create backup for stage 3 (including punctuation): 
reviews_backup <- reviews 

#Create dataset as a complete fully cleaned word vector representation
#first stage of cleaning
reviews$text <- as.character(reviews$text)  %>% 
  tolower() %>% 
  {gsub(":( |-|o)*\\("," SADSMILE ", .)} %>%       # Find :( or :-( or : ( or :o(
  {gsub(":( |-|o)*\\)"," HAPPYSMILE ", .)} %>%     # Find :) or :-) or : ) or :o)
  {gsub("(\"| |\\$)-+\\.-+"," NUMBER", .)} %>%     # Find numbers
  {gsub("([0-9]+:)*[0-9]+ *am"," TIME_AM", .)} %>%         # Find time AM
  {gsub("([0-9]+:)*[0-9]+ *pm"," TIME_PM", .)} %>%         # Find time PM
  {gsub("-+:-+","TIME", .)} %>%                    # Find general time
  {gsub("\\$ ?[0-9]*[\\.,]*[0-9]+"," DOLLARVALUE ", .)} %>%           # Find Dollar values
  {gsub("[0-9]*[\\.,]*[0-9]+"," NUMBER ", .)} %>%           # Find remaining numbers
  {gsub("-"," ", .)} %>%                           # Remove all -
  {gsub("\\("," ", .)} %>%                           #Remove all (
  {gsub("\\)"," ", .)} %>%                           #Remove all )
  {gsub("\""," ", .)} %>%                           #Remove all "
  {gsub("\\,"," ", .)} %>%                            #Remove all ,
  {gsub("&"," and ", .)} %>%                       # Find general time
  {gsub("\"+"," ", .)} %>%                         # Remove all "
  {gsub("\\|+"," ", .)} %>%                        # Remove all |
  {gsub("_+"," ", .)} %>%                          # Remove all _
  {gsub(";+"," ", .)} %>%                          # Remove excess ;
  {gsub(" +"," ", .)} %>%                          # Remove excess spaces
  {gsub("\\.+","\\.", .)}                          # Remove excess .

#stemming, unnesting and removing stop words and create word vector
j <- 1
for (j in 1:nrow(reviews)) {
  stemmed_description <- anti_join((reviews[j,] %>% unnest_tokens(word,text, drop=FALSE,to_lower=TRUE) ),stop_words)
  
  stemmed_description <- (wordStem(stemmed_description[,"word"], language = "porter"))
  
  reviews[j,"text"] <- paste((stemmed_description),collapse = " ")
  
}

reviews_vector <- reviews %>% unnest_tokens(word,text, to_lower=TRUE)

#Je kunt beter gsub gebruiken ipv removePunctuation, 
#removePunctuation houdt namelijk geen rekening met spaties: https://stackoverflow.com/questions/29098801/removing-punctuations-from-text-using-r/29099172 

#clean.vec<-function(text.vec){       
 # text.vec <- tryTolower(text.vec)
  #text.vec <- removeWords(text.vec, custom.stopwords)
  #text.vec <- removePunctuation(text.vec)
  #text.vec <- stripWhitespace(text.vec)
  #text.vec <- removeNumbers(text.vec)
  #return(text.vec)
#}

#clean.vec(reviews)
#good.vec <- clean.vec(good.vec)
#bad.vec <- clean.vec(bad.vec)

#remove very frequent and infrequent words
counts <- reviews_vector %>% count(word, sort=TRUE)
infrequent <- counts %>% filter(n <0.00005 * nrow(reviews_vector))
frequent <- counts[1:2,] #remove words ski and "c"
toremove <- full_join(frequent,infrequent)
reviews_vector_cleaned <- reviews_vector %>% anti_join(toremove) 
#Ik weet dus niet hoe je ervoor kan zorgen dat deze woorden ook worden weggehaald in de non-vector data

#Create distinction between happy and unhappy reviews
good.vec <- reviews_vector_cleaned %>% filter(reviews_vector_cleaned[,"stars"] >= 4) 
bad.vec <- reviews_vector_cleaned %>% filter(reviews_vector_cleaned[,"stars"] <= 2)
#Dit is hoe hij het in het college had gedaan, dus reviews met 3 vallen weg -> nog overleggen

#word counts in happy reviews 
counts_happy <- good.vec %>% count(word, sort=TRUE)
counts_happy_high <- head(counts_happy, n = 25)
counts_happy_high %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word,n)) +  
  geom_col() + 
  coord_flip() 

#word counts in unhappy reviews
counts_unhappy <- bad.vec %>% count(word, sort = TRUE)
counts_unhappy_high <- head(counts_unhappy, n = 25)
counts_unhappy_high %>% 
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(word,n)) +
  geom_col() + 
  coord_flip() 

#Document frequency histogram; create corpus based on entire dataset 
corpus <- corpus(reviews, docid_field = "ID", text_field = "text",  
                 metacorpus = NULL, compress = TRUE)
reviews_dfm <- dfm(corpus)
docfreqs <- docfreq(reviews_dfm) %>% sort(decreasing = TRUE)
docfreqs <- data.frame(word = names(docfreqs), n_docs=docfreqs)

docfreqs %>%
  mutate(word = reorder(word,n_docs)) %>% 
  top_n(20, word) %>%
  ggplot(aes(word,n_docs)) +  
  geom_col() + 
  labs(x = NULL, y = "Number of occurences") + 
  coord_flip() + 
  theme(text = element_text(size = 17)) + 
  ggtitle("Document Frequency Histogram")

#TF-IDF = term frequency / document frequency -> high if it appears a lot in a smaller number of docs
wordcounts <- reviews_vector_cleaned %>% count(word, sort=TRUE) 
tf_idf_table <- merge(docfreqs, wordcounts)
tf_idf_table$tf_idf <- tf_idf_table$n/tf_idf_table$n_docs
tf_idf_table<-tf_idf_table[order(-tf_idf_table$tf_idf),]

tf_idf_table %>%
  mutate(word = reorder(word,tf_idf)) %>% 
  top_n(20, tf_idf) %>%
  ggplot(aes(word,tf_idf)) +  
  geom_col() + 
  labs(x = NULL, y = "tf_idf") + 
  coord_flip() + 
  theme(text = element_text(size = 17)) + 
  ggtitle("TF-IDF value ")

#Comparison cloud with ratings 1 to 5 
review_tdm <- reviews_vector_cleaned %>% count(word,stars,sort=TRUE) %>%
  ungroup()%>% cast_tdm(word,stars,n)

comparison.cloud(as.matrix(review_tdm), scale=c(3,0.5), random.order=FALSE ,  
                 max.words=60, rot.per = 0.3)

#Create corpus based on "good" and "bad" reviews
good.vec <- reviews %>% filter(reviews[,"stars"] >= 4) 
bad.vec <- reviews %>% filter(reviews[,"stars"] <= 2)

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
            colou.cr="white",hjust=1.25, size=5.0)

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

# Plotting common words with biggest differences in frequencies between good and bad reviews
pyramid.plot(top25.df$x, top25.df$y,
             labels = top25.df$labels,
             gap = 18, top.labels = c("Good reviews",
                                      "Words", "Bad reviews"),
             main = "Words in Common", laxlab = NULL,
             raxlab = NULL, unit = NULL) #Deze werkt nog niet goed bij mij (Larissa)

#___________STAGE 2: data visualization with PCA and MDS__________________________________

#MDS: first, create distance matrix based on co-occurence matrix
corpus <- corpus(reviews, docid_field = "ID", text_field = "text")
co_occurrence_matrix <- fcm(x = corpus, context = "document", count = "frequency", tri=FALSE)
co_occurrence_matrix <- as.matrix(co_occurrence_matrix)
co_occurrence_matrix <- co_occurrence_matrix[sortednames[1:nwords],sortednames[1:nwords]]
co_occurrence_matrix[1:15,1:15] #show co-occurence matrix

distances <-sim2diss(co_occurrence_matrix, method = "cooccurrence") #Transform similarities to distances
distances[1:15,1:15] #show distances

#Visualize distances
MDS_map <-smacofSym(distances) #dit duurt echt een eeuwigheid

#run the routine that finds the best matching coordinates in a 2D mp given the distances 
ggplot(as.data.frame(MDS_map$conf),
       aes(D1, D2, label =rownames(MDS_map$conf))) + 
  geom_text(check_overlap = TRUE)+ theme_minimal(base_size = 15) + 
  xlab('')+ ylab('')+scale_y_continuous(breaks = NULL) + 
  scale_x_continuous(breaks = NULL)

#PCA: 

