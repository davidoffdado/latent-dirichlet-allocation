library(topicmodels)
library(tidyverse)
library(tidytext)
library(tm)
library(stringr)
library(SnowballC)
library(wordcloud2)
library(syuzhet)
library(readr)
library(gofastr)
options(scipen = 10000)

#----------------------------------------------------------------------------#
# EXCEPT THE LDA MODELING SECTION THE SINGLE ALBUM'S LYRICS CODE IS THE SAME #
#----------------------------------------------------------------------------#

### LOADING DATA ####

getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
text=read_file(file.choose())
# Make all the text low-case
text=tolower(text)
text=as.list(strsplit(text, "aaaa")[[1]])

# Create a Vector Corpus
DocText = VCorpus(VectorSource(text))

#Replacing "-" and other character with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
DocText <- tm_map(DocText, toSpace, "-")
DocText <- tm_map(DocText, toSpace, "\r")
DocText <- tm_map(DocText, toSpace, "\n")
DocText <- tm_map(DocText, toSpace, "'")
DocText <- tm_map(DocText, toSpace, "[")
DocText <- tm_map(DocText, toSpace, "]")
# Remove numbers
DocText <- tm_map(DocText, removeNumbers)
# Remove italian common stopwords
DocText <- tm_map(DocText, removeWords, stopwords("italian"))
DocText <- tm_map(DocText, removeWords, read_file("C:/Users/David/Desktop/Unifi/SECONDO ANNO/Multivariate analysis and statistical learning/PROJECT/stopWords.txt"))
DocText <- tm_map(DocText, removeWords, stopwords("english"))
# Remove punctuations
DocText <- tm_map(DocText, removePunctuation)
# Eliminate extra white spaces
DocText <- tm_map(DocText, stripWhitespace)
# Text stemming - which reduces words to their root form
DocText <- tm_map(DocText, stemDocument)
# Removing the least and the most frequent words across the documents
ndocs <- length(DocText)
# ignore overly sparse terms (appearing in less than 1% of the documents)
minDocFreq <- ndocs * 0.01
# ignore overly common terms (appearing in more than 80% of the documents)
maxDocFreq <- ndocs * 0.8
# Remove your own stop word 
# specify your custom stopwords as a character vector
# DocText <- tm_map(DocText, removeWords, c("niente", "stesso", "fuori", "proprio", "due", "mai", "roba", "vedo", "vuoi", "tempo", "porto", "allora", "dentro", "noyz narcos", "noyz", "narcos", "carter", "cole", "metal carter", "gel", "chicoria", "verse", "truceklan", "puoi", "quel", "qui", "piu", "cosi", "solo", "cosa", "crew", "adesso", "dopo", "ora", "rit", "vedi", "poi", "strofa", "quando", "ancora", "ogni", "perche", "com", "perch", "nessun", "andata", "narco", "senza", "altro", "ecco", "truceboy", "rap", "sai", "fare", "sotto", "sopra", "porta", "strada", "capir", "sempr", "gent", "prima", "posto"))
# DocText <- tm_map(DocText, removeWords, c("piu", "quando", "dentro", "solo", "ancora", "senza", "poi", "ogni", "qui", "sai", "fare", "ora", "vedo", "prima", "adesso", "cosa", "strofa", "verso", "rit", "verse", "ritornello", "vedi", "vuoi", "altro", "quel", "sopra", "sotto", "ecco", "troppo", "allora"))
## MIGLIORE ##
#DocText <- tm_map(DocText, removeWords, c("cazzo", "dentro", "solo", "vita", "senza", "amor", "mondo", "sempr", "testa", "mai", "ogni", "fare", "gent", "roba", "nient", "male", "merda", "hop", "hip", "brado", "sai", "trucegel", "gel", "qui", "vuoi", "finir", "stato", "ora", "poi", "vedi", "ancora", "com", "strofa", "quando", "piu", "rap", "sangu", "narco", "truceklan", "noyz", "cole"))
#DocText <- tm_map(DocText, removeWords, c("cazzo", "rime", "così", "coinvolti", "insiem", "mmagini", "eppur", "così", "cio", "batti", "assolti", "paura", "strada", "casa", "giardino", "fuori", "giorno", "dio", "dormir", "voglio", "capisci", "problemi", "fatto", "letto", "nero", "sort", "discorsi", "fotter", "mort", "capir", "nott", "vedo", "occhi", "dormono", "puoi", "cosi", "allora", "ragazzi", "fino", "tanto", "chicoria", "intorno", "mezzo", "poco", "vision", "fort", "gioca", "metal", "metto", "neanch", "soltanto", "ormai", "altra", "basta", "dire", "dopo", "fra", "giro", "guarda", "meglio", "solament", "serv", "prima", "adesso", "puoi",  "carter", "crew", "truceboy", "frega", "cosa", "tempo", "altro", "quel", "sopra", "sotto", "bene", "porto", "mano", "perch", "troppo", "ecco", "nessun", "porta", "aver", "due", "mentr", "posto", "stesso", "fors", "proprio", "vuol", "dentro", "solo", "vita", "senza", "amor", "mondo", "sempr", "testa", "mai", "ogni", "fare", "gent", "roba", "nient", "male", "merda", "hop", "hip", "brado", "sai", "trucegel", "gel", "qui", "vuoi", "finir", "stato", "ora", "poi", "vedi", "ancora", "com", "strofa", "quando", "piu", "rap", "sangu", "narco", "truceklan", "noyz", "cole"))
#DocText <- tm_map(DocText, removeWords, c("piu", "stato", "frega", "sopra", "sotto", "sangu", "cazzo", "male", "rap", "truceklan", "narco", "noyz", "sempr", "senza", "dentro", "solo", "ancora", "mai", "sai", "fare", "ora", "nient", "quando", "poi", "ogni", "qui", "cosa", "quel", "cosi"))
DocText <- tm_map(DocText, removeWords, c("piu", "poi", "quando", "ogni", "quel", "cosi", "senza", "ancora"))
#Create the Document Term Matrix
DocText_dtm <- DocumentTermMatrix(DocText)
# Removing words according to sparsity
DocText_dtm 

#Generate a wordlcloud
dtm_m <- as.matrix(DocText_dtm)
# Sort by decreasing value of frequency
dtm_v <- sort(colSums(dtm_m), decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v), freq=dtm_v)
# Display the top 100 most frequent words
NROW(dtm_d)
head(dtm_d, 100)
tail(dtm_d, 100)
#wordcloud2(dtm_d)

#### SENTIMENT ANALYSIS ####
#doing sentiment analysis on the joint lyrics it's a little unuseful, but it's just to see how now it's all mixed up#
# maybe "Sangue" album overcome others lyrics, indeed tends to use a more repetitive style, with a recurring refrain and with more words due to the velocity of rapping#
#associating a sentiment value to each word
sentiments=get_nrc_sentiment(as.character(dtm_d$word), language = "italian")
colSums(sentiments)


#Transpose the matrix
sentMatr<-data.frame(t(sentiments))
#Computing the sum of the sentiment value
sentSum <- data.frame(rowSums(sentMatr))
#Naming columns
names(sentSum)[1] <- "count"
sentSum <- cbind("sentiment" = rownames(sentSum), sentSum)
#Not taking the last two sentiment (positive or negative)
sentSum<-sentSum[1:8,]

write.table(data.frame(t(colSums(sentiments))), file = "Songs' sentiments.csv", append = TRUE, col.names = FALSE, sep = ",")



#write.csv(data.frame(t(colSums(sentiments))), "Songs' sentiments.csv")


