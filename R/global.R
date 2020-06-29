# Define a server for the Shiny app
# Load
library("tm")
library("swirl")
library("tmap")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("memoise")
library("stringr")
library(RMySQL)
library(dbConnect)
library("devtools")
require(devtools)
library("plyr")
library("sentiment")


con <- dbConnect(MySQL(), user="root", password="root",dbname="demo", host="localhost")
myQuery<-"Select DISTINCT Locality from hotellists"
df<- dbGetQuery(con,myQuery)
cities=apply(df, 1, function(r) paste(names(df)[-1], r, sep="", collapse=" "))
print(cities)



getTermMatrix1 <- memoise(function(incity) {
  myQuery1<-paste("Select Hotel from hotellists where Locality='",incity,"'",sep='')
  df1<- dbGetQuery(con,myQuery1)
  apply(df1, 1, function(r) paste(names(df1)[-1], r, sep="", collapse=" "))
})


# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(inbook) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  
  inbook= str_replace_all(inbook, pattern=" ", repl="_")
  inbook= str_replace_all(inbook, pattern="-", repl="_")
  
  if(str_length(inbook)>30)
    inbook <- substr(inbook, 1, 30)
  
  testplz = paste("E:/HRS/",inbook,".txt", sep="")
  print(testplz)
  filePath <- testplz
  
  text <- readLines(filePath)
  # Load the data as a corpus
  docs <- Corpus(VectorSource(text))
  
  print(file.names[i])
  
  
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("hotel", "blabla2")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  # docs <- tm_map(docs, stemDocument)
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  sort(rowSums(m),decreasing=TRUE)   
  
  
})

getTermMatrix4 <- memoise(function(inbook) {
  inbook= str_replace_all(inbook, pattern=" ", repl="_")
  inbook= str_replace_all(inbook, pattern="-", repl="_")
  
  if(str_length(inbook)>30)
    inbook <- substr(inbook, 1, 30)
  
  inbook
  
})

getTermMatrix3 <- memoise(function(inbook) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  
  inbook= str_replace_all(inbook, pattern=" ", repl="_")
  inbook= str_replace_all(inbook, pattern="-", repl="_")
  
  if(str_length(inbook)>30)
    inbook <- substr(inbook, 1, 30)
  
  testplz = paste("E:/HRS/TxtFiles/",inbook,".txt", sep="")
  print(testplz)
  filePath <- testplz
  
  text <- readLines(filePath)
  # Load the data as a corpus
  docs <- Corpus(VectorSource(text))
  
  print(file.names[i])
  
  
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("hotel", "blabla2")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  # docs <- tm_map(docs, stemDocument)
  
  TermDocumentMatrix(docs)
  
  
})

getSCR <- memoise(function(hn) {
  hn= str_replace_all(hn, pattern=" ", repl="_")
  hn= str_replace_all(hn, pattern="-", repl="_")
  hn= str_to_lower(hn, locale = "")
  
  if(str_length(hn)>30)
    hn <- substr(hn, 1, 30)
  
  serviceQuery<-paste("SELECT Content FROM demo.",hn," WHERE Service<=2 AND Cleanliness>2 AND Value>2 AND SleepQuality>2 AND Rooms>2 LIMIT 3;",sep="")
  serviceQuerydf<- dbGetQuery(con,serviceQuery)
  topServiceCriticalReview= apply(serviceQuerydf, 1, function(r) paste(names(serviceQuerydf)[-1], r, sep="", collapse=" "))
  topServiceCriticalReview
  
  
})

getCCR <- memoise(function(hn) {
  hn= str_replace_all(hn, pattern=" ", repl="_")
  hn= str_replace_all(hn, pattern="-", repl="_")
  hn= str_to_lower(hn, locale = "")
  
  if(str_length(hn)>30)
    hn <- substr(hn, 1, 30)
  
  cleanlinessQuery<-paste("SELECT Content FROM demo.",hn," WHERE Cleanliness<=2 AND Service>2 AND Value>2 AND SleepQuality>2 AND Rooms>2 LIMIT 3;",sep="")
  cleanlinessQuerydf<- dbGetQuery(con,cleanlinessQuery)
  topCleanlinessCriticalReview= apply(cleanlinessQuerydf, 1, function(r) paste(names(cleanlinessQuerydf)[-1], r, sep="", collapse=" "))
  topCleanlinessCriticalReview
  
  
})

getRCR <- memoise(function(hn) {
  hn= str_replace_all(hn, pattern=" ", repl="_")
  hn= str_replace_all(hn, pattern="-", repl="_")
  hn= str_to_lower(hn, locale = "")
  
  if(str_length(hn)>30)
    hn <- substr(hn, 1, 30)
  
  RoomsQuery<-paste("SELECT Content FROM demo.",hn," WHERE Rooms<=2 AND Service>2 AND Value>2 AND SleepQuality>2 AND Cleanliness>2 LIMIT 3;",sep="")
  RoomsQuerydf<- dbGetQuery(con,RoomsQuery)
  topRoomsCriticalReview= apply(RoomsQuerydf, 1, function(r) paste(names(RoomsQuerydf)[-1], r, sep="", collapse=" "))
  topRoomsCriticalReview
  
  
})