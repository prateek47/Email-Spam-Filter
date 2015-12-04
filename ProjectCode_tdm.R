# Data Mining Project Code 

library(tm)
library(stringr)
library(plyr)
library(kernlab)

# Parts of the code are taken from the book Machine Learning for Hackers

# We set the directories where data is stored
spam.data <- 'C:/UVa/Data_Mining/workspace/project_data/spam/'
easyham.data <- 'C:/UVa/Data_Mining/workspace/project_data/easy_ham/'
spam2.data <- 'C:/UVa/Data_Mining/workspace/project_data/spam_2/'
easyham2.data <- 'C:/UVa/Data_Mining/workspace/project_data/easy_ham_2/'
hardham.data <- 'C:/UVa/Data_Mining/workspace/project_data/hard_ham/'

# Extracting the body of the emails, as explained in the book, the email always start 
# after the line break.
extract_email <- function(path){
  con <- file(path, open="rt", encoding="latin1")
  text <- readLines(con)
  if (is.numeric(which(text == "")[1]) && is.finite(which(text == "")[1]) && 
      which(text == "")[1] < length(text)) {
    msg <- text[seq(which(text == "")[1] + 1, length(text), 1)]
  } else {
    msg <- ""
  } 
  close(con)
  return(paste(msg, collapse="\n"))
}

# extracting the spam emails in a vector
spam.docs <- dir(spam.data)
spam.docs <- spam.docs[which(spam.docs!= 'cmds')]
spam.emails <- sapply(spam.docs, function(x){
  extract_email(paste(spam.data,x,sep = ""))
  })
# inspecting the vector
#spam.emails

# apply topic modelling 
# creating the corpus
spam.corpus <- Corpus(VectorSource(spam.emails))

# applying topic modelling techniques for better interpretation of the text
spam.corp <- tm_map(spam.corpus, content_transformer(tolower))
removespec <- content_transformer(function(x, pattern){
  return(gsub(pattern, "", x))
})
spam.corp <- tm_map(spam.corp, removespec, "[áàæöø??îïãëçýäåðßûúìèõüñ]")
spam.corp <- tm_map(spam.corp, removePunctuation)
spam.corp <- tm_map(spam.corp, removeNumbers)
spam.corp <- tm_map(spam.corp, removeWords, stopwords('english'))
spam.corp <- tm_map(spam.corp, stripWhitespace)

# inspect the corpus
spam.corp[[2]]$content

# creating the term document matrix
spam.tdm <- TermDocumentMatrix(spam.corp, control = list(wordLengths=c(4,15), bounds=list(global=c(2,Inf))))

# inspecting the term document matrix
spam.tdm
spam.tdm$dimnames

# frequeny of words in the corpus
freq <- rowSums(as.matrix(spam.tdm))

# creating a dataframe with words and their frequencies
spam.df <- data.frame(word=spam.tdm$dimnames$Terms, frequency=freq)
# inspecting the data frame
spam.df[1:10,]

spam.mat <- as.matrix(spam.tdm)
# adding the percentage of times a term occurs in spam emails, i.e. the probability of
# occurence of term
word.occurences <- sapply(1:nrow(spam.mat), function(x){
  length(which(spam.mat[x,]>0))/ncol(spam.mat)
})

spam.df$occurence <- word.occurences

# probability of the word occurence in the entire corpus
word.prob <- sapply(1:nrow(spam.mat), function(x){
  spam.df$frequency[x]/sum(spam.df$frequency)
})

spam.df$density <- word.prob
# removing the irrelevant words
spam.df <- spam.df[-(1:7), ]

spam.df[1:10, ]
# inspecting in ascending order of occurences
head(spam.df[with(spam.df, order(-occurence)), ])

# same is done for easy ham
#------------------------------

# extracting the spam emails in a vector
easy_ham.docs <- dir(easyham.data)
easy_ham.docs <- easy_ham.docs[which(easy_ham.docs!= 'cmds')]
easy_ham.emails <- sapply(easy_ham.docs, function(x){
  extract_email(paste(easyham.data,x,sep = ""))
})
# inspecting the vector
#easy_ham.emails

# apply topic modelling 
# creating the corpus
easy_ham.corpus <- Corpus(VectorSource(easy_ham.emails))

# applying topic modelling techniques for better interpretation of the text
easy_ham.corp <- tm_map(easy_ham.corpus, content_transformer(tolower))
removespec <- content_transformer(function(x, pattern){
  return(gsub(pattern, "", x))
})
easy_ham.corp <- tm_map(easy_ham.corp, removespec, "[áàæöø??îïãëçýäåðßûúìèõüñ]")
easy_ham.corp <- tm_map(easy_ham.corp, removePunctuation)
easy_ham.corp <- tm_map(easy_ham.corp, removeNumbers)
easy_ham.corp <- tm_map(easy_ham.corp, removeWords, stopwords('english'))
easy_ham.corp <- tm_map(easy_ham.corp, stripWhitespace)

# inspect the corpus
easy_ham.corp[[2]]$content

# creating the term document matrix
easy_ham.tdm <- TermDocumentMatrix(easy_ham.corp, control = list(wordLengths=c(4,15), bounds=list(global=c(2,Inf))))

# inspecting the term document matrix
easy_ham.tdm
easy_ham.tdm$dimnames

# frequeny of words in the corpus
freq <- rowSums(as.matrix(easy_ham.tdm))

# creating a dataframe with words and their frequencies
easy_ham.df <- data.frame(word=easy_ham.tdm$dimnames$Terms, frequency=freq)
# inspecting the data frame
easy_ham.df[1:10,]

easy_ham.mat <- as.matrix(easy_ham.tdm)
# adding the percentage of times a term occurs in easy_ham emails, i.e. the probability of
# occurence of term
word.occurences2 <- sapply(1:nrow(easy_ham.mat), function(x){
  length(which(easy_ham.mat[x,]>0))/ncol(easy_ham.mat)
})

easy_ham.df$occurence <- word.occurences2

# probability of the word occurence in the entire corpus
word.prob2 <- sapply(1:nrow(easy_ham.mat), function(x){
  easy_ham.df$frequency[x]/sum(easy_ham.df$frequency)
})

easy_ham.df$density <- word.prob2
# removing the irrelevant words
easy_ham.df <- easy_ham.df[-(1:7), ]

easy_ham.df[1:10, ]
# inspecting in ascending order of occurences
head(easy_ham.df[with(easy_ham.df, order(-occurence)), ])

# calculating the spamicity
match.df <- data.frame(matrix(ncol=3, nrow = 3041))
colnames(match.df) <- c("word", "Prob.spam", "Prob.ham")

matching.words <- intersect(spam.df$word, easy_ham.df$word)
match.df$word <- matching.words
s <- spam.df[intersect(spam.df$word, match.df$word),]
match.df$Prob.spam <- s$occurence
h <- easy_ham.df[intersect(easy_ham.df$word, match.df$word),]
match.df$Prob.ham <- h$occurence

# Spamicity
for(i in 1:nrow(match.df)){
  match.df$spamicity[i] <- (match.df$Prob.spam[i])/(match.df$Prob.spam[i]+match.df$Prob.ham[i])
}

# probability of spam - probability of ham
match.df$Pr.S_Pr.H <- abs(match.df$Prob.spam-match.df$Prob.ham)


ord1 <- match.df[order(c(match.df$Pr.S_Pr.H, match.df$spamicity), decreasing = TRUE), ]
ord2 <- ord1[complete.cases(ord1),]
top.100.spam.words <- ord2$word[1:100]
top.100.spam.words

# creating the data for SVM usage
spam.dtm <- DocumentTermMatrix(spam.corp, control = list(dictionary=top.100.spam.words))
spam.dtm.df<- data.frame(as.matrix(spam.dtm))
easy_ham.dtm <- DocumentTermMatrix(easy_ham.corp, control = list(dictionary=top.100.spam.words))
easy_ham.dtm.df<- data.frame(as.matrix(easy_ham.dtm))

spam.dtm.df$typeofemail <- 0
easy_ham.dtm.df$typeofemail <- 1


# the data frame with only the top 100 words
data.svm <- rbind.fill(spam.dtm.df, easy_ham.dtm.df)
data.svm[is.na(data.svm)] <- 0

samp <- sample(1:nrow(data.svm), as.integer(nrow(data.svm)*0.75))
training.data <- data.svm[samp, ]
hard_ham.dtm.df <- data.svm[-samp, ]

svm.fit <- ksvm(typeofemail~ ., data = training.data, type="C-svc", kernel="vanilladot", C=100)

svm.predict <- predict(svm.fit, newdata= testing.data)

accuracy <- sum(svm.predict == testing.data[,"typeofemail"])/ nrow(testing.data)

true_values <- svm.predict[svm.predict == testing.data[,"typeofemail"]]
false_values <- svm.predict[!(svm.predict == testing.data[,"typeofemail"])] 
false.positive <- sum(false_values == 1) #18
false.negative <- sum(false_values == 0) #16
true.positive <- sum(true_values == 1) # 633

precision <- true.positive/(true.positive+false.positive)
recall <- true.positive/(true.positive+false.negative)

# creating the hard_harm prediction data
#--------------------------------------------

# extracting the spam emails in a vector
hard_ham.docs <- dir(hardham.data)
hard_ham.docs <- hard_ham.docs[which(hard_ham.docs!= 'cmds')]
hard_ham.emails <- sapply(hard_ham.docs, function(x){
  extract_email(paste(hardham.data,x,sep = ""))
})
# inspecting the vector
#hard_ham.emails

# apply topic modelling 
# creating the corpus
hard_ham.corpus <- Corpus(VectorSource(hard_ham.emails))

# applying topic modelling techniques for better interpretation of the text
hard_ham.corp <- tm_map(hard_ham.corpus, content_transformer(tolower))
removespec <- content_transformer(function(x, pattern){
  return(gsub(pattern, "", x))
})
hard_ham.corp <- tm_map(hard_ham.corp, removespec, "[áàæöø??îïãëçýäåðßûúìèõüñ]")
hard_ham.corp <- tm_map(hard_ham.corp, removePunctuation)
hard_ham.corp <- tm_map(hard_ham.corp, removeNumbers)
hard_ham.corp <- tm_map(hard_ham.corp, removeWords, stopwords('english'))
hard_ham.corp <- tm_map(hard_ham.corp, stripWhitespace)

# inspect the corpus
hard_ham.corp[[2]]$content

# creating the term document matrix
hard_ham.dtm <- DocumentTermMatrix(hard_ham.corp, control = list(dictionary=top.100.spam.words))

hard_ham.dtm.df <- data.frame(as.matrix(hard_ham.dtm))
hard_ham.dtm.df$typeofemail <- 1


svm.fit1 <- ksvm(typeofemail~ ., data = data.svm, type="C-svc", kernel="vanilladot", C=100)

svm.predict1 <- predict(svm.fit1, newdata= hard_ham.dtm.df)

accuracy1 <- sum(svm.predict1 == hard_ham.dtm.df[,"typeofemail"])/ nrow(hard_ham.dtm.df)

true_values1 <- svm.predict1[svm.predict1 == hard_ham.dtm.df[,"typeofemail"]]
false_values1 <- svm.predict1[!(svm.predict1 == hard_ham.dtm.df[,"typeofemail"])] 

save.image("Projectcode1.RData")

# ----------------------------------------------------------------------------------------

# not required...


# the regex used to find http tags
#[-a-zA-Z0-9@:%_\+.~#?&//=]{2,256}\.[a-z]{2,4}\b(\/[-a-zA-Z0-9@:%_\+.~#?&//=]*)?
# e.g.  
text <-  "dfbiouhfibiwouosidhojf http://www.pnas.org poloudaboiasdofasdd"

url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

str_extract(text, url_pattern)
