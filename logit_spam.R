library(tm)
library(stringr)
library(plyr)
library(kernlab)
library(glmnet)

spam.data <- 'SpamAssassin/spam/'
ham.data <- 'SpamAssassin/ham/'


## Extracting the body of the emails, as explained in the book, the email always start 
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

## getting all the spam and ham emails
spam.emails <- sapply(dir(spam.data), function(x){
  extract_email(paste(spam.data,x,sep = ""))
  })
#1898
ham.emails <- sapply(dir(ham.data), function(x){
  extract_email(paste(ham.data,x,sep = ""))
  })
#5052

## dividing into train and test data
# keeping a spam ratio of arnd 30% in both training and test data
spam.ind=sample(1:length(spam.emails),1200)
spam.train=spam.emails[spam.ind]
spam.test=spam.emails[-spam.ind]
# 698

ham.ind=sample(1:length(ham.emails),2500)
ham.train=ham.emails[ham.ind]
ham.test=ham.emails[-ham.ind][1:1700]

## text cleaning
#remove html tags
spam.train=sapply(spam.train,function(x) gsub('<[^>]*>','',x))
spam.test=sapply(spam.test,function(x) gsub('<[^>]*>','',x))
ham.train=sapply(ham.train,function(x) gsub('<[^>]*>','',x))
ham.test=sapply(ham.test,function(x) gsub('<[^>]*>','',x))
#remove line break
spam.train=sapply(spam.train,function(x) gsub('(\\[a-z]){1}','',x))
ham.train=sapply(ham.train,function(x) gsub('\\n','',x))
spam.test=sapply(spam.test,function(x) gsub('\\n','',x))
ham.test=sapply(ham.test,function(x) gsub('\\n','',x))


# creating the corpus for spams
create.corpus <- function(data)
    {
        data.corp <- Corpus(VectorSource(data))
        data.corp <- tm_map(data.corp, content_transformer(tolower))

        data.corp <- tm_map(data.corp, removePunctuation)
        data.corp <- tm_map(data.corp, removeNumbers)
        data.corp <- tm_map(data.corp, removeWords, stopwords('english'))
        data.corp <- tm_map(data.corp, stripWhitespace)
    }

spam.corp=create.corpus(spam.train)

generate.probs <- function(data.corp)
    {
# creating the term document matrix
data.tdm <- TermDocumentMatrix(data.corp, control = list(wordLengths=c(4,15), bounds=list(global=c(5,Inf))))


## frequeny of words in the corpus
freq <- rowSums(as.matrix(data.tdm))

# creating a dataframe with words and their frequencies
data.df <- data.frame(word=data.tdm$dimnames$Terms, frequency=freq)

data.mat <- as.matrix(data.tdm)
# adding the percentage of times a term occurs in spam emails, i.e. the probability of
# occurence of term
word.occurences <- sapply(1:nrow(data.mat), function(x){
  length(which(data.mat[x,]>0))/ncol(data.mat)
})

data.df$occurence <- word.occurences

# probability of the word occurence in the entire corpus
word.prob <- sapply(1:nrow(data.mat), function(x){
  data.df$frequency[x]/sum(data.df$frequency)
})

data.df$density <- word.prob
# removing the irrelevant words
data.df <- data.df[-(1:7), ]

return (data.df)
}

spam.df=generate.probs(spam.corp)

# creating the corpus for spams
ham.corp <- create.corpus(ham.train)

#generate probs for ham
ham.df=generate.probs(ham.corp)

# spamicity
match.df=merge(spam.df[c('word','density')],ham.df[c('word','density')],by='word',all.x=F,all.y=F)
colnames(match.df) <- c("word", "Prob.spam", "Prob.ham")
match.df$spamicity=match.df$Prob.spam/(match.df$Prob.spam+match.df$Prob.ham)
match.df$Pr.S_Pr.H <- abs(match.df$Prob.spam-match.df$Prob.ham)
match.df.ord <- match.df[order(c(match.df$Pr.S_Pr.H, match.df$spamicity), decreasing = TRUE), ]
match.df.ord <- match.df.ord[complete.cases(match.df.ord),]

# top 100 words to be used as features for logistic 
top.100.spam.words <- as.character(match.df.ord$word[1:90])

#creating feature vector for logsitc regression
emails=DocumentTermMatrix(c(spam.corp,ham.corp),control = list(dictionary=top.100.spam.words))
emails=data.frame(as.matrix(emails))
emails[emails>0]=1

emails$Spam.Ind=1
emails$Spam.Ind[(length(spam.train)+1):nrow(emails)]=0

## running logistic regresion model
lo1=glm(Spam.Ind~.,emails,family='binomial')


## running lasso logistic model
x=model.matrix(Spam.Ind~.,emails)
x=x[,-1]
ls1=cv.glmnet(x,emails$Spam.Ind,nfolds=5,alpha=1,family='binomial')


# on test data
emails.test=c(spam.test,ham.test)
emails.test.corp=create.corpus(emails.test)
emails.test.tdm=DocumentTermMatrix(emails.test.corp,control = list(dictionary=top.100.spam.words))
emails.test.df=data.frame(as.matrix(emails.test.tdm))
emails.test.df[emails.test.df>0]=1
emails.test.df$Spam.Ind=1
emails.test.df$Spam.Ind[(length(spam.test)+1):nrow(emails.test.df)]=0
p=predict(lo1,emails.test.df,type='response')
p=as.vector(round(p,0))

# metrics
table(round(p,0),emails.test.df[,'Spam.Ind'])
#       0    1
#  0 1650  144
#  1   50  554
