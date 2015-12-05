## DATA EXTRACTION AND CLEANING

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

## text cleaning
# removing html tags
spam.emails=sapply(spam.emails,function(x) gsub('<[^>]*>','',x))
ham.emails=sapply(ham.emails,function(x) gsub('<[^>]*>','',x))


# removing line breaks
spam.emails=sapply(spam.emails,function(x) gsub('\\n','',x))
ham.emails=sapply(ham.emails,function(x) gsub('\\n','',x))

## dividing into train and test data
# keeping a spam ratio of arnd 20% in both training and test data

spam.emails.mod=spam.emails[sample(1:length(spam.emails),length(spam.emails)*2/3)]

# dividing into trainng and test sets in the ratio 3:1
spam.ind=sample(1:length(spam.emails.mod),ceiling(length(spam.emails.mod)*3/4))
spam.train=spam.emails.mod[spam.ind]
spam.test=spam.emails.mod[-spam.ind]


ham.ind=sample(1:length(ham.emails),ceiling(length(ham.emails)*3/4))
ham.train=ham.emails[ham.ind]
ham.test=ham.emails[-ham.ind]
