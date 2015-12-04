# The Code for Testing SVM

source("ProjCod_spmwrd.R")
source("ProjCod_Feature.R")

# List of Libraries used
library(quanteda)
library(tm)
library(kernlab)

# The Training Corpus and Document-Term Matrix
#---------------------------------------------------

# ---------------- Spam Email----------------------

# creation of spam, document term matrix
spam.C <- corpus(spam.emails)
dfm4 <- dfm(spam.C, verbose = FALSE, toLower = TRUE, removeNumbers = TRUE,
            removePunct = TRUE, valuetype = "glob", ignoredFeatures = stopwords("english"))

spam.dfm <- applyDictionary(dfm4, myDict, valuetype = "glob")

spam.dtm <- as.DocumentTermMatrix(spam.dfm)
spam.dtm.df <- data.frame(as.matrix(spam.dtm))

#-------------------------- Easy_Ham --------------------------

# creation of easy_ham2 document term matrix
easy_ham.C <- corpus(easy_ham.emails)
dfm5 <- dfm(easy_ham.C, verbose = FALSE, toLower = TRUE, removeNumbers = TRUE,
            removePunct = TRUE, valuetype = "glob", ignoredFeatures = stopwords("english"))

easy_ham.dfm <- applyDictionary(dfm5, myDict, valuetype = "glob")

easy_ham.dtm <- as.DocumentTermMatrix(easy_ham.dfm)
easy_ham.dtm.df <- data.frame(as.matrix(easy_ham.dtm))
#---------------------------------------------------------------------------------------------------------------

# The Testing Corpus and Document Term Matrix
#----------------------------------------------

# ---------------- SPAM EMAILS 2----------------------
# extracting the spam2 emails in a vector
spam2.docs <- dir(spam2.data)
spam2.docs <- spam2.docs[which(spam2.docs!= 'cmds')]
spam2.emails <- sapply(spam2.docs, function(x){
  extract_email(paste(spam2.data,x,sep = ""))
})

# creation of spam2 document term matrix
spam2.C <- corpus(spam2.emails)
dfm1 <- dfm(spam2.C, verbose = FALSE, toLower = TRUE, removeNumbers = TRUE,
            removePunct = TRUE, valuetype = "glob", ignoredFeatures = stopwords("english"))

spam2.dfm <- applyDictionary(dfm1, myDict, valuetype = "glob")

spam2.dtm <- as.DocumentTermMatrix(spam2.dfm)
spam2.dtm.df <- data.frame(as.matrix(spam2.dtm))

#-------------------------- Easy_Ham2 --------------------------

# extracting the easy_ham2 emails in a vector
easy_ham2.docs <- dir(easyham2.data)
easy_ham2.docs <- easy_ham2.docs[which(easy_ham2.docs!= 'cmds')]
easy_ham2.emails <- sapply(easy_ham2.docs, function(x){
  extract_email(paste(easyham2.data,x,sep = ""))
})

# creation of easy_ham2 document term matrix
easy_ham2.C <- corpus(easy_ham2.emails)
dfm2 <- dfm(easy_ham2.C, verbose = FALSE, toLower = TRUE, removeNumbers = TRUE,
            removePunct = TRUE, valuetype = "glob", ignoredFeatures = stopwords("english"))

easy_ham2.dfm <- applyDictionary(dfm2, myDict, valuetype = "glob")

easy_ham2.dtm <- as.DocumentTermMatrix(easy_ham2.dfm)
easy_ham2.dtm.df <- data.frame(as.matrix(easy_ham2.dtm))

#-------------------------------- Hard_Ham -----------------------------------------------

# extracting the spam emails in a vector
hard_ham.docs <- dir(hardham.data)
hard_ham.docs <- hard_ham.docs[which(hard_ham.docs!= 'cmds')]
hard_ham.emails <- sapply(hard_ham.docs, function(x){
  extract_email(paste(hardham.data,x,sep = ""))
})

# creation of hard_ham document term matrix
hard_ham.C <- corpus(hard_ham.emails)
dfm3 <- dfm(hard_ham.C, verbose = FALSE, toLower = TRUE, removeNumbers = TRUE,
            removePunct = TRUE, valuetype = "glob", ignoredFeatures = stopwords("english"))

hard_ham.dfm <- applyDictionary(dfm3, myDict, valuetype = "glob")

hard_ham.dtm <- as.DocumentTermMatrix(hard_ham.dfm)
hard_ham.dtm.df <- data.frame(as.matrix(hard_ham.dtm))

#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------

#  Support Vector Machine
#-------------------------
# The training Data
spam.dtm.df$typeofemail <- 0
easy_ham.dtm.df$typeofemail <- 1

# the data frame with only the top 100 words
train.svm <- rbind.fill(spam.dtm.df, easy_ham.dtm.df)
train.svm[is.na(train.svm)] <- 0
#--------------------------------------------------------

# Fitting the SVM model
svm.fit <- ksvm(typeofemail~ ., data = train.svm, type="C-svc", kernel="vanilladot", C=100)

#------------------------------------------------------------------------

# The testing Data of spam and easy_ham
spam2.dtm.df$typeofemail <- 0
easy_ham2.dtm.df$typeofemail <- 1
# the data frame with only the top 100 words
test.svm <- rbind.fill(spam2.dtm.df, easy_ham2.dtm.df)
test.svm[is.na(test.svm)] <- 0
#----------------------------------------------------

# Predicting using the fitted SVM
svm.predict <- predict(svm.fit, newdata= test.svm)

# Metrics
#---------
# calculating the accuracy
accuracy <- sum(svm.predict == test.svm[,"typeofemail"])/ nrow(test.svm) # 88.841

true_values <- svm.predict[svm.predict == test.svm[,"typeofemail"]]
false_values <- svm.predict[!(svm.predict == test.svm[,"typeofemail"])] 
false.positive <- sum(false_values == 1) # 278
false.negative <- sum(false_values == 0) # 34
true.positive <- sum(true_values == 1) # 1366

# calculating the Precision and Recall
precision <- true.positive/(true.positive+false.positive) # 83.09
recall <- true.positive/(true.positive+false.negative) # 97.57

# Testing on Hard_Ham
hard_ham.dtm.df$typeofemail <- 1

# prediction
svm.predict1 <- predict(svm.fit, newdata= hard_ham.dtm.df)

# Metrics
#---------
# calculating the accuracy
accuracy1 <- sum(svm.predict1 == hard_ham.dtm.df[,"typeofemail"])/ nrow(hard_ham.dtm.df)

true_values1 <- svm.predict1[svm.predict1 == hard_ham.dtm.df[,"typeofemail"]] # 26.8
false_values1 <- svm.predict1[!(svm.predict1 == hard_ham.dtm.df[,"typeofemail"])] 

#----------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

save.image("ProjCod_final.RData")
