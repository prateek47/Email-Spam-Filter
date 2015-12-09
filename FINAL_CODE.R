# creating the model

#list of libraries used
library(kernlab)
library(ROCR)
library(klaR)

# creation of data
source("ProjCod_data.R")

#--------------------------------------------
# Support Vector Machine

# Applying linear kernal
#svm.fit <- ksvm(Spam.Ind~ ., data = tr.emails.df, type="C-svc", kernel="vanilladot", C=100)
svm.fit1 <- ksvm(Spam.Ind~ ., data = tr.emails.df, type="C-svc", kernel="vanilladot",cross=3, C=100, prob.model= TRUE)
# applying k-fold cross validation, with k=3, k
# Training error : 0.061845 
# Cross validation error : 0.073858 
# Number of Support Vectors : 620 (cost value is 100).

# Predictions using the model

# finding the labels
svm.predict <- predict(svm.fit1, newdata= emails.test.df)
# finding the probabilities
svm.predict2 <- predict(svm.fit1, newdata= emails.test.df, type= "probabilities")

# accuracy
accuracy <- sum(svm.predict == emails.test.df[,"Spam.Ind"])/ nrow(emails.test.df)
# .9302

# creating a dataframe of label with their probabilities
df <- data.frame(label=svm.predict, prob0=svm.predict2[,1], prob1=svm.predict2[,2])
df$probabilities <- apply(df[,2:3], 1, max)

# plotting ROC curve
# 
pred <- prediction(df$probabilities, df$label)
# calculating the performance
perf <- performance(pred, "tpr", "fpr")

# ROC curve
plot(perf,colorize = TRUE, main= "Linear Kernel with cost=100") 

# AUC value
auc <- attr(performance(pred ,"auc"), "y.values")
#0.492
#-------------------------------------------------------
# Another model with different cost value

svm.fit2 <- ksvm(Spam.Ind~ ., data = tr.emails.df, type="C-svc", kernel="vanilladot",cross=3, C=10, prob.model= TRUE)
# applying k-fold cross validation, with k=3,
# Number of Support Vectors : 610 , cost =10
# Training error : 0.060056 
# Cross validation error : 0.07488 
# Predictions using the model

# finding the labels
svm.predict3 <- predict(svm.fit2, newdata= emails.test.df)
# finding the probabilities
svm.predict4 <- predict(svm.fit2, newdata= emails.test.df, type= "probabilities")

# accuracy
accuracy1 <- sum(svm.predict3 == emails.test.df[,"Spam.Ind"])/ nrow(emails.test.df)
# .9302

# creating a dataframe of label with their probabilities
df1 <- data.frame(label=svm.predict3, prob0=svm.predict4[,1], prob1=svm.predict4[,2])
df1$probabilities <- apply(df1[,2:3], 1, max)

# plotting ROC curve
# 
pred1 <- prediction(df1$probabilities, df1$label)
# calculating the performance
perf1 <- performance(pred1, "tpr", "fpr")

# ROC curve
plot(perf1,colorize = TRUE, main= "Linear Kernel with cost=10") 

# AUC value
auc1 <- attr(performance(pred1 ,"auc"), "y.values")
# 0.478
#--------------------------------

# Another model with Radial Basis kernel "Gaussian"

svm.fit3 <- ksvm(Spam.Ind~ ., data = tr.emails.df, type="C-svc", kernel="rbfdot", kpar="automatic", cross=3, C=10, prob.model= TRUE)
# applying k-fold cross validation, with k=3,
# Number of Support Vectors : 1227, cost=10
# Training error : 0.023 
# Cross validation error : 0.062099

# finding the labels
svm.predict5 <- predict(svm.fit3, newdata= emails.test.df)
# finding the probabilities
svm.predict6 <- predict(svm.fit3, newdata= emails.test.df, type= "probabilities")

# accuracy
accuracy2 <- sum(svm.predict5 == emails.test.df[,"Spam.Ind"])/ nrow(emails.test.df)
# .9547

# creating a dataframe of label with their probabilities
df2 <- data.frame(label=svm.predict5, prob0=svm.predict6[,1], prob1=svm.predict6[,2])
df2$probabilities <- apply(df2[,2:3], 1, max)

# plotting ROC curve
# 
pred2 <- prediction(df2$probabilities, df2$label)
# calculating the performance
perf2 <- performance(pred2, "tpr", "fpr")

# ROC curve
plot(perf2,colorize = TRUE, main= "Radial Basis kernel 'Gaussian' with C=10") 

# AUC value
auc2 <- attr(performance(pred2 ,"auc"), "y.values")
# 0.412
#-----------------------------------------

# Naive Bayes

# taking the prior distribution as 0.2
naive.bayes1 <- NaiveBayes(tr.emails.df[,-101], grouping = as.factor(tr.emails.df$Spam.Ind), 
                           prior=.2)

# predicting using the naive bayes
nb.prediction <- predict(naive.bayes1, emails.test.df[,-101], type= 'raw')
nb.pred <- data.frame(nb.prediction)
nb.pred$probabilities <- apply(nb.pred[,2:3], 1, max)
# taking the probabilities of spams
score.nb <- nb.pred$posterior.1
# the actual labels in the testing data
actual_class <- emails.test.df$Spam.Ind
# 
# calculating the prediction for ROC
pred <- prediction(score.nb, actual_class)
perf <- performance(pred, "tpr", "fpr")
# plotting the ROC
plot(perf,colorize = TRUE, main="The Naive Bayes Classifier") 
# calculating the AUC value
auc <- attr(performance(pred ,"auc"), "y.values")
# 0.91

# The raw accuracy
accuracy.nb <- sum(nb.pred$class == emails.test.df[,"Spam.Ind"])/ nrow(emails.test.df)
# 87.96

#------------------------

# Logistic Regression

## running logistic regresion model
lo1=glm(Spam.Ind~.,tr.emails.df,family='binomial')


## running lasso logistic model
x=model.matrix(Spam.Ind~.,emails)
x=x[,-1]
ls1=cv.glmnet(x,emails$Spam.Ind,nfolds=5,alpha=1,family='binomial')


## creating test data
emails.test=c(spam.test,ham.test)
emails.test.corp=create.corpus(emails.test)
emails.test.corp <- corpus(emails.test.corp)
emails.test.dfm <- dfm(emails.test.corp, verbose = FALSE, toLower = TRUE, removeNumbers = TRUE, removePunct = TRUE, valuetype = "glob", ignoredFeatures = stopwords("english"))

emails.test.df <- data.frame(as.matrix(applyDictionary(emails.test.dfm, synDict, valuetype = "glob")))

emails.test.df[emails.test.df>0]=1

# creating indicator variable for evaluating test data later
emails.test.df$Spam.Ind=1
emails.test.df$Spam.Ind[(length(spam.test)+1):nrow(emails.test.df)]=0

# predictions
logit.p=predict(lo1,emails.test.df,type='response')

# evaluating model -metrics
t=table(round(logit.p,0),emails.test.df[,'Spam.Ind'])
colnames(t)=c('Actual Ham','Actual Spam')
rownames(t)=c('Predicted Ham','Predicted Spam')
t


# ROC curve

pred=prediction(logit.p,emails.test.df$Spam.Ind)
perf=performance(pred,measure = "tpr", x.measure = "fpr")
plot(perf,col=rainbow(10),colorize=T)
logit.auc=performance(pred,measure='auc')



#------------------------

# LDA

lda.fit=lda(Spam.Ind~.,tr.emails.df)
lda.pred=predict(lda.fit,emails.test.df,type='response')
lda.p=lda.pred$posterior[,2]

# evaluating model -metrics
t=table(round(lda.p,0),emails.test.df[,'Spam.Ind'])
colnames(t)=c('Actual Ham','Actual Spam')
rownames(t)=c('Predicted Ham','Predicted Spam')
t

# ROC curve

pred=prediction(lda.p,emails.test.df$Spam.Ind)
perf=performance(pred,measure = "tpr", x.measure = "fpr")
plot(perf,col=rainbow(10),colorize=T)
lda.auc=performance(pred,measure='auc')



