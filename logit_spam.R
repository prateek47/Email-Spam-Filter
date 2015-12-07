
library(glmnet)
library(ggplot2)
source('Codes/data.R')
source('Codes/feature_creation.R')



## running logistic regresion model
lo1=glm(Spam.Ind~.,tr.emails.df,family='binomial')


## running lasso logistic model
x=model.matrix(Spam.Ind~.,emails)
x=x[,-1]
ls1=cv.glmnet(x,emails$Spam.Ind,nfolds=5,alpha=1,family='binomial')


# on test data
emails.test=c(spam.test,ham.test)
emails.test.corp=create.corpus(emails.test)
emails.test.corp <- corpus(emails.test.corp)
emails.test.dfm <- dfm(emails.test.corp, verbose = FALSE, toLower = TRUE, removeNumbers = TRUE,
            removePunct = TRUE, valuetype = "glob", ignoredFeatures = stopwords("english"))

emails.test.df <- data.frame(as.matrix(applyDictionary(emails.test.dfm, synDict, valuetype = "glob")))

emails.test.df[emails.test.df>0]=1

emails.test.df$Spam.Ind=1
emails.test.df$Spam.Ind[(length(spam.test)+1):nrow(emails.test.df)]=0

p=predict(lo1,emails.test.df,type='response')
p=as.vector(round(p,0))

# metrics
t=table(round(p,0),emails.test.df[,'Spam.Ind'])
#                  Ham Spam
#  Predicted Ham  1229   56
#  Predicted Spam   34  260


# ROC curve
# threshold values for which plot is to be drawn
thresh=seq(0.2,0.8,0.001)

# tp and fp values for each threshold value
roc.value <- function (x)
    {   pred=p
        pred[pred>x]=1
        pred[pred<x]=0
        t=table(pred,emails.test.df[,'Spam.Ind'])
        tp=t[2,2]
        fp=t[2,1]
        return(c(x,tp,fp))
          }
values=lapply(thresh,roc.value)
values=unlist(values)
values.df=data.frame(threshold=values[c(TRUE,FALSE,FALSE)],tp=values[c(FALSE,TRUE,FALSE)],fp=values[c(FALSE,FALSE,TRUE)])



pl <- ggplot(data=values.df,aes(x=fp,y=tp),geom='smooth',span=0.5)+stat_smooth()
pl <- pl+ geom_point(data=values.df[c(50,300,550),],aes(x=fp,y=tp,colour=as.factor(round(threshold,2))),size=7)
pl <- pl + xlab('False Positive') +ylab('True Positive')
pl <- pl + scale_colour_discrete(name='THRESHOLD VALUE') + ggtitle('ROC CURVE') + theme(plot.title=element_text(face='bold'))
print(pl)













