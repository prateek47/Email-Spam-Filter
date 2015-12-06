library(tm)
library(quanteda)
library(wordnet)


# creating the corpus for spams
create.corpus <- function(data)
    {
        data.corp <- Corpus(VectorSource(data))
        data.corp <- tm_map(data.corp, content_transformer(tolower))
t
        data.corp <- tm_map(data.corp, removePunctuation)
        data.corp <- tm_map(data.corp, removeNumbers)
        data.corp <- tm_map(data.corp, removeWords, stopwords('english'))
        data.corp <- tm_map(data.corp, stripWhitespace)
    }


generate.probs <- function(data.corp)
    {
         # creating the term document matrix
        data.tdm <- TermDocumentMatrix(data.corp,
                                       control = list(wordLengths=c(4,15),
                                       bounds=list(global=c(5,Inf))))


        ## frequeny of words in the corpus
        freq <- rowSums(as.matrix(data.tdm))

         # creating a dataframe with words and their frequencies
        data.df <- data.frame(word=data.tdm$dimnames$Terms,
                              frequency=freq)

        data.mat <- as.matrix(data.tdm)
        # adding the percentage of times a term occurs in spam emails, i.e. the probability of
        # occurence of term
        word.occurences <- sapply(1:nrow(data.mat),
                                  function(x){
                                      length(which(data.mat[x,]>0))/ncol(data.mat)
                                  })

        data.df$occurence <- word.occurences

         # probability of the word occurence in the entire corpus
        word.prob <- sapply(1:nrow(data.mat),
                            function(x){
                                data.df$frequency[x]/sum(data.df$frequency)
                            })

        data.df$density <- word.prob
         # removing the irrelevant words
        data.df <- data.df[-(1:7), ]

        return (data.df)
    }


spam.corp=create.corpus(spam.train)
spam.df=generate.probs(spam.corp)

# creating the corpus for hams
ham.corp <- create.corpus(ham.train)

#generate probs for ham
ham.df=generate.probs(ham.corp)

# calculating spamicity
match.df=merge(spam.df[c('word','density')],ham.df[c('word','density')],by='word',all.x=F,all.y=F)
colnames(match.df) <- c("word", "Prob.spam", "Prob.ham")
match.df$spamicity=match.df$Prob.spam/(match.df$Prob.spam+match.df$Prob.ham)
match.df$Pr.S_Pr.H <- abs(match.df$Prob.spam-match.df$Prob.ham)
match.df.ord <- match.df[order(c(match.df$Pr.S_Pr.H, match.df$spamicity), decreasing = TRUE), ]
match.df.ord <- match.df.ord[complete.cases(match.df.ord),]

# top 100 words to be used as features for logistic 
top.100.spam.words <- as.character(match.df.ord$word[1:100])

# addding synonyms for these top 100 words
verbs=sapply(top.100.spam.words,function (x) {synonyms(x,"VERB")})
nouns=sapply(top.100.spam.words,function (x) {synonyms(x,"NOUN")})


syns=mapply(c,verbs,nouns,top.100.spam.words)
synDict=dictionary(syns)

#creating feature vector for modelling
tr.emails <- corpus(spam.corp)+corpus(ham.corp)
tr.emails.dfm <- dfm(emails.tr, verbose = FALSE, toLower = TRUE, removeNumbers = TRUE,
            removePunct = TRUE, valuetype = "glob", ignoredFeatures = stopwords("english"))

tr.emails.df <- data.frame(as.matrix(applyDictionary(tr.emails.dfm, synDict, valuetype = "glob")))
tr.emails.df[tr.emails.df>0]=1
tr.emails.df$Spam.Ind=1
tr.emails.df$Spam.Ind[(length(spam.train)+1):nrow(tr.emails.df)]=0
