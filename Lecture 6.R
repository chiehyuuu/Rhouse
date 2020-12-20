##Textual data
## ------------------------------------------------------------------------
## load two required libraries
library(tm, SnowballC)

## load the raw corpus #???????????????fp??????
corpus.raw=VCorpus(DirSource(directory="C:/Users/Ann/Desktop/federalist/federalist", pattern = "fp")) 


corpus.raw
#list can include data of different types and length
#give me everything from #10
is.list(corpus.raw)
corpus.raw[[10]][]

#data cleansing 
## make lower case, turn upper cases to lower
corpus.prep=tm_map(corpus.raw, content_transformer(tolower)) 
## remove white space
corpus.prep=tm_map(corpus.prep, stripWhitespace) 
## remove punctuation 
corpus.prep=tm_map(corpus.prep, removePunctuation)
## remove numbers
corpus.prep=tm_map(corpus.prep, removeNumbers) 


head(stopwords("english"))

## remove stop words 
corpus=tm_map(corpus.prep, removeWords, stopwords("english")) 

## finally stem remaining words
corpus =tm_map(corpus, stemDocument) 

## the output is truncated here to save space
content(corpus[[10]]) # Essay No. 10


###Document-Term Matrix #??????????????????????????????(sparse)+?????????????????????
dtm=DocumentTermMatrix(corpus)
dtm

inspect(dtm[1:5, 1:8])

dtm.mat=as.matrix(dtm)


## ?????????------------------------------------------------------------------------
library(wordcloud)
wordcloud(colnames(dtm.mat), dtm.mat[12, ], max.words = 20)  # essay No. 12
wordcloud(colnames(dtm.mat), dtm.mat[24, ], max.words = 20)  # essay No. 24

stemCompletion(c("revenu", "commerc", "peac", "army"), corpus.prep)

# tf-idf calculation
dtm.tfidf=weightTfIdf(dtm)
dtm.tfidf.mat=as.matrix(dtm.tfidf)  # convert to matrix

## 10 most important words for Paper No. 12
head(sort(dtm.tfidf.mat[12, ], decreasing = TRUE), n = 10)

## 10 most important words for Paper No. 24
head(sort(dtm.tfidf.mat[24, ], decreasing = TRUE), n = 10)


## subset The Federalist papers written by Hamilton
hamilton=c(1, 6:9, 11:13, 15:17, 21:36, 59:61, 65:85)

## essays written by Madison
madison=c(10, 14, 37:48, 58)


## ------------------------------------------------------------------------
## document-term matrix converted to matrix for manipulation 
dtm1=as.matrix(DocumentTermMatrix(corpus.prep)) 
tfm=dtm1/rowSums(dtm1) * 1000 # term frequency per 1000 words,(??????1000??????????????????) rowsum???????????? #???????????????
dim(tfm)
summary(tfm[1,])

## words of interest
words = c("although", "always", "commonly", "consequently",
           "considerable", "enough", "there", "upon", "while", "whilst")

## select only these words
tfm = tfm[, words]


## average among Hamilton/Madison essays
tfm.ave = rbind(colSums(tfm[hamilton, ]) / length(hamilton), 
                colSums(tfm[madison, ]) / length(madison))

tfm.ave

author = rep(NA, nrow(dtm1)) # a vector with missing values
author[hamilton] = 1  # 1 if Hamilton
author[madison] = -1  # -1 if Madison

## data frame for regression
author.data = data.frame(author = author[c(hamilton, madison)], 
                          tfm[c(hamilton, madison), ])

hm.fit = lm(author ~ upon + there + consequently + whilst, 
             data = author.data)
hm.fit

hm.fitted = fitted(hm.fit) # fitted values
sd(hm.fitted)  

## ------------------------------------------------------------------------
## proportion of correctly classified essays by Hamilton
mean(hm.fitted[author.data$author == 1] > 0)

## proportion of correctly classified essays by Madison
mean(hm.fitted[author.data$author == -1] < 0)

n = nrow(author.data)
hm.classify = rep(NA, n) # a container vector with missing values 

for (i in 1:n) {
    ## fit the model to the data after removing the ith observation
    sub.fit = lm(author ~ upon + there + consequently + whilst, 
                  data = author.data[-i, ]) # exclude ith row
    ## predict the authorship for the ith observation
    hm.classify[i]= predict(sub.fit, newdata = author.data[i, ])
}

## proportion of correctly classified essays by Hamilton
mean(hm.classify[author.data$author == 1] > 0)

## proportion of correctly classified essays by Madison
mean(hm.classify[author.data$author == -1] < 0)

disputed = c(49, 50:57, 62, 63) # 11 essays with disputed authorship
tf.disputed = as.data.frame(tfm[disputed, ])

## prediction of disputed authorship
pred = predict(hm.fit, newdata = tf.disputed)
pred # predicted values

## fitted values for essays authored by Hamilton; red squares
plot(hamilton, hm.fitted[author.data$author == 1], pch = 15, 
     xlim = c(1, 85), ylim  = c(-2, 2), col = "red", 
     xlab = "Federalist Papers", ylab = "Predicted values")
abline(h = 0, lty = "dashed")

## essays authored by Madison; blue circles
points(madison, hm.fitted[author.data$author == -1], 
       pch = 16, col = "blue")

## disputed authorship; black triangles
points(disputed, pred, pch = 17) 



