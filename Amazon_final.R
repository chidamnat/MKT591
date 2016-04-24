setwd('/Users/chidam/Desktop/Official Course documents/Semester 2/MKT591/Amazon/')

#Loading Amazon Reviews DataSet
Amzreviews = read.csv("Amazon_reviews.csv")
str(Amzreviews)
Amzproducts = read.csv("Amazon_Products.csv")
str(Amzproducts)
#Selecting Only the required columns
reviews = Amzreviews[,c(1,2,3,4,5,6,7)]
str(reviews)
#Merging the review heading and the review
reviews$review=paste(reviews$rev_heading, reviews$review, sep = ",")

#Converting ratings into classes
#reviews$rev_rating[reviews$rev_rating == 5] = 5
#reviews$rev_rating[reviews$rev_rating == 4] = 5
#reviews$rev_rating[reviews$rev_rating == 3] = 3
#reviews$rev_rating[reviews$rev_rating == 2] = 1
#reviews$rev_rating[reviews$rev_rating == 1] = 1
reviews$rev_rating = as.factor(reviews$rev_rating)

#Text mining & Building Corpus
library(tm)
library(quanteda)
myCorpus = Corpus(VectorSource(reviews$review))
myCorpus = corpus(myCorpus)
mydfm = dfm(myCorpus, verbose = TRUE, toLower = TRUE, removeNumbers = TRUE,
            removePunct = TRUE, removeSeparators = TRUE, 
            stem = TRUE, ignoredFeatures = c(stopwords("english")), ngrams = 1:3)
dtm = as.DocumentTermMatrix(mydfm, weighting = weightTfIdf)
dtm2 = removeSparseTerms(dtm, 0.97)
#Splitting the data into train & test
library(caTools)
library(e1071)
Label = as.data.frame(as.matrix(dtm2))
Label$rev_rating = reviews$rev_rating
Label$prod_id = reviews$prod_id
Label$review_id = reviews$review_id
set.seed(100)

#sample = sample.split(Label$rev_rating, SplitRatio = 0.75)
sample = sample.split(Label, SplitRatio = 0.75)
train = subset(Label, sample == TRUE)
test = subset(Label, sample == FALSE)

#NaiveBayes Model
model = naiveBayes(rev_rating ~ .-(prod_id+review_id), data = train, laplace = 3)
summary(model)
pred = predict(model,test)
table(test$rev_rating, pred)
sum(diag(table(test$rev_rating, pred)))
pred1 = predict(model, Label)

#Adding predictions
Amzreviews$prediction = pred1

#Joining reviews and products datasets
library(dplyr)
library(sqldf)
library(gsubfn)
library(proto)
library(RSQLite)
library(DBI)
cond = 'select * from Amzproducts left outer join Amzreviews on Amzproducts.prod_id = Amzreviews.prod_id'
Amzcons = sqldf(cond)

unique(Amzcons$prod_brand)

## Brand Traction in the Market:
brands<-data.frame(table(Amzcons$prod_brand))
library(ggplot2)
names(brands) <- c('Brands','Freq')
ggplot(data=brands, aes(x=Brands, y=Freq, fill=Brands)) +
geom_bar(stat="identity", position="dodge")

## Brand wise sentiments stats
brand_pred <- data.frame(table(Amzcons$prod_brand, Amzcons$prediction))
names(brand_pred) <-c('brand_name','rating','counts')
brand_pred<-sqldf('select brand_name,counts*rating as Total_ratings from brand_pred
      group by brand_name')

ggplot(data=brand_pred, aes(x=brand_name, y=Total_ratings, fill=brand_name)) +
  geom_bar(stat="identity", position="dodge")
  
## Most pouplar phone in each brand
most_pop_in_each_brand <- sqldf('select A.prod_brand, A.prod_name, A.rev_count
      from Amzproducts A join 
      (select max(B.rev_count) as max_count, B.prod_name
      from Amzproducts B
      group by B.prod_brand) T
      on A.rev_count = T.max_count and
      A.prod_name = T.prod_name')
ggplot(data=most_pop_in_each_brand, aes(x=prod_brand, y=rev_count, fill=prod_brand)) +
  geom_bar(stat="identity", position="dodge")
      

#WORDCLOUD - APPLE

apple = Amzcons[Amzcons$prod_brand == "Apple",]

#Converting the text into vector source and setting up corpus
myCorpus = Corpus(VectorSource(apple$review))
myCorpus = corpus(myCorpus)
mydfm = dfm(myCorpus, verbose = TRUE, toLower = TRUE, removeNumbers = TRUE,
            removePunct = TRUE, removeSeparators = TRUE, 
            stem = FALSE, ignoredFeatures = c(stopwords("english")))
dtm = as.DocumentTermMatrix(mydfm, weighting = weightTfIdf)
dtm2 = removeSparseTerms(dtm, 0.97)
dtm2 = as.matrix(dtm2)

#Finding the Frequency
freq = colSums(dtm2)
freq = sort(freq, decreasing = TRUE)

library(wordcloud)

words = names(freq)

wordcloud(words[1:30], freq[1:30], random.color = FALSE, colors = "BLUE")

#WORDCLOUD - ASUS

asus = Amzcons[Amzcons$prod_brand == "Asus",]

#Converting the text into vector source and setting up corpus
myCorpus = Corpus(VectorSource(asus$review))
myCorpus = corpus(myCorpus)
mydfm = dfm(myCorpus, verbose = TRUE, toLower = TRUE, removeNumbers = TRUE,
            removePunct = TRUE, removeSeparators = TRUE, 
            stem = FALSE, ignoredFeatures = c(stopwords("english")))
dtm = as.DocumentTermMatrix(mydfm, weighting = weightTfIdf)
dtm2 = removeSparseTerms(dtm, 0.97)
dtm2 = as.matrix(dtm2)

#Finding the Frequency
freq = colSums(dtm2)
freq = sort(freq, decreasing = TRUE)



words = names(freq)

wordcloud(words[1:30], freq[1:30], random.color = FALSE, colors = "BLUE")

#WORDCLOUD - HuaWei

Huawei = Amzcons[Amzcons$prod_brand == "Huawei",]
#Converting the text into vector source and setting up corpus
myCorpus = Corpus(VectorSource(Huawei$review))
myCorpus = corpus(myCorpus)
mydfm = dfm(myCorpus, verbose = TRUE, toLower = TRUE, removeNumbers = TRUE,
            removePunct = TRUE, removeSeparators = TRUE, 
            stem = FALSE, ignoredFeatures = c(stopwords("english")))
dtm = as.DocumentTermMatrix(mydfm, weighting = weightTfIdf)
dtm2 = removeSparseTerms(dtm, 0.97)
dtm2 = as.matrix(dtm2)

#Finding the Frequency
freq = colSums(dtm2)
freq = sort(freq, decreasing = TRUE)


words = names(freq)

wordcloud(words[1:30], freq[1:30], random.color = FALSE, colors = "BLUE")

#WORDCLOUD - NOKIA

Nokia = Amzcons[Amzcons$prod_brand == "Nokia",]
#Converting the text into vector source and setting up corpus
myCorpus = Corpus(VectorSource(Nokia$review))
myCorpus = corpus(myCorpus)
mydfm = dfm(myCorpus, verbose = TRUE, toLower = TRUE, removeNumbers = TRUE,
            removePunct = TRUE, removeSeparators = TRUE, 
            stem = FALSE, ignoredFeatures = c(stopwords("english")))
dtm = as.DocumentTermMatrix(mydfm, weighting = weightTf)
dtm2 = removeSparseTerms(dtm, 0.97)
dtm2 = as.matrix(dtm2)

#Finding the Frequency
freq = colSums(dtm2)
freq = sort(freq, decreasing = TRUE)


words = names(freq)

wordcloud(words[1:30], freq[1:30], random.color = FALSE, colors = "BLUE")

#WORDCLOUD - BlackBerry

Blackberry = Amzcons[Amzcons$prod_brand == "BlackBerry",]

#Converting the text into vector source and setting up corpus
myCorpus = Corpus(VectorSource(Blackberry$review))
myCorpus = corpus(myCorpus)
mydfm = dfm(myCorpus, verbose = TRUE, toLower = TRUE, removeNumbers = TRUE,
            removePunct = TRUE, removeSeparators = TRUE, 
            stem = FALSE, ignoredFeatures = c(stopwords("english")))
dtm = as.DocumentTermMatrix(mydfm, weighting = weightTfIdf)
dtm2 = removeSparseTerms(dtm, 0.97)
dtm2 = as.matrix(dtm2)

#Finding the Frequency
freq = colSums(dtm2)
freq = sort(freq, decreasing = TRUE)


words = names(freq)

wordcloud(words[1:30], freq[1:30], random.color = FALSE, colors = "BLUE")

#WORDCLOUD - LG

LG = Amzcons[Amzcons$prod_brand == "LG",]

#Converting the text into vector source and setting up corpus
myCorpus = Corpus(VectorSource(LG$review))
myCorpus = corpus(myCorpus)
mydfm = dfm(myCorpus, verbose = TRUE, toLower = TRUE, removeNumbers = TRUE,
            removePunct = TRUE, removeSeparators = TRUE, 
            stem = FALSE, ignoredFeatures = c(stopwords("english")))
dtm = as.DocumentTermMatrix(mydfm, weighting = weightTfIdf)
dtm2 = removeSparseTerms(dtm, 0.97)
dtm2 = as.matrix(dtm2)

#Finding the Frequency
freq = colSums(dtm2)
freq = sort(freq, decreasing = TRUE)


words = names(freq)

wordcloud(words[1:30], freq[1:30], random.color = FALSE, colors = "BLUE")

#WORDCLOUD - Motorola

mtr = Amzcons[Amzcons$prod_brand == "Motorola",]

#Converting the text into vector source and setting up corpus
myCorpus = Corpus(VectorSource(mtr$review))
myCorpus = corpus(myCorpus)
mydfm = dfm(myCorpus, verbose = TRUE, toLower = TRUE, removeNumbers = TRUE,
            removePunct = TRUE, removeSeparators = TRUE, 
            stem = FALSE, ignoredFeatures = c(stopwords("english")))
dtm = as.DocumentTermMatrix(mydfm, weighting = weightTfIdf)
dtm2 = removeSparseTerms(dtm, 0.97)
dtm2 = as.matrix(dtm2)
#Finding the Frequency
freq = colSums(dtm2)
freq = sort(freq, decreasing = TRUE)

words = names(freq)

wordcloud(words[1:30], freq[1:30], random.color = FALSE, colors = "BLUE")

#WORDCLOUD - Samsung

Samsung = Amzcons[Amzcons$prod_brand == "Samsung",]
#Converting the text into vector source and setting up corpus
myCorpus = Corpus(VectorSource(Samsung$review))
myCorpus = corpus(myCorpus)
mydfm = dfm(myCorpus, verbose = TRUE, toLower = TRUE, removeNumbers = TRUE,
            removePunct = TRUE, removeSeparators = TRUE, 
            stem = FALSE, ignoredFeatures = c(stopwords("english")))
dtm = as.DocumentTermMatrix(mydfm, weighting = weightTfIdf)
dtm2 = removeSparseTerms(dtm, 0.97)
dtm2 = as.matrix(dtm2)
#Finding the Frequency
freq = colSums(dtm2)
freq = sort(freq, decreasing = TRUE)

words = names(freq)

wordcloud(words[1:30], freq[1:30], random.color = FALSE, colors = "BLUE")

#WORDCLOUD - Sony

Sony = Amzcons[Amzcons$prod_brand == "Sony",]
#Converting the text into vector source and setting up corpus
myCorpus = Corpus(VectorSource(Sony$review))
myCorpus = corpus(myCorpus)
mydfm = dfm(myCorpus, verbose = TRUE, toLower = TRUE, removeNumbers = TRUE,
            removePunct = TRUE, removeSeparators = TRUE, 
            stem = FALSE, ignoredFeatures = c(stopwords("english")))
dtm = as.DocumentTermMatrix(mydfm, weighting = weightTfIdf)
dtm2 = removeSparseTerms(dtm, 0.97)
dtm2 = as.matrix(dtm2)
#Finding the Frequency
freq = colSums(dtm2)
freq = sort(freq, decreasing = TRUE)


words = names(freq)

wordcloud(words[1:50], freq[1:50], random.color = FALSE, colors = "BLUE")

