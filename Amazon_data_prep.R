setwd('/Users/chidam/Desktop/Official Course documents/Semester 2/MKT591/Amazon/')
## reading the products and reviews datasets
products <- read.csv('Amazon_Products.csv',head=TRUE)
reviews <- read.csv('Amazon_reviews.csv', head = TRUE)
reviews$review=paste(reviews$rev_heading, reviews$review, sep = ",")
## loading the tm package
library(tm)
library(quanteda)
library(slam)
library(Matrix)

myCorpus = Corpus(VectorSource(reviews$review))
myCorpus = corpus(myCorpus)
mydfm <- dfm(myCorpus, 
             ignoredFeatures = c(stopwords('english')),
             toLower = T,
             removeNumbers = TRUE,
             removePunct = T,
             removeSeparators = TRUE,
             stem = TRUE,
             ngrams=1:2)
dtm = as.DocumentTermMatrix(mydfm, weighting=weightTfIdf)
dtm2 = removeSparseTerms(dtm,sparse = 0.97)
dtm2

#creating data frame
labeledTerms = as.data.frame(as.matrix(dtm2))
labeledTerms$rev_rating <- as.factor(reviews$rev_rating)
#labeledTerms$polarity <- as.factor(reviews$polarity)
#labeledTerms$pleasantness <- as.factor(reviews$pleasantness)
#labeledTerms$sensitivity <- as.factor(reviews$sensitivity)
#labeledTerms$attention <- as.factor(reviews$attention)
#labeledTerms$aptitude <- as.factor(reviews$aptitude)

   
#labeledTerms <-cbind(labeledTerms,reviews[,5:ncol(reviews)])
#labeledTerms = labeledTerms[,-which(names(labeledTerms)=='concepts')]
#labeledTerms = labeledTerms[,-which(names(labeledTerms)=='aspects')]

str(labeledTerms)

#Build train & test dataset
library(caTools)
set.seed(144)
spl = sample.split(labeledTerms$rev_rating,0.75)
train = subset(labeledTerms,spl == TRUE)
test = subset(labeledTerms,spl == FALSE)

# CART using RPART
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)

library(e1071)
set.seed(100)

#reviewCART = rpart(rev_rating ~ .,data = train, method = 'class')
#prp(reviewCART)
#reviewRFT = randomForest(rev_rating ~ .,data = train, method = 'class')

#reviewRFT = train(rev_rating ~.,data = train,method='rf',trControl = trainControl(method = 'cv',number = 1,repeats = 5))

#predictRF = predict(reviewRFT , newdata = test)

#NaiveBayes Model
model = naiveBayes(rev_rating ~ ., data = train, laplace = 3)
summary(model)
pred = predict(model,test)
table(test$rev_rating, pred)
sum(diag(table(test$rev_rating, pred)))
sum(diag(table(test$rev_rating, pred)))/nrow(test)
save(model, file = "mymodel.rda")
write.csv(train, file = "my_train.csv")


