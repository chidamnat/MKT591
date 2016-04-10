#mainscript.R
maintitle = function(sent_text){
  
load(file = "mymodel.rda")
summary(model)
ls()
library(tm)
library(quanteda)
library(slam)
library(Matrix)

myCorpus = Corpus(VectorSource(sent_text))
myCorpus = corpus(myCorpus)
mydfm <- dfm(myCorpus, 
             ignoredFeatures = c(stopwords('english')),
             toLower = T,
             removeNumbers = TRUE,
             removePunct = T,
             removeSeparators = TRUE,
             stem = TRUE,
             ngrams=1:2)
dtm = as.DocumentTermMatrix(mydfm)
#dtm2 = removeSparseTerms(dtm,sparse = 0.97)
res = as.data.frame(as.matrix(dtm))
train = read.csv('my_train.csv',header = T)

res = res[,which(names(res) %in% names(train))]

res = as.matrix(res)
pred = predict(model,res)
#pred
}

