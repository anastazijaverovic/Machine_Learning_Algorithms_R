# Natural Language Processing

# Libraries

# corpus
install.packages("slam", type = "binary")
install.packages('tm')
library(tm)

# stopwords()
install.packages('SnowballC')
library(SnowballC)

# splitting the dataset
library(caTools)

# classification model
library(randomForest)

# Importing the dataset

dataset = read.delim('Restaurant_Reviews.tsv', # get a dataset with columns separated by a tab, a sentence can contain a comma
                     quote = '', #ignore quotes in the text
                     stringsAsFactors = FALSE #reviews shouldn't be identified as factors (categorical)
                     )


# Cleaning the review texts

# create the independent variables to predict the dependent variable
# dependent variable = liked
# independent variables = distinct words

# distinct words = columns
# count the number of times the word goes with which review


corpus = VCorpus(VectorSource(dataset$Review))  #we won't clean the reviews directly in the dataset - new dataset only for reviews = corpus

# 1. words -> lower cases
#so we don't get two versions of the same word in lower and upper case
corpus = tm_map(corpus, content_transformer(tolower)) #content_transformer - transforming function for all the words in the corpus

as.character(corpus[[1]]) #view the first element of the corpus

# 2. numbers -> remove
corpus = tm_map(corpus, removeNumbers)

as.character(corpus[[841]]) #review with the number in it

# 3. punctuation -> remove
corpus = tm_map(corpus, removePunctuation)

# 4. non-relevant words -> remove
corpus = tm_map(corpus, removeWords, stopwords())

# 5. stemming - reduce the total number of words -> getting the root of each word
corpus = tm_map(corpus, stemDocument)

# 6. extra spaces -> remove (extra spaces left from removing numbers for example)
corpus = tm_map(corpus, stripWhitespace)


# Creating the Bag of Words model

# sparse matrix (very few non-zero values)
dtm = DocumentTermMatrix(corpus) #dtm = sparse matrix

# filter - the most frequent words from dtm
dtm = removeSparseTerms(dtm, 0.999) # we want to keep 99.9% of the most frequent words, the smaller the number of reviews -> the bigger the proportion


# Classification model (Naive Bayes/Decision Tree/Random Forest)
# Random Forest

#dataset is a dataframe, not a matrix
dataset_cl = as.data.frame(as.matrix(dtm))

#add dependent variable as column
dataset_cl$Liked = dataset$Liked

# Encoding the tagret feature as a factor
dataset_cl$Liked = factor(dataset_cl$Liked, levels = c(0,1))

# Splitting the dataset into the Training set and Test set
set.seed(123)
split = sample.split(dataset_cl$Liked, SplitRatio = 0.8)
training_set = subset(dataset_cl, split == TRUE)
test_set = subset(dataset_cl, split == FALSE)

# Fitting Random Forest Classification to the Training set
classifier = randomForest(x = training_set[-692], #training set without the dependent variable
                          y = training_set$Liked,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-692])

# Making the Confusion Matrix
cm = table(test_set[,692], y_pred)

# Acuraccy = (82+77)/200 = 0.795
