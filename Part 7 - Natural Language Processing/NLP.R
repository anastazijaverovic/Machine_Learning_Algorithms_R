# Natural Language Processing

# Libraries

# corpus
install.packages("slam", type = "binary")
install.packages('tm')
library(tm)

# stopwords()
install.packages('SnowballC')
library(SnowballC)

# Importing the dataset

# get a dataset with columns separated by a tab
# a sentence can contain a comma

dataset = read.delim('Restaurant_Reviews.tsv',
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
#tm_map - update the corpus
#content_transformer - transforming function for all the words in the corpus

corpus = tm_map(corpus, content_transformer(tolower))

as.character(corpus[[1]]) #to view the first element of the corpus

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

