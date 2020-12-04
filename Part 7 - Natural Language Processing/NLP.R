# Natural Language Processing

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

install.packages("slam", type = "binary")

install.packages('tm')
library(tm)

corpus = VCorpus(VectorSource(dataset$Review))  #we won't clean the reviews directly in the dataset


