# Natural Language Processing

# Importing the dataset

# get a dataset with columns separated by a tab
# a sentence can contain a comma

dataset = read.delim('Restaurant_Reviews.tsv',
                     quote = '', #ignore quotes in the text
                     stringsAsFactors = FALSE #reviews shouldn't be identified as factors (categorical)
                     )