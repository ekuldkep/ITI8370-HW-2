library(tm)
library(stringr)

library(ggplot2)

folder = paste(getwd(), "/Files", sep="")

temp = list.files(path = folder, pattern="*.csv", full.names=TRUE)

read = function(path){
  csv = read.csv(file=path, header=TRUE, sep=",")
  colnames(csv) = c("doc_id", "author", "date", "text", "dmeta1")
  asd = gsub(".{3}$", " ", csv$text)
  csv$text = asd
  return(csv);
}

file = read(temp[1]) 
### we now have a dataframe from the CSV
#print(file)

# some class, repr collection of documents, i.e dict of id;stringphrase
docs <- VCorpus(DataframeSource(file))

# Preprocessing
toSpace = content_transformer( function(x, pattern) gsub(pattern," ",x) )
docs = tm_map( docs, toSpace, "http*")
docs = tm_map( docs, toSpace, "www*")

docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

docs <- tm_map(docs, stripWhitespace)
#docs <- tm_map(docs, PlainTextDocument)
#docs <- tm_map(docs, stemDocument)

# as processing is done,
# show some interesting words filtered by frequency
dtm <- DocumentTermMatrix(docs)
inspect(dtm)

freq <- colSums(as.matrix(dtm))
length(freq)


dtms <- removeSparseTerms(dtm, 0.95)

print(length(rownames(dtms)))
spam = file$dmeta1


# table after removing sparse terms
freq <- colSums(as.matrix(dtms))
print(freq)
