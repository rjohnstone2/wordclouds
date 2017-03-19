wordcloudDiffDirs <- function(dir1, dir2, minFreq=50, minWordLength=5, maxWordLength=20)
{

library(tm)
library(wordcloud)

baseCorpus <- Corpus(DirSource(dir1))
baseCorpus <- tm_map(baseCorpus, removeURLs)
baseCorpus <- tm_map(baseCorpus, removePunctuation)
baseCorpus <- tm_map(baseCorpus, content_transformer(tolower))
baseCorpus <- tm_map(baseCorpus, removeNumbers)
baseCorpus <- tm_map(baseCorpus, removeWords, stopwords("english"))
baseCorpus <- tm_map(baseCorpus, stripWhitespace)
dtmBase <- DocumentTermMatrix (baseCorpus, control=list(wordLengths=c(minWordLength,maxWordLength), bounds=list(global=c(3,100))))
freqBase <- colSums(as.matrix(dtmBase))

compareCorpus <- Corpus(DirSource(dir2))
compareCorpus <- tm_map(compareCorpus, removeURLs)
compareCorpus <- tm_map(compareCorpus, removePunctuation)
compareCorpus <- tm_map(compareCorpus, content_transformer(tolower))
compareCorpus <- tm_map(compareCorpus, removeNumbers)
compareCorpus <- tm_map(compareCorpus, removeWords, stopwords("english"))
compareCorpus <- tm_map(compareCorpus, stripWhitespace)
dtmCompare <- DocumentTermMatrix (compareCorpus, control=list(wordLengths=c(minWordLength,maxWordLength), bounds=list(global=c(3,100))))
freqCompare <- colSums(as.matrix(dtmCompare))

freqDiff <- colSums(as.matrix(dtmCompare))
for (val in names(freqCompare)) { if (is.na(freqBase[val])) { freqDiff[val] = freqCompare[val]} }
for (val in names(freqCompare)) { if (!is.na(freqBase[val])) { freqDiff[val] = (freqCompare[val]-freqBase[val]) } }
dfDiff <- data.frame(I(names(freqCompare)),freqCompare,freqDiff)
set.seed(131)
par(bg='lightblue')
dfLength <- length(dfDiff$freqCompare)
wordcloud(dfDiff[order(dfDiff$freqDiff),c(1)],dfDiff[order(dfDiff$freqDiff),c(2)],min.freq=minFreq,colors=colorRampPalette(brewer.pal(11, "RdYlGn"))(dfLength),random.order=FALSE,ordered.colors=TRUE)
}

removeURLs <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x)
