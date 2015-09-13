# Text Analysis Demo for Meetup

# load libraries
library(tm) # text-mining tools
library(RTextTools) # text-mining tools
library(wordcloud) # word-cloud
# library(ggplot2) # plot
library(e1071) # naive-bayes

### (1) Twitter User Sentiment ####

# read user tweets / for syria sep = ";"
user = read.csv("./meetup_demo/data/user_tweets.csv", header = TRUE, stringsAsFactors=FALSE)

# create term document matrix
tdm = as.matrix(create_matrix(user$text, language="english", removeStopwords=FALSE, removeNumbers=TRUE, stemWords=FALSE, weightTf))
write.csv2(tdm, file = "./meetup_demo/data/tdm.csv")

# frequency analysis
word_freqs = sort(colSums(tdm), decreasing=TRUE) 
dm = data.frame(word=names(word_freqs), freq=word_freqs)
write.csv2(dm, file = "./meetup_demo/data/dm.csv")

# wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"), max.words=100)


#### (2) Twitter Syria Refugees ####

cleanCorpus = function(corpus) {
  # DEFINETLY TO CLEAN
  corpus.tmp = tm_map(corpus, content_transformer(tolower)) # transform all to lower case
  corpus.tmp = tm_map(corpus.tmp, removeWords, stopwords("english")) # remove english stopwords
  corpus.tmp = tm_map(corpus.tmp, removeWords, c("http","syrianrefugees","https")) 
  corpus.tmp = tm_map(corpus.tmp, removeNumbers) # remove number
  return(corpus.tmp)
}

# read tweets
user = read.csv("./meetup_demo/data/syria.csv", header = TRUE, sep = ";", stringsAsFactors=FALSE)

tweet_text = data.frame(as.character(user$text)) # get only text
tweet_text = as.data.frame(apply(tweet_text, 1, FUN = function(x) as.character(gsub("[[:punct:]]", " ", x)))) # substitute punctuation with empty spaces
tweet_corpus = Corpus(DataframeSource(tweet_text)); # create corpus
tweet_corpus = cleanCorpus(tweet_corpus) # clean corpus
tweet_tdm = TermDocumentMatrix(tweet_corpus) # generate TDM
tweet_tdm = as.matrix(tweet_tdm)

# generate frequencies
word_freq = sort(rowSums(tweet_tdm), decreasing=TRUE)
dm = data.frame(word = names(word_freq), freq=word_freq)

# ggplot 
sub_dm = subset(dm, freq > 5) # subset frequent words
q = ggplot(sub_dm, aes(word, freq))
qq = q + geom_bar(stat = "identity")
qq + theme(axis.text.x = element_text(angle=90, hjust=1))

# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"), max.words=100)



