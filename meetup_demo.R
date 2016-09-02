# Text Analysis: Twitter Syria Refugees

# load libraries
library(tm) # text-mining tools
library(RTextTools) # text-mining tools
library(wordcloud) # word-cloud
library(ggplot2) # plot
# library(e1071) # naive-bayes
library(proxy) # cosine similarity / for clustering

# define a function for cleaning tweets
cleanCorpus = function(corpus) {
  corpus.tmp = tm_map(corpus, content_transformer(tolower)) # transform all to lower case
  corpus.tmp = tm_map(corpus.tmp, removeWords, stopwords("english")) # remove english stopwords
  corpus.tmp = tm_map(corpus.tmp, removeWords, c("http","syrianrefugees","https")) 
  corpus.tmp = tm_map(corpus.tmp, removeNumbers) # remove number
  return(corpus.tmp)
}

#### (1) Visual Inspection ####

# read tweets
dset = read.csv("./meetup_demo/data/syria.csv", header = TRUE, sep = ";", stringsAsFactors=FALSE)

# create a corpus, clean up and generated TDM
tweet_text = data.frame(as.character(dset$text)) # get only text
tweet_text = as.data.frame(apply(tweet_text, 1, FUN = function(x) as.character(gsub("[[:punct:]]", " ", x)))) # substitute punctuation with empty spaces
tweet_corpus = Corpus(DataframeSource(tweet_text)); # create corpus
tweet_corpus = cleanCorpus(tweet_corpus) # clean corpus
tweet_tdm = TermDocumentMatrix(tweet_corpus) # generate TDM
tweet_tdm = as.matrix(tweet_tdm)

write.csv2(tweet_tdm, file = "./meetup_demo/data/tdm.csv") # show TDM

# generate frequencies
word_freq = sort(rowSums(tweet_tdm), decreasing=TRUE)
dm = data.frame(word = names(word_freq), freq=word_freq)

# plot hist frequent words
sub_dm = subset(dm, freq > 5) # subset frequent words
q = ggplot(sub_dm, aes(word, freq))
qq = q + geom_bar(stat = "identity")
qq + theme(axis.text.x = element_text(angle=90, hjust=1))

# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"), max.words=100)


#### (2) Clustering ####

# Calculate distance
cos.dist = dist(t(tweet_tdm), method = "cosine")
write.csv2(as.matrix(cos.dist), file = "./meetup_demo/data/distance_matrix.csv") # show the distance matrix

# inspect dendrogram
dendro = hclust(cos.dist, method = "ward")
plot(dendro)

# how many clusters you want to have? and prune
n_clusters = 3
dendro.pruned = cutree(dendro, n_clusters)

# export clusters
clustering.results = dset
clustering.results$clusterId = dendro.pruned
write.csv2(clustering.results, file = "./meetup_demo/data/clustering_results.csv") # export tweets with cluster

# how many tweets in each cluster?
splitT = split(clustering.results,clustering.results$clusterId)
tweetsInCluster = as.numeric(lapply(splitT, function(x) nrow(x))); tweetsInCluster


