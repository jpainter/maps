# word cloud from http://onertipaday.blogspot.com/2011/07/word-cloud-in-r.html

library(RXKCD)
library(tm)
library(wordcloud)
library(RColorBrewer)

# prepare list of words from a text (in this case, a package description file)
path <- system.file("xkcd", package = "RXKCD")
datafiles <- list.files(path)
xkcd.df <- read.csv(file.path(path, datafiles))
xkcd.corpus <- Corpus(DataframeSource(data.frame(xkcd.df[, 1])))
xkcd.corpus <- tm_map(xkcd.corpus, removePunctuation)
xkcd.corpus <- tm_map(xkcd.corpus, tolower)

# remove 'the', 'and', etc.  takes long time to run
xkcd.corpus <- tm_map(xkcd.corpus, function(x) removeWords(x, stopwords("english")))

# extract frequency of words from text
tdm <- TermDocumentMatrix(xkcd.corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)


pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
png("wordcloud.png", width=1280,height=800)


wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
dev.off()