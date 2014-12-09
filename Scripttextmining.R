##Loading a corpus \Users\Alfonso\Documents\TextMining
### Para funcionar correctamente requiere: R version 3.1
## Paquetes: tm, wordcloud, SnowballC, RcolorBrewer, ggplot2
## Especialmente: Rgraphviz, graph y BiocGenerics del "mirror" bioconductor
#library("tm", lib.loc="~/R/win-library/3.1") # Framework for text mining.
library("SnowballC") # Provides wordStem() for stemming.
library("RColorBrewer") # Generate palette of colours for plots.
library("ggplot2") # Plot word frequencies.
library("wordcloud")
library("Rgraphviz") # Correlation plots.
cname <- file.path("~", "", "Documentos", "TyP", "txt")
#cname <- file.path(".")
cname
##We can list some of the file names.
length(dir(cname))
dir(cname)
##Cargar TM
library("tm")
## cargar pdfs
#docs <- Corpus(DirSource(cname), readerControl=list(reader=readPDF))
## Al parecer funciona solo en linux
# Cargar txt's
#VCorpus(DirSource(txt, encoding = "UTF-8")
docs <- Corpus(DirSource(cname, encoding = "UTF-8"))
docs
class(docs)
class(docs[[1]])
## [1] visualiza el primero
##quite basic information about the corpus.
summary(docs)
#This will assure us that data has
#been loaded properly and as we expect.
####inspect(docs[1])
#We start with some manual special transforms we may want to do. 
#for (j in seq(docs))
#{
#        docs[[j]] <- gsub("/", " ", docs[[j]])
#        docs[[j]] <- gsub(")", " ", docs[[j]])
#        docs[[j]] <- gsub("\\|", " ", docs[[j]])
#}
# valid functions on tm_map
getTransformations()
#Conversion to Lower Case
#docs <- tm_map(docs, tolower)
#Remove Numbers
docs <- tm_map(docs, removeNumbers)
#Remove Punctuation
docs <- tm_map(docs, removePunctuation)
# acortar espacios
docs <- tm_map(docs, stripWhitespace)
#Remove Spanish Stop Words
docs <- tm_map(docs, removeWords, stopwords("spanish"))
#Remove Own Stop Words
quitarpalabras <- c("Teoría y Praxis", "teoría", "praxis", "RESUMEN", "TEORÍA Y PRAXIS", "FUENTES CONSULTADAS", "and", "the", " teoría", " praxis")
docs <- tm_map(docs, removeWords, quitarpalabras)
#Specific transformations
#for (j in seq(docs))
#{
#        docs[[j]] <- gsub("turístico", "turística", docs[[j]])
#        docs[[j]] <- gsub("turísticos", "turística", docs[[j]])
#        docs[[j]] <- gsub("turísticas", "turística", docs[[j]])
#}
##Stemming
#docs2 <- tm_map(docs, stemDocument, language = "spanish")
##We use TermDocumentMatrix()
##to create the matrix:
tdm <- TermDocumentMatrix(docs)
##we select a subset of inspect.
inspect(tdm[5:10, 740:743])
#We can obtain the term frequencies as a vector by converting the 
#term document matrix into a matrix and summing the column counts:
freq <- rowSums(as.matrix(tdm))
length(freq)
#By ordering the frequencies we can list the most frequent terms and the 
#least frequent terms:
ord <- order(freq)
# Least frequent terms
freq[head(ord)]
# Most frequent terms
freq[tail(ord)]
# Frequency of frequencies.
head(table(freq), 15)
tail(table(freq), 15)
#convert the data structure into a simple matrix:
m <- as.matrix(tdm)
dim(m)
#To write the data to file.
write.csv(m, file="~//Documentos/teoriaypraxis.csv")
##"sparse" terms can be removed from the document term matrix 
tdms <- removeSparseTerms(tdm, 0.8)
dim(tdms)
##We can see the effect by looking at the terms we have left:
freq <- rowSums(as.matrix(tdms))
freq
########################################hasta aqui voy
##table(freq)
# we limit the output to those terms that occur at least 50 times:
###findFreqTerms(dtm, lowfreq=50)
##We can also find associations with a word, specifying a correlation limit.
###findAssocs(dtm, "experi", corlimit=0.6)
##Graficar NOTA: USA Rgraphviz que no existe en esta versi?n.
plot(dtm, terms=findFreqTerms(dtm, lowfreq=100)[1:15], corThreshold=0.9)
#We can generate the frequency count of all words in a corpus:
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
head(freq, 14)
wf <- data.frame(word=names(freq), freq=freq)
head(wf)
# plot the frequency of those words that occur at least 200 times in the corpus:
library("ggplot2")
p <- ggplot(subset(wf, freq>200), aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
# Hacer wordcloud
library("wordcloud")
#set.seed(123)
#wordcloud(names(freq), freq, min.freq=40)
# limitar el numero de palabras
#set.seed(142)
#wordcloud(names(freq), freq, max.words=100)
# limitar por frecuencia
#set.seed(142)
#wordcloud(names(freq), freq, min.freq=100)
# Colorear
#set.seed(142)
#wordcloud(names(freq), freq, min.freq=30, colors=brewer.pal(6, "Dark2"))
#Variar la escala
#set.seed(142)
#wordcloud(names(freq), freq, min.freq=50, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))
#change the proportion of words that are rotated by 90 degrees from the default 
#10% to, say, 20% using rot.per=0.2.
set.seed(142)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, max.words=500, rot.per=0.1, colors=dark2)
#writeCorpus(docs)
