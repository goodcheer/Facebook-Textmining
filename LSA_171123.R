setwd("C:/Users/kyuchul/Documents/Carrer/학교_2/2017-2/CschoolProject/LSA,LDA구현")
load("LSA_171123.RData")

# 필요 패키지
packages = c("Rfacebook", "tm", "lsa", "wordcloud","ggplot2","KoNLP",
             "GPArotation","cluster","RWeka","ROAuth","fpc","stringr","ape","devtools")

for (i in packages){
  if(!require( i , character.only = TRUE))
  {install.packages(i, dependencies = TRUE)}
}


#Setting

pdf.options(family="Korea1deb") #not to tear down the letters
options(java.parameters=c("-Xmx4g","-Dfile.encoding=UTF-8")) #to increse heap size of rjava
pal <- brewer.pal(9,"Set1")
options(mc.cores=1)

useNIADic()

#=========================================================#
# SimplePos22                                             #
#=========================================================#

# # remove pucntuation: punctuation raise error
# 
# file1 <- unlist(file1)
# file1 <- gsub("[[:punct:]]"," ",file1)
# 
# # remove redundant whitespace
# file1 <- gsub("\\s+"," ", file1)
# 
# 
# words <- function(doc){
#   
#   doc <- as.character(doc)
#   
#   doc2 <- paste(SimplePos22(doc))
#   
#   doc3 <- str_match(doc2, "([가-??+)/NC")
#   
#   if( dim(doc3)[2] == 2){
#     doc4 <- doc3[,2]
#     doc4 <- doc4[!is.na(doc4)]
#     return(doc4)
#     }
#   
#   
# }
# 
# nouns = sapply(file1,words, USE.NAMES = F)
# 
# txt_noun1 <- nouns


#=========================================================#
# Settings                                                #
#=========================================================#
# 
# words<-read.csv("2nd_stopwords.txt",header=FALSE)
# words <- as.character(words$V1)

#=========================================================#
# Corpus ??성                                             #
#=========================================================#

# corpus <- Corpus(VectorSource(txt_noun1)) #명사??벡터???부??
# corpus <- tm_map(corpus, removeNumbers)
# corpus <- tm_map(corpus, removeWords, words)


#Document-Term Matrix

uniTokenizer <- function(x) unlist(strsplit(as.character(x), "[[:space:]]+"))
control = list(tokenize = uniTokenizer,
               removeNumbers = TRUE,
               wordLengths=c(2,20),
               removePunctuation = TRUE,
               stopwords = c("\\n","\n","것","c"),
               weighting = function (x) {weightTfIdf(x, TRUE)})

dtm <- DocumentTermMatrix(corpus, control=control)
object.size(dtm)

Encoding(dtm$dimnames$Terms) ='UTF-8'

findFreqTerms(dtm,lowfreq = 100)

dt <- removeSparseTerms(dtm,0.999)
as.numeric(object.size(dt)/object.size(dtm)) * 100 # ratio of reduced/ original

dt$dimnames$Terms[1:10]

library(slam) # for apply function on DocumentTermMatrix
check<-which(row_sums(td>1))
dt<-(dt[check,])
colTotal<-apply(dt,1,sum)
which(colTotal<=0)
findFreqTerms(dt, lowfreq=5)

TermFreq<-colSums(as.matrix(dt))
TermFreq2 <-subset(TermFreq,TermFreq>4)
gframe<-data.frame(term=names(TermFreq2),freq=TermFreq2)
ggplot(data=gframe)+aes(x=term,y=freq)+geom_bar(stat="identity")+coord_flip()
wordcloud(names(TermFreq2),TermFreq2,random.color=TRUE,colors=pal)

#LSA
LSA <-lsa(dt,dim=5)
st <-LSA$tk
wd <- LSA$dk
strength <- LSA$sk
rot <- GPForth(wd, Tmat=diag(ncol(wd)), normalize=FALSE, eps=1e-5,
               maxit=10000, method="varimax",methodArgs=NULL)


cord<-st %*% diag(strength) %*% rot$Th
signs<-sign(colSums(rot$loadings))
cord<-cord %*% diag(signs)
text_lsa<-data.frame(cord=cord,file=file1[check])

####################################################################

showmedim <- function(dimen){
  t<-rot$loadings[,dimen]
  tt<-abs(t)
  terms<-names(tt)
  wordcloud(terms,tt,scale=c(4,1),rot.per=0,max.words=50, colors=pal)
}

showmedim(1)
showmedim(2)
showmedim(3)
showmedim(4)
showmedim(5)

###################################################################

save.image(file = "LSA_171123.RData")

###################################################################

install_github("cpsievert/LDAvisData")
data(reviews, package = "LDAvisData")

install.packages("lda")
install.packages("LDAvis")
library(lda)
library(LDAvis)

doc.list <- txt_noun1[1:1000]

term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

del <- names(term.table) %in% words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]

K <- 5
G <- 5000
alpha <- 0.02
eta <- 0.02

library(lda)
set.seed(357)
t1<-Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab=vocab,
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2-t1

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

MovieReviews <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

options(encoding = 'UTF-8') #???????결과 보기

library(LDAvis)

# create the JSON object to feed the visualization:
json <- createJSON(phi = MovieReviews$phi, 
                   theta = MovieReviews$theta, 
                   doc.length = MovieReviews$doc.length, 
                   vocab = MovieReviews$vocab, 
                   term.frequency = MovieReviews$term.frequency, encoding='UTF-8')
install.packages("servr")
library(servr)

serVis(json, out.dir = 'vis', open.browser = TRUE)
