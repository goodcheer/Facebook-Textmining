#Environment Setting
setwd("C:\\Users\\kyuchul\\Documents\\Carrer\\Rzip")

#필요 패키지
packages = c("Rfacebook", "tm", "lsa", "wordcloud","ggplot2","KoNLP",
             "GPArotation","cluster","RWeka","ROAuth","fpc","stringr","ape")

for (i in packages){
  if(!require( i , character.only = TRUE))
  {install.packages(i, dependencies = TRUE)}
}

#Setting
pdf.options(family="Korea1deb") #not to tear down the letters
options(java.parameters=c("-Xmx4g","-Dfile.encoding=UTF-8")) #to increse heap size of rjava
pal <- brewer.pal(9,"Set1")
options(mc.cores=1)

#페이스북 앱id 정보와 secret 값 입
fbAuth = fbOAuth(app_id = "1881608938717379",
                 app_secret = "6a5145b2b699d4644a8553518ff1833e",
                 extended_permissions = FALSE)

#get the Post from SKKUBamboo page (in small data)

fb_data9<-getPage(page="SKKUBamboo", token=fbAuth, n=1000, since='2017/09/01', until='2017/09/21')
fb_data9<-rbind(fb_data9,getPage(page="SKKUBamboo", token=fbAuth, n=1000, since='2017/08/01', until='2017/08/31'))
fb_data9<-rbind(fb_data9,getPage(page="SKKUBamboo", token=fbAuth, n=1000, since='2017/07/01', until='2017/07/31'))
fb_data9<-rbind(fb_data9,getPage(page="SKKUBamboo", token=fbAuth, n=1000, since='2017/06/01', until='2017/06/30'))
fb_data9<-rbind(fb_data9,getPage(page="SKKUBamboo", token=fbAuth, n=1000, since='2017/05/01', until='2017/05/31'))
fb_data9<-rbind(fb_data9,getPage(page="SKKUBamboo", token=fbAuth, n=1000, since='2017/04/01', until='2017/04/30'))
fb_data9<-rbind(fb_data9,getPage(page="SKKUBamboo", token=fbAuth, n=1000, since='2017/03/01', until='2017/03/31'))
fb_data9<-rbind(fb_data9,getPage(page="SKKUBamboo", token=fbAuth, n=1000, since='2017/02/01', until='2017/02/28'))
fb_data9<-rbind(fb_data9,getPage(page="SKKUBamboo", token=fbAuth, n=1000, since='2017/01/01', until='2017/01/31'))
fb_data9<-data.frame(fb_data9$message)

write.csv(fb_data9,"fb_data9.txt")

###############################################################################################
#get the Post from SKKUBamboo page (in big data) - it didn't work - so, rbind by month

start_date = '2017/01/01'
end_date = '2017/09/19'
scrape_days=seq(from = as.Date(start_date), to = as.Date(end_date), by = 'days')
posts <- c()

for(i in scrape_days) {
  daypost=c()
      tryCatch(
        {daypost = getPage(page ="SKKUBamboo",
                        token = fbAuth,
                        since = as.Date(i, origin="1970-01-01"),
                        until = as.Date(i, origin="1970-01-01") + 1)},
        error=function(e){}
        )
        posts = rbind(posts, daypost)
      }


fb_data2<-data.frame(posts$message)

###############################################################################################

#Proprocessing
txt <- as.character(fb_data9$fb_data9.message)
txt <- gsub("[[:digit:]]", "", txt)
txt <- gsub("[#번째울림]","",txt)
txt <- str_replace_all(txt,"\n","")
txt <- str_replace_all(txt,"[:punct:]","")
txt <- str_replace_all(txt,"ㅠ","")
txt <- str_replace_all(txt,"오후","")
txt <- str_replace_all(txt,"<사는얘기>","")
txt <- str_replace_all(txt,"<사랑>","")
txt <- str_replace_all(txt,"<취향입니다 존중해주시죠>","")
txt <- str_replace_all(txt,"<저 진지합니다>","")
txt <- str_replace_all(txt,"<질문건의의견>","")
txt <- str_replace_all(txt,"<성균관대학교 대나무숲>","")
txt <- str_replace_all(txt,"<인간관계>","")
txt <- str_replace_all(txt,"<게임>","")
txt <- str_replace_all(txt,"오전","")
txt <- str_replace_all(txt,"오후","")
txt <- str_replace_all(txt,"<군대>","")
txt <- str_replace_all(txt,"<학교>","")
txt <- str_replace_all(txt,"<술>","")
txt <- str_replace_all(txt,"<그 외 위의 카테고리 외 기타 등등>","")
txt <- str_replace_all(txt,"ㅋㅋㅋ","")
txt <- str_replace_all(txt,"ㅋㅋ","")
txt <- str_replace_all(txt,"ㅋ","")
txt <- str_replace_all(txt,"것","")
txt <- str_replace_all(txt,"들","")
txt <- str_replace_all(txt,"때","")
txt <- str_replace_all(txt,"ㅎㅎㅎ","")
txt <- str_replace_all(txt,"ㅎㅎ","")
txt <- str_replace_all(txt,"ㅎ","")

Encoding(txt) <- "UTF-8"

#Organize Corpus

txt_noun<-sapply(txt,extractNoun)
corpus <- Corpus(VectorSource(txt_noun)) #명사에 벡터값 부여
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
myStopwords <- c(stopwords('english'))
corpus <- tm_map(corpus, removeWords, myStopwords)

#Document-Term Matrix
uniTokenizer <- function(x) unlist(strsplit(as.character(x), "[[:space:]]+"))
control = list(tokenize = uniTokenizer,
               removeNumbers = TRUE,
               wordLengths=c(2,20),
               removePunctuation = TRUE,
               stopwords = c("\\n","\n","것"),
               weighting = function (x) weightTfIdf(x, TRUE))
scanner <- function(x) strsplit(x," ") # 대안적인 control 함수
tdm <- DocumentTermMatrix(corpus, control=list(tokenize=control)) #ha....encoding...

Encoding(tdm$dimnames$Terms) ='UTF-8'
findFreqTerms(tdm,lowfreq = 2)

td <- removeSparseTerms(tdm,0.99)
td$dimnames$Terms[1:10]
check<-which(rowSums(as.matrix(td))>1)
td<-(td[check,])
colTotal<-apply(td,1,sum)
which(colTotal<=0)
findFreqTerms(td, lowfreq=5)

TermFreq<-colSums(as.matrix(td))
TermFreq2 <-subset(TermFreq,TermFreq>4)
gframe<-data.frame(term=names(TermFreq2),freq=TermFreq2)
ggplot(data=gframe)+aes(x=term,y=freq)+geom_bar(stat="identity")+coord_flip()
wordcloud(names(TermFreq2),TermFreq2,random.color=TRUE,color=pal)

#LSA
LSA <-lsa(td,dim=4)
st <-LSA$tk
wd <- LSA$dk
strength <- LSA$sk

rot <- GPForth(wd, Tmat=diag(ncol(wd)), normalize=FALSE, eps=1e-5,
               maxit=10000, method="varimax",methodArgs=NULL)

cord<-st %*% diag(strength) %*% rot$Th #원래는 rot대신 wd를 쓰지만 여기서는 회전시킨 rot을 쓴다(김청택 교수님의 제안)
signs<-sign(colSums(rot$loadings))
cord<-cord %*% diag(signs)
text_lsa<-data.frame(cord=cord,txt=txt[check])

#######

t<-rot$loadings[,3]
tt<-abs(t)
terms<-names(tt)
wordcloud(terms,tt,scale=c(6,1),rot.per=0,max.words=50,color=pal)