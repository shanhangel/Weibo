#0.安装需要的包
#install.packages("Rweibo", repos = "http://R-Forge.R-project.org")
#install.packages("RCurl")
#install.packages("rjson")
#install.packages("XML")
#install.packages("Rwordseg")
#install.packages("wordcloud")
#install.packages("tm")
#install.packages("fpc")

#1.抓数据
require(Rweibo)
res<-web.search.content("葡萄牙",page=50,sleepmean=10,sleepsd=1, since="2014-06-17")$Weibo 

#2.分词
require(Rwordseg)
insertWords("葡萄牙")
n<-length(res)
res<-res[res!=" "]
words<-unlist(lapply(X=res,FUN=segmentCN))
word=lapply(X=words,FUN=strsplit," ")
v=table(unlist(word))
v=sort(v,deceasing=T)
d=data.frame(word=names(v),freq=v)

#3.词云展示
require(wordcloud) 
dd<-tail(d,150)
op<-par(bg="lightyellow")
#grayLevels<-gray((dd$freq)/(max(dd$freq)+140))
#wordcloud(dd$word,dd$freq,colors=grayLevels)
rainbowLevels<-rainbow((dd$freq)/(max(dd$freq)-10))
wordcloud(dd$word,dd$freq,col=rainbow(length(d$freq)))
par(op)

