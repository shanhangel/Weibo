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
res<-web.search.content("雷霆",page=50,sleepmean=10,sleepsd=1)$Weibo ##page参数请自己设置

#2.分词
require(Rwordseg)
insertWords("雷霆")
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

#4.建立语料库
require(tm)
#先生成一个语料库，来清理一下微博的文本
weiboCorpus<-Corpus(VectorSource(res))
#删除标点符号
weiboCorpus<-tm_map(weiboCorpus,removePunctuation)
#删除数字
weiboCorpus<-tm_map(weiboCorpus,removeNumbers)
#删除URL,使用了一点正则表达式
removeURL<-function(x) gsub("http[[:alnum:]]*","",x)
weiboCorpus<-tm_map(weiboCorpus,removeURL)
#再次分词
weiboData<-as.data.frame(weiboCorpus)
weiboData<-t(weiboData)
weiboData<-as.data.frame(weiboData)
#head(weiboData) #再次加入一些词
insertWords(c("泰囧","十二生肖","一代宗师","黄渤","人在囧途","人再囧途","三俗"))
weiboData$segWord<-segmentCN(as.matrix(weiboData)[,1])
#head(weiboData)
#形成了一个data.frame--weiboData，第一个变量为微博内容本身，第二个变量为分词的结果 
#再次形成一个语料库，用来做更进一步的分析
weiboCorpusForAnys <- Corpus(DataframeSource(weiboData))
#其实这个时候再画一个词云，可能效果会更好些
#目前代码运行到这一步都是没有问题的。我反复试了几次了
#下面的fpc做聚类，最终图形无法展示出来。回头我5.1放假回来会扣一下的。

#5.pam算法对微博进行聚类分析
require(fpc)
weiboTDMatrix <- TermDocumentMatrix(weiboCorpusForAnys,
                                    control = list(wordLengths = c(1, Inf)))
TDMforCluster<-removeSparseTerms(weiboTDMatrix,sparse=0.9)
MatrixForCluster<-as.matrix(TDMforCluster)
MatrixWeiboForCluster<-t(MatrixForCluster) 
pamRes<-pamk(MatrixWeiboForCluster,metric="manhattan")
k<-pamRes$nc
k
pamResult<-pamRes$pamobject 
pamResult$clustering

#画图有问题
layout(matrix(c(1,2),2,1))
plot(pamResult,color=F,labels=4,lines=0,cex=0.8,col.clus=1,
     col.p=pamResult$clustering)
layout(matrix(1))

