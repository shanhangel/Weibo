#0.��װ��Ҫ�İ�
#install.packages("Rweibo", repos = "http://R-Forge.R-project.org")
#install.packages("RCurl")
#install.packages("rjson")
#install.packages("XML")
#install.packages("Rwordseg")
#install.packages("wordcloud")
#install.packages("tm")
#install.packages("fpc")

#1.ץ����
require(Rweibo)
res<-web.search.content("����",page=50,sleepmean=10,sleepsd=1)$Weibo ##page�������Լ�����

#2.�ִ�
require(Rwordseg)
insertWords("����")
n<-length(res)
res<-res[res!=" "]
words<-unlist(lapply(X=res,FUN=segmentCN))
word=lapply(X=words,FUN=strsplit," ")
v=table(unlist(word))
v=sort(v,deceasing=T)
d=data.frame(word=names(v),freq=v)

#3.����չʾ
require(wordcloud) 
dd<-tail(d,150)
op<-par(bg="lightyellow")
#grayLevels<-gray((dd$freq)/(max(dd$freq)+140))
#wordcloud(dd$word,dd$freq,colors=grayLevels)
rainbowLevels<-rainbow((dd$freq)/(max(dd$freq)-10))
wordcloud(dd$word,dd$freq,col=rainbow(length(d$freq)))
par(op)

#4.�������Ͽ�
require(tm)
#������һ�����Ͽ⣬������һ��΢�����ı�
weiboCorpus<-Corpus(VectorSource(res))
#ɾ��������
weiboCorpus<-tm_map(weiboCorpus,removePunctuation)
#ɾ������
weiboCorpus<-tm_map(weiboCorpus,removeNumbers)
#ɾ��URL,ʹ����һ���������ʽ
removeURL<-function(x) gsub("http[[:alnum:]]*","",x)
weiboCorpus<-tm_map(weiboCorpus,removeURL)
#�ٴηִ�
weiboData<-as.data.frame(weiboCorpus)
weiboData<-t(weiboData)
weiboData<-as.data.frame(weiboData)
#head(weiboData) #�ٴμ���һЩ��
insertWords(c("̩��","ʮ����Ф","һ����ʦ","�Ʋ�","���ڇ�;","���ه�;","����"))
weiboData$segWord<-segmentCN(as.matrix(weiboData)[,1])
#head(weiboData)
#�γ���һ��data.frame--weiboData����һ������Ϊ΢�����ݱ������ڶ�������Ϊ�ִʵĽ�� 
#�ٴ��γ�һ�����Ͽ⣬����������һ���ķ���
weiboCorpusForAnys <- Corpus(DataframeSource(weiboData))
#��ʵ���ʱ���ٻ�һ�����ƣ�����Ч�������Щ
#Ŀǰ�������е���һ������û������ġ��ҷ������˼�����
#�����fpc�����࣬����ͼ���޷�չʾ��������ͷ��5.1�żٻ������һ�µġ�

#5.pam�㷨��΢�����о������
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

#��ͼ������
layout(matrix(c(1,2),2,1))
plot(pamResult,color=F,labels=4,lines=0,cex=0.8,col.clus=1,
     col.p=pamResult$clustering)
layout(matrix(1))
