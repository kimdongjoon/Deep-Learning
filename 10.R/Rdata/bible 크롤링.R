setwd("c:/Rdata")
library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
library(wordcloud)
install.packages("wordcloud2")
library(wordcloud2)

cnt=c()
b_url="https://www.bskorea.or.kr/bible/korbibReadpage.php?version=GAE&book=gen&chap="
for(i in 1:50){
  cr_url=paste0(b_url,i)
  t_css="#tdBible1 span"
  hdoc=read_html(cr_url,encoding = "UTF-8")
  n_css=html_nodes(hdoc,t_css)
  cnt_part=html_text(n_css)
  cnt_part=gsub("\\d+","",cnt_part)
  cnt_part
  cnt_part=str_trim(cnt_part,side="both")
  cnt_part
  cnt=c(cnt,cnt_part)
}
library(KoNLP)
txt=sapply(cnt,extractNoun,USE.NAMES = F)
txt=unlist(txt)
count=Filter(function(x){nchar(x)>=2},txt)
word=table(count)

kk=head(sort(word,decreasing = T),20)
kk
tt=barplot(kk,col=rainbow(20),ylim=c(0,300),las=2)
text(tt,kk,label=paste0(kk,"°³"),pos=3,col=2)

library(RColorBrewer)
display.brewer.all()
palate=brewer.pal(9,"Greens")
wordcloud(names(word),
          freq=word,
          min.freq = 2,
          scale=c(5,0.5),
          random.order = F,
          random.color = T,
          colors=palate)
?wordcloud2()
wordcloud2(data=word,
           size=0.4,
           shape='diamond')

