##movie 크롤링을 통한 시나리오 분석
library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)
setwd("c:/Rdata")
rm(list=ls())

title = c()
grade = c()
time = c()

t_css = ".color_b"
#gr_css = ".title em"   #.list_netizen_score em
gr_css = ".list_netizen_score em"
pt_css = ".title+ .num"

base_url = "https://movie.naver.com/movie/point/af/list.nhn?&page="

for(i in 1:100){
cr_url = paste0(base_url, i)
hdoc = read_html(cr_url, encoding = "CP949")

n_title = html_nodes(hdoc,t_css)
n_title

n_gr = html_nodes(hdoc,gr_css)
n_gr

n_pt = html_nodes(hdoc,pt_css)
n_pt



title_part = html_text(n_title)
title_part
grade_part = html_text(n_gr)
grade_part
pt_part = html_text(n_pt)
time_part = str_sub(pt_part,-8)
time_part

title = c(title,title_part)
grade = c(grade,grade_part)
time  = c(time,time_part)
}
movie = data.frame(title, grade, time)

View(movie)

write.csv(movie,"movie.csv")
data = read.csv("movie.csv")

head(data)

top10_A = data%>% 
  select(title,grade)%>% 
  group_by(title)%>% 
  summarise(total=sum(grade), 
            count=n())%>%
  arrange(desc(total),desc(count))%>%
  head(10)

top10_A
ggplot(data=top10_A, aes(x=reorder(title,total), y=total))+
  geom_col(fill=rainbow(10))+
  geom_text(aes(label=top10_A$total), hjust = -0.2, col = 'blue')+
  coord_flip()
