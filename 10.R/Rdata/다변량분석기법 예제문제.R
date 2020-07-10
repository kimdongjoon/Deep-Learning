setwd("c:/Rdata")

##다변량 연습문제
data  = read.csv("20140528_baseball.csv")
head(data)

rownames(data) = data[,1]
head(data)

stars(data[,2:6], flip.labels = F, key.loc = c(8,3), draw.segments = TRUE)

faces(data[,2:6])




##2.
bb2013=read.csv("2013_baseball.csv")
head(bb2013)
position = bb2013$포지션
head(position)

base2_pos = bb2013[,c(2,4:11)]
base2_pos2 = aggregate(base2_pos[,2:9], by=list(포지션=base2_pos$포지션), sum)
head(base2_pos2)

rownames(base2_pos2) = base2_pos2[,1]
head(base2_pos2)

library(lattice)
parallel((base2_pos2[,2:9]),horizontal.axis = FALSE, col = 1)

#팀별평행좌표
team=bb2013$팀
head(team)
parallel(~bb2013[,4:11]|team,horizontal.axis =FALSE, col =1)

#포지션별 좌표
positionArr=bb2013$포지션
head(positionArr)
parallel(~bb2013[,4:11]|positionArr,horizontal.axis =FALSE, col =1)


#3.
rownames(bb2013) = bb2013[,1]
rownames(bb2013)
head(bb2013)
model = prcomp(bb2013[,4:11],scale = T)
plot(model)
summary(model)

biplot(model)
