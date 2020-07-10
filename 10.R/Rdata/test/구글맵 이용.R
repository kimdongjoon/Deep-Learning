setwd("c:/Rdata")
library(dplyr)
install.packages("ggmap")
library(ggmap)
library(stringr)
library(rvest)

register_google(key = "AIzaSyCcHwEAt_eIkxdA_qNS-rPwUc8WjBzlZN8")

x= get_map(location = "대전광역시 유성구 반석동",
        zoom = 15, 
        maptype = 'roadmap', #hybrid, satellite, terrian, ... 
        source = 'google')

ggmap(x)



qmap(location = "대전광역시 유성구 반석동",
     zoom = 15, 
     maptype = 'roadmap', #hybrid, satellite, terrian, ... 
     source = 'google')

plot.new()
frame()



geocodeQueryCheck()
geocode(location = "대전광역시 유성구 반석동", 
        output = 'latlon',
        source = 'google')

geocode(location = "대전광역시 유성구 반석동", 
        output = 'latlona',
        source = 'google')


#한글이 되어야 하는데 안되네..? 
geocode(location = enc2utf8("대전광역시 유성구 반석동"),
        output = 'latlona',
        source = 'google')

myloc = geocode(location="대전광역시 서구 둔산2동",
        output = 'latlon',
        source = 'google')

myloc
center = c(myloc$lon, myloc$lat)
qmap(location = center, 
     zoom = 18, 
     maptype = 'hybrid', #hybrid, satellite, terrian, ... 
     source = 'google')+
  geom_point(data = myloc,
             mapping = aes(x = lon, y=lat), 
             shape = '*',
             color = 'red',
             stroke = 18,
             size = 10)



### 서울특별시의 대학목록추출하기

url = "https://namu.wiki/w/%EC%84%9C%EC%9A%B8%ED%8A%B9%EB%B3%84%EC%8B%9C%EC%9D%98%20%EB%8C%80%ED%95%99%EA%B5%90%20%EB%AA%A9%EB%A1%9D"

hdoc= read_html(url, encoding = 'UTF-8')
df = hdoc%>% 
  html_nodes(".wiki-paragraph a") %>% 
  html_text

head(df,50)
str_detect(df,pattern = '대학교')

univ = ifelse(str_detect(df,pattern = '대학교'),df,"")
univ
kk = univ%>% 
  data.frame()
kk = Filter(function(x){nchar(x)>=5},univ)
univName = kk [2:28]
univName
univCord = geocode(location = univName,
                   output="latlon",
                   source ='google')


univDf = data.frame(univ= univName
                    ,lon = univCord$lon
                    ,lat = univCord$lat
                    )

univDfna = na.omit(univDf)
univDfna


center = c(mean(x= univDfna$lon), mean(x=univDf$lat))
center


qmap(location = center,
     zoom = 12, 
     maptype = 'satellite', #hybrid, satellite, terrian, ... 
     source = 'google')+
   geom_point(data=univDfna,
             aes(x=lon,y=lat),
             shape='*',
             color='red',size=6)
  geom_text(data=univDfna,
            aes(x=lon,y=lay,label=univ),
            color='green',hjust=0.5,
            size=6
            )
        
             

##movie 크롤링을 통한 시나리오 분석
