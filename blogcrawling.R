#패키지 설치
install.packages('rJava')
install.packages('memoise')
install.packages('KoNLP')
install.packages('remotes')
install.packages('data.table')
install.packages('reshape2')
install.packages('knitr')

install.packages("htmlwidgets")
install.packages("htmltools")
install.packages("jsonlite")
install.packages("yaml")
install.packages("base64enc")

library(htmlwidgets)

library(htmltools)
# writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
# Sys.which("make")
library(jsonlite)
library(yaml)
library(base64enc)

library(devtools)
devtools::install_github("lchiffon/wordcloud2")


library(dplyr)
library(data.table) 
library(reshape2)
library(knitr)

remotes::install_github('haven-jeon/KoNLP', upgrade = 'never', INSTALL_opts=c('--no-multiarch'))

library(KoNLP) #한글 형태소분석
library(dplyr)

# java 경로
Sys.setenv(JAVA_HOME="C:/Users/c/.sdkman/candidates/java/current/11.0.5-zulu/bin/javaw.exe")

# 데이터 읽어들이기
txt=fread("web1_df.csv", sep = "," ,header = TRUE, stringsAsFactors = FALSE, encoding = 'UTF-8')



knitr::kable(txt)
str(txt)
names(txt)
txt$Title


nouns = extractNoun(txt$Title)
head(nouns)

#unlist(nouns) list -> vector
#단어별 빈도수 구하기
wordcount = table(unlist(nouns))

df_word = as.data.frame(wordcount, stringsAsFactors = F)
head(df_word)

# header name 변경
df_word = rename(df_word,
                 word = Var1,
                 freq = Freq)
# 두 글자 이상 단어 추출
df_word = filter(df_word, nchar(word)>=2)

# 빈도수 최대값 제외 등 제한조건
df_word$freq = ifelse(df_word$freq > 100 , 0, df_word$freq)

# 편의점 제외
df_word$word = ifelse(df_word$word == "CU" | df_word$word == "GS25" |df_word$word == "세븐일레븐", NA, df_word$word )

#100개만 가지고오기
top20 = df_word %>% 
  filter(!is.na(word)) %>% 
  arrange(desc(freq)) %>% 
  head(200)
top20

# install.packages("wordcloud")
install.packages("wordcloud2")
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

# Dark2 색상 목록에서 8개 색상 추출
pal <- brewer.pal(8,"Dark2")  

# 워드클라우드2
wordcloud2(top20, size = 0.5, fontFamily = '나눔바른고딕')

letterCloud(df_word, word = "도시락", size = 1)

# figPath = system.file("sunglasses.jpg",package = "wordcloud2")
wordcloud2(df_word, figPath = "sunglasses.jpg", size = 1.5, color = "skyblue")

set.seed(1234)                   # 난수 고정
wordcloud(words = df_word$word,  # 단어
          freq = df_word$freq,   # 빈도
          min.freq = 2,          # 최소 단어 빈도
          max.words = 200,       # 표현 단어 수
          random.order = F,      # 고빈도 단어 중앙 배치
          rot.per = .3,          # 회전 단어 비율
          scale = c(4, 0.3),     # 단어 크기 범위
          colors = pal)          # 색깔 목록
 
