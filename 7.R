#패키지 설치
install.packages('rJava')
install.packages('memoise')
install.packages('KoNLP')
install.packages('remotes')
install.packages('data.table')
install.packages('reshape2')
install.packages('knitr')

library(dplyr)
library(data.table)
library(reshape2)
library(knitr)

remotes::install_github('haven-jeon/KoNLP', upgrade = 'never', INSTALL_opts=c('--no-multiarch'))

library(KoNLP)
library(dplyr)
Sys.setenv(JAVA_HOME="C:/Users/c/.sdkman/candidates/java/current/11.0.5-zulu/bin/javaw.exe")

# txt = readLines('hiphop.txt')
# txt = readLines('web_df.csv')
head(txt)
# 데이터 읽어들이기
# data1 = read.table(choose.files(), header = T, sep = ',')
txt=fread("web_df.csv", sep = "," ,header = TRUE, stringsAsFactors = FALSE)
      
knitr::kable(txt)
str(txt)
names(txt)
txt$Description

install.packages('stringr')
library(stringr)
txt = str_replace_all(txt, "\\W"," ")
head(txt)

library(KoNLP)
library(dplyr)
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다")

nouns = extractNoun(txt$Description)
head(nouns)
#unlist(nouns) list -> vector
#단어별 빈도수 구하기
wordcount = table(unlist(nouns))

df_word = as.data.frame(wordcount, stringsAsFactors = F)
head(df_word)

#header name 변경
df_word = rename(df_word,
                 word = Var1,
                 freq = Freq)
#두 글자 이상 단어 추출
df_word = filter(df_word, nchar(word)>=2)
df_word$freq = ifelse(df_word$freq > 900, mean(df_word$freq), df_word$freq)

top20 = df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top20

# install.packages("wordcloud")
install.packages("wordcloud2")
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

# Dark2 색상 목록에서 8개 색상 추출
pal <- brewer.pal(8,"Dark2")  

wordcloud2(df_word)
set.seed(1234)                   # 난수 고정
wordcloud(words = df_word$word,  # 단어
          freq = df_word$freq,   # 빈도
          min.freq = 2,          # 최소 단어 빈도
          max.words = 200,       # 표현 단어 수
          random.order = F,      # 고빈도 단어 중앙 배치
          rot.per = .1,          # 회전 단어 비율
          scale = c(4, 0.3),     # 단어 크기 범위
          colors = pal)          # 색깔 목록

