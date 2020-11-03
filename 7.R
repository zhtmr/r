#패키지 설치
install.packages('rJava')
install.packages('memoise')
install.packages('KoNLP')
install.packages('remotes')

remotes::install_github('haven-jeon/KoNLP', upgrade = 'never', INSTALL_opts=c('--no-multiarch'))

library(KoNLP)
library(dplyr)
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_261/")

txt = readLines('hiphop.txt')
head(txt)

install.packages('stringr')
library(stringr)
txt = str_replace_all(txt, "\\W"," ")
head(txt)

library(KoNLP)
library(dplyr)
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다")

nouns = extractNoun(txt)
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

top20 = df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top20

install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)

# Dark2 색상 목록에서 8개 색상 추출
pal <- brewer.pal(8,"Dark2")  

set.seed(1234)                   # 난수 고정
wordcloud(words = df_word$word,  # 단어
          freq = df_word$freq,   # 빈도
          min.freq = 2,          # 최소 단어 빈도
          max.words = 200,       # 표현 단어 수
          random.order = F,      # 고빈도 단어 중앙 배치
          rot.per = .1,          # 회전 단어 비율
          scale = c(4, 0.3),     # 단어 크기 범위
          colors = pal)          # 색깔 목록

