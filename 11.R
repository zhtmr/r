install.packages('data.table')
install.packages('reshape2')
install.packages('knitr')

library(dplyr)
library(data.table)
library(reshape2)
library(knitr)

#데이터 읽어 들이기
data1 = read.table(choose.files(), header = T, sep = ',')
knitr::kable(data1)
data1


movie_ratings = acast(data1, title~critic, value.var = 'rating')
movie_ratings = as.data.frame(movie_ratings)
knitr::kable(movie_ratings)

#사용자별 유사도 complete.obs: 결측치 제거
sim_users = cor(movie_ratings, use='complete.obs')
knitr::kable(sim_users)

rating_critic = setDT(movie_ratings[colnames(movie_ratings)[6]],
                      keep.rownames = T)[]
rating_critic
names(rating_critic) = c('title', 'rating')

#Toby가 안본 영화 추출
titlesNAcritic = rating_critic$title[is.na(rating_critic$rating)]
titlesNAcritic

# Toby가 안본제목 영화 모두 출력
ratings_t = data1[data1$title %in% titlesNAcritic, ]

x=setDT(data.frame(sim_users[,6]), keep.rownames = T)[]
names(x)=c('critic', 'similarity')

ratings_t = merge(ratings_t, x, by='critic', all.x=T)
knitr::kable(ratings_t)

#유사도점수 구하기 (나랑 유사도가 높은사람이 준 점수가 곧 Toby가 메길 점수와 비슷할것이다)
ratings_t$sim_rating = ratings_t$rating * ratings_t$similarity
knitr::kable(ratings_t)

#Toby의 예상점수
result1 = ratings_t %>% 
  group_by(title) %>% 
  summarise(sum(sim_rating) / sum(similarity))
result1

names(result1) = c('title','rating')
toby.mean = mean(rating_critic$rating, na.rm=T)
result1 %>% 
  filter(result1$rating > toby.mean)



# 모든 사람에게 추천알고리즘 적용
getMovie=function(){
  data1 <- read.table(choose.files(), header = TRUE, sep = ",")
  knitr::kable(data1)
  
  movie_ratings <- acast(data1, title ~ critic, value.var = "rating")
  movie_ratings <- as.data.frame(movie_ratings)
}
getUser=function(movie, usr){
  return(colnames(movie)[usr])
}
# INPUT : USER ID
generate_Recomm <- function(user) {
  
  # 3. 1.
  rating_critic <- setDT(movie_ratings[colnames(movie_ratings)[user]],
                         keep.rownames = TRUE)[]
  names(rating_critic) <- c("title", "rating")
  
  # 3. 2.
  title_NA_critic <- rating_critic$title[is.na(rating_critic$rating)]
  
  ratings_t <- data1[data1$title %in% title_NA_critic, ]
  
  x <- setDT(data.frame(sim_users[, user]),
             keep.rownames = TRUE)[]
  names(x) <- c("critic", "similarity")
  
  ratings_t <- merge(ratings_t, x,
                     by = "critic", all.x = TRUE)
  
  # 3. 3.
  ratings_t$sim_rating <- ratings_t$rating * ratings_t$similarity
  
  # 3. 4.
  result <- ratings_t %>% 
    group_by(title) %>% 
    summarise( sum(sim_rating) / sum(similarity) )
  
  # 3.5
  names(result) <- c("title", "rating")
  usr.mean=mean(rating_critic$rating, na.rm = TRUE)
  result = result %>% filter(result$rating>usr.mean)
  
  return(result)
}
for (i in 1:6) {
  print(getUser(movie_ratings, i))
  print( generate_Recomm(i) )
}

