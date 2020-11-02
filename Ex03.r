#1. 변수 검토 및 전처리
#- 나이
#- 월급
#2. 변수 간 관계 분석
#- 나이에 따른 월급 평균표 만들기
#- 그래프 만들기

class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

table(is.na(welfare$birth))

welfare$age = 2015-welfare$birth+1
summary(welfare$age)
qplot(welfare$age)

age_income=welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income=mean(income))
head(age_income)
ggplot(data = age_income, aes(x=age, y=mean_income))+
  geom_line()









