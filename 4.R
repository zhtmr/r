install.packages("foreign")
library(foreign) #spss 파일로드
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
raw_welfare=read.spss(file="Koweps_hpc10_2015_beta1.sav",to.data.frame = T)

welfare=raw_welfare
head(welfare)

welfare=rename(welfare, 
               sex=h10_g3, 
               birth=h10_g4, 
               marriage=h10_g10, 
               religion=h10_g11, 
               income=p1002_8aq1,
               code_job=h10_eco9,
               code_region=h10_reg7)

#성별에 따른 월급 차이
#1. 변수 검토 및 전처리
# - 성별
# - 월급
#2. 변수 간 관계 분석
# - 성별 월급 평균표 만들기
# - 그래프 만들기

class(welfare$sex)
#종류별 빈도수
table(welfare$sex)

welfare$sex=ifelse(welfare$sex>2,NA,welfare$sex)
welfare$sex=ifelse(welfare$sex<1,NA,welfare$sex)

welfare$sex=ifelse(welfare$sex==1, 'male','female')
table(welfare$sex)
qplot(welfare$sex)



class(welfare$income)
summary(welfare$income)


attach(welfare)
summary(income)
boxplot(income)
boxplot(income)$stat
detach(welfare)

attach(welfare)
class(income)
summary(income)
na.mean=mean(income, na.rm = T)
na.mean
income=ifelse(is.na(income), na.mean, income)
summary(income)
detach(welfare)

welfare$income = ifelse(welfare$income==0 | welfare$income>608, NA, welfare$income)
table(is.na(welfare$income))

sex_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income=mean(income))
sex_income

ggplot(data=sex_income, aes(x=sex, y=mean_income))+
  geom_col(mapping=aes(x=sex, fill=sex))+
  #scale_x_continuous(breaks = c(1,2), labels = c('남','여'))+  # 위에서 이미 male, female로 바꿔서 할필요없음
  guides(fill='none')+
  theme(plot.title=element_text(family='serif', face = 'bold',
                                hjust=0.5, size=25, 
                                color = 'darkblue'),
        axis.title.x = element_text(face = 'bold',size=15, 
                                    color = '#339933'),
        axis.title.y = element_text(face = 'bold',size=15, 
                                    color = '#993333'))+
  ggtitle('성별에 따른 평균 급여')+
  labs(x='성별', y='평균임금')+
  geom_text(
    aes(label=round(mean_income, 2)),
    vjust=2, colour='white', size=6
  )


#1. 변수 검토 및 전처리
# - 나이
# - 월급
#2. 변수 간 관계 분석
# - 나이에 따른 월급 평균포 만들기
# - 그래프 만들기

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

ggplot(data=age_income, aes(x=age, y=mean_income))+
  geom_line()

