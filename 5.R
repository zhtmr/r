library(dplyr)

head(welfare)

head(welfare$age)

welfare=welfare %>% 
  mutate(ageg = ifelse(age<30, 'young', ifelse(age<=59, 'middle', 'old')))

table(welfare$ageg)
qplot(welfare$ageg)

ageg_income=welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income=mean(income))
ageg_income

#시각화
ggplot(data = ageg_income, aes(x=ageg, y=mean_income))+
  geom_col()

# 정렬
ggplot(data = ageg_income, aes(x=ageg, y=mean_income))+
  geom_col()+
  scale_x_discrete(limits=c('young','middle','old'))


sex_income=welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg,sex) %>% 
  summarise(mean_income=mean(income))

#이중 차트 만들기
ggplot(data = sex_income,
       aes(x=ageg,y=mean_income,
         #fill=sex
         fill=as.factor(sex)
       ))+
  geom_col(position='dodge')+
  scale_x_discrete(limits=c('young','middle','old'))+
  theme(plot.title = element_text(family = 'serif',
                                  face='bold',
                                  hjust=0.5,
                                  size=15,
                                  color='darkblue'))+
  labs(x='연령대', y='평균급여', title='연령대별 성별 평균급여')+
  scale_fill_manual(
    values=c('darkblue','hotpink'),
    name='성별',
    labels=c('남자','여자')
  )


agesex_income=welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age,sex) %>% 
  summarise(mean_income=mean(income))

ggplot(data=agesex_income, aes(x=age,
                               y=mean_income,
                               #col=sex
                               col=as.factor(sex)
                               ))+
  scale_color_discrete(
    name='성별',
    labels=c('남','여')
  )+
  geom_line()
 

