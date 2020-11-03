#직업별 급여 차이
library(readxl)
list_job=read_excel('Koweps_Codebook.xlsx',
                    col_names = T,
                    sheet=2)
head(list_job)
head(welfare)
head(welfare$code_job)
#welfare에 직업명 추가
welfare = left_join(welfare, list_job, id='code_job')
head(welfare$job)
#결측치 제거
welfare %>% 
  filter(!is.na(code_job)) %>%
  select(code_job, job) %>% 
  head

###########################################################
#직업 급여 상위 10개 추출
job_income=welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income=mean(income))
head(job_income)

#상위 10개 직업
top10 = job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)
top10
#시각화
ggplot(data=top10,
       aes(x=reorder(job,mean_income),
           y=mean_income))+
  geom_col()+
  coord_flip()

#하위 5위 직업
bot5 = job_income %>% 
  arrange(mean_income) %>% 
  head(5)
#시각화
ggplot(data=bot5,
       aes(x=reorder(job,-mean_income),
           y=mean_income,
           fill=job))+
  geom_col()+
  coord_flip()+
  guides(fill='none')+
  ylim(0,100)

#남성 직업 빈도 상위 10개 추출
# summarise(n = n())
table(welfare$sex)
job_male=welfare %>% 
  filter(sex=='male' & !is.na(job)) %>% 
  group_by(sex,job) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

ggplot(data=job_male, aes(x=reorder(job,n), y=n))+
  geom_col()+coord_flip()

#종교 유무에 따른 이혼율
class(welfare$religion)
table(welfare$religion) # 1:종교있음
welfare$religion = ifelse(welfare$religion==1,'yes','no')

class(welfare$marriage)
table(welfare$marriage) # 1:유배우, 3:이혼

welfare$group_marriage = ifelse(welfare$marriage==1,'marriage',ifelse(welfare$marriage==3,'divorce',NA))
table(welfare$group_marriage)

rema=welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion,group_marriage) %>% 
  summarise(n=n()) %>% 
  #총 데이터
  mutate(tot_group=sum(n)) %>% 
  #각 데이터별 확률
  mutate(pct=round(n/tot_group*100,1))
rema

#이혼 추출
divorce = ageg_religion_marriage %>% 
  filter(group_marriage=='divorce') %>% 
  select(ageg,religion,pct)
divorce

ggplot(data=divorce,
       aes(x=ageg, y=pct, fill=religion))+
  geom_col(position = 'dodge')

#연령대, 종교, 결혼 상태별 비율표 만들기
welfare$age = 2015-welfare$birth+1
welfare = welfare %>% 
  mutate(ageg = ifelse(age<30, "young",
                       ifelse(age<=59, "middle", "old")))

ageg_religion_marriage = welfare %>% 
  filter(!is.na(group_marriage) & ageg!='young') %>% 
  group_by(ageg, religion, group_marriage) %>% 
  summarise(n=n()) %>% 
  #총 데이터
  mutate(tot_group=sum(n)) %>% 
  #각 데이터별 확률
  mutate(pct=round(n/tot_group*100,1))
ageg_religion_marriage


install.packages('plotly')
library(plotly)
p = ggplot(data=mpg, aes(x=displ, y=hwy, col=drv))+
  geom_point()
ggplotly(p)

#시계열 그래프
install.packages('dygraphs')
library(dygraphs)
library(xts)
economics=ggplot2::economics
eco = xts(economics$unemploy, order.by = economics$date)
dygraph(eco)

dygraph(eco) %>% 
  dyRangeSelector()
