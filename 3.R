library(ggplot2)

# x축 displ, y축 hwy로 지정해 배경 생성
ggplot(data=mpg,aes(x=displ,y=hwy,color=drv))+
  # 배경에 산점도 추가
  geom_point(
    # 점 크기 지정 
    size=5
  )+
  #범위 지정
  xlim(3,6)+
  ylim(10,30)+
  stat_smooth()

head(mpg)

ggplot(data=mpg,aes(x=cty,y=hwy,color=drv))+
  geom_point()+
  stat_smooth()

str(midwest)
ggplot(data=midwest,aes(x=poptotal,y=popasian,color=state))+
  geom_point()+
  xlim(0,500000)+
  ylim(0,10000)+
  stat_smooth()


library(dplyr)
df.mpg=mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy))

df.mpg

ggplot(data=df.mpg,
       aes(x=drv,y=mean_hwy))+
  geom_col()


ggplot(data=df.mpg,
       # reorder를 이용하여 정렬
       # +: 오름, -:내림
       aes(x=reorder(drv,mean_hwy),y=mean_hwy))+
  geom_col()

?geom_col()

ggplot(data=df.mpg,
       aes(x=reorder(drv,-mean_hwy),y=mean_hwy))+
  geom_col()


# 빈도막대그래프
ggplot(data=mpg,aes(x=drv))+geom_bar(
  mapping = aes(
    x=drv,
    fill=drv
  )
)+
  theme(legend.position = "bottom",
        plot.title = element_text(
          size=30, face = 'bold'
        ))+
  scale_fill_manual(
    #객체에 대한 색상 지정
    values = c('red','green','blue'),
    #범례 이름 지정
    name="구동방식",
    #각 객체별 이름 지정
    labels=c('4륜','전륜','후륜')
  )+
  labs(
    #각 축별 이름 지정
    x='drv(구동)',
    y='count(빈도)',
    #그래프 이름 지정
    title='구동별 빈도 수 분석',
    #그래프 부제목 지정
    subtitle='막대그래프',
    #부연설명
    caption='출처 : 제작팀'
  )+
  geom_text(
    #y축의 수치 출력
    stat='count',
    aes(label=..count..),
    #좌우측 여백 지정
    position=position_dodge(width=1.8),
    #수치값의 위치 지정, +:아래로, -:위로
    vjust=-0.5
  )+
  ylim(c(0,120))

# 어떤 회사에서 생산한 'suv' 차종의 도시 연비가 높은가
mpg=as.data.frame(ggplot2::mpg)
df.mpg=mpg %>% group_by(manufacturer) %>% filter(class=='suv')%>% summarise(mean_cty=mean(cty)) %>% arrange(desc(mean_cty)) %>% head(5)

ggplot(data=df.mpg,aes(x=reorder(manufacturer,-mean_cty),y=mean_cty))+
  geom_col(
    mapping = aes(
      x=reorder(manufacturer,-mean_cty),
      fill=reorder(manufacturer,-mean_cty),
    )
  )+guides(fill='none')+
  geom_text(
    aes(label=round(mean_cty,2)),
    vjust=2,
    colour='white'
  )+
  ggtitle('회사별 도시연비')+
  labs(
    x='회사명',
    y='도시연비'
  )+
  theme(
    plot.title = element_text(
      family='serif', size=15, face = 'bold', colour = 'darkblue', hjust = 0.5
    )
  )
  
  

#어떤 class 자동차가 가장 많은가
ggplot(data=mpg,aes(x=class))+
  geom_bar(
    mapping = aes(
      x=class,
      fill=class
    )
  )+
  ggtitle('차종별 빈도 수 분석')+
  labs(
    x='class(차종)',
    y='count(빈도)',
    subtitle = '막대그래프'
  )+
  geom_text(
    stat = 'count',
    aes(label=..count..),
    vjust=-1,
    position=position_dodge(width = 1.8)
  )+
  scale_fill_discrete(
    name="차종"
  )



ggplot(data=mpg, aes(x=class))+geom_bar() 

ggplot(data = economics, aes(x=date, y=unemploy))+
  geom_line()

head(Orange)
table(Orange$Tree)

Orange %>% 
  filter(Tree==1) %>% 
  ggplot(aes(age,circumference))+
  geom_line()

ggplot(Orange, aes(age,circumference))+
  geom_line()

ggplot(Orange, aes(age,circumference, color=Tree))+
  geom_line()

ggplot(Orange, aes(age,circumference))+
  geom_line(aes(color=Tree))

ggplot(Orange, aes(age,circumference, color=Tree))+
  geom_line(linetype=6)+
  #그래프의 배경색 제거
  theme(panel.background = element_blank())

ggplot(Orange, aes(age,circumference, color=Tree))+
  geom_line(aes(linetype=Tree))+
  #그래프의 배경색 제거
  theme(panel.background = element_blank())
