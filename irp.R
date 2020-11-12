library(readxl)
library(ggplot2)
library(dplyr)
install.packages("writexl")
library(writexl)



item=read.csv("archive/olist_order_items_dataset.csv")
review=read.csv("archive/olist_order_reviews_dataset.csv")
ir=inner_join(item,review,by="order_id")
str(ir)
product=read.csv("archive/olist_products_dataset.csv")
irp=inner_join(ir,product,by="product_id")
str(irp)

table(irp$review_score)
summarise(irp,sum=irp$review_score)

irp$review_comment_message=ifelse(irp$review_comment_message=="", NA, irp$review_comment_message)
table(!is.na(irp$review_comment_message))
#irp=as.data.frame(irp)
#irp$review_comment_message <- na.omit(irp$review_comment_message)
# 리뷰 없는 데이터 빼기
irp=irp %>% 
  filter(!is.na(review_comment_message)) %>% 
  select(product_category_name,review_score,review_comment_message)


table(!is.na(irp$review_comment_message))
#카테고리별 갯수
table(irp$product_category_name)
# 평점 별 갯수
table(irp$review_score)

#table(irp$review_comment_message)
write.csv(irp,"archive/irp3.csv")

# 구별 생활인구변화
gupop=read_excel("gupop.xlsx", col_names=T, sheet=2)
table(gupop)
str(gupop)
min(gupop$합계)
sort(gupop$합계, decreasing=T)[9]

# 2018년 1분기 가장인구수 많은 구
gupop.2018=gupop %>% 
  filter(기간=='2018.1/4') %>%
  summarise(자치구,합계)

# 분기별 총 합계 제외
gupop.2018$합계=ifelse(gupop.2018$합계>690000,NA,gupop.2018$합계)
table(gupop.2018$합계)
ggplot(gupop.2018$합계)

ggplot(data = gupop.2018, aes(x=자치구, y=합계))+
  geom_col()+
  coord_flip()

min(gupop.2018$합계)
max(gupop.2018$합계)
sort(gupop.2018$합계, decreasing=T)[1]

###################################################################
# 자치구별 남자 수
names(gupop)[names(gupop)=="총남자"]=c("totalman")
gupop$totalman = ifelse(gupop$totalman>340000,NA,gupop$totalman)

table(na.omit(gupop$totalman))
min(na.omit(gupop$totalman))
max(na.omit(gupop$totalman))


gupop.man=gupop %>% 
  filter(!is.na(totalman)) %>% 
  group_by(자치구,기간) %>% 
  mutate(mean_totalman=mean(totalman)) %>% 
  summarise(mean_totalman)
  
# 남자시각화
ggplot(data=gupop.man, aes(x=gupop.man$자치구, y=gupop.man$mean_totalman))+
  geom_col()+
  coord_flip()

# 자치구별 여자 수
sort(gupop$총여자, decreasing=T)
names(gupop)[names(gupop)=="총여자"]=c("totalwoman")
gupop$totalwoman = ifelse(gupop$totalwoman>360000,NA,gupop$totalwoman)

gupop.woman=gupop %>% 
  filter(!is.na(totalwoman)) %>% 
  group_by(자치구,기간) %>% 
  mutate(mean_tatalwoman=mean(totalwoman)) %>% 
  summarise(mean_tatalwoman)

#여자 시각화
ggplot(data=gupop.woman, aes(x=gupop.woman$자치구, y=gupop.woman$mean_tatalwoman))+
  geom_col()+
  coord_flip()

#  +age data
gupopage=read_excel("gupopage.xlsx")
View(gupopage)


gupopage=gupopage %>% 
  mutate(
    ten=gupopage[,17]+gupopage[,18],
    twenty=gupopage[,19]+gupopage[,20],
    thirty=gupopage[,21]+gupopage[,22],
    fourty=gupopage[,23]+gupopage[,24],
    fifty=gupopage[,25]+gupopage[,26],
    sixty=gupopage[,27]+gupopage[,28]) 

gupopage=as.data.frame(gupopage)

write_xlsx(gupopage, path = 'gupopage1.xlsx')

# 10대가 많은 지역
gupopten=gupopage %>% 
  group_by(자치구) %>% 
  summarise(mean_ten=mean(ten)) %>% 
  arrange(desc(mean_ten))


ggplot(data = gupopten, aes(x=reorder(자치구,mean_ten), y=mean_ten))+
  geom_col()+
  coord_flip()

# 20대가 많은 지역
gupoptwenty=gupopage %>% 
  group_by(자치구) %>% 
  summarise(mean_twenty=mean(twenty)) %>% 
  arrange(desc(mean_twenty))

ggplot(data = gupoptwenty, aes(x=reorder(자치구,mean_twenty), y=mean_twenty))+
  geom_col()+
  coord_flip()

# 30대가 많은 지역
gupopthirty=gupopage %>% 
  group_by(자치구) %>% 
  summarise(mean_thirty=mean(thirty)) %>% 
  arrange(desc(mean_thirty))

ggplot(data = gupopthirty, aes(x=reorder(자치구,mean_thirty), y=mean_thirty))+
  geom_col()+
  coord_flip()



# 10대 + 20대
tentwenty=inner_join(gupopten, gupoptwenty, by="자치구")
# 10대 + 20대 + 30대
tentwentythirty=inner_join(tentwenty,gupopthirty,by="자치구")





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




