library(readxl)
library(ggplot2)
library(dplyr)
install.packages("writexl")
library(writexl)
install.packages("lubridate")
library(lubridate)

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






################################################################################
#남녀


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

################################################################################
# 연령대


#  +age data
gupopage=read_excel("gupopage.xlsx")
View(gupopage)

# 10, 20, 30, 40, 50, 60대 표시
gupopage=gupopage %>% 
  mutate(
    ten=gupopage[,17]+gupopage[,18],
    twenty=gupopage[,19]+gupopage[,20],
    thirty=gupopage[,21]+gupopage[,22],
    fourty=gupopage[,23]+gupopage[,24],
    fifty=gupopage[,25]+gupopage[,26],
    sixty=gupopage[,27]+gupopage[,28]) 
gupopage=gupopage %>% 
  mutate(
    tenTosixtySum=ten + twenty + thirty + fourty + fifty + sixty
  )
gupopage=as.data.frame(gupopage)

# 파일로 저장
#write_xlsx(gupopage, path = 'gupopage1.xlsx')
gupopage=read_excel("gupopage1.xlsx")
View(gupopage)

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

# 40대가 많은 지역
gupopfourty=gupopage %>% 
  group_by(자치구) %>% 
  summarise(mean_fourty=mean(fourty)) %>% 
  arrange(desc(mean_fourty))

ggplot(data = gupopfourty, aes(x=reorder(자치구,mean_fourty), y=mean_fourty))+
  geom_col()+
  coord_flip()


# 10대 + 20대
tentwenty=inner_join(gupopten, gupoptwenty, by="자치구")
# 10대 + 20대 + 30대
tentwentythirty=inner_join(tentwenty,gupopthirty,by="자치구")


# 지역별 가장 많은 연령대 비율
ages=colnames(gupopage[36:41])[max.col(gupopage[36:41], ties.method = "first")]
#gupopage=subset(gupopage, select = -많은연령대)

gupopage=gupopage %>% 
  group_by(자치구) %>% 
  mutate(많은연령대=ages)



gupopage=cbind(gupopage,ages)

names(gupopage)
gupopage=rename(gupopage, 많은연령대="NA")
View(gupopage)
#names(gupopage)[36] : ten
#names(gupopage)[41] : sixty

colnames(gupopage[36:41])[max.col(gupopage[36:41], ties.method = "first")]

################################################################################
# 내부데이터


# read inner data
customer_table=read.csv('inner_data/customer_table.csv')
order_table=read.csv('inner_data/order_2.csv')
order_item=read.csv('inner_data/order_item.csv')
product=read.csv('inner_data/product_2.csv')


# order_item.csv + product_2.csv
orderItemProduct=left_join(order_item, product, by="product_id")
View(orderItemProduct)
# + order_table.csv
orderItemProductTime=left_join(orderItemProduct, order_table, by ="order_id")
View(orderItemProductTime)

write.csv(orderItemProductTime, file="orderItemProductTime12.csv",row.names = F)
# + customer.csv
orderItemProductTime=read.csv('orderItemProductTime12.csv')
names(orderItemProductTime)
names(customer_table)[1]=c("customer_id")
OIPTC=left_join(orderItemProductTime, customer_table, by="customer_id")
View(OIPTC)
write.csv(OIPTC, file = "OIPTC.csv", row.names = F)
read.csv('OIPTC.csv')

# 구 코드
code=c(1111000000,1114000000,1117000000,1120000000,1121500000,1123000000,1126000000,1129000000,1130500000,1132000000,
1135000000,1138000000,1141000000,1144000000,1147000000,1150000000,1153000000,1154500000,1156000000,1159000000,
1162000000,1165000000,1168000000,1171000000,1174000000)

guname=c("종로구","중구","용산구","성동구","광진구","동대문구","중랑구","성북구","강북구","도봉구","노원구","은평구","서대문구","마포구","양천구","강서구",
"구로구","금천구","영등포구","동작구","관악구","서초구","강남구","송파구","강동구")

code[1]
guname[1]


# 지역 코드 한글로 변경
OIPTC$gu_code=recode(OIPTC$gu_code, '1111000000' ='종로구', '1114000000'='중구', '1117000000'='용산구', '1120000000'='성동구', '1121500000'='광진구',
       '1123000000'='동대문구', '1126000000'='중랑구', '1129000000'='성북구', '1130500000'='강북구', '1132000000'='도봉구', '1135000000'='노원구',
       '1138000000'='은평구', '1141000000'='서대문구', '1144000000'='마포구', '1147000000'='양천구', '1150000000'='강서구', '1153000000'='구로구',
       '1154500000'='금천구', '1156000000'='영등포구', '1159000000'='동작구', '1162000000'='관악구', '1165000000'='서초구', '1168000000'='강남구',
       '1171000000'='송파구', '1174000000'='강동구')



# 구매날짜 분기로 변경
year=year(OIPTC$order_purchase_time_stamp)
month=month(OIPTC$order_purchase_time_stamp)
OIPTC$order_purchase_time_stamp=paste(year, ".", month, sep = '')
View(OIPTC)


OIPTC$order_purchase_time_stamp=recode(OIPTC$order_purchase_time_stamp, '2018.1'='2018.1/4', '2018.2'='2018.1/4', '2018.3'='2018.1/4', '2018.4'='2018.2/4', '2018.5'='2018.2/4',
       '2018.6'='2018.2/4', '2018.7'='2018.3/4', '2018.8'='2018.3/4', '2018.9'='2018.3/4', '2018.10'='2018.4/4', '2018.11'='2018.4/4', '2018.12'='2018.4/4', '2019.1'='2019.1/4',
       '2019.2'='2019.1/4', '2019.3'='2019.1/4', '2019.4'='2019.2/4', '2019.5'='2019.2/4', '2019.6'='2019.2/4', '2019.7'='2019.3/4', '2019.8'='2019.3/4',
       '2019.9'='2019.3/4', '2019.10'='2019.4/4', '2019.11'='2019.4/4', '2019.12'='2019.4/4')


write.csv(OIPTC, file = "OIPTC1.csv", row.names = F)

str(OIPTC)

# 분기별 많이 팔린 제품
View(OIPTC %>% 
  group_by(product_id, order_purchase_time_stamp) %>% 
  summarise(sum=sum(quantity)) %>% 
  arrange(desc(sum)))
  

# 연령대별 잘 팔린 물건
hotitem=OIPTC %>% 
  group_by(product_id,age) %>% 
  summarise(sum=sum(quantity)) %>% 
  arrange(desc(sum))

# 10대 top10 item
hotitem %>% 
  filter(age==1) %>% 
  summarise(product_id,sum) %>% 
  arrange(desc(sum)) %>% 
  head(10)

# 20대 top10 item
hotitem %>% 
  filter(age==2) %>% 
  summarise(product_id,sum) %>% 
  arrange(desc(sum)) %>% 
  head(10)

# 30대 top10 item
hotitem %>% 
  filter(age==3) %>% 
  summarise(product_id,sum) %>% 
  arrange(desc(sum)) %>% 
  head(10)

# 40대 top10 item
hotitem %>% 
  filter(age==4) %>% 
  summarise(product_id,sum) %>% 
  arrange(desc(sum)) %>% 
  head(10)

# 지역별 나이대 분포
ageItem=OIPTC %>% 
  group_by(gu_code, age) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

# 10대 구매자가 많은 지역
ageItem %>% 
  filter(age==1) %>% 
  select(gu_code)






 
  



