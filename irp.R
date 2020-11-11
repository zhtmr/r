library(readxl)
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

# 2018년 1분기 가장인구수 많은 구
gupop.2018=gupop %>% 
  filter(기간=='2018.1/4') %>%
  summarise(자치구,합계)

gupop.2018$합계=ifelse(gupop.2018$합계>680000,NA,gupop.2018$합계)
table(gupop.2018$합계)
ggplot(gupop.2018$합계)

ggplot(data = gupop.2018, aes(x=자치구, y=합계))+
  geom_col()+
  coord_flip()

min(gupop.2018$합계)
max(gupop.2018$합계)
sort(gupop.2018$합계, decreasing=T)[2]


