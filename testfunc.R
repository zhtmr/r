
#customer의 소비성향 분석*반복
testFunc = function(i){
  customer = df_data %>% filter(customer_id==i)
  
  return(prop.table(table(factor(customer$trendy, levels = c(0,1))))*100)
}

customer11= df_data %>% filter(customer_id==11)
customer11[is.na(customer11)]=0

table(customer11$trendy)
table(factor(customer11$trendy, levels = c(0,1)))

testFunc(11)

a=list()
for (i in 1:99441){
  a[[i]] <- testFunc(i-1)
}



#데이터프레임 형식으로 변환
library(plyr)
b=ldply(a, data.frame)
View(b)

aa = b[!(b$Var1 == "0"),]
View(aa)

aa[,"customer_id"] = c(0:99440)

names(aa)
names(customer_trend) = c("customer_id","trendy_bought_proportion")
aa = aa[,-c(1)]
customer_trend = aa[c(2,1)]
View(customer_trend)

#트렌디한 소비자 추출하기
trendy_customer = customer_trend[customer_trend$trendy_bought_proportion>=50,]
View(trendy_customer)
trendy_customer %>% filter(!is.na(customer_id)&!is.na(trendy_bought_proportion))

#df_data에서 트렌디한 소비자만 추출하기
# trendy_order_info=left_join(trendy_customer,df_data,by="customer_id")
# trendy_order_info <- na.omit(trendy_order_info)
# View(trendy_order_info)
trendy_order_info=read.csv("trendy_order_info.csv")

customer_table<-read.csv("customer_table.csv")
View(customer_table)
names(customer_table)[1] = c("customer_id")
trendy_customer_info=left_join(trendy_customer,customer_table,by="customer_id")
View(trendy_customer_info)

#트렌디한 소비자가 자주 구매하는 물품 카테고리 추출하기

detach(package:plyr)
cnt = trendy_order_info %>% group_by(category) %>% filter(customer_id ==80) %>% summarise(n=n()) 
cnt


cntFunc = function(i){
 trendy_order_info %>% group_by(category) %>% filter(customer_id=={{i}}) %>% summarise(n=n()) %>% top_n(n=1)
}
cntFunc(108)

trendy_customer_list = unique(trendy_order_info[,1])
trendy_customer_list
length(trendy_customer_list)
View(trendy_customer_list)

unique(trendy_order_info$customer_id)

for (i in unique(trendy_order_info$customer_id)){
  cntFunc(i)
  #trendy_order_info %>% group_by(category) %>% filter(customer_id=={{i}}) %>% summarise(n=n()) %>% top_n(n=1)
}

cnt = trendy_order_info %>% group_by(customer_id,category) %>% summarise(n=n()) %>% top_n(n=1)
View(cnt)
head(z)
View(z)
