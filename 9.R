install.packages('arules')
library(arules)

buyItems <- list(
  c("삼겹살", "생수", "소주", "과자")
  ,c("삼겹살", "생수", "소주", "사과")
  ,c("장어", "생수", "소주", "양파")
  ,c("땅콩", "생수", "맥주", "오이")
  ,c("땅콩", "생수", "맥주", "감")
)
buyItemStr = as(buyItems, 'transactions')
buyItemStr
inspect(buyItemStr)
buyItemResult = apriori(buyItemStr, parameter = list(
  support = 0.1, confidence=0.8
))

buyItemResult[1:5]
inspect(buyItemResult[1:5])


#subset
subBuyResult = subset(buyItemResult, subset=lift>1)
inspect(subBuyResult[1:5])


inspect(subset(buyItemResult, subset=lhs %in% c('삼겹살')))
inspect(subset(buyItemResult, subset=lhs %ain% c('삼겹살','과자')))
inspect(subset(buyItemResult, subset=lhs %oin% c('삼겹살','과자')))
inspect(subset(buyItemResult, subset=lhs %pin% c('겹')))

subBuyResultOrder = sort(subBuyResult,
                         by=c('support', 'lift', 'confidence'))
inspect(subBuyResultOrder[1:10])

