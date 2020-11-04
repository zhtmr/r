library(arules)
itemFrequencyPlot(buyItemStr, support=0.2)

install.packages('arulesViz')
library(arulesViz)
subBuyResultOrder
inspect(subBuyResultOrder[3])
plot(subBuyResultOrder[3], method='paracoord')

inspect(subBuyResultOrder[c(3,5,33,50,1,2)])
#화살표 굵을수록 지지도높음
plot(subBuyResultOrder[c(3,5,33,50,1,2)],
     method='paracoord')

inspect(subBuyResultOrder[1:10])
#graph:원 크기 클수록 지지도 높음
plot(subBuyResultOrder[1:10], method='graph')


str(searchL)
searchT = as(searchL, 'transactions')
inspect(searchT)
#연관성 분석
aResult=apriori(searchT, parameter = list(
  support=0.1, confidence=0.8
))

aResultOrder = sort(aResult,
                    by=c('support', 'lift', 'confidence'))
inspect(aResultOrder)

packResult = subset(aResultOrder, subset=lhs %in% c('배낭여행') | rhs %in% c('배낭여행'))
inspect(packResult)


#키워드 추출
packLhs = as(lhs(packResult), 'list')
packRhs = as(rhs(packResult), 'list')
vPackWord = unlist(packLhs, packRhs)
vPackWord
unique(vPackWord)

plot(aResult, method='graph')
