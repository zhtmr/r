#vector, 배열 개념
#c(Combine) 함수
vec<-c(1,2,3,4)
vec
#타입 확인
class(vec)
#원소 개수 구하기
length(vec)

vec=c(1,"hi",2)
vec
class(vec)

#수치 연산
n_vec=c(1,2,3,4,5,6,7,8,9)
n_vec

#최소값
min(n_vec)
#최대값
max(n_vec)
#평균
mean(n_vec)
#중위수
median(n_vec)
#합계
sum(n_vec)


#Inf와 NaN이 포함된 벡터
n_vec=c(1/0, 2/2, -2/2, -1/0, 0/0)
n_vec

#논리
#TRUE, FALSE
logical1=c(TRUE,FALSE,TRUE,FALSE)
logical1
logical2=c(T,F,T,F)
logical2
logical3=c(true,false)
logical4=c("TRUE","FALSE","TRUE","FALSE")
logical4

#형 변환
exLog=as.logical(c(0,-1,1,100,-7))
print(exLog)
print(as.numeric(exLog))

#문자열 처리
str=c("f123","f124","f125","f126")
#문자의 개수 출력
nchar(str)
#문자열 자르기 substr(data,start,end)
substr("123456789",2,4)
substr(str,3,4)
#문자열 나누기
strsplit('2020/10/27',split = "/")
#문자열 합치기
paste("50","40","30",sep = "*")
paste("50=","40+","30",sep = "")
#sep가 존재하지 않으면 빈공간을 구분자로 사용
paste("50=","40+","30")

str="abCd"
#대문자 변환
toupper(str)
#소문자 변환
tolower(str)

#범위지정
n_vec=3:7
n_vec
n_vec=51:100
n_vec[30:40]
n_vec[0]
n_vec[1]
idx=c(1,3,5,6)
n_vec[idx]
idx=c(6,5,3,1)
n_vec[idx]
idx=c(F,F,T,T,F)
n_vec[idx]
idx=c(T,F)
n_vec[idx]

n_vec[n_vec<=60]

#1부터 100까지 변수 vce에 저장 후 출력
vec=1:100
vec
#vec을 이용하여 10부터 30사이의 수만 출력
vec[10:30]
#vec을 이용하여 50~60 사이의 최소값과 최대값 출력
min(vec[50:60])
max(vec[50:60])
#vec을 이용하여 15,17,29,35,62,85의 중간값 출력
median(vec[c(15,17,29,35,62,85)])
#median(c(15,17,29,35,62,85)) 다름
#vec을 이용하여 50이상인 수의 평균값 출력
mean(vec[vec>=50])
#vec을 이용하여 짝수를 출력
idx=c(F,T)
vec[idx]
#vec을 이용하여 30보다 작은 홀수 합계
idx=c(T,F)
vec=vec[idx]
vec
sum(vec[vec<=30])

#벡터 수정
vec=1:5
vec
vec[3]=10
vec
vec[c(2,4)]=9
vec
vec[vec>5]=3
vec
vec[2:5]=0
vec
vec[1:length(vec)]=1
vec
#추가
vec=c(0, vec)
vec
vec=c(vec,7:10)
vec

vec1=1:3
vec2=4:6
vec3=7:9
newvec=c(vec1,vec2,vec3)
newvec

veca=c("a","b","c","f","g")
vecb=c("d","e")
#veca에서 3위치 다음에 vecb추가
append(veca,vecb,3)

#삭제
vec=1:10
#지정된 위치 삭제
vec[-c(1,3,5)]
#마지막 데이터 삭제
vec[-length(vec)]

#factor
#범주형 데이터
#성별(남녀)
#혈액형
#설문지(매우만족~매우불만족)
#factor(x,levels,ordered)
# - x : factor로 변환할 데이터
# - levels : 입력한 x의 범주를 정의한 벡터
#            순서 지정시 사용
# - ordered : 순위 지정 시 사용

#벡터에 데이터 저장
v_char=c("사과","복숭아","사과","오렌지","사과","오렌지","복숭아")
#벡터를 factor로 변환. enum같음
v_factor= factor(v_char)
v_factor
mode(v_factor)
#문자화
v_factor_char=as.character(v_factor)
v_factor_char
#숫자화
v_factor_num=as.numeric(v_factor)
v_factor_num

#levels
v_factor=factor(v_char,levels = c("사과","오렌지"))
v_factor

v_factor=factor(v_char,levels = c("사과","복숭아","오렌지"))
v_factor

v_factor=factor(v_char,levels = c("복숭아","오렌지","사과"))
v_factor

#ordered
ex_label=c("매우불만족",'불만족','보통','만족','매우만족')
orderedFactor=factor(ex_label,ordered = T)
orderedFactor
min(orderedFactor)
max(orderedFactor)
sum(orderedFactor)
mean(orderedFactor)

#다음 내용을 factor로 가공하여 surveyFactor에 저장하시오
#＂매우불만족＂, ＂불만족＂, ＂보통＂, ＂만족＂,＂매우만족“
surveyFactor=factor(c('매우불만족','불만족','보통','만족','매우만족'))
surveyFactor
#surveyFactor의 객체 정보를 확인하고 수치값만 출력하시오
str(surveyFactor)
surveyFactor_num=as.numeric(surveyFactor)
surveyFactor_num
#surveyFactor의 최대값과 최소값을 구하시오
ordered=factor(surveyFactor,ordered = T)
max(ordered)
min(ordered)
#surveyFactor의 합계를 구하시오
sum(surveyFactor)
sum(surveyFactor_num)

# 1.
lvl=c('매우불만족','불만족','보통','만족','매우만족')
v_char=c('매우만족','보통','보통','만족','보통','만족','매우만족','보통','만족','보통')
v_factor=factor(v_char,levels = lvl)

ExceptNomal=v_factor[v_factor!='보통']
v_factor_num=as.numeric(ExceptNomal)
mean(v_factor_num)
mean(as.numeric(v_factor))
str(v_factor)


# 2.
lvl=c('하하','하중','하상',
      '중하','중중','중상',
      '상하','상중','상상')
v_char=c('중상','하상','상상','상하','상상','하중','중중','하중','중상','중하','상중','상하')
#levels : 가중치 적용, ordered : min,max 함수 사용
v_factor=factor(v_char,levels = lvl,ordered=T)
str(v_factor)
v_factor[v_factor==min(v_factor)]
v_factor[v_factor==max(v_factor)]
#최소값 제외한 v_factor
v_min=v_factor[v_factor!=min(v_factor)]
#최소값,최대값 제외
v_max=v_min[v_min!=max(v_min)]

mean(as.numeric(v_max))


#dataframe
#다양한 유형을 하나의 공간에 저장할 경우 사용
#data.frame(vector1,vector2, ..., stringsAsFactors = )
#vector : 열이 될 데이터
#stringAsFactors : 문자의 경우 factor로 변환할지 유무
id = c('jin','infiscap','din')
age = c(20,30,40)
isMarried=c(F,T,T)
df = data.frame(id,age,isMarried)
df[2,2]
df[c(2,3),1]
df[c(2,3),c(1,3)]
#행 전체
df[,c(1,3)]
#열 전체
df[c(2,3),]
df[,c("id","age")]
str(df)

# $접근
df$id
str(df$age)
str(df$id)

sum(df$age)

df$age[2:3]
#iris(샘플데이터) 구조 확인
str(iris)
# 총 행수 확인
nrow(iris)
# 총 열수 확인
ncol(iris)
#head(데이터 프레임, 보고싶은 행수)
head(iris)
head(iris,30)
#tail
tail(iris,3)
summary(iris)
summary(df)
min(iris$Sepal.Length)
max(iris$Sepal.Length)
median(iris$Sepal.Length)
mean(iris$Sepal.Length)
quantile(iris$Sepal.Length)

View(iris)

#데이터 프레임 조건식
#subset(데이터프레임, 조건식, 열)
subset(iris,Sepal.Length>7)
subset(iris,Sepal.Length>7&Petal.Length<=6.5)
subset(iris,Sepal.Length>7|Petal.Length>6.5, c(1,5))
subset(Sepal.Length>7|Petal.Length>6.5, c(1,5),iris)
subset(subset=(Sepal.Length>7|Petal.Length>6.5),select= c(1,5),x=iris)

str(longley)
#조건검색
longley[longley$GNP>200 & longley$Population>=109 & longley$Year>1960 & longley$Employed>50,]

#객체 탐색 경로에 추가하여 세부 내용 활용
attach(longley)
longley[GNP>200 & Population>=109 & Year>1960 & Employed>50,]

detach(longley)
longley[GNP>200 & Population>=109 & Year>1960 & Employed>50,]

#sqldf 패키지 설치
install.packages("sqldf")
library(sqldf)
sqldf("select GNP, Year, Employed from longley where GNP > 400")
