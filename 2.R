vec=1
vec
vec<-2
vec
vec=c(1,2,4)
vec
vec=c(1:10)
vec
vec=seq(1,5)
vec
vec=seq(1,10,by = 2)
vec
vec+2
vec2=c(1,5)
vec+vec2
mean(vec)
max(vec)
min(vec)
str=c("hello","world","is","good")
str
paste(str,collapse = "?")
# 시각화
install.packages("ggplot2")

library(ggplot2)
x=c("a","a","b","c")
qplot(x)
# mpg 는 ggplot2에서 지원하는 데이터
mpg
str
# hwy 연비
qplot(data = mpg,x=hwy)
qplot(data = mpg,x=cty)
qplot(data=mpg,x=drv,y=hwy,geom = "line")
qplot(data=mpg,x=drv,y=hwy,geom = "boxplot", colour=drv)
?qplot
example(qplot)

#data.frame
eng=c(90,80,60,70)
math=c(50,70,80,60)
df=data.frame(eng,math)
df

제품=c("사과","딸기","수박")
가격=c(1800,1500,3000)
판매량=c(24,38,13)
df=data.frame(제품,가격,판매량)
df
mean(가격)
mean(판매량)

install.packages("readxl")

library(readxl)
df=read_excel("excel_exam.xlsx")
df
attach(df)
mean(english)
mean(science)
detach(df)

df=read_excel("excel_exam_novar.xlsx",col_names = F)
df

df=read_excel("excel_exam_sheet.xlsx",sheet=3)
df

df=read.csv("csv_exam.csv")
df

write.csv(df,file="dfSave.csv")


df
head(df,10)
tail(df,10)
View(df)
dim(df)
str(df)
summary(df)

install.packages("dplyr")
library(dplyr)
df=data.frame(var1=c(1,2,1),var2=c(2,3,2))
df.new = df
df.new
#헤더변경
df.new=rename(df.new, v2=var2)
df.new


#df.new=data.frame(mpg)
mpg=as.data.frame(ggplot2::mpg)
mpg.new=mpg
mpg.new=rename(mpg.new,city=cty,highway=hwy)
head(mpg.new)

df=data.frame(var1=c(4,3,8),var2=c(2,6,1))
df
df$var_sum=df$var1 + df$var2
df


mpg=as.data.frame(ggplot2::mpg)
mpg.new=mpg
mpg.new$coplx=(mpg.new$cty + mpg.new$hwy)/2
head(mpg.new)
mean(mpg.new$coplx)
summary(mpg.new$coplx)
h=hist(mpg.new$coplx)
#x좌표 수치
h$breaks
#y좌표 수치
h$counts
mpgMean=mean(mpg.new$coplx)
#표준편차
mpgsd=sd(mpg.new$coplx)
#26까지의 누적치(%)
pnorm(26,mpgMean,mpgsd)

mpg.new$test=ifelse(mpg.new$coplx>=mpgMean,"pass","fail")
head(mpg.new,20)
#빈도표
table(mpg.new$test)
qplot(mpg.new$test)


exam=read.csv("csv_exam.csv")
exam
# 추출 ctrl + shift + m
exam %>% filter(class==1)
exam %>% filter(class==2)
exam %>% filter(class!=1)
exam %>% filter(class==1 & math>=50)
exam %>% filter(math>=90 | english>=90)

exam %>% filter(class==1 | class==3 | class==5)
exam %>% filter(class %in% c(1,3,5))

exam
class1=exam %>% filter(class==1)
#평균
mean(class1$math)
#분산
var(class1$math)
#표준편차
sd(class1$math)

car4=mpg.new %>% filter(displ<=4)
car5=mpg.new %>% filter(displ>=5)
mean(car4$hwy)
mean(car5$hwy)

audi=mpg.new %>% filter(manufacturer=="audi") 
toyota=mpg.new %>% filter(manufacturer=="toyota")
mean(audi$cty)
mean(toyota$cty)

cfh=mpg.new %>% filter(manufacturer %in% c('chevrolet','ford','honda'))
mean(cfh$hwy)

#select 컬럼추출
exam
exam %>% select(math)
exam %>% select(class,math,english)
exam %>% select(-math)

exam %>% filter(class==1) %>% select(math)
exam %>% select(id,math) %>% head
exam %>% select(id,math) %>% head(10)

#mpg.new %>% select(class,cty) %>% filter(class %in% c('suv','compact')) %>% if


#오름차순정렬
exam %>% arrange(math)
#내림차순
exam %>% arrange(desc(math))
exam %>% arrange(class,math)


mpg.new %>% filter(manufacturer=='audi') %>% select(model,hwy) %>% arrange(desc(hwy)) %>% head(5)

exam %>% mutate(total=math+english+science) %>% head
exam %>% mutate(total=math+english+science,mean=(math+english+science)/3) %>% head
exam %>% mutate(test=ifelse(science>=60,"pass","fail")) %>% head
exam %>% mutate(total=math+english+science) %>% arrange(total) %>% head


mpg.new=as.data.frame(mpg)
mpg.new %>% mutate(total=cty+hwy,avg=total/2) %>% arrange(desc(avg)) %>% head(3)

exam %>% summarise(mean_math=mean(math))
exam %>% group_by(class) %>% summarise(mean_math=mean(math))
exam %>% group_by(class) %>% summarise(mean_math=mean(math),sum_math=sum(math),median_math=median(math),n=n())

mpg %>% group_by(manufacturer,drv) %>% summarise(mean_cty=mean(cty)) %>% head


mpg %>%
  group_by(manufacturer) %>%
  filter(class=='suv') %>%
  mutate(total=cty+hwy) %>%
  summarise(mean_total=mean(total)) %>% 
  arrange(desc(mean_total)) %>% 
  head(5)

mpg %>% 
  group_by(class) %>%
  mutate(mean=mean(cty)) %>% 
  summarise(mean) %>% 
  arrange(desc(mean))

mpg %>% 
  group_by(manufacturer) %>% 
  mutate(hwy_mean=mean(hwy)) %>% 
  summarise(hwy_mean) %>% 
  head(3)

mpg %>% 
  group_by(manufacturer) %>% 
  filter(class=='compact') %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))


test1=data.frame(id=c(1,2,3,4,5), midterm=c(60,80,70,90,85))
test2=data.frame(id=c(1,2,3,4,5), final=c(70,83,65,95,80))
total=left_join(test1,test2,by="id")
total

exam
name=data.frame(class=c(1:5),
                teacher=c('kim', 'lee', 'park', 'choi', 'jung'))
exam.new=left_join(exam,name,by='class')
exam.new

group.a=data.frame(id=c(1:5), test=c(60,70,80,44,80))
group.b=data.frame(id=c(6:10), test=c(60,70,80,44,80))
group.all=bind_rows(group.a,group.b)
group.all


flpr=data.frame(fl=c('c','d','e','p','r'),price_fl=c(2.35,2.38,2.11,2.76,2.22),stringsAsFactors = F)
mpg=left_join(mpg,flpr,by='fl')


mpg %>% 
  select(model,fl,price_fl) %>% 
  head(5)

midwest=as.data.frame(ggplot2::midwest)
midwest=mutate(midwest,kids_percent=(poptotal-popadults)/poptotal*100)

midwest%>%
  select(county,kids_percent) %>%
  arrange(desc(kids_percent)) %>% 
  head(5)


midwest %>% 
  mutate(grade=ifelse(kids_percent>=40,'large',ifelse(kids_percent>=30,'middle','small'))) %>% 
  group_by(grade) %>% 
  summarise(count=n())
  #table(midwest$grade) 그룹&summarise 대신 이걸로 해도됨

midwest %>% 
  mutate(asian_percent=midwest$popasian/midwest$poptotal*100) %>% 
  arrange(asian_percent) %>% 
  select(state,county,asian_percent) %>% 
  head(10)


#
df=data.frame(sex=c("M","F",NA,"M","F"),score=c(5,4,3,4,NA))
df
is.na(df)
table(is.na(df))
table(is.na(df$sex))
table(is.na(df$score))
mean(df$score)

library(dplyr)

df %>% filter(is.na(score))
df %>% filter(!is.na(score))
df.nomiss=df %>% filter(!is.na(score))
mean(df.nomiss$score)

df.nomiss=df %>% filter(!is.na(score)&!is.na(sex))
df.nomiss
# 모든 결측치 제거
df.nomiss2=na.omit(df)
df.nomiss2
# score 결측치 제거후평균
mean(df$score,na.rm = T)

#
exam=read.csv("csv_exam.csv")
exam
exam[c(3,8,15),"math"]=NA
exam

exam %>% summarise(mean_math=mean(math))
exam %>% summarise(mean_math=mean(math,na.rm = T))

meanMath=mean(exam$math,na.rm = T)
meanMath
meanMath=as.integer(meanMath)
meanMath

exam$math=ifelse(is.na(exam$math),meanMath,exam$math)
table(is.na(exam$math))
mean(exam$math)

#
mgp<-as.data.frame(ggplot2::mpg)
mpg[c(65,124,131,153,212),'hwy']<-NA


table(is.na(mpg$drv))
table(is.na(mpg$hwy))

#
mpg %>% filter(!is.na(hwy)) %>% group_by(drv) %>% summarise(mean_hwy=mean(hwy))

#
outliner=data.frame(sex=c(1,2,5,2,1),score=c(5,4,3,2,6))
table(outliner$sex)
table(outliner$score)

#결측처리
outliner$sex=ifelse(outliner$sex==5,NA,outliner$sex)
outliner$score=ifelse(outliner$score>5,NA,outliner$score)

#결측치 제외하고 성별 평균점수 구하기
outliner %>% filter(!is.na(sex)&!is.na(score)) %>% group_by(sex) %>% summarise(mean_score=mean(score))

#
mpg=as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy)$stats

mpg$hwy=ifelse(mpg$hwy<12 | mpg$hwy>37, NA, mpg$hwy)
table(is.na(mpg$hwy))

mpg %>% group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy,na.rm = T))

#
mpg=as.data.frame(ggplot2::mpg)
mpg[c(10,14,58,93),'drv']<-"k"
mpg[c(29,43,129,203),'cty']<-c(3,4,39,42)

#drv 이상치 결측처리
mpg$drv=ifelse(!mpg$drv %in% c('4','f','r'),NA,mpg$drv)
table(mpg$drv)

#cty 이상치 결측처리
boxplot(mpg$cty)$stats
mpg$cty=ifelse(mpg$cty<9 | mpg$cty>26,NA,mpg$cty)
boxplot(mpg$cty)

#drv별로 cty 평균
mpg %>%
  filter(!is.na(drv)) %>%
  group_by(drv) %>% 
  summarise(mean_cty=mean(cty,na.rm = T))

#
?mpg 
?boxplot

?mtcars
str(mtcars)

# x : am, y : mpg
boxplot(formula=mpg~am, data = mtcars)
boxplot(formula=mpg~am, data = mtcars, col=c("yellow",'green'))
boxplot(formula=mpg~am, data = mtcars, col=c("yellow",'green'), ylim=c(5,40))
boxplot(formula=mpg~am, data = mtcars, col=c("yellow",'green'), ylim=c(5,40), 
        main="Box plot", 
        xlab="Transmission", 
        ylab="Miles/(US)gallon",
        names=c("automatic","manual"))

boxplot(formula=mpg~am, data=mtcars)
boxplot(formula=mpg~am, data=mtcars, axes=F)
#상(3),하(1)좌(2)우(4)
axis(1)
axis(2)
#axis(3)
#axis(4)

#mtext(c("automatic","manual"))
#상(3),하(1)좌(2)우(4)
#mtext(c("automatic","manual",side=1))
#mtext(c("automatic","manual",side=1, line=2.5))
#mtext(c("automatic","manual",side=1, line=2.5, at=c(0,1)))
mtext(c("automatic","manual"),side=1, line=2.5, at=c(1,2))


boxplot(formula=mpg~am, data = mtcars, col=c("yellow",'green'), ylim=c(5,40), 
        main="Box plot", 
        xlab="Transmission", 
        ylab="Miles/(US)gallon",
        names=c("automatic","manual"),
        notch=T,
        horizontal = T)

boxplot(mpg~cyl, data=mtcars,
        main="연료소비량",
        col=c("cornsilk",'darkgray','cadetblue'),
        ylab="Number of Cylinders",
        xlab="Miles per Gallon",
        notch=T,
        horizontal=T)

?ToothGrowth

attach(ToothGrowth)
supp=as.factor(supp)
dose=as.factor(dose)

boxplot(len~supp+dose,
        xlab = "supplement type", ylab='length',
        main='Pig Tooth Growth',
        notch=T, col=c('red','blue'))
legend(4,12,c('Orange Juice', "Ascorbic acid"), fill=c('red','blue'))

?iris
str(iris)
head(iris)

boxstats=boxplot(iris$Sepal.Width)
text(x=rep(1,NROW(boxstats$out)), y=boxstats$out, pos=c(1,2,3,4))

boxstats=boxplot(iris$Sepal.Width, horizontal = T)
# pos: 라벨 위치 하(1)하(1)상(3)하(1)
text(y=rep(1,NROW(boxstats$out)), x=boxstats$out, pos=c(1,1,3,1))
