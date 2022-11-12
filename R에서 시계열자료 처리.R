library(tidyverse); library(ggfortify); library(xts); library(zoo); library(patchwork); library(quantmod)
library(lubridate); library(fastDummies); library(stargazer)

# 시계열자료는 기본적으로 벡터 or 행렬로 처리가 되며 시계열자료를 합칠 때는 모두 동일하게 숫자이어야 됨

# 시계열 자료를 선언하는 기본 명령어는 ts()임
# frequency=1은 년별, frequency=4는 분기별 자료, frequency=12는 월별, frequency=365는 일별, 
q.ts <- ts(1:20, start=c(2000,1), frequency=4)
q.ts

# 위 ts 시계열자료에서 날짜를 뽑고 싶으면 time(),index() 함수를 사용하면 됨
date_ts <- time(q.ts)
date_ts <- index(q.ts)
date_ts

# 월별자료로 선언
m.ts <- ts(1:20, start=c(2010,5), frequency = 12)
m.ts

# 서로다른 3개의 시계열 자료 만들기, 벡터로 인식
aa <- ts(1:20, start=c(2010,1), frequency = 12)
bb <- ts(21:40, start=c(2009,6), frequency = 12)
cc <- ts(41:60, start=c(2010,5), frequency = 12)
aa
bb
cc

# 3개의 시계열 자료를 합치면 mts(multiple time seires)로 인식됨, 행렬로 인식
dd <- cbind(aa,bb,cc)
dd

# 행렬의 열 이름을 바꾸고 싶으면 colnames()
# 데이터프레임의 변수 이름을 바꾸고 싶으면 names()
# dd는 시계열 자료라 행렬의 형태로 되어있기 때문에 colnames()를 사용
colnames(dd) <- c('GDP', 'CPI', 'EMPLOYMENT')
dd

# 변수를 불러오기 위해서는 열로 지정
dd[,'GDP']

# 날짜는 index(), time()으로 불러옴 
index(dd)
time(dd)

# ts() 함수는 시계열자료가 일정한 간격으로 구성되어야 함
# 일별 주가자료의 경우 토,일,공휴일에는 자료가 없음
# 이 경우 자료들간의 갭이 일정하지가 않음. 월-금 사이에는 간격이 1일이지만 금-월의 갭은 3일임
# ts()로 일별자료를 분석할 경우 이러한 갭을 무시하고 모든 자료들의 갭을 마치 1일인 것처럼 처리 가능

# 자료중 일부 구간만을 뽑기 위해서는 window() 함수 이용
ee <- window(dd, start=c(2010,2), end=c(2011,3))
ee

# 날짜의 처리(lubridate내의 함수들)
mdy('01162011') # '01162011'을 월일년으로 인식해서 년-월-일로 표시
ymd('20070210') # '20070210'을 년월일로 인식해서 년-
yq('2013-2') # '2013-2'을 년분기로 처리, 2013년 2분기
myd('Aug 1999 13') # 'Aug 1999 13'을 월일년으로 인식해서 년-월-일로 처리
dmy('13 Aug 1999') # '13 Aug 1999'을 일월년으로 인식해 년-월-일로 처리
dmy('3rd July 2008') # '3rd July 2008'을 일월년으로 인식해서 년-월-일로 처리

# 아래와 같이 월이 문자로 중간에 있을 경우 작동 안함
# 이 경우 날짜에 있는 13을 첫째(1)를 월로, 둘째(3)를 일로 인식
ymd('1999 Aug 13')

# 아래도 변환이 안됨
# 날짜에 있는 1을 월로, 3을 일로 인식
mdy('December 13, 2002')

# 위와 같은 경우에는 parse_date_tiem을 이용하여 형식을 보다 정확하게 지정을 해줘야됨
# %d : day as two digits
# %a : abbreviated weekday (eg. Wed)
# %A : unabbreviated weekday (eg. Monday)
# %m : month as two digits
# %b : abbreviated month
# %B : unabbreviated month
# %y : two digit year
# %Y : four digit year
parse_date_time('1999 Aug 13', '%Y%b%d') # '1999 Aug 13'을 '%Y%b%d'로 인식시킨 다음 년월일로 바꿔줌
parse_date_time('December 13, 2002', '%B%d%Y') 

# 서로다른 날짜가 있따면, parse_date_time()을 이용해 모두 동일한 포맷으로 만들 수 있음
# 서로 다른 날짜 형식을 '%m%d%y', '%y-%m-%d', '%b %d, %y'로 인식시킨 다음 모두 년-월-일로 바꿔줌
date <- c('01/03/2008', '2008-03-12', 'jan 12, 2013')
parse_date_time(date, c('%m%d%y', '%y-%m-%d', '%b %d, %y'))



# quantmod 패키지를 이용해 애플주가와 금리자료 불러오기
# auto.assign = T으로 해주면 변수명을 임의로 자동설정
apple <- getSymbols('AAPL', auto.assign = F)

# 4번째 종가를 사용
apple <- apple[,4]

# 이름 변경
names(apple) <- 'apple'

# fred에서 일 연방금리(Federal Funds Effective Rate, DFF) 불러오기
ffr <- getSymbols('DFF', src='FRED', auto.assign = F)
colnames(ffr) <- 'ffr'

# 애플과 금리 자료 합치기
data <- cbind(apple, ffr)
data1 <- merge(apple, ffr, join='outer')

# merge로도 합칠 수 있음
# join으로 inner, left, right, outer 사용 가능
# merge(apple, ffr, join='inner')

# 두 자료가 공통으로 들어 있는 최초시점이 어디인지를 살펴보고 거기서부터 자료를 이용
# "/"로 기간을 나누느데 "/a"는 처음부터 a까지, "a/b"는 a와 b사이, "a/"는 a부터 끝까지
first(data1[complete.cases(data1),]) # data1에서 두 컬럼에 모두 데이터가 있는 행을 표시
data1 <- data1['2007-01-03/']
data11 <- data1

# na.omit은 na를 포함하는 모든 행을 삭제
data12 <- na.omit(data)


# 중간중간에 있는 na값들은 임의로 채워넣을 수 있음
# na.locf()는 last observation carried forward의 약자로 이전값을 그대로 채워넣음
# na.approx는 근처 데이터와 유사한 값으로 채워 넣음
data22 <- data11
data22 <- na.locf(data22)

data33 <- data11
data33 <- na.approx(data33)

data44 <- data11
data44 <- na.omit(data44)

# regression으로 결과가 어떻게 다른지 비교
reg1 <- lm(apple~ffr, data=data22)
reg2 <- lm(apple~ffr, data=data33)
reg3 <- lm(apple~ffr, data=data44)
reg1
reg2
reg3

stargazer(reg1, reg2, reg3, type='html', out='결측치처리별결과.doc')

# 날짜자료(index) 중에서 날짜, 연, 월, 일 뽑아내기
# 날짜는 숫자가 아니라 date형식임

# str()로 data44의 구조를 살펴봄
str(data44)

# An ‘xts’ object on 2007-01-03/2022-09-02 containing:
# Data: num [1:3946, 1:2]
# Indexed by objects of class: [Date] TZ: UTC

# xts형태의 시계열 자료임
# ts, xts는 numeric형태만 인식
# 인덱스는 date형식으로 되어있음
# data44에서 인덱스만 뽑아서 xts에 다시 집어넣을라면 인덱스가 date형식이기 때문에 오류가 남
# 그래서 숫자형태로 변환
# 나중에 as.Date()로 숫자를 날짜로 변환가능
date <- as.numeric(index(data44))


# 년,월,일로 뽑아내고 싶을 때
year <- year(index(data44))
month <- month(index(data44))
day <- day(index(data44))

final <- cbind(data44, date, year, month, day)

# xts를 ts로 변환(정확히는 mts)
# ts로 변환시에 gap이 일정하게 됨
final_ts <- ts(final)
str(final_ts)

# xts인 final을 데이터프레임으로 변환
final_df <- as.data.frame(final)
str(final_df)

# 데이터프레임인 final_df에서 숫자로 된 date를 날짜로 변환
final_df$date <- as.Date(final_df$date)

# 데이터프레임에서 날짜를 다양하게 변환
final_df2 <- final_df %>% mutate(date2 = as.yearqtr(date),
                                 date3 = as.yearmon(date),
                                 date4=gsub(' ', '', date2),
                                 date5=gsub(' ', '-', date3))

# 데이터프레임을 xts로 변환
# 날짜를 제외한 변수를 불러들이고 날짜를 이용해 자료를 정렬
# 날짜가 문자로 되어있으면 as.Date() 함수를 이용해서 날짜로 먼저 변환
str(final_df)
# final_df에서 date열을 빼고 불러오고, date열을 기준으로 정렬
final_xts <- xts(select(final_df, -date), order.by=final_df$date)
final_xts

g1 <- autoplot(final_xts[,1])+ggtitle('APPLE 주가 추이')
g2 <- autoplot(final_xts[,'ffr'])+ggtitle('금리추이')

g1 | g2


# 2020.1.1 ~ 현재까지 주가 살펴보기
temp <- final_xts['2020/2022']
autoplot(temp[,'apple']) + ggtitle('2020년 이후 apple 주가')

# 아래도 위와 동일
temp2 <- window(final_xts, start='2020-01-01')
autoplot(temp2[,'apple'])

# dummy_cols()를 이용해 월별 더미변수 만들기 (fastDummies 패키지 이용)
final_xts <- dummy_cols(final_xts, select_columns = 'month', remove_first_dummy = TRUE)



# 애플주가에 로그를 취하기
# 아래의 방식은 xts,ts에 모두 동일하게 적용

final_xts[, 'lapple'] <- log(final_xts[, 'apple'])

# lag를 이용해 1기 전의 값 만들기
final_xts[, 'lapple1'] <- lag(final_xts[, 'lapple'])

# 원자료와 이전자료를 뺀 차분컬럼을 생성
final_xts[, 'dlapple'] <- final_xts[, 'lapple'] - final_xts[, 'lapple1']

# diff()로 차분값을 구할 수 있는 데 맨 앞에 NA를 넣어줘야 오류가 안뜬다.
final_xts[, 'dlapple2'] <- c(NA, diff(final_xts[, 'lapple']))

# 한기 미래의 값을 넣고 싶으면 lead() 함수 이용
# lag(x, k=-1)도 동일함
final_xts[, 'flapple'] <- lead(final_xts[, 'lapple'])


# 금리가 주가에 미치는 영향 분석
reg4 <- lm(data=final_xts, lapple~ lag(apple,1) + lag(apple,2) + ffr + lag(ffr,1))


# 아래도 위와 동일
reg5 <- lm(data=final_xts, lapple ~ lag(apple, k=1:2) + lag(ffr, k=0:1))

stargazer(reg4, reg5, type='html', out='결과비교.doc')


# zoo패키지의 rollapply를 이용해 CAPM의 market beta 구하기
# 개별수익률 = alpha + beta*시장수익률 + e
# beta는 시장수익률 대비 개별 수익률이 얼마나 반응하는지 나타내는 민감도
# 추가적으로 beta를 50개씩 샘플을 이동시켜 추정

Price <- getSymbols('^GSPC', auto.assign = FALSE)

# quantmod 패키지의 dailyReturn() 함수를 이용해 일별 수익률을 구함
R <- dailyReturn(Price[,6])
 
# 25일 이동평균 수익률
R_ma <- rollapply(R, 25, mean)
autoplot(R_ma)

# 23일 이동평균 표준편차(위험)
R_mv <- rollapply(R, 25, sd)
autoplot(R_mv)


# 애플주가 불러들이기
Price2 <- getSymbols('AAPL', auto.assign = FALSE)
R2 <- dailyReturn(Price2[,6])
R12 <- merge(R, R2)

Beta <- rollapply(R12, 50, by.column=F, function(x) lm(x[,2] ~ x[,1])[[1]][[2]])
plot(Beta)











