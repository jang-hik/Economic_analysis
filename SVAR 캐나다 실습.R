setwd('C:/Users/wkdgu/시계열/시계열_R')
getwd()

library(tidyquant)
library(ggplot2)
library(ggfortify)
library(stargazer)
library(vars)
library(forecast)
library(dplyr)
library(grDevices)
library(patchwork)
library(scales)
library(svglite)

# 데이터 로딩
# canada 자료는 1980년 1분기-2000년 4분기 동안의 분기별 고용(e), 노동생산성(prod), 실질임금(rw), 실업(U)의 순서로 자료가 수록
  # 고용(e)은 고용자수(단위:천명, E)에 대해서 자연로그를 취하고 100을 곱한 값임
  # 노동생산성(prod)는 노동자 1인당 실질GDP에 대해서 자연로그를 취하고 100을 곱한 값임
  # 실질임금(rw)은 실질임금에 자연로그를 취하고 100을 곱한 값
  # U는 실업률(%)을 나타냄
data(Canada)

# 데이터 살펴보기
# prod, e, rw는 로그취하고 100곱한 값이고, 실업률자료는 %로 되어 있음
head(Canada)
summary(Canada)
str(Canada)


# 데이터 차분
dprod <- diff(Canada[, 'prod'])
de <- diff(Canada[, 'e'])
drw <- diff(Canada[, 'rw'])
dU <- diff(Canada[, 'U'])
Canada2 <- cbind(de, dprod, drw, dU)
head(Canada2)
# 위 방식대로 하거나 diff(Canada)로 해서 일괄적으로 변경시킬 수도 있음
# Canada2 <- diff(Canada)
# colnames(Canada2) <- c('de', 'dprod', 'drw', 'dU')


# Canada(수준변수) 시각화
# autoplot은 시계열 그릴때 사용한다.
aa1 <- autoplot(Canada[,'prod'], ylab='prod')
aa2 <- autoplot(Canada[,'e'], ylab='e')
aa3 <- autoplot(Canada[,'U'], ylab='U')
aa4 <- autoplot(Canada[,'rw'], ylab='rw')

# patchwork로 시각화
# patchwork에서 /은 행을 나누고 밑에 있는 작대기는 열을 나눕니다.
(aa1 / aa2) | (aa3 / aa4)

# Canada2(차분변수) 시각화
bb1 <- autoplot(Canada2[,'dprod'], ylab='dprod')
bb2 <- autoplot(Canada2[,'de'], ylab='de')
bb3 <- autoplot(Canada2[,'dU'], ylab='dU')
bb4 <- autoplot(Canada2[,'drw'], ylab='drw')
(bb1 / bb2) | (bb3 / bb4)


# 단위근 검정
urresults <- matrix(nrow=8, ncol=7)
urresults[,1] <- c('prod', 'dprod', 'e', 'de', 'U', 'dU', 'rw', 'drw')
urresults[,2] <- c('trend', 'constant', 'trend', 'constant', 'constant', '-', 'trend', 'constant')
colnames(urresults) <- c('Variable', 'Det. terms', 'Lags', 'Test value', '1%', '5%', '10%') 
# 단위근 검정시 type 설정법
# 추세가 있어보이면 trend, 
# 추세는 없지만 평균이 0이 아닌 것 같으면 constant
# 추세도 없고 평균도 0인 것 같으면 none


# 최적시차는 vars패캐지의 VARselect()함수를 이용하면 됨
# 분기자료는 최대시차를 4로 설정
# type은 c('constant', 'trend', 'both', 'none')중에서 택일
ur1lagselect <- VARselect(Canada[, 'prod'], lag.max=4, type='trend')
ur2lagselect <- VARselect(diff(Canada[, 'prod']), lag.max=4, type='const')
ur3lagselect <- VARselect(Canada[, 'e'], lag.max=4, type='trend')
ur4lagselect <- VARselect(diff(Canada[, 'e']), lag.max=4, type='const')
ur5lagselect <- VARselect(Canada[, 'U'], lag.max=4, type='const')
ur6lagselect <- VARselect(diff(Canada[, 'U']), lag.max=4, type='none')
ur7lagselect <- VARselect(Canada[, 'rw'], lag.max=4, type='trend')
ur8lagselect <- VARselect(diff(Canada[, 'rw']), lag.max=4, type='const')

# AIC결과는 아래에 저장되어 있음
ur1lag <- ur1lagselect[['selection']][['AIC(n)']]
ur2lag <- ur2lagselect[['selection']][['AIC(n)']]
ur3lag <- ur3lagselect[['selection']][['AIC(n)']]
ur4lag <- ur4lagselect[['selection']][['AIC(n)']]
ur5lag <- ur5lagselect[['selection']][['AIC(n)']]
ur6lag <- ur6lagselect[['selection']][['AIC(n)']]
ur7lag <- ur7lagselect[['selection']][['AIC(n)']]
ur8lag <- ur8lagselect[['selection']][['AIC(n)']]

# 1.단위근 검정결과 (drift는 VARselect const(상수항)와 동일)
# ur.df()는 단위근 검정(unit root) 함수
ur1 <- summary(ur.df(Canada[, 'prod'], type = 'trend', lags=ur1lag))
ur1

# 검정통계량 (만약 전체결과를 보고 싶으면 ur1을 타이핑하면 됨)
round(ur1@teststat[1], 2)

# 유의수준
ur1@cval[1,]

# 단위근 검정결과 테이블 첫번째 열은 변수명, 두번째는 추정시 포함되는 사항들(예 : 상수항, 추세)
# 3번째는 최적시차, 4번째는 단위근 통계량, 5,6,7은 각각 1%, 5%, 10% 유의수준이 들어감
urresults[1, 3:7] <- c(ur1lag, round(ur1@teststat[1],2), ur1@cval[1,])


# 2. 단위근 검정결과(drift는 VARselect const(상수항)와 동일)
# drift는 constant는 동일
ur2 <- summary(ur.df(diff(Canada[, 'prod']), type = 'drift', lags=ur2lag))
urresults[2, 3:7] <- c(ur2lag, round(ur2@teststat[1],2), ur2@cval[1,])

# 3. 단위근 검정결과
ur3 <- summary(ur.df(Canada[, 'e'], type = 'trend', lags=ur3lag))
urresults[3, 3:7] <- c(ur3lag, round(ur3@teststat[1],2), ur3@cval[1,])

# 4. 단위근 검정결과
ur4 <- summary(ur.df(diff(Canada[, 'e']), type = 'drift', lags=ur4lag))
urresults[4, 3:7] <- c(ur4lag, round(ur4@teststat[1],2), ur4@cval[1,])

# 5. 단위근 검정결과
ur5 <- summary(ur.df(Canada[, 'U'], type = 'drift', lags=ur5lag))
urresults[5, 3:7] <- c(ur5lag, round(ur5@teststat[1],2), ur5@cval[1,])

# 6. 단위근 검정결과
ur6 <- summary(ur.df(diff(Canada[, 'U']), type = 'none', lags=ur6lag))
urresults[6, 3:7] <- c(ur6lag, round(ur6@teststat[1],2), ur6@cval[1,])

# 7. 단위근 검정결과
ur7 <- summary(ur.df(Canada[, 'rw'], type = 'trend', lags=ur7lag))
urresults[7, 3:7] <- c(ur7lag, round(ur7@teststat[1],2), ur7@cval[1,])

# 8. 단위근 검정결과
ur8 <- summary(ur.df(diff(Canada[, 'rw']), type = 'drift', lags=ur8lag))
urresults[8, 3:7] <- c(ur8lag, round(ur8@teststat[1],2), ur8@cval[1,])


# stargazer를 이용해 단위근검정 결과(urresults)를 워드로 저장
stargazer(urresults, type='html', out='ur_results.doc')


# 수준변수에서 단위근이 있을 시에 공적분 검정 시행후 공적분이 있으면 var모형을 못 쓸수도 있음
# 최적시차 선택
VARselect(Canada, lag.max = 8, type = 'both')

# 공적분 검정
ect <- summary(ca.jo(Canada, type='trace', ecdet='trend', K=3, spec='transitory'))

# 공적분결과 테이블로 만들기
# 1열은 공적분의 갯수, 2열은 test statistics, 3-5 열은 90%, 95%, 99% 신뢰구간
ectresults = matrix(nrow=4, ncol=5)

ectresults[1:4,1] <- c(0,1,2,3)
ectresults[1:4,2] <- round(c(ect@teststat[4],ect@teststat[3],ect@teststat[2],ect@teststat[1]), 2)
ectresults[1:4,3] <- c(ect@cval[4,1], ect@cval[3,1], ect@cval[2,1], ect@cval[1,1])
ectresults[1:4,4] <- c(ect@cval[4,2], ect@cval[3,2], ect@cval[2,2], ect@cval[1,2])
ectresults[1:4,5] <- c(ect@cval[4,3], ect@cval[3,3], ect@cval[2,3], ect@cval[1,3])

colnames(ectresults) <- c('r', 'Test statistics', '90%', '95%', '99%')
ectresults

stargazer(ectresults, type='html', out='coint_results.doc')

# 여기서는 공적분이 없다고 가정하고 var모형 진행





# VAR 추정 : 차분된 변수들을 이용
# 먼저 변수의 순서를 재지정하고 변수 이름을 바꿀 것
Canada3 <- cbind(dprod, de, dU, drw)
colnames(Canada3) <- c('prod', 'e', 'U', 'rw') # 차분변수지만 이름은 앞에 d는 빼주는 걸로 한다.

# 최적시차 결정
VARselect(Canada3, lag.max = 8, type = 'const') # AIC기준 최적시차는 2

# VAR 모형추정
varresult <- VAR(Canada3, p=2, type='const') # 차분변수는 추세가 없어서 var모형을 돌릴 때 const만 넣음
summary(varresult)

# 위와 같이 VAR모형을 추정한 다음 잔차항(residual)에 대한 몇 가지 검정을 수행(SVAR에는 꼭 필요하지 않음)

# 오차항들이 안정적인지 테스트
# 안정적이지 않다고 나오면 시차변수를 충분히 포함하지 않은 것임
plot(stability(varresult), nc=2)

# 잔차항의 정규분포 테스트
# Jarque Bera test
# 귀무가설 : 정규분포를 따른다. 
# 대립가설 : 정규분포를 따르지 않는다.
normality.test(varresult)

# 잔차항의 자기상관 테스트
serial.test(varresult, lags.pt=16, type='PT.asymptotic') # 검정 결과 자기상관이 없다.
# 만약에 오차항관에 자기상관이 있으면 변수를 추가하거나 최적시차를 늘리는 방법이 있음





# SVAR 모형추정예제1 : A 모형(단기제약조건 부과)
# 변수의 순서가 중요한데, prod -> e -> U -> rw 순서임
# 단기제약조건을 통해 n(n-1)/2 = 4*3/2 = 6개의 0의 단기제약조건 부과
# 생산성은 생산성 충격에 의해서만 영향을 받음 ((1,2), (1,3), (1,4) =0) 3개
# 고용은 생산성, 고용충격에 의해서만 영향을 받음 ((2,3), (2,4)=0) 2개
# 실업은 생산성, 고용, 실업에 의해서만 영향을 받음 ((3,4)=0) 1개

# amat
#      [,1]  [,2]  [,3]  [,4]
#[1,]   na    0     0     0
#[2,]   na    na    0     0
#[3,]   na    na    na    0
#[4,]   na    na    na    na

amat <- matrix(NA, 4, 4)
amat[upper.tri(amat)] <- 0 
amat

svarresult <- SVAR(varresult, estmethod='scoring', Amat = amat)
summary(svarresult)

# 실업율 충격이 왔을 때의 변수들의 반응을 살펴본다.
# n.head=13 : 13기까지 살펴본다. 분기자료여서 3년치 자료를 확인한다.
# 대부분 충격에 대한 변수들의 반응은 차분변수보다는 수준변수를 이용한다.
# "cumulative=TRUE"로 충격반응함수를 누적시켜서 차분변수를 이용해서 수준변수의 반응을 구할 수 있다.
# ci=0.68 : 신뢰구간은 1시그마이다. 
irfresult <- irf(svarresult, impulse=c('U'), response=c('prod', 'e', 'U', 'rw'), n.head=13, cumulative=TRUE, 
                 boot=TRUE, runs=100, ci=0.68)
plot(irfresult)

# 모든 변수에서 충격이 왔을 때 실업률이 어떻게 반응하는지
irfresult2 <- irf(svarresult, impulse=c('prod', 'e', 'U', 'rw'), response=c('U'), n.head=13, cumulative=TRUE, 
                  boot=TRUE, runs=100, ci=0.68)
plot(irfresult2)


# 예측오차분산분해 : 다양한 충격이 변수에 영향을 미칠 때 그 중에 어떤 변수가 중요하는지 알아봄
fevdresult <- fevd(varresult, n.ahead=13)
par('mar')
par(mar=c(1,1,1,1))
plot(fevdresult)
fevdresult


# ggplot을 이용해서 fevd그림 이쁘게 그리기
library(purrr); library(tidyverse)
dat <- map_df(fevdresult, ~as.data.frame(.x), .id='id') %>%
  mutate(horizon = rep(0:12, 4)) %>%
  pivot_longer(names_to = 'var', cols = c(prod, e, U, rw)) %>%
  mutate(FEVD=value)
head(dat)

ggplot(data = dat, mapping = aes(x = horizon, y = FEVD, fill = var)) +
  facet_wrap(~id) +
  geom_bar(stat = 'identity') +
  scale_x_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  scale_fill_brewer(palette='Setl', direction = -1)








