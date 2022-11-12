getwd()
setwd('C:/Users/wkdgu/시계열/시계열_R')

library(tidyverse); library(ggfortify); library(stargazer); library(vars); library(patchwork)

# 데이터 로딩
# canada 자료는 1980년 1분기-2000년 4분기 동안의 분기별 고용(e), 노동생산성(prod), 실질임금(rw), 실업(U)의 순서로 자료가 수록
# 고용(e)은 고용자수(단위:천명, E)에 대해서 자연로그를 취하고 100을 곱한 값임
# 노동생산성(prod)는 노동자 1인당 실질GDP에 대해서 자연로그를 취하고 100을 곱한 값임
# 실질임금(rw)은 실질임금에 자연로그를 취하고 100을 곱한 값
# U는 실업률(%)을 나타냄
data(Canada)



# Canada를 차분
Canada2 <- diff(Canada)
colnames(Canada2) <- c('de', 'dprod', 'drw', 'dU')

# 아래와 같이도 차분 가능
dprod <- diff(Canada[, 2])
de <- diff(Canada[, 1])
drw <- diff(Canada[, 3])
dU <- diff(Canada[, 4])
Canada3 <- cbind(de, dprod, drw, dU)

# 자료 그림을 그려보고 단위근 존재 여부 판단하기
aa1 <- autoplot(Canada[,'prod'], ylab='prod')
aa2 <- autoplot(Canada[,'e'], ylab='e')
aa3 <- autoplot(Canada[,'U'], ylab='U')
aa4 <- autoplot(Canada[,'rw'], ylab='rw')

(aa1 / aa2) | (aa3 / aa4)

# Canada2(차분변수) 시각화
bb1 <- autoplot(Canada2[,'dprod'], ylab='dprod')
bb2 <- autoplot(Canada2[,'de'], ylab='de')
bb3 <- autoplot(Canada2[,'dU'], ylab='dU')
bb4 <- autoplot(Canada2[,'drw'], ylab='drw')
(bb1 / bb2) | (bb3 / bb4)


#-----------------------------------------------------------------------------------------------
# 단위근 검정 

# 첫번째 : 결과 저장용 matrix 만들기
# 먼저 단위근 검정 결과 저장용 matrix 만들기 : 8개의 변수를 단위근 검정
# 결과물은 변수, deterministic terms, 시차, 검정통계량, 1%, 5%, 10% 유의수준을 저장
urresults <- matrix(nrow=8, ncol=7)
urresults[,1] <- c('prod', 'dprod', 'e', 'de', 'U', 'dU', 'rw', 'drw')
urresults[,2] <- c('trend', 'constant', 'trend', 'constant', 'constant', '-', 'trend', 'constant')
colnames(urresults) <- c('Variable', 'Det. terms', 'Lags', 'Test value', '1%', '5%', '10%') 

# 단위근 검정시 type 설정법
# 추세가 있어보이면 trend, 
# 추세는 없지만 평균이 0이 아닌 것 같으면 constant
# 추세도 없고 평균도 0인 것 같으면 none



# 두번째 : 최적시차 설정
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



# 세번째 : 최적시차 뽑아내기
# 시차는 AIC로 결정
# AIC결과는 아래에 저장되어 있음
ur1lag <- ur1lagselect[['selection']][['AIC(n)']]
ur2lag <- ur2lagselect[['selection']][['AIC(n)']]
ur3lag <- ur3lagselect[['selection']][['AIC(n)']]
ur4lag <- ur4lagselect[['selection']][['AIC(n)']]
ur5lag <- ur5lagselect[['selection']][['AIC(n)']]
ur6lag <- ur6lagselect[['selection']][['AIC(n)']]
ur7lag <- ur7lagselect[['selection']][['AIC(n)']]
ur8lag <- ur8lagselect[['selection']][['AIC(n)']]



# 네번째 : 단위근 검정
# 위에서 단위근 검정에 필요한 시차를 정한 것
# 단위근 검정결과 테이블 첫번째 열은 변수명, 두번째는 추정시 포함되는 사항들(예 : 상수항, 추세)
# 3번째는 최적시차, 4번째는 단위근 통계량, 5,6,7은 각각 1%, 5%, 10% 유의수준이 들어감

# 1.단위근 검정결과 
ur1 <- summary(ur.df(Canada[, 'prod'], type = 'trend', lags=ur1lag))
urresults[1, 3:7] <- c(ur1lag, round(ur1@teststat[1],2), ur1@cval[1,])

# 2. 단위근 검정결과(drift는 VARselect const(상수항)와 동일)
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



# 워드파일로 단위근 검정결과를 변환
stargazer(urresults, type='html', out='ur_results.doc')



# --------------------------------------------------------------------------------
# 공적분 검정

# 단위근 검정은 단일변수에 대해서 검정을 수행
# 공적분 검정은 단위근을 갖는 여러 변수들간의 안정적인 관계여부를 테스트하는 것

# 공적분 검정을 위해서는 먼저 수준변수를 이용해서 VAR모형을 추정하고 최적시차 선택
# type은 c('const', 'trend', 'both', 'none') 중에서 택일
VARselect(Canada, lag.max=8, type = 'both')

# $selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 3      2      1      3 
# AIC 기준으로 시차는 3으로 정한다.

# 공적분 검정 수행(시차는 3)
ect <- summary(ca.jo(Canada, type='trace', ecdet='trend', K=3, spec='transitory'))










