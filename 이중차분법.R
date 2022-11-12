
# 이중차분법

# 정책뿐만 아니라 외생적인 환경이 변화했을 때 환경변화에 따른 처치그룹과 통제그룹간의 차이를 비교할 때도 DID 사용
# 쓰레기 소작장이 주택가격에 미치는 영향
# 1978년에 소문이 돌고 1981년에 소각장 공사를 시작
# 1978년(정책 시행 전)과 1981년(정책 시행 후)의 데이터만 사용
# 정책 더미변수 : 1978년, 1981년
# 환경 더미변수 : 소각장 근처 그룹, 소각장에서 먼 그룹

# 2개의 더미 변수를 활용해 이중차분법으로 소각장(정책변수)이 처치그룹(소각장에서 가까운)과 통제그룹(소각장에서 먼)에
# 미치는 가격 변화를 분석

library(wooldridge); library(stargazer); library(haven)
data(kielmc, package='wooldridge')

# 종속변수 : 주택가격(rprice)
# 통제(설명)변수 : 주택연수(age), 주택연수제곱(age2), 주경계까지의 거리(intst), 땅크기(land), 주택면적(area), 방수(rooms), 화장실수(baths)
# 소각장을 짓기 시작한 1981년에 대한 더미(y81), 78년이면 y81=0, 81년이면 y81=1
# 소각장 근처 그룹이면 nearinc = 1, 소각장에서 먼 그룹이면 nearinc=0
# nearinc*y81은 두 더미변수간의 interaction effect를 측정


# 단순히 DID추정 계수만을 포함시켜 측정
# nearinc*y81 이렇게 넣어주면 nearinc, y81, nearinc*y81 세개의 더미변수를 자동으로 추정해줌
# "log(rprice) ~ nearinc*y81" 는 "log(rprice) ~ nearinc + y81 + nearinc*y81" 과 동일
DID <- lm(log(rprice) ~ nearinc*y81, data=kielmc)


# 주택가격에 영향을 줄 수 있는 모든 변수를 포함시켜 제대로 모형 측정
# 주택연수의 제곱을 사용하고 싶으면 I(age^2)로 사용해야됨 ^는 다른 의미를 지니고 있어 I()로 고립시켜줘야 됨
DIDcontr <- lm(log(rprice) ~ nearinc*y81 + age + I(age^2) + log(intst) + log(land) + log(area) + 
            rooms + baths, data=kielmc)

stargazer(DID, DIDcontr, type='html', out='DID비교.doc')


# 기본 모형
# log(rprice) ~ B0 + B1*nearinc + B2*y81 + B3*nearinc*y81 + 통제변수들.... + 오차항

# 처치그룹(소각장에 가까이 있는 그룹, nearinc=1, nr)의 78년과 81년의 가격차이를 구함
# log(rprice),81,nr ~ B0 + B1 + B2 + B3 + 통제변수들.... + 오차항
# log(rprice),78,nr ~ B0 + B1 +           통제변수들.... + 오차항
# 가격차이 : Δlog(price)nr = B2 + B3


# 통제그룹(소각장에 먼 그룹, nearinc=1, fr)의 78년과 81년의 가격차이를 구함
# log(rprice),81,fr ~ B0 + B2 + 통제변수들.... + 오차항
# log(rprice),78,fr ~ B0 +    + 통제변수들.... + 오차항
# 가격차이 : Δlog(price)fr = B2


# 소각장근처 그룹(처치그룹)과 멀리 있는 그룹(통제그룹)간의 소각장 건설이후(정책 전후)의 가격 차이
# Δlog(price)nr - Δlog(price)fr = B3
# 소각장 건설에 따른 효과(정책효과)는 그룹의 구분자(nearinc)와 정책시점변수(y81)간의 교호작용 항인
# nearinc*y81의 계수를 보면 알 수 있음!

# nearinc*y81의 계수는 -0.132이고 5%수준에서 유의함
# 해석하면 소각장 가까이에 있는 주택들의 가격이 1981년 소각장 건설로 소각장에서 멀리 떨어져 있는
# 주택들의 가격보다 13.2% 낮아졌음
# 종속변수에 로그를 씌우고 독립변수에는 로그가 없어서 계수에 *100을 하고 %로 해석






# 삼중차분법
# DID는 2개의 더미변수(정책, 처지/통제그룹)가 필요하지만 DDD는 3개의 더미변수가 필요

# DID에서는 정책 전후를 나타내는 변수는 P, 처치그룹/통제그룹 변수는 G, 기타 종속변수에 영향을 줄 수 있는 변수는 Z라고 한다.
# 기본모형 : y ~ B0 + B1*P + B2*G + B3*P*G + Z들 + 오차항
# 정책의 효과는 B3를 통해 알 수 있다.

# DDD에서는 정책 전후를 나타내는 변수는 P, 처치그룹/통제그룹 변수는 G, 기타 종속변수에 영향을 줄 수 있는 변수는 Z,
# 특정 지역그룹에서의 차이여부를 살펴보기 위한 더미를 R이라고 한다.
# 기본모형 : y ~ B0 + B1*P + B2*G + B3*R + B4*P*G + B5*P*R + B6*G*R + B7*P*G*R + Z들 + 오차항
# R의 구분(예 : 대도시 vs 농어촌)에 따른 정책의 효과 차이는 P*G*R의 계수인 B7로 확인할 수 있음 

haven::read_dta('ddd.dta')
install.packages("haven")









