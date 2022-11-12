
setwd('C:/Users/wkdgu/Desktop/학술제')
getwd()

library(plm); library(dplyr); library(stargazer)

# plm 패키지 내에 어떤 data가 있는지 검색
data(package='plm')
data('Gasoline')
data('LaborSupply')
aa <- Gasoline

# 패널자료임을 선언하는 명령어는 pdata.frame()
# N은 country, T는 year로 나타냄
aa <- pdata.frame(aa, index=c('country', 'year'))

# %>%는 파이프 연산자, 앞의 데이터를 뒤로 전달
# mutate는 데이터프레임에서 새로운 칼럼을 생성하는 함수
aa <- aa %>% mutate(cindex=country)

# 변수설명
# country : 18개 국가 (factor 변수)
# year : 년도
# lgaspcar : 자동차당 휘발유소비량 (log)
# lincomep : 1인당 실질소득 (log)
# lrpmg : 실질 자동차가격 (log)
# lcarpcap : 1인당 실질 자동차보유대수 (log)

#       종속변수 ~ 설명변수1 + 설명변수2 + 설명변수3
form <- lgaspcar ~ lincomep + lrpmg + lcarpcap

# plm()은 패널데이터 함수
# plm은 세가지 model이 있음, 고정효과, 임의효과, OLS
fe <- plm(form, data=aa, model='within') # 고정효과 모형
re <- plm(form, data=aa, model = 'random') # 임의효과 모형

# OLS를 돌릴 때 국가 더미를 넣으면 계수값들은 FE결과와 동일
# R에서는 factor변수를 넣으면 자동으로 더미를 생성해줌
# 모든 더미를 넣을 때는 상수항을 빼줘야됨
# R에서는 0 or -1을 회귀식에 넣은면 상수항을 빼고 추정함
form2 <- lgaspcar ~ 0 + lincomep + lrpmg + lcarpcap + cindex

po <- plm(form2, data=aa, model='pooling') # OLS 모형

stargazer(fe, re, po, type='html', out='패널모형비교1.doc')

# 위처럼 국가더미를 모두 넣고 stargazer를 이용하면 테이블이 너무 길어짐
# 국가더미들은 제외하고 테이블을 아래와 같은 명령어로 재정리
stargazer(fe, re, po, type='html',
          column.labels = c('FE', 'RE', 'OLS (국가더미포함)'),
          omit='cindex', # cindex로 시작하는 국가더미들은 테이블 결과표에서 omit함
          star.cutoffs = c(0.1, 0.05, 0.01), # 유의수준, 디폴트이므로 설정안해줘도 됨
          star.char = c('*', "**", "***"), # 유의수준에 따른 별 개수
          keep.stat = c('N', 'adj.rsq'), # N은 샘플 수, adj.rsq는 adjusted R squared
          out='패널모형비교2.doc')


# Hausman Test를 통해 RE와 FE 중에서 더 좋은 것을 고를 수 있음
# 귀무가설 채택 : RE선택, 대립가설 채택 : FE선택
# 변수를 쓰는 순서는 상관없음
phtest(re,fe) # 귀무가설을 기각하고 대립가설 채택, FE모형 선택

# FE모형은 time-invariant 변수들도 시간차분으로 모두 없애는게 문제, 상수항을 다 없애버림
# 이럴때는 모든 id더미를 넣은 OLS를 사용하는게 좋은 방법임

# 순서 상관없이 이렇게도 가능
phtest(form, data=aa)





