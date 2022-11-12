# 경제분석에서는 계절조정된 자료를 사용하는 것이 중요
# 계절조정 : Seasonally adjusted, SA
# 비계절조정 : Non-Seasonally adjusted

# 계절조정이 되지 않은 자료를 분석할 경우 계절성의 존재로 전년동기 대비로 분석을 해야함

# 계절조정되지 않은 자료는 일반적으로 톱니바퀴 형태를 보임
# 통상적으로 경기요소인 GDP, 수출, 물가 등은 계절성을 지니고 있음
# 금리, 통화량은 계절성에 영향을 잘 받지 않음


# GDP갭 = log(실질GDP) - log(잠재GDP), (실질GDP - 잠재GDP)/잠재GDP
# 경기변동은 GDP갭으로 표현이 되는데 실질GDP는 쉽게 자료를 구할 수 있음
# 잠재GDP는 일종의 장기적인 추세로 계산되며 HP filter를 이용하여 장기적인 추세를 구하고 
# 이 둘의 차이를 이용하여 주기(또는 cycle)를 추출

library(ecos); library(seasonal); library(mFilter); library(tempdisagg); library(tidyverse)
library(tsbox); library(xts); library(patchwork); library(ggfortify); library(timeSeries)

my_key <- c('D9X3413P7XEYGVU65ZHL')

# 통계표[코드][주기] : 국내총생산에 대한 지출(원계열, 실질, 분기 및 연간), 200Y010
# 통계항목[코드][단위] : 국내총생산에 대한 지출, 10601
# 주기는 cycle로 해서 DD, MM, QQ, YY 단위로 추출가능

gdp_nsa <- statSearch(api_key = my_key, stat_code = '200Y010', item_code1='10601', cycle='Q')
gdp_nsa

# 자료는 1960년 1분기부터 존재
gdp_nsa <- ts(gdp_nsa$data_value, frequency = 4, start=c(1960,1))

# 로그취하기
lgdp_nsa1 <- log(gdp_nsa)

# xts로 변환
lgdp_nsa2 <- as.xts(lgdp_nsa1)

# 시각화
gdp1 <- autoplot(lgdp_nsa2)+ggtitle('log(실질 GDP) (원계열 : 계절조정X')+ylab('')+xlab('') + theme(text = element_text(size=20))
gdp1


# 통계표[코드][주기] : 국내총생산에 대한 지출(계절조정, 실질, 분기 및 연간), 200Y008
# 통계항목[코드][단위] : 국내총생산에 대한 지출, 10601
gdp_sa <- statSearch(api_key = my_key, stat_code = '200Y008', start_time='196001', item_code1='10601', cycle='Q')
gdp_sa <- ts(gdp_sa$data_value, frequency=4, start=c(1960,1))
lgdp_sa1 <- log(gdp_sa)
lgdp_sa2 <- as.xts(lgdp_sa1)

gdp2 <- autoplot(lgdp_sa1) + ggtitle('log(실질gdp) (계절조정 o)')+ylab('')+xlab('') + theme(text = element_text(size=20))

gdp1 | gdp2


# 계절조정안된 lgdp_sa1을 계절조정 하기
autoplot(lgdp_nsa1)
lgdp_sa_new <- seas(lgdp_nsa1)

# seas() 함수를 실행하면 여러가지 결과물이 생기는데, s12값이 계절조정된 값임
lgdp_sa_new1 <- series(lgdp_sa_new, c('s12'))
lgdp_sa_new2 <- as.xts(lgdp_sa_new1)

# 이제 한국은행에서 받은 계절조정자료(gdp2)와 원계열을 계절조정한 그림(gdp4)를 비교
gdp4 <- autoplot(lgdp_sa_new2)
gdp2 | gdp4

# 이제 계절조정된 자료를 mFilter()를 이용해서 순환요소 뽑아내기
# lgdp_sa1은 ts자료임, HP(Hodrick-Prescott) 방식으로 추출
# mFilter을 이용하면 다양한 요소들이 추출되는데, 이 중에서 cycle만 뽑아내면 됨
lgdp_temp <- mFilter(lgdp_sa1, filter=c('HP'))
data_trend <- cbind(lgdp_temp$x, lgdp_temp$trend)
colnames(data_trend) <- c('원자료', '장기추세')

autoplot(data_trend, facets = FALSE) | autoplot(lgdp_temp$cycle)
autoplot(data_trend, facets = TRUE)

lgdp_temp1 <- lgdp_temp$cycle
lgdp_cycle <- lgdp_temp1
lgdp_cycle2 <- as.xts(lgdp_cycle)

autoplot(lgdp_cycle2)

# 이제 위 자료들을 모두 병합하기
# 이를 위해 시계열(ts) 자료들을 xts 형태로 변환하고 merge()를 이용해 변수들을 그룹화
lgdp_nsa <- as.xts(lgdp_nsa1)
lgdp_sa <- as.xts(lgdp_sa_new1)

final <- merge(lgdp_nsa, lgdp_sa) %>% merge(lgdp_cycle2)

final1 <- autoplot(final[, c('lgdp_nsa')])
final2 <- autoplot(final[, c('lgdp_sa')])
final3 <- autoplot(final[, c('lgdp_cycle2')])

(final1 / final2) | final3


# 저빈도에서 고빈도로 자료를 변환(분가지료를 월별로 변환)
# tempdisagg 패캐지의 td() 함수 이용, 변환방법은 여러가지가 있음
# dendon_cholette() 방식은 형태보존(movement preservation)에 초점을 두고 있으며 변환을 위해 다른 변수 불필요
# chow-lin 방식은 정상성(stationary)을 갖거나 공적분을 갖을 경우 적절, 다른 변수들과 회귀분석을 통해 고빈도 자료를 생성
# 여기서는 dendon-cholette() 방식을 이용
# lgdp_sa (분기자료를) 상수 1에 놓고 추정을 하고, 이를 통해 추정된 값을 이용해서 월별자료로 변환

lgdp_temp <- td(final$lgdp_sa ~ 1, to='monthly', method='denton-cholette')
lgdp_monthly <- predict(lgdp_temp)

autoplot(lgdp_monthly)


# 고빈도에서 저빈도로 자료를 변환 (월별자료를 분기로 변환)
# ta() 함수로 월별을 분기로 변환하기 위해 3가지 옵션이 존재
# 전체합산(sum), 평균(average), 처음값(first)/마지막값
# GDP는 월별자료를 합산해서 도출됨

lgdp_monthly <- ts(lgdp_monthly, frequency = 12, start = c(1960,1))
lgdp_quartely <- ta(lgdp_monthly, conversion = 'sum', to = 'quarterly')
gdp_comp <- merge(final$lgdp_sa, lgdp_quartely)

autoplot(gdp_comp)




