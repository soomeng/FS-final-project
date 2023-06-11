# FS-final-project


>PER 값만을 보고 PER이 큰지 작은지 판단할 수 없다 생각이 되어 해당 업종 평균 PER을 비교하여 업종 평균 대비 PER이 큰지 작은지 분석
이 분석을 통해 해당 종목이 고평가 되어있는지 저평가 되어있는지 확인

---
* PER
```
주가수익비율로 기업 이익에 비해 주가가 어느 정도 수준인지 나타낸 비율

주가 / 주당 순이익(EPS) 로 계산하며 1주당 수익의 몇 배가 되는지 나타냄

* EPS = 당기 순이익 / 발행된 주식 수
```

* 시가총액
```
발행주식수와 주가를 곱하여 계산

상장주식을 시가로 평가한 것으로 회사의 규모를 평가할 때 사용
```
---


분석을 위해 필요한 패키지를 불러오고, "게임엔터테인먼트" 업종별로 분석에 필요한 데이터를 체크해주기 위해 doker를 사용

```{r}
library(rvest)
library(dplyr)
library(RSelenium)
library(ggplot2)

remDr <- remoteDriver(
  remoteServerAddr = 'localhost', 
  port = 4429, 
  browserName = 'chrome') 

remDr$getStatus()

remDr$open()

url <- "https://finance.naver.com/sise/sise_group_detail.naver?type=upjong&no=263"

remDr$navigate(url)
remDr$screenshot(display=TRUE)

webElem <- remDr$findElements(using="css", "input[checked]")
for(i in 1:length(webElem)) webElem[[i]]$clickElement()

option <- paste("#option", 1:27, sep="")

for(i in 1:6){
  webElem <- remDr$findElement(using="css", option[i])
  webElem$clickElement()
}
remDr$screenshot(display=TRUE)

element <- remDr$findElement(using="css", "div.item_btn > a")
element$clickElement()
html <- read_html(remDr$getPageSource()[[1]])
```

html을 이용해 원하는 테이블을 추출하고, 필요한 데이터인 종목명, 시가총액, PER 데이터를 스크래핑

```{r}
table <- html %>% 
  html_table() %>% 
  .[[3]]

# 종목명
name <- table[[1]] %>% 
  .[nchar(.) > 0]
  
# 시가총액
market_cap <- table[[8]] %>% 
  .[nchar(.) > 0] %>% 
  gsub(",", "", .)  %>%
  as.numeric()
  
# PER
per <- table[[10]] %>% 
  .[!is.na(.)]
```

* PER을 이용한 종목 분석석

업종 평균 PER 대비 해당 종목의 PER 비율을 구하기 위해 업종 전체 PER의 평균을 계산

``` {r}
# 업종 전체의 평균 PER
industry_avg_per <- mean(per)
## 24.7
```

비교데이터는 시가총액이 가장 큰 종목을 선택

주가가 큰 기업이 고평가 되어있다고 가정하여 분석 하기 위해 시가총액이 가장 큰 종목을 선택

``` {r}
# 시가총액 가장 큰 종목 위치
max_cap_index <- which.max(market_cap)
## 1

# 시가총액 가장 큰 종목 확인
name[max_cap_index]
## 크래프톤

# 시가총액 가장 큰 종목의 PER
high_cap_per <- per[max_cap_index] 
```
* PER 비율 = 종목의 PER / 해당 업종 평균 PER

시가총액이 가장 큰 종목의 PER 비율 계산을 통한 종목 평가

PER 비율이 0.759919로 1보다 작은 값으로, '크래프톤' 종목의 주가는 업종 평균에 비해 비교적 저평가되어 있는 것으로 볼 수 있음

``` {r}
# 데이터 분석
data <- data.frame(Company = name[max_cap_index] , PER = high_cap_per, AVG_PER = industry_avg_per)
data <- data %>% mutate(PER_Ratio = PER / AVG_PER)
data
##    Company   PER AVG_PER PER_Ratio
## 1 크래프톤 18.77    24.7  0.759919
```

* PER과 시가총액의 상관관계 분석

상관계수가 -0.1070046로 매우 약한 음의 상관관계를 가진다는 것을 알 수 있음 (상관계수의 값이 0에 가까울수록 두 변수 간의 선형관계가가 약함)

데이터 분석 결과를 시각화 하기 위해 PER과 시가총액의 관계를 나타내는 산점도를 그림
위의 산점도를 통해해 얻을 수 있는 결과는 시가총액이 증가함에 따라 PER가  감소하는 경향을 확인

``` {r set up, include=true}
# PER과 시가총액의 상관관계 분석
per_cap_data <- data.frame(PER = per, Market_Cap = market_cap)

ggplot(per_cap_data, aes(x = Market_Cap, y = PER)) +
  geom_point(color = "blue") +
  labs(title = "시가총액과 주가 수익비율의 상관관계", x = "시가총액", y = "PER")

cor(market_cap, per)
## -0.1070046
```

---
* 분석 결과에 대한 설명
```
위에 설명한 PER와 시가총액의 정의를 빌려 설명 할 수 있음음

1) PER의 산정방식에서 EPS를 필연적으로 계산할수 밖에 없는데, EPS라는 개념이 해당 기업의 재무제표상 당기순이익을 기반으로 산정된 수치라는 것

재무제표상 자본 및 부채는 공정가치와 장부가치를 기반으로 작성

2)시가총액은 말그대로 정통한 시장에서의 해당 기업의 객관적인 시장가치성격을 가짐짐

결) 재무제표상 자본총계와 시가총액이 필연적으로 다를 수 밖에 없기에 상관계수가 낮게 나타남
```
---


* 검증

시가총액이 아닌 PER값이 가장 큰 종목을 이용해  

이 그래프를 통해 '액토즈소프트' 종목이 가장 높은 PER을 가지는 것을 확인.

```{r}
# per 값 시각화
per_data <- data.frame(Name = name, PER = per)

ggplot(per_data, aes(x = Name, y = PER)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "기업별 주가 수익비율", x = "종목명", y = "PER") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

```{r}
# 전체 종목에 대한 PER 비율 계산
per_ratio <- per / industry_avg_per

# 종목명과 per_ratio 데이터 프레임 생성
data <- data.frame(name, per_ratio)

# 고평가, 저평가 레이블 생성
data$valuation <- ifelse(data$per_ratio > 1, "고평가", "저평가")

# 그래프 그리기
ggplot(data, aes(x = name, y = per_ratio, fill = valuation)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "주식 가치평가", x = "종목명", y = "PER 비율") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```


# 해당 통계 분석의 신뢰성 검증을 위해 반도체 업종의 데이터를 이용해 동일한 분석을 행하여 동일하게 상관관계가 매우 낮음을 확인했다.
``` {r}
url <- "https://finance.naver.com/sise/sise_group_detail.naver?type=upjong&no=278"

remDr$navigate(url)
remDr$screenshot(display=TRUE)

webElem <- remDr$findElements(using="css", "input[checked]")
for(i in 1:length(webElem)) webElem[[i]]$clickElement()

option <- paste("#option", 1:27, sep="")

for(i in 1:6){
  webElem <- remDr$findElement(using="css", option[i])
  webElem$clickElement()
}

remDr$screenshot(display=TRUE)

element <- remDr$findElement(using="css", "div.item_btn > a")
element$clickElement()
html <- read_html(remDr$getPageSource()[[1]])

table <- html %>% 
  html_table() %>% 
  .[[3]]

# PER 데이터 스크래핑
n <- length(table[[10]])
per <- table[[10]] %>% 
  gsub(",", "", .)  %>%
  .[-c(1,n-1,n)] %>% 
  as.numeric()

# per 벡터에서 NA의 위치 찾기
na_indices <- which(is.na(per))

per <- per[-na_indices]

# 시가총액 데이터 스크래핑
market_cap <- table[[8]] %>% 
  .[nchar(.) > 0] %>% 
  gsub(",", "", .)  %>%
  as.numeric()

market_cap <- market_cap[-na_indices]

cor(market_cap, per)
## 0.00692
```