# FS-final-project

This is my final project.

>PER 값만을 보고 PER이 큰지 작은지 판단할 수 없다 생각이 되어 해당 업종 평균 PER을 비교하여 업종 평균 대비 PER이 큰지 작은지 분석.
이 분석을 통해 해당 종목이 고평가 되어있는지 저평가 되어있는지 확인.

분석을 위해 필요한 패키지를 불러오고, "게임엔터테인먼트" 업종별로 분석에 필요한 데이터를 체크해주기 위해 doker를 사용.

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

html을 이용해 원하는 테이블을 추출하고, 필요한 데이터인 종목명, 시가총액, PER 데이터를 스크래핑 .
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

PER을 이용해 해당 종목이 저평가 또는 고평가 되어있는지 분석. 

업종 평균 PER 대비 해당 종목의 PER 비율을 구하기 위해 업종 전체 PER의 평균 계산.
``` {r}
# 업종 전체의 평균 PER
industry_avg_per <- mean(per)
## 24.7
```
비교데이터는 시가총액이 가장 큰 종목을 선택.
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

시가총액이 가장 큰 종목의PER 비율 계산을 통한 종목 평가.

PER 비율이 0.759919로 비교적 낮은 편으로, '크래프톤' 종목의 주가는 업종 평균에 비해 비교적 저평가되어 있는 것으로 볼 수 있음.
``` {r}
# 데이터 분석
data <- data.frame(Company = name[max_cap_index] , PER = high_cap_per, AVG_PER = industry_avg_per)
data <- data %>% mutate(PER_Ratio = PER / AVG_PER)
data
##    Company   PER AVG_PER PER_Ratio
## 1 크래프톤 18.77    24.7  0.759919
```

PER과 시가총액의 상관관계 분석.

상관계수가 -0.1070046로 매우 약한 음의 상관관계를 가진다는 것을 알 수 있음. (상관계수의 값이 0에 가까울수록 두 변수 간의 선형관계는 약함)

데이터 분석 결과를 시각화 하기 위해 PER과 시가총액의 관계를 나타내는 산점도를 그림. 이 산점도만을 통해서 얻을 수 있는 결과는 시가총액이 증가함에 따라 PER이 감소하는 경향을 확인.

``` {r}
# PER과 시가총액의 상관관계 분석
per_cap_data <- data.frame(PER = per, Market_Cap = market_cap)

ggplot(per_cap_data, aes(x = Market_Cap, y = PER)) +
  geom_point(color = "blue") +
  labs(title = "시가총액과 주가 수익비율의 상관관계", x = "시가총액", y = "PER")

cor(market_cap, per)
## -0.1070046
```

위와 같은 결과를 얻으며 PER값을 더 자세히 분석하기 위해 종목별로 PER 값을 막대 그래프로 확인.

이 그래프를 통해 '액토즈소프트' 종목이 가장 높은 PER을 가지는 것을 확인.

```{r}
# per 값 시각화
per_data <- data.frame(Name = name, PER = per)

ggplot(per_data, aes(x = Name, y = PER)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "기업별 주가 수익비율", x = "종목명", y = "PER") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

'액토즈소프트' 종목을 이용해 다시 종목 분석을 실행.

분석 결과 PER 비율이 13.12915로 비교적 높은 편. '액토즈소프트' 종목의 주가는 업종 평균에 비해 비교적 고평가 되어있는 것으로 볼 수 있음.
```{r}
# per가 가장 높은 액토즈소프트 데이터 이용
max_per_index <- which.max(per)
name[max_per_index]
high_per <- per[max_per_index]

data_ <- data.frame(Company = name[max_per_index] , PER = high_per, AVG_PER = industry_avg_per)
data_ <- data_ %>% mutate(PER_Ratio = PER / AVG_PER)
data_
##          Company    PER AVG_PER PER_Ratio
## 1 액토즈소프트 * 324.29    24.7  13.12915
```

PER 값 중 이상치 때문에 상관관계가 없다는 결과가 나온 것인지 확인하기 위해 이상치를 제거 후 다시 상관관계 분석.

이상치 제거 후에도 상관계수가 0.1524286로 매우 작은 값.
```{r}
# 이상치 데이터의 위치 찾기
Q1 <- quantile(per, 0.25)
Q3 <- quantile(per, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# 이상치 데이터의 위치 찾기
outlier_indices <- which(per < lower_bound | per > upper_bound)
outlier_indices

# 이상치 제거
per_no_outliers <- per[-outlier_indices]
cap_no_outliers <- market_cap[-outlier_indices]

per_cap_data_ <- data.frame(PER = per_no_outliers, Market_Cap = cap_no_outliers)

ggplot(per_cap_data_, aes(x = Market_Cap, y = PER)) +
  geom_point(color = "blue") +
  labs(title = "시가총액과 PER의 상관관계(이상치 제거)", x = "시가총액", y = "PER")

cor(per_no_outliers, cap_no_outliers)
## 0.1524286
```