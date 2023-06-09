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

# 시가총액 가장 큰 종목 확인인
name[max_cap_index]
## 크래프톤

# 시가총액 가장 큰 종목의 PER
high_cap_per <- per[max_cap_index] 
```

시가총액이 가장 큰 종목의PER 비율 계산을 통한 종목 평가.

PER 비율이 0.759919로 비교적 낮은 편으로, '크래프톤' 종목의 주가는 평균에 비해 비교적 저평가되어 있는 것으로 볼 수 있음.
``` {r}
# 데이터 분석
data <- data.frame(Company = name[max_cap_index] , PER = high_cap_per, AVG_PER = industry_avg_per)
data <- data %>% mutate(PER_Ratio = PER / AVG_PER)
data
##    Company   PER AVG_PER PER_Ratio
## 1 크래프톤 18.77    24.7  0.759919
```