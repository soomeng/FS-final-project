---
title: "FS-final-project"
date: 2023-06-11
output: html_document
---

```{r setup}
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

```{r setup}
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

```{r setup}
# 업종 전체의 평균 PER
industry_avg_per <- mean(per)
industry_avg_per
## 24.7
```

```{r}
# 시가총액 가장 큰 종목 위치
max_cap_index <- which.max(market_cap)
## 1

# 시가총액 가장 큰 종목 확인
name[max_cap_index]
## 크래프톤

# 시가총액 가장 큰 종목의 PER
high_cap_per <- per[max_cap_index] 
```

```{r setup}
# 데이터 분석
data <- data.frame(Company = name[max_cap_index] , PER = high_cap_per, AVG_PER = industry_avg_per)
data <- data %>% mutate(PER_Ratio = PER / AVG_PER)
data
##    Company   PER AVG_PER PER_Ratio
## 1 크래프톤 18.77    24.7  0.759919
```

```{r setup}
# PER과 시가총액의 상관관계 분석
per_cap_data <- data.frame(PER = per, Market_Cap = market_cap)

ggplot(per_cap_data, aes(x = Market_Cap, y = PER)) +
  geom_point(color = "blue") +
  labs(title = "시가총액과 주가 수익비율의 상관관계", x = "시가총액", y = "PER")

cor(market_cap, per)
## -0.1070046
```

```{r setup}
# per 값 시각화
per_data <- data.frame(Name = name, PER = per)

ggplot(per_data, aes(x = Name, y = PER)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "기업별 주가 수익비율", x = "종목명", y = "PER") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 전체 종목에 대한 PER 비율 계산
per_ratio <- per / industry_avg_per

# 종목명과 per_ratio 데이터 프레임 생성
data <- data.frame(name, per_ratio)

# 고평가, 저평가 레이블 생성
data$valuation <- ifelse(data$per_ratio > 1, "고평가", "저평가")

# 그래프 그리기
ggplot(data, aes(x = name, y = per_ratio, fill = valuation)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("black", "blue", "red")) +
  labs(title = "주식 가치평가", x = "종목명", y = "PER 비율") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 1, color = "gray50")
```

```{r setup}
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
