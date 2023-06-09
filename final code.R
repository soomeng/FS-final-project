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

table <- html %>% 
  html_table() %>% 
  .[[3]]
table

# PER 데이터 스크래핑
per <- table[[10]] %>% 
  .[!is.na(.)]

# 해당 산업의 평균 PER
industry_avg_per <- mean(per)
industry_avg_per
## 24.7

# 종목명 스크래핑
name <- table[[1]] %>% 
  .[nchar(.) > 0]

# 시가총액 데이터 스크래핑
market_cap <- table[[8]] %>% 
  .[nchar(.) > 0] %>% 
  gsub(",", "", .)  %>%
  as.numeric()

max_cap_index <- which.max(market_cap)
max_cap_index
## 1

# 시가총액 큰 기업
name[max_cap_index]
## 크래프톤
high_cap_per <- per[max_cap_index] 

# 데이터 분석 및 시각화
data <- data.frame(Company = name[max_cap_index] , PER = high_cap_per, AVG_PER = industry_avg_per)
data <- data %>% mutate(PER_Ratio = PER / AVG_PER)
data
##    Company   PER AVG_PER PER_Ratio
## 1 크래프톤 18.77    24.7  0.759919

per_cap_data <- data.frame(PER = per, Market_Cap = market_cap)

ggplot(per_cap_data, aes(x = Market_Cap, y = PER)) +
  geom_point(color = "blue") +
  labs(title = "시가총액과 주가 수익비율의 상관관계", x = "시가총액", y = "PER")

cor(market_cap, per)
## -0.1070046

# per 값 시각화
per_data <- data.frame(Name = name, PER = per)

ggplot(per_data, aes(x = Name, y = PER)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "기업별 주가 수익비율", x = "종목명", y = "PER") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# per가 매우 높은 액토즈소프트 데이터 이용
max_per_index <- which.max(per)
name[max_per_index]
high_per <- per[max_per_index]

data_ <- data.frame(Company = name[max_per_index] , PER = high_per, AVG_PER = industry_avg_per)
data_ <- data_ %>% mutate(PER_Ratio = PER / AVG_PER)
data_
##          Company    PER AVG_PER PER_Ratio
## 1 액토즈소프트 * 324.29    24.7  13.12915



# Tukey의 극단치 검출 방법을 사용하여 이상치 데이터의 위치 찾기
Q1 <- quantile(per, 0.25)
Q3 <- quantile(per, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# 이상치 데이터의 위치 찾기
outlier_indices <- which(per < lower_bound | per > upper_bound)
boxplot(outlier_indices)

# 이상치 데이터의 위치 출력
outlier_indices

# 이상치와 제거거
per_no_outliers <- per[-outlier_indices]
cap_no_outliers <- market_cap[-outlier_indices]

per_cap_data_ <- data.frame(PER = per_no_outliers, Market_Cap = cap_no_outliers)

ggplot(per_cap_data_, aes(x = Market_Cap, y = PER)) +
  geom_point(color = "blue") +
  labs(title = "시가총액과 주가 수익비율의 상관관계(이상치 제거)", x = "시가총액", y = "PER")

cor(per_no_outliers, cap_no_outliers)

# ------------------------------------------------------------------------------
remDr$getStatus()

remDr$open()

semi_url <- "https://finance.naver.com/sise/sise_group_detail.naver?type=upjong&no=278"

remDr$navigate(semi_url)
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
semi_per <- table[[10]] %>% 
  gsub(",", "", .)  %>%
  .[-c(1,n-1,n)] %>% 
  as.numeric()

# semi_per 벡터에서 NA의 위치 찾기
na_indices <- which(is.na(semi_per))
na_indices

semi_per <- semi_per[-na_indices]
length(semi_per)

semi_market_cap <- table[[8]] %>% 
  .[nchar(.) > 0] %>% 
  gsub(",", "", .)  %>%
  as.numeric()

semi_market_cap <- semi_market_cap[-na_indices]
length(semi_market_cap)

semi_per_cap_data <- data.frame(PER = semi_per, Market_Cap = semi_market_cap)

ggplot(semi_per_cap_data, aes(x = Market_Cap, y = PER)) +
  geom_point(color = "blue") +
  labs(title = "시가총액과 주가 수익비율의 상관관계", x = "시가총액", y = "PER")
