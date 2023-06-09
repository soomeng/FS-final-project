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

# 업종 전체의 평균 PER
industry_avg_per <- mean(per)
industry_avg_per
## 24.7

# 시가총액 가장 큰 종목 위치
max_cap_index <- which.max(market_cap)
## 1

# 시가총액 가장 큰 종목 확인인
name[max_cap_index]
## 크래프톤

# 시가총액 가장 큰 종목의 PER
high_cap_per <- per[max_cap_index] 

# 데이터 분석
data <- data.frame(Company = name[max_cap_index] , PER = high_cap_per, AVG_PER = industry_avg_per)
data <- data %>% mutate(PER_Ratio = PER / AVG_PER)
data
##    Company   PER AVG_PER PER_Ratio
## 1 크래프톤 18.77    24.7  0.759919

# PER과 시가총액의 상관관계 분석
per_cap_data <- data.frame(PER = per, Market_Cap = market_cap)

ggplot(per_cap_data, aes(x = Market_Cap, y = PER)) +
  geom_point(color = "blue") +
  labs(title = "시가총액과 PER의 상관관계", x = "시가총액", y = "PER")

cor(market_cap, per)
## -0.1070046

# per 값 시각화
per_data <- data.frame(Name = name, PER = per)

ggplot(per_data, aes(x = Name, y = PER)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "기업별 PER", x = "종목명", y = "PER") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# per가 가장 높은 액토즈소프트 데이터 이용
max_per_index <- which.max(per)
name[max_per_index]
high_per <- per[max_per_index]

data_ <- data.frame(Company = name[max_per_index] , PER = high_per, AVG_PER = industry_avg_per)
data_ <- data_ %>% mutate(PER_Ratio = PER / AVG_PER)
data_
##          Company    PER AVG_PER PER_Ratio
## 1 액토즈소프트 * 324.29    24.7  13.12915



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