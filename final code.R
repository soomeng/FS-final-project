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

# 시가총액 가장 큰 종목 위치
max_cap_index <- which.max(market_cap)

# 시가총액 가장 큰 종목 확인
name[max_cap_index]
## 크래프톤

# 시가총액 가장 큰 종목의 PER
high_cap_per <- per[max_cap_index] 

# 데이터 분석
data <- data.frame(Company = name[max_cap_index] , PER = high_cap_per, AVG_PER = industry_avg_per)
data <- data %>% mutate(PER_Ratio = PER / AVG_PER)
data

# PER과 시가총액의 상관관계 분석
per_cap_data <- data.frame(PER = per, Market_Cap = market_cap)

ggplot(per_cap_data, aes(x = Market_Cap, y = PER)) +
  geom_point(color = "blue") +
  labs(title = "시가총액과 PER의 상관관계", x = "시가총액", y = "PER")

cor(market_cap, per)

# per 값 시각화
per_data <- data.frame(Name = name, PER = per)

ggplot(per_data, aes(x = Name, y = PER)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "종목별 PER", x = "종목명", y = "PER") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 전체 종목에 대한 PER 비율 계산
per_ratio <- per / industry_avg_per

# 종목명과 per_ratio 데이터 프레임 생성
data <- data.frame(name, per_ratio)

# 고평가, 저평가, 음수 레이블 생성
data$valuation <- ifelse(data$per_ratio > 1, "고평가", 
                         ifelse(data$per_ratio < 0, "-", "저평가"))

# 그래프 그리기
ggplot(data, aes(x = name, y = per_ratio, fill = valuation)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("black", "blue", "red")) +
  labs(title = "종목 가치평가", x = "종목명", y = "PER 비율") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 1, color = "gray50")


# 고평가된 종목인 액토즈소프트의 주가변화 확인
library(httr)
library(dplyr)
library(stringr)
siseJson <- function(url){
  # 데이터 가져오기
  data <- GET(url)
  
  # HTML 데이터를 문자열로 변환
  df <- data %>%
    read_html() %>%        # HTML 파싱
    html_text() %>%        # HTML 텍스트 추출
    read_csv()             # CSV 형식의 데이터로 읽기
  
  # 열 이름에서 작은 따옴표와 대괄호 제거
  names(df) <- str_replace_all(names(df), "['\\[\\]]", "")
  
  # 모든 열에서 큰따옴표, 대괄호, 백슬래시 제거
  df[] <- lapply(df, function(x) str_replace_all(x, '["\\[\\]\\\\]', ""))
  
  # 8번째 열 제거
  df <- df[, -8]
  
  # 첫 번째 열을 날짜 데이터로 변환
  df$날짜 <- as.Date(df$날짜, format = "%Y%m%d")
  
  # 마지막 NA 데이터 제거
  df <- df[1:(nrow(df)-1), ]
  
  # 종가, 거래량 데이터를 숫자로 변환
  df$종가 <- as.numeric(df$종가)
  df$거래량 <- as.numeric(df$거래량)
  print(df)
}

actoz_url <- "https://api.finance.naver.com/siseJson.naver?symbol=052790&requestType=1&startTime=20230101&endTime=20230531&timeframe=day"
actoz <- siseJson(actoz_url)

ggplot(actoz, aes(x = 날짜, y = 종가)) +
  geom_line() +
  labs(x = "날짜", y = "종가") +
  ggtitle("액토즈소프트의 주가 변화")
