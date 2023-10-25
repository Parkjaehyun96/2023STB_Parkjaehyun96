#1. 실습에 필요한 packages를 라이브러리에 등록
library(dplyr)
library(ggplot2)

#2. 데이터를 새객체 foodshop으로 불러오기
#비어있는 셀은 결측치 처리/파라미터 문자형으로 변환
foodshop <- read.csv("ggd_food_2023.csv", na="", stringsAsFactors = F)

#3. 데이터의 구조 확인
str(foodshop)

#4. 분석변수 추출 및 변수이름 변경
foodshop <- foodshop %>%
  rename(open_date=인허가일자, status=상세영업상태명, 
         close_date=폐업일자, name=사업장명, type=업태구분명,
         address=소재지전체주소) %>%
  select("name","type","status","open_date","close_date", "address")

#5. 데이터 구조 재확인
str(foodshop)

#6. 날짜데이터를 분석용 데이터로 변경
#YYYYMMDD형식으로 변경
foodshop$open_date <- gsub("-","",foodshop$open_date)
foodshop$close_date <- gsub("-","",foodshop$close_date)
#문자형 데이터를 정수형 데이터로 변환
foodshop$open_date <- as.integer(foodshop$open_date)
foodshop$close_date <- as.integer(foodshop$close_date)

# 데이터 구조 확인
str(foodshop)

#7.파생변수 만들기
#status변수
table(foodshop$status)
#영업상태가 영업/폐업이 아닌 것을 제외
foodshop <- foodshop %>% 
  filter(status == '영업' | status == '폐업') %>%
  select(name,type,status,open_date,close_date,address)

#type변수
table(foodshop$type)

#na값제외
foodshop<-foodshop%>%filter(open_date!= '') %>%select(name,type,status,open_date,close_date,address)

#8.open_date변수
range(foodshop$open_date, na.rm = T)
table(is.na(foodshop$open_date))#결측치 없음
foodshop$open_year<-substr(foodshop$open_date,1,4)#인허가년도 변수 생성

#9.close_date변수
range(foodshop$close_date, na.rm = T)
foodshop$close_year<-substr(foodshop$close_date,1,4)#폐업년도 변수 생성

#10.address변수
foodshop$district<-substr(foodshop$address,5,7)#시 정보를 분리하여 변수 생성
table(foodshop$district)#이상치 확인
foodshop$district <- ifelse(foodshop$district%in%c("106","시 강","시 계","시 관","시 금","시 남","시 노","시 마","시 미","시 서","시 용","시 은","시흥시","회"),NA,foodshop$district)#이상치제거

table(foodshop$district)#이상치 확인

#11. 최종 확인
str(foodshop)
#문자형데이터를 정수형으로 변경
foodshop$open_year <- as.integer(foodshop$open_year)
foodshop$close_year <- as.integer(foodshop$close_year)
str(foodshop)
