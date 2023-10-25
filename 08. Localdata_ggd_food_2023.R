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


#--------------------------------- 여기서부터는 데이터 분석
#데이터분석
#1.가장 오래 영업 중인 음식점
foodshop %>%
  filter(!is.na(open_date)&status=="영업") %>% #결측치제거, 영업데이터 추출
  filter(open_date==min(open_date)) %>% #개업일이 가장 빠른 데이터 추출
  select(name, type, open_date, address)
#2.주요 업종별로 가장 오래 영업중인 음식점
foodshop %>%
  filter(!is.na(open_date)&status=="영업") %>% #결측치제거, 영업데이터 추출
  filter(type%in%c("기타","경양식","분식","일식","중국식","호프/통닭"))%>%
  group_by(type) %>%#업종별 분류
  filter(open_date==min(open_date)) %>% #개업일이 가장 빠른 데이터 추출
  select(name, type, open_date, address)
#3.업종별 개업 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% #결측치제외
  group_by(type) %>%
  summarise(n=n()) %>% #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1)) %>% #범주별비율계산
  arrange(desc(n)) %>%
  head(10)
#4.영업 중인 음식점의 업종별 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% #결측치제외
  filter(status=="영업") %>% #영업만 추출
  group_by(type) %>%
  summarise(n=n()) %>% #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1)) %>% #범주별비율계산
  arrange(desc(n)) %>%
  head(10)
#5.전체 음식점의 영업과 폐업 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% #결측치제외
  group_by(status) %>%
  summarise(n=n()) %>% #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1)) #범주별비율계산
#6.주요 업종별 영업과 폐업 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% #결측치제외
  filter(type%in%c("기타","경양식","분식","일식","중국식","호프/통닭"))%>%
  group_by(type,status) %>%#교차차 분류
  summarise(n=n()) %>% #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1))%>% #범주별비율계산
  filter(status=="영업") %>% #영업만 추출
  arrange(desc(n))
#7.개업이 많았던 연도
foodshop %>%
  filter(!is.na(open_date)&!is.na(district))%>% #결측치제외
  group_by(open_year) %>%
  summarise(n=n()) %>% #범주빈도계산
  arrange(desc(n)) %>%
  head(5)
#8.폐업이 많았던 연도
foodshop %>%
  filter(!is.na(close_date)&!is.na(district))%>% #결측치제외
  group_by(close_year) %>%
  summarise(n=n()) %>% #범주빈도계산
  arrange(desc(n)) %>%
  head(5)
#9.연도별 개업 음식점수 그래프
#연도별 개업 음식점수
open_trend <- foodshop %>%
  filter(!is.na(open_date)&!is.na(district)) %>% #결측치제외
  group_by(open_year) %>%
  summarise(open_n=n())
#open_trend 구조
str(open_trend)
#연도별 개업 음식점수 막대그래프
ggplot(data=open_trend,aes(x=open_year,y=open_n))+
  geom_col()+
  xlab("연도") + ylab("개업수")
#10.연도별 폐업 음식점수 그래프
#연도별 폐업 음식점수
close_trend <- foodshop %>%
  filter(!is.na(open_date)&!is.na(district)) %>% #결측치제외
  group_by(close_year) %>%
  summarise(close_n=n())
#open_trend 구조
str(close_trend)
#연도별 폐업 음식점수 막대그래프
ggplot(data=close_trend,aes(x=close_year,y=close_n))+
  geom_col()+
  xlab("연도") + ylab("폐업수")
#11.개업과 폐업 음식점 통합 그래프
open_trend1<-rename(open_trend,year=open_year)#연도이름 변경
close_trend1<-rename(close_trend,year=close_year)#연도이름 변경

open_close_trend<-left_join(open_trend1,close_trend1,by="year")#통합

ggplot()+
  geom_line(data=open_close_trend, aes(year,open_n))+#개업그래프
  geom_line(data=open_close_trend, aes(year,close_n,color="red"))+#폐업그래프
  xlab("연도") + ylab("개수")
#12.폐업음식점수가 개업음식점수보다 많았던 기간 확인
open_close_trend %>%
  filter(close_n>open_n)
#13.영업중인 음식점수가 가장 많은 5개 시
district_business<-foodshop %>%
  filter(!is.na(open_date)&!is.na(district)&status=="영업") %>% #결측치제거
  group_by(district) %>%
  summarise(n=n())

district_business %>%
  arrange(desc(n)) %>%
  head(5)
#14,25개 시의 음식점수 막대그래프
ggplot(data = district_business, aes(x=reorder(district,n),y=n))+
  geom_col()+
  coord_flip()+#막대 90도회전
  xlab("영업시")+
  ylab("영업 음식점 수")
#15.주요 업종별로 영업하는 음식점이 많은 시
foodshop %>%
  filter(!is.na(open_date)&!is.na(district)) %>% #결측치제거
  filter(type%in%c("기타","경양식","분식","일식","중국식","호프/통닭"))%>%
  filter(status=="영업") %>% #영업만 추출
  group_by(type,district) %>%
  summarise(n=n()) %>%
  mutate(total=sum(n),pct=round(n/total*100,1))%>% #범주별비율계산
  group_by(type) %>%
  filter(pct==max(pct))#type별 district비율이 가장 높은 데이터 추출
