# 구조확인
str(congestion)

#변수의 이상치와 결측치 확인하고 처리
summary(congestion)

#결측치 개수 확인
is.na(congestion)
sum(is.na(congestion))
colSums(is.na(congestion))

#결측치가 있는 행을 제거한 새로운 데이터 프레임 생성
congestion1 <- congestion[!is.na(congestion$s0600),]
#6시 출발기차의 결측치를 제거
colSums(is.na(congestion1))

#23시 30분 출발기차의 결측치를 제거
congestion1 <- congestion1[!is.na(congestion1$s2330),]
colSums(is.na(congestion1))

#남은 결측치를 0으로 대체
congestion1[is.na(congestion1)] <- 0
colSums(is.na(congestion1))

#이상치 확인
ggplot(congestion1, aes(y=s0530))+
  geom_boxplot()

summary(congestion1$s0530)

#파생변수 만들기
#1.지하철역의하루평균혼잡도
congestion1$day_mean <-rowMeans(congestion1[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330')])

#1 하루 평균 혼잡도 내림차순
congestion1 %>%
  + group_by(line)%>%
  + select(day_mean)%>%
  + arrange(desc(day_mean))

#2.지하철 호선별 출근시간대 평균혼잡도
workgo <- congestion1 %>%
  group_by(line)%>%
  summarise(m0730=mean(s0730),m0800=mean(s0800),m0830=mean(s0830),m0900=mean(s0900))

summary(workgo)

#막대그래프로 그리기
ggplot(data=workgo, aes(x=reorder(line,m0800),y=m0800))+
  + geom_col()+
  + coord_flip()+
  + ggtitle("호선별 8시 평균혼잡도")

#3. 출발시간8시의지하철혼잡도범주화/범주별빈도분석
congestion1 %>%
  + mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>%
  + group_by(s80_grade) %>% 
  + summarise(n=n())%>% 
  + mutate(total=sum(n), pct=round(n/total*100,1))%>% 
  + select(s80_grade,n,pct)%>%
  + arrange(desc(n))

congestion1 %>% 
  + mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>% 
  + group_by(line, s80_grade) %>%
  + summarise(n=n())%>%  
  + mutate(total=sum(n), pct=round(n/total*100,1))%>% 
  + filter(s80_grade=="caution")%>% 
  + select(line, s80_grade,n,pct)%>% 
  + arrange(desc(pct))%>% 
  + head(5)

#4. 지하철 호선별 퇴근시간대 평균혼잡도
workout <- congestion1 %>%
  group_by(line)%>%
  summarise(m1800=mean(s1800),m1830=mean(s1830),m1900=mean(s1900),m1930=mean(s1930),m2000=mean(s2000))

summary(workout)

# 막대그래프로 나타내기
ggplot(data=workout, aes(x=reorder(line,m1800),y=m1800))+
  geom_col()+
  coord_flip()+
  ggtitle("호선별 18시 평균혼잡도")

#출발시간18시의 지하철혼잡도범주화/범주별빈도분석
congestion1 %>%
  + mutate(s1800_grade=ifelse(s1800<=80, "good", ifelse(s1800<=130, "normal", ifelse(s1800<=150, "caution", "bad"))))%>%
  + group_by(s1800_grade) %>% 
  + summarise(n=n())%>% 
  + mutate(total=sum(n), pct=round(n/total*100,1))%>% 
  + select(s1800_grade,n,pct)%>%
  + arrange(desc(n))

congestion1 %>% 
  + mutate(s1800_grade=ifelse(s1800<=80, "good", ifelse(s1800<=130, "normal", ifelse(s1800<=150, "caution", "bad"))))%>% 
  + group_by(line, s1800_grade) %>%
  + summarise(n=n())%>%  
  + mutate(total=sum(n), pct=round(n/total*100,1))%>% 
  + filter(s1800_grade=="bad")%>% 
  + select(line, s1800_grade,n,pct)%>% 
  + arrange(desc(pct))%>% 
  + head(5)

# 호선별 역별 기여도 구하기
line2_s1800 <- congestion %>%
  + filter(line==2)%>%
  + select(line,s1800,station)

line2_s1800_pct <- line2_s1800 %>%
  +     + group_by(station)%>%
  +     + summarise(total=sum(s1800))%>%
  +     + mutate(all=sum(total),pct=round(total/all*100,2))
