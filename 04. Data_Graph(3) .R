# step2 1개 인자로 도수분포표 작성
table(X2023STB_survey $Gender)

# step3 1개 인자로 상대도수분포표 작성
ECN <- table(X2023STB_survey $Gender)
prop.table(ECN)

# step4 2개 인자로 교차표 작성
table(X2023STB_survey $Gender, X2023STB_survey $Grade)

# step5 1개 인자로 막대그래프 작성
barplot(table(X2023STB_survey $Nationality))

# step6 1개 인자로 가로 막대그래프 작성
barplot(table(X2023STB_survey $`residential area`), xlab= "number", ylab= "residental area", xlim=c(0,40), horiz=TRUE)

# step 7 2개 인자로 막대그래프 작성
entry <- table(X2023STB_survey $Gender, X2023STB_survey $Grade)
barplot(entry, legend = TRUE)

# step 8 1개 인자로 파이그래프 작성
pie(table(X2023STB_survey $Grade))

# step 9 age 인자로 히스토그램 작성
hist(X2023STB_survey$Age, main="연령", col=terrain.colors(12))

# step 10 grade별 age를 비교하는 박스 플롯
boxplot(Grade_age$`2th Grade`, Grade_age$`3rd Grade`, Grade_age$`4th Grade` , main="grade별 age", col="yellow", names = c("2학년","3학년","4학년"))

# step 11 Grade를x로, age를 y로 하는 산점도
plot(x=X2023STB_survey$Grade, y=X2023STB_survey$Age, xlab="학년", ylab="나이", main="학년과 나이", pch=24, col="blue", bg="red", cex=1)