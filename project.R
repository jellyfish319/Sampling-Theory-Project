population <- read.csv("통합 문서1.csv")
disease <- read.csv("통합 문서2.csv", header = F)
frame <- read.csv("시도별_장애유형별_장애정도별_성별_등록장애인수_20231125051646.csv",header = F,encoding = "UTF-8")

library(tidyverse)

dim(population)
population <- population[2:22,1:6]
names(population) <- c("시도","60세이상", "60~64", "65~69", "70세이상", "총인구")

population[,-1] <- apply(population[,-1], 2, function(x) {as.numeric(gsub(",","",x))})

population %>% mutate("고령화인구" = `65~69`+`70세이상`) %>% mutate("고령화비율" = 고령화인구/총인구) -> population

b <- ifelse((population$고령화비율 >= 0.07) & (population$고령화비율 < 0.14), print("고령화지역"), ifelse((population$고령화비율 < 0.2) & (population$고령화비율 >= 0.14), "고령지역", "초고령화지역"))

population$고령지역 <- b

population <- population[-c(1:4),]

table(population$고령지역)/17

disease[,-1] <- as.numeric(gsub(",","",disease[,-1]))

sum(disease[15:27,]$V2)/2652860

n <- 8000

population %>% mutate(고령지역 = factor(고령지역, labels = c("고령지역", "고령화지역", "초고령화지역"))) -> population

names(population)

frame[-1,] -> frame

names(frame) <- c("시도", "장애인구", "심한 장애", "심하지않은 장애")

frame <- cbind(frame, population[,-1])

총장애인구 <- sum(frame$장애인구)

frame %>% mutate(제곱근변환 = sqrt(장애인구)) %>% group_by(고령지역) %>% summarise("고령화 지역별 장애인구" = sum(장애인구),
                                       "비례배분" = sum(장애인구)/총장애인구)

# 비례배분 : 고령지역 0.48, 고령화지역 : 0.245, 초고령화지역 : 0.275

p <- c(0.48, 0.245, 0.275)

p*8000

# 1차층화

frame2 <- read.csv("시군구별_장애정도별_성별_등록장애인수_20231125152558.csv")

# R&D 예산 조회

univ <- read_csv(file = "고등교육기관 개황.csv", na = "-")

names(univ)

univ %>% select(-c(입학정원...13,입학정원...14,입학자...15,입학자...16,입학자...17,졸업자...18,졸업자...19,졸업자...20,직원수...21,직원수...22,직원수...23))-> univ1

names(univ1)

for (i in 1:ncol(univ)) {
  if (i == 1 | i == 2 | i == 3 | i == 6) {
    names(univ1)[i] <- univ1[1,i]
  }
  else {
    names(univ1)[i] <- paste0(gsub("\\...+[0-9]$", "",names(univ1)[i]), "(", univ1[1,i],")")
  }
  
}

univ1[-1,] -> univ1

univ1 <- as.data.frame(univ1)



for (i in 4:12) {
  univ1[,i] <- as.numeric(univ1[,i])
}

tibble(univ1) -> univ1

view(univ1)

univ1 %>% filter(학제 == "대학" | 학제 == "전문대학") -> univ0

univ0 %>% select(-`학교수(부설대학원)`) -> univ0



sum(univ0$`재적학생수(전체)`[-11])/univ0$`재적학생수(전체)`[11]

unique(univ$학제)
unique(univ$시도)

univ1 <- univ1[-c(183:186),]

# 학제 별 학교 수
univ1 %>% filter(시도 == "전국") %>% select(학제,`학교수(학교수)`) %>% arrange(desc(`학교수(학교수)`))

# 지역 별 학교 수
univ1 %>% filter(시도 != "전국", 학제 != "소계") %>% select(시도,`학교수(학교수)`) %>% group_by(시도) %>% summarise(sum = sum(`학교수(학교수)`, na.rm = T)) %>% arrange(desc(sum))

# 학제 별 학생 수
univ1 %>% filter(시도 == "전국", 학제 != "소계") %>% select(학제,`재적학생수(전체)`) %>% arrange(desc(`재적학생수(전체)`))

# 지역 별 학생 수
univ1 %>% filter(시도 != "전국", 학제 != "소계", 구분 != "전체") %>% select(시도,`재적학생수(전체)`) %>% group_by(시도) %>% summarise(sum = sum(`재적학생수(전체)`, na.rm = T)) %>% arrange(desc(sum))

# 학제 별 교원 수
univ1 %>% filter(시도 == "전국") %>% select(학제,`전임교원수(전체)`) %>% arrange(desc(`전임교원수(전체)`))

# 지역 별 교원 수
univ1 %>% filter(시도 != "전국", 학제 != "소계") %>% select(시도,`전임교원수(전체)`) %>% group_by(시도) %>% summarise(sum = sum(`전임교원수(전체)`, na.rm = T)) %>% arrange(desc(sum))


(1855374 + 509169 + 325549 + 145480 + 125667 + 3267)/3042848
(65939+11179+6049)/87576

univ1 %>% filter(시도 != "전국", 학제 == "대학" | 학제 == "전문대학" | 학제 == "부설대학원") %>% 
  select(학제, `전임교원수(전체)`) %>% 
  group_by(학제) %>% 
  summarise(sum = sum(`전임교원수(전체)`))

univ1  %>% filter(시도 == "전국")


83167/88165

univ1 %>% filter(시도 != "전국") %>% select(학제,`재적학생수(전체)`,`전임교원수(전체)`, `학교수(학교수)`) %>% filter(학제!="소계", 학제 != "대학", 학제 != "전문대학", 학제 != "부설대학원") %>% group_by(학제) %>%
  summarise(학생수 = sum(`재적학생수(전체)`),
            교원수 = sum(`전임교원수(전체)`),
            학교수 = sum(`학교수(학교수)`, na.rm = T)
            )

univ1 %>% filter(시도 != "전국") %>% select(학제,`재적학생수(전체)`,`전임교원수(전체)`, `학교수(학교수)`) %>%
  filter(학제!="소계", 학제 != "대학", 학제 != "전문대학", 학제 != "부설대학원", 학제 != "사내대학", 학제 != "기술대학") %>% group_by(학제) %>%
  summarise(학생수 = sum(`재적학생수(전체)`),
            교원수 = sum(`전임교원수(전체)`),
            학교수 = sum(`학교수(학교수)`, na.rm = T)
  )

# 학생 표본 수

# 표본배분 학생수

univ1 %>% filter(시도 != "전국") %>% select(학제,`재적학생수(전체)`,`전임교원수(전체)`, `학교수(학교수)`) %>%
  filter(학제!="소계", 학제 != "대학", 학제 != "전문대학", 학제 != "부설대학원", 학제 != "사내대학", 학제 != "기술대학") %>% group_by(학제) %>%
  summarise(학생수 = sum(`재적학생수(전체)`),
            교원수 = sum(`전임교원수(전체)`),
            학교수 = sum(`학교수(학교수)`, na.rm = T)
 ) %>% mutate(가중치 = 학생수/sum(학생수), 제곱근학생수 = sqrt(학생수),제곱근가중치 = 제곱근학생수/sum(제곱근학생수), 비례배분 = n*가중치, 제곱근비례배분 = n*제곱근가중치) -> sample1

sample1 %>% select(학제,학생수,가중치,비례배분,제곱근가중치,제곱근비례배분)

N <- sample1$학생수
D <- 0.01*0.01/4
a <- sample1$학생수/sum(sample1$학생수)
p <- 0.5

sum(N^2*p*(1-p)/a)/(sum(N)^2*D+sum(N*p*(1-p)))


# 표본배분 교원

N <- 4979
p <- 0.5
B <- 0.01
n <- N*p^2/((N-1)*(B^2/4)+p^2)

univ1 %>% filter(시도 != "전국") %>% select(학제,`재적학생수(전체)`,`전임교원수(전체)`, `학교수(학교수)`) %>%
  filter(학제!="소계", 학제 != "대학", 학제 != "전문대학", 학제 != "부설대학원", 학제 != "사내대학", 학제 != "기술대학", 학제 != "원격대학") %>% group_by(학제) %>%
  summarise(학생수 = sum(`재적학생수(전체)`),
            교원수 = sum(`전임교원수(전체)`),
            학교수 = sum(`학교수(학교수)`, na.rm = T)
  ) %>% mutate(가중치 = 교원수/sum(교원수), 제곱근교원수 = sqrt(교원수),제곱근가중치 = 제곱근교원수/sum(제곱근교원수), 비례배분 = n*가중치, 제곱근비례배분 = n*제곱근가중치) -> sample2

sample2 %>% select(학제,교원수,가중치,비례배분,제곱근가중치,제곱근비례배분)

N <- sample2$교원수
D <- 0.01*0.01/4
a <- sample2$교원수/sum(sample2$교원수)
p <- 0.5

sum(N^2*p*(1-p)/a)/(sum(N)^2*D+sum(N*p*(1-p)))


univ1 %>% filter(시도 != "전국") %>% select(시도,학제,`재적학생수(전체)`,`전임교원수(전체)`, `학교수(학교수)`) %>% 
  filter(학제 != "소계" & (학제 == "대학" | 학제 == "전문대학" | 학제 == '부설대학원')) -> univ3

univ3 %>% group_by(학제) %>% summarise(학생수 = sum(`재적학생수(전체)`))

# 표본의 크기

univ3 %>% filter(!(학제 == "전문대학")) %>% pivot_wider(names_from = 학제, values_from = c(`재적학생수(전체)`, `전임교원수(전체)`,`학교수(학교수)`)) %>% 
  mutate(`재적학생수(대학전체)` = `재적학생수(전체)_대학` + `재적학생수(전체)_부설대학원`,
         `전임교원수(대학전체)` = `전임교원수(전체)_대학` + `전임교원수(전체)_부설대학원`,
         학교수 = `학교수(학교수)_대학`) %>% select(시도,`재적학생수(대학전체)`, `전임교원수(대학전체)`, 학교수) %>% 
  mutate(`학교 수 당 조사단위 수` = (`재적학생수(대학전체)`+`전임교원수(대학전체)`)/학교수) -> college

univ3 %>% filter(학제 == "전문대학") %>% mutate(`학교 수 당 조사단위 수` = (`재적학생수(전체)`+`전임교원수(전체)`)/`학교수(학교수)`) -> junior

sum(college$`재적학생수(대학전체)`)
sum(junior$`재적학생수(전체)`)
sum(college$학교수)
sum(junior$`학교수(학교수)`)

(sum(college$`재적학생수(대학전체)`) + sum(college$`전임교원수(대학전체)`))/(sum(junior$`재적학생수(전체)`)+sum(junior$`전임교원수(전체)`))
sum(college$학교수)/sum(junior$`학교수(학교수)`)
(sum(college$`재적학생수(대학전체)`)/sum(college$학교수))/(sum(junior$`재적학생수(전체)`)/sum(junior$`학교수(학교수)`))

(sum(college$`재적학생수(대학전체)`) + sum(college$`전임교원수(대학전체)`) +sum(junior$`재적학생수(전체)`)+sum(junior$`전임교원수(전체)`))/(sum(college$학교수) + sum(junior$`학교수(학교수)`))



N <- c(sum(college$`재적학생수(대학전체)`+college$`전임교원수(대학전체)`),sum(junior$`재적학생수(전체)`+junior$`전임교원수(전체)`))
D <- 0.01*0.01/4
a <- N/sum(N)
p <- 0.5

sum(N^2*p*(1-p)/a)/(sum(N)^2*D+sum(N*p*(1-p)))


c <- (sum(college$`재적학생수(대학전체)`) + sum(college$`전임교원수(대학전체)`))/sum(college$학교수)
j <-(sum(junior$`재적학생수(전체)`)+sum(junior$`전임교원수(전체)`))/sum(junior$`학교수(학교수)`)
c/j

kor <- data.frame(학생수 = 26684+9047, 대학원생수 = 8786+3191, 전임교원수 = 1785)
cnu <- data.frame(학생수 = 22385, 대학원생수 = 4293+1361, 전임교원수 = 1037)
inh <- data.frame(학생수 = 24981, 대학원생수 = 2966+803, 전임교원수 = 891)
kyu <- data.frame(학생수 = 3953, 전임교원수 = 80)

kor$학생수 + cnu$학생수+inh$학생수 + kyu$학생수

N <- c(87050, 21400, 3793)
p <- 0.5
B <- 0.01
a <- N/sum(N)

n <- sum(N^2*p*(1-p)/a)/(sum(N)^2*D+sum(N*p*(1-p)))

w <- N/112243
w*9182

w <- sqrt(N)/sum(sqrt(N))
w*9182

s <-c(kyu$학생수,kor$학생수, cnu$학생수,inh$학생수)
w <- s/sum(s)
5387*w

w <- sqrt(s)/sum(sqrt(s))
5387*w

s <-c(kor$대학원생수, inh$대학원생수, cnu$대학원생수)
w <- s/sum(s)
2671*w

w <- sqrt(s)/sum(sqrt(s))
2671*w

s <- c(kyu$전임교원수,kor$전임교원수, inh$전임교원수,cnu$전임교원수)
w <- s/sum(s)

1124*w

w <- sqrt(s)/sum(sqrt(s))
1124*w

245/3953 ; 2211/35731 ; 1546/24981 ; 1385/22385

11857.43/3912.391
