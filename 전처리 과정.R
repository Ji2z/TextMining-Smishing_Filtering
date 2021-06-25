## 전처리

library(KoNLP)
library(stringr)
library(tm)
useSejongDic()

##파일 불러오기
ding <- readLines("Dingdong.txt")
head(ding)

##중복제거, 특수기호 제거
ding2 <- unique(ding)
ding2 <- str_replace_all(ding,"[^[:alpha:][:blank:]]"," ")
head(ding2)

##단어로 분리후 영어제거
ding2 <- extractNoun(ding2)
ding2 <-lapply(ding2,function(x){gsub("[^가-힣, ]"," ",x)})
head(ding2)

##한 글자, 긴 문자 제거
ding2 <- lapply(ding2, function(x){
  Filter(function(y){nchar(y)<=10&nchar(y)>1},x)
})
head(ding2)

##정렬하지 않고 이중리스트를 1차원의 리스트로 변환(tm에 넘겨주기 위한 작업) 후 콤마(,) 제거
ding2 <- format(ding2,justify = "none")
ding2 <- gsub("\\,","",ding2)
head(ding2)

##tm을 통해 코퍼스로 만든다.
ding3 <- Corpus(VectorSource(ding2))

##Doc-Term Matrix 만들기 (weighting을 kmeans,knn,svm은 tf-idf, naive bayes는 바이너리)
ding_tfidf <- DocumentTermMatrix(ding3, control = list(weighting = function(x)
  weightTfIdf(x, normalize = F)))
ding_bin <- DocumentTermMatrix(ding3,control = list(weighting=function(x)
  weightBin(x)))

##Term sparse(단어 출현 확률이 99%에 들지 않으면 제거) ##단어 86개
library(class)
cleandtm = removeSparseTerms(ding_tfidf,0.99)
cleandtm2 = removeSparseTerms(ding_bin,0.99)
cleandtm2$dimnames

##데이터프레임으로 변환
cleanframe <- as.data.frame(as.matrix(cleandtm))
cleanframe2 <- as.data.frame(as.matrix(cleandtm2))

##txt파일에 순서대로 153개의 비정상 문자와 80개의 정상 문자를 넣었으므로 그대로 라벨 지정
cleandata <- cbind("Class" = c(rep("spam",153),rep("ham",80)), cleanframe)
cleandata2 <- cbind("Class" = c(rep("spam",153),rep("ham",80)), cleanframe2)

#전체 데이터 중 랜덤으로 60%는 트레이닝, 40%는 테스트 (뽑은 샘플은 document의 번호를 리턴한다.)
train <- sample(nrow(cleandata),ceiling(nrow(cleandata)*0.60)) 
test = (1:nrow(cleandata))[-train]
