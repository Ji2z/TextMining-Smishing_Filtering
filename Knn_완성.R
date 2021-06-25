##KNN

library(class)

#트레이닝 데이터와 테스트 데이터 분리
train_data = cleandata[train,2:87]
test_data = cleandata[test,2:87]
train_label = cleandata[train,1]
test_label = cleandata[test,1]

#정확도가 가장 높을 때의 k값 구하기
#(전처리에서 샘플을 새로 설정할 때마다 적정한 k값이 달라진다.)
acc_k <- NULL
for(kk in c(1:nrow(train_data))){
  set.seed(1234) 
  knn_k <- knn(train_data, test_data, train_label, k = kk) 
  acc_k <- c(acc_k, sum(knn_k == test_label) / length(test_label)) } 
valid_k <- data.frame(k = c(1:nrow(train_data)), accuracy = acc_k) 

plot(formula = accuracy ~ k, data = valid_k, type = "o", pch = 20, main = "k값에 따른 정확도")
with(valid_k, text(accuracy ~ k, labels = rownames(valid_k), pos = 3, cex = 0.7))

#위의 결과를 따라 정확도가 가장 높을 때의 k를 사용해 knn테스트
knntest = knn(train_data, test_data, train_label,k=11)

#확률 표
library(gmodels)
CrossTable(test_label,knntest,prop.chisq = F)

