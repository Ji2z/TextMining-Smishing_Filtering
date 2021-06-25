##NaiveBayes

naive_train_data = cleandata2[train,2:87]
naive_test_data = cleandata2[test,2:87]

naive_train_label = cleandata2[train,1]
naive_test_label = cleandata2[test,1]


#나이브 베이즈 분류기는 일반적으로 범주형 특징으로 된 데이터에 대해 훈련
#셀의 값을 단어가 나타나는지 여부에 따라 단순히 yes 혹은 no를 나타내는 범주형 변수로 바꿀 필요가 있음
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No") }

#행이나 열을 명시하기 위해 margin 사용. 열에 관심있으므로 margin=2
sms_train <- apply(naive_train_data, MARGIN = 2, convert_counts)
sms_test <- apply(naive_test_data, MARGIN = 2, convert_counts)


#데이터에 대한 모델 훈련(라플라스의 유무 차이 설정)
library(e1071)
sms_classifier <- naiveBayes(sms_train, as.factor(naive_train_label))
sms_classifier2 <- naiveBayes(sms_train, as.factor(naive_train_label), laplace=1) 

#모델성능평가
#처음보는 문자의 특징은 sms_test 행렬에 저장돼 있고, 클래스 레이블은 sms_test_labels에 저장
#훈련했던 분류기 이름은 sms_classifier.
#이 분류기로 예측을 생성한 후, 예측된 값과 실제 값을 비교할 것임. 예측하는데 predict 함수 사용
sms_test_pred <- predict(sms_classifier, sms_test)
sms_test_pred2 <- predict(sms_classifier2, sms_test)


#예측을 실제값과 비교하기 위해 gmodels 패키지의 CrossTable() 함수를 사용
library(gmodels)
CrossTable(sms_test_pred, naive_test_label,
           dnn = c('predicted', 'actual'))
CrossTable(sms_test_pred2, naive_test_label, 
           dnn = c('predicted', 'actual'))




