##SVM

# 트레이닝 데이터와 테스트 데이터 분류
svmtrain <- cleandata[train,]
svmtest <- cleandata[-train,]

#svm실행, 예측모델 구축
library(e1071)
svm.linear <- svm(Class~., data=svmtrain, scale=FALSE, kernel='linear')
pred.linear <- predict(svm.linear, svmtest[,-1])

#결과 확인
CrossTable(pred.linear,svmtest$Class)
