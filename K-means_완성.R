##K-means

##cosine distance를 이용해 doc-doc형식의 매트릭스로 변환
library(proxy)
coco = as.matrix(dist(cleanframe, method = "cosine"))
View(coco)

##k-means
kmeans_test <- kmeans(coco, 2) 

#k-means graph
library(cluster)
clusplot(coco,kmeans_test$cluster,color=TRUE,main="K-means clustering (k=2)",cex=2,shade = TRUE)

#실제 데이터와 얼마나 차이가 있게 분류되었는지 테이블로 확인
table(cleandata$Class,kmeans_test$cluster)

#k를 정하기 위한 elbow method graph
k_max<-50
sumwit <- sapply(1:k_max,function(k){kk<-kmeans(coco,k,nstart = 10,iter.max = 9999)$tot.withinss})
plot(1:k_max,sumwit,xlab="Number of K", ylab="Total within-clusters sum of squares",
     main="Elbow method to find K", type="o",pch=20)
text(1:k_max,sumwit,pos=3)
