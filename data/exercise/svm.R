# https://www.cis.doshisha.ac.jp/~mjin/R/31/31.html
library(kernlab)
data(spam)

set.seed(50)
tr.num <- sample(4601,2500)
spam.train<-spam[tr.num,]
spam.test<-spam[-tr.num,]

spam.svm <- ksvm(type~.,data=spam.train, kernel="rbfdot",kpar=list(sigma=0.01))
spam.pre <- predict(spam.svm, spam.test[,-58])
print(spam.tab<-table(spam.test[,58], spam.pre))
print(train.cro <- ksvm(type~.,data=spam.train,kernel="rbfdot",kpar=list(sigma=0.05), C=5,cross=10))
