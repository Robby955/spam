set.seed(2020)
library(caret)
library(caTools)

spam=read.table('spam.txt')
colNames=c('make','address','all','3d','our','over','remove','internet','order','mail','recieve','will','people','report','addresses','free','business','email','you','credit','your','font','000','money','hp','hpl','george','650','lab','labs','telnet','857','data','415','85','technology','1999','parts','pm','direct','cs','meeting','original','project','re','edu','table','confrence',';','(','[','!','$','#','CAPAVE','CAPMAX','CAPTOT',"SPAM")
colnames(spam)=colNames

# Shuffle
rows<-sample(nrow(spam))
spam=spam[rows,]
# Train Test Split
train_ind <- sample(seq_len(nrow(spam)), size = 0.66*nrow(spam))
spam_train <- spam[train_ind, ]
spam_test <- spam[-train_ind, ]

spam_train_pred=log(spam_train[,1:57]+0.1)
spam_train_ind=spam_train[,58]

spam_test_pred=log(spam_test[,1:57]+0.1)
spam_test_ind=spam_test[,58]

# Desicion Tree
library(rpart)
library(rpart.plot)
modelTree=rpart(as.factor(spam_train_ind)~.,data=spam_train_pred,method='class',control = rpart.control(min_bucket=5,cp=0.005))

getPred=predict(modelTree,newdata=spam_test_pred)[,2]
getPred[getPred>0.5]=1
getPred[getPred<=0.5]=0

confusionMatrix(as.factor(spam_test_ind),as.factor(getPred))

# Random Forest
library(randomForest)
modelForest=randomForest(as.factor(spam_train_ind)~.,data=data.frame(spam_train_pred),type='classification',importance=TRUE,n.trees=5000)
pforest=predict(modelForest,newdata=data.frame(spam_test_pred))
confusionMatrix(as.factor(spam_test_ind),pforest)

# Gradient Boosting 

library(gbm)

modelGBM=gbm(as.character(spam_train_ind)~.,data=data.frame(spam_train_pred),n.trees=10000,distribution='adaboost',interaction.depth=6,shrinkage=0.1)
pgbm=predict(modelGBM,newdata=data.frame(spam_test_pred),n.trees = 10000,type='response')
pgbm[pgbm>0.5]=1
pgbm[pgbm<=0.5]=0

confusionMatrix(as.factor(spam_test_ind),as.factor(pgbm))











