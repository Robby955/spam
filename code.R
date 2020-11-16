set.seed(2020)
setwd('C:/Users/User/Desktop')
spam=read.table('spam.txt')


colNames=c('make','address','all','3d','our','over','remove','internet','order','mail','recieve','will','people','report','addresses','free','business','email','you','credit','your','font','000','money','hp','hpl','george','650','lab','labs','telnet','857','data','415','85','technology','1999','parts','pm','direct','cs','meeting','original','project','re','edu','table','confrence',';','(','[','!','$','#','CAPAVE','CAPMAX','CAPTOT',"SPAM")
colnames(spam)=colNames


dim(spam)

# Shuffle
rows<-sample(nrow(spam))
spam=spam[rows,]


library(caTools)
library(caret)

# Train Test Split

train_ind <- sample(seq_len(nrow(spam)), size = 0.66*nrow(spam))

spam_train <- spam[train_ind, ]
spam_test <- spam[-train_ind, ]

dim(spam_train)
dim(spam_test)

spam_train_pred=log(spam_train[,1:57]+0.1)
spam_train_ind=spam_train[,58]

spam_test_pred=log(spam_test[,1:57]+0.1)
spam_test_ind=spam_test[,58]




#Basic Tree
library(rpart)
modelTree=rpart(as.factor(spam_train_ind)~.,data=spam_train_pred,method='class')

printcp(modelTree)
plot(modelTree)


plot(modelTree, uniform=TRUE,
   main="Classification Tree for Spam")
text(modelTree, use.n=TRUE, all=TRUE, cex=.8)




# create attractive postscript plot of tree
post(modelTree,
   title = "Classification Tree for Spam")

library(rpart.plot)
rpart.plot(modelTree)


getPred2=predict(modelTree,newdata=spam_test_pred)[,2]

getPred2[getPred2>0.5]=1
getPred2[getPred2<=0.5]=0

confusionMatrix(as.factor(spam_test_ind),as.factor(getPred2))



# Random Forest

modelForest=randomForest(as.factor(spam_train_ind)~.,data=data.frame(spam_train_pred),type='classification',importance=TRUE,n.trees=3000)
pforst=predict(modelForest,newdata=data.frame(spam_test_pred))
confusionMatrix(as.factor(spam_test_ind),pforst)



# ADA Boost 
library(gbm)

modelGBM=gbm(as.character(spam_train_ind)~.,data=data.frame(spam_train_pred),n.trees=3000,distribution='adaboost',interaction.depth=6)
pgbm=predict(modelGBM,newdata=data.frame(spam_test_pred),n.trees = 3000,type='response')

pgbm[pgbm>0.5]=1
pgbm[pgbm<=0.5]=0

confusionMatrix(as.factor(spam_test_ind),as.factor(pgbm))



