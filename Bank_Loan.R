#Setting up working directory for Bank_Personal_Loan_Modelling
setwd("C:/Users/bhavn/Desktop/Great Lakes - PG BABI/Data Mining/Project")

#Importing the given dataset
Cust_Loan_data = read.csv("Bank_Personal_Loan_Modelling.csv",header=T)

#Getting to know features of dataset
dim(Cust_Loan_data)
#5000   14

#Identifying NA values
sum(is.na(Cust_Loan_data))

which(is.na(Cust_Loan_data))

colSums(is.na(Cust_Loan_data))

# replacement of NA values with 0 in family memeber columns
Cust_Loan_data$Family.members[which(is.na(Cust_Loan_data$Family.members))] <- 0
#Cross-Checking of NA values
sum(is.na(Cust_Loan_data$Family.members))


#identifying Negative values from which rows
has.neg <- apply(Cust_Loan_data, 1, function(row) any(row < 0))
which(has.neg)
length(which(has.neg))

#identifying Negative values from which coloumns
names(Cust_Loan_data)[sapply(Cust_Loan_data, function(x) min(x))<0]

#Converting rows with negative values to postive
Cust_Loan_data$Experience..in.years. = abs(Cust_Loan_data$Experience..in.years.)

#Identifying important columns for data analysis by excluding 
names(Cust_Loan_data)
#separating un wanted columns
cust_data = Cust_Loan_data[-c(1,5)]


#Five point summary
summary(cust_data)

#Identifying Outliers in data
boxplot(cust_data,cex.axis = 0.7,col = c(rainbow(12)),main = "Outlier Detection among variables")

#Correlation among the bank customer details
corrplot(cor_cust_data,method = "number",col = c(rainbow(140)),tl.col = "black",tl.cex = 1,bg="grey60")

#Education variable analyisis
hist(cust_data$Education,xlab = "Education level among bank customers",col = c("black"),xlim = c(1,3),ylim = c(0,2500),main = "Histogram of Education level")

#Percentage analyisis on Securities account, CD Account, Online,Creditcard 
# Securities_Account = table(Securities.Account)
# prop.table(Securities_Account)
# Securities.Account
# 0      1 
# 0.8956 0.1044 
# CD_Account = table(CD.Account)
# prop.table(CD_Account)
# CD.Account
# 0      1 
# 0.9396 0.0604 
# Online = table(Online)
# prop.table(Online)
# Online
# 0      1 
# 0.4032 0.5968 
# CreditCard = table(CreditCard)
# prop.table(CreditCard)
# CreditCard
# 0     1 
# 0.706 0.294 

#Clustering of the sample - K-means Clustering - Idendtufying clusters
library(NbClust)
set.seed(seed = 100)
clust_size = NbClust(cust_data,min.nc = 2,max.nc = 6,method = "kmeans")

#Scaling the data:
Scaled_cust_data = scale(cust_data)
print(Scaled_cust_data)

seed = 1000
set.seed(seed)
final_clust = kmeans(Scaled_cust_data, centers = 4,nstart = 5)
print(final_clust)
clusplot(Scaled_cust_data,final_clust$cluster,color = T,shade =T,label=5,lines=3,cex = 0.7,main = "Cluster segmenation of bank customer data")

#Adding cluster data to orginal dataset
cust_data$cluster = final_clust$cluster
print(cust_data)
head(cust_data,10)

#Brief working on how clusters work on dataset:
custProfile = aggregate(cust_data,list(cust_data$cluster),FUN = "mean")
custProfile_round = round(custProfile)
View(custProfile_round)

#Changing numerical categorical varaiables to factors in dataset
cust_data$Personal.Loan = as.factor(cust_data$Personal.Loan)
cust_data$CD.Account = as.factor(cust_data$CD.Account)
cust_data$CreditCard = as.factor(cust_data$CreditCard)
cust_data$Online = as.factor(cust_data$Online)
cust_data$Securities.Account = as.factor(cust_data$Securities.Account)
cust_data$Education = as.factor(cust_data$Education)
cust_data$Family.members = as.factor(cust_data$Family.members)

#*************** CART desision tree*****************************
#Creating Training and Testing Dataset for CART
library(caTools)
set.seed(5000)
Target_CART = sample.split(cust_data$Personal.Loan, SplitRatio = 0.7)
Train_CART = subset(cust_data, Target_CART == TRUE)
Test_CART = subset(cust_data, Target_CART == FALSE)
nrow(Train_CART)
nrow(Test_CART)
str(Train_CART)
head(Train_CART)
table(Train_CART$Personal.Loan)


#constructing CART
library(rpart)

#setting complexity parameter
r.ctrl = rpart.control(minisplit =50,minbucket = 10, cp = 0, xval = 10)

#***********Growing Desicion Tree for Training CART model********
Train_DT_CART = rpart(Personal.Loan~Age..in.years.+ Experience..in.years.+Income..in.K.month.+Family.members+CCAvg+Education+Mortgage+Securities.Account+CD.Account+Online+CreditCard, data = Train_CART, method = "class",control = r.ctrl)

#plotting CART using "rattle" library - fancyRpartPlot for Train data
library(rattle)
fancyRpartPlot(Train_DT_CART, main = "Desicion Tree for Train data")

#Pruning thr tree for Train data
printcp(Train_DT_CART)

#Trying to identify parameters for pruning the tree by plotting - complexity parameter for Train data
plotcp(Train_DT_CART)

#Pruned CART for Train data 
prune_CART_Train = prune(Train_DT_CART,cp=0.020,"CP")

printcp(prune_CART_Train)

#Plotting pruned CART for Train data
fancyRpartPlot(prune_CART_Train)

path.rpart(prune_CART_Train,c(1:7))

#Scoring/Predicting the training dataset
Train_CART$prediction = predict(prune_CART_Train,data = Train_CART, type = "class")
Train_CART$score = predict(prune_CART_Train,data = Train_CART, type = "prob")
head(Train_CART)

#*********Growing Desicion Tree for Test CART model*********
Test_DT_CART = rpart(Personal.Loan~Age..in.years.+ Experience..in.years.+Income..in.K.month.+Family.members+CCAvg+Education+Mortgage+Securities.Account+CD.Account+Online+CreditCard, data = Test_CART, method = "class",control = r.ctrl)

#Pruning thr tree for Test data
printcp(Test_DT_CART)

#Trying to identify parameters for pruning the tree by plotting - complexity parameter for Test data
plotcp(Test_DT_CART)

#pruned Test CART
prune_CART_Test = prune(Test_DT_CART,cp=0.020,"CP")

printcp(prune_CART_Test)

#Plotting pruned CART for Test data
fancyRpartPlot(prune_CART_Test)

path.rpart(prune_CART_Test,c(1:7))

#Scoring/Predicting the testing dataset
Test_CART$prediction = predict(prune_CART_Test,data = Test_CART, type = "class")
Test_CART$score = predict(prune_CART_Test,data = Test_CART, type = "prob")
head(Test_CART)


#*********************RANDOM FOREST*************************
library(caTools)
seed=5000
set.seed(5000)
Target_RF = sample.split(cust_data$Personal.Loan, SplitRatio = 0.7)
Train_RF = subset(cust_data, Target_RF == TRUE)
Test_RF = subset(cust_data, Target_RF == FALSE)
library(randomForest)
nrow(Train_RF)
nrow(Test_RF)
Intial_Random_Forest = randomForest(Train_RF$Personal.Loan~.,data = Train_RF, ntree = 501,mtry = 3,nodesize=10,importance = T)

#Excuting the RF
print(Intial_Random_Forest)

#Ploting the RF for identifying the right number of trees
plot(Intial_Random_Forest)

#Identifying the critical variables which is used to build RF
importance(Intial_Random_Forest)


#Tuning Rf model to find the best mtry
set.seed(seed)
Tuned_Random_Forest = tuneRF(x=Train_RF[,-c(8)],y=Train_RF$Personal.Loan,mtryStart = 10,stepFactor = 1.5,ntree=151,improve = 0.0001,nodesize=10,trace = T,plot = T,doBest = T,importance = T)

#Building refined RF for Train data 
Final_Train_RF = randomForest(Train_RF$Personal.Loan~.,data = Train_RF, ntree = 151,mtry = 5,nodesize=10,importance = T)

#Scoring/Predicting the testing  for RF Train data dataset
Train_RF$prediction = predict(Final_Train_RF,data = Train_RF, type = "class")
Train_RF$score = predict(Final_Train_RF,data = Train_RF, type = "prob")


#Building refined RF for Test data
Final_Test_RF = randomForest(Test_RF$Personal.Loan~.,data = Test_RF, ntree = 81,mtry = 7,nodesize=10,importance = T)

#Scoring/Predicting the testing  for RF Train data dataset
Test_RF$prediction = predict(Final_Test_RF,data = Train_RF, type = "class")
Test_RF$score = predict(Final_Test_RF,data = Train_RF, type = "prob")

##*************************Performance Measure of models**********************

##************************Confusion Matrix for CART model*********************

## CART Model Confusion Matrix
cfm_Train_CART = table(Train_CART$Personal.Loan,Train_CART$prediction)
cfm_Test_CART = table(Test_CART$Personal.Loan,Test_CART$prediction)

##Accuracy
(cfm_Train_CART[1,1]+cfm_Train_CART[2,2])/nrow(Train_CART)
(cfm_Test_CART[1,1]+cfm_Test_CART[2,2])/nrow(Test_CART)

##Error rate
(cfm_Train_CART[1,2]+cfm_Train_CART[2,1])/nrow(Train_CART)
(cfm_Test_CART[1,2]+cfm_Test_CART[2,1])/nrow(Test_CART)

##***********************Confusion Matrix for Random forest model***************
cfm_Train_RF = table(Train_RF$Personal.Loan,Train_RF$prediction)
cfm_Test_RF = table(Test_RF$Personal.Loan,Test_RF$prediction)

##Accuracy
(cfm_Train_RF[1,1]+cfm_Test_RF[2,2])/nrow(Train_RF)
(cfm_Test_RF[1,1]+cfm_Test_RF[2,2])/nrow(Test_RF)

##Error rate
(cfm_Train_RF[1,2]+cfm_Train_RF[2,1])/nrow(Train_RF)
(cfm_Test_RF[1,2]+cfm_Test_RF[2,1])/nrow(Test_RF)

##************************ROCR curve for CART model*********************
##ROC curve Train Data for CART model
Pred_obj_Train_CART = prediction(Train_CART$score[,2],Train_CART$Personal.Loan)
perf_Train_CART = performance(Pred_obj_Train_CART,"tpr","fpr")
plot(perf_Train_CART, main = "ROC curve for Training Data of CART model")
##ROC curve Test Data for CART model
Pred_obj_Test_CART = prediction(Test_CART$score[,2],Test_CART$Personal.Loan)
perf_Test_CART = performance(Pred_obj_Train_CART,"tpr","fpr")
plot(perf_Test_CART, main = "ROC curve for Test Data CART model")

#************************************************************************




##************************ROCR curve for RF model*********************
##ROC curve Train Data for RF model
Pred_obj_Train_RF = prediction(Train_RF$score[,2],Train_RF$Personal.Loan)
perf_Train_RF = performance(Pred_obj_Train_RF,"tpr","fpr")
plot(perf_Train_RF, main = "ROC curve for Training Data of RF model")
##ROC curve Test Data for RF model
Pred_obj_Test_RF = prediction(Test_RF$score[,2],Test_RF$Personal.Loan)
perf_Test_RF = performance(Pred_obj_Train_RF,"tpr","fpr")
plot(perf_Test_RF, main = "ROC curve for Test Data of RF model")

#************************************************************************

##************************Kolomogorov-Smirnov chart for CART model*****
max(perf_Train_CART@y.values[[1]]-perf_Train_CART@x.values[[1]])
0.9172724
max(perf_Test_CART@y.values[[1]]-perf_Test_CART@x.values[[1]])
0.9172724
##********************************************************************

##************************Kolomogorov-Smirnov chart for RF model******
max(perf_Train_RF@y.values[[1]]-perf_Train_RF@x.values[[1]])
0.9336283
max(perf_Test_RF@y.values[[1]]-perf_Test_RF@x.values[[1]])
0.9336283
##*********************************************************************

##*************Area-Under-Curve*********************
##*************A-U-C**for**CART-model***** 
Auc_Train_CART=performance(Pred_obj_Train_CART,"auc")
as.numeric(Auc_Train_CART@y.values)
0.9842852
Auc_Test_CART=performance(Pred_obj_Train_CART,"auc")
as.numeric(Auc_Test_CART@y.values)
0.9842852
##***************************************************

##*************A-U-C**for**RF-model******** 
Auc_Train_RF=performance(Pred_obj_Train_RF,"auc")
as.numeric(Auc_Train_RF@y.values)
0.9935702
Auc_Test_RF=performance(Pred_obj_Train_RF,"auc")
as.numeric(Auc_Test_RF@y.values)
0.9935702
##***************************************************

##*************Gini-Index*********************
##*************Gini-Index**for**CART-model***** 
ineq(Train_CART$score,"gini")
0.4970078
ineq(Test_CART$score,"gini")
0.807838
##*************Gini-Index**for**RF-model*****
ineq(Train_RF$score,"gini")
0.4977267
ineq(Test_RF$score,"gini")
0.4966353
##***************************************************

##***************Concordance and discordance ratio*******
##**for CART-model***** 
Concordance(actuals = Train_CART$Personal.Loan,predictedScores = Train_CART$score)
Concordance(actuals = Test_CART$Personal.Loan,predictedScores = Test_CART$score)

##****for RF-model*****
Concordance(actuals = Train_RF$Personal.Loan,predictedScores = Train_RF$score)
Concordance(actuals = Test_RF$Personal.Loan,predictedScores = Test_RF$score)
##***************************************************************