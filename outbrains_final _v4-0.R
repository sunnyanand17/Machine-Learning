# ---------------------------------------------------Downloading and Installing Libraries ---------------------------------------------------
install.packages("ff")
install.packages("sqldf")
install.packages("rpart")
install.packages("Amelia")
install.packages("class")                     
install.packages("caret")
install.packages("corrplot", dependencies = TRUE)
install.packages("e1071")
install.packages("truncnorm")
install.packages("grDevices")
install.packages("pca")
install.packages("neuralnet")
install.packages("devtools")
install.packages("ggplot2")
install.packages("ggvis")
install.packages("pROC")
install.packages("ROCR")   
install.packages("randomForest")
install.packages("caTools")

library(ff)
library(sqldf)
library(rpart)
library(corrplot)
library(Amelia)
library(class)
library(caret)
library(e1071)
library(truncnorm)
library(grDevices)
library(pca)
library(neuralnet)
library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)
library(ggplot2)
library(ggvis)
library(pROC)
library(ROCR)
library(randomForest)
library(caTools)

args <- commandArgs(TRUE)

# ----------------------------------------------- Load the tables into dataFrames --------------------------------------------
events <- read.table(file=args[1], nrows = 1000000, header = TRUE,sep = ",")
page_views_sample <- read.table(file=args[2], header=TRUE, nrows = 1000000, sep = ",")
clicks_train <- read.table(file=args[3],nrows = 1000000, header=TRUE, sep =  ",")
clicks_test <- read.table(file=args[4], header=TRUE, nrows = 1000000, sep = ",")
promoted_content <- read.table(file=args[5], header=TRUE, nrows = 1000000, sep = ",")
document_entities <- read.table(file=args[6], header=TRUE, nrows = 1000000, sep = ",")
document_topics <- read.table(file=args[7], header=TRUE, nrows = 1000000, sep = ",")
document_categories <- read.table(file=args[8], header=TRUE, nrows = 1000000, sep = ",")

# --------------------------------------CHECK FOR NA AND INF VALUES, DATATYPES AND OUTLIERS------------------------------------------

#first, check for click_test table-----------------------------------------------------
typeof(clicks_test$display_id)
boxplot.stats(clicks_test$display_id)
typeof(clicks_test$ad_id)
boxplot.stats(clicks_test$ad_id)

indx_na<- apply(clicks_test, 2, function(x) any(is.na(x)))
indx_na
indx_inf<- apply(clicks_test, 2, function(x) any(is.infinite(x)))
indx_inf
indx_nan<- apply(clicks_test, 2, function(x) any(is.nan(x)))
indx_nan
indx_null<- apply(clicks_test, 2, function(x) any(is.null(x)))
indx_null

#second, check for events table--------------------------------------------------------
typeof(events$display_id)
boxplot.stats(events$display_id)

typeof(events$uuid)
#boxplot doesn't evaluate user_id variable because it isn't numeric/character.
boxplot.stats(events$uuid)

typeof(events$document_id)
#as all document_id are independent, hence outliers doesn't make sense here.
boxplot.stats(events$document_id)

typeof(events$timestamp)
boxplot.stats(events$timestamp)

typeof(events$platform)
typeof(events$geo_location)

indx_na<- apply(events, 2, function(x) any(is.na(x)))
indx_na
indx_inf<- apply(events, 2, function(x) any(is.infinite(x)))
indx_inf
indx_nan<- apply(events, 2, function(x) any(is.nan(x)))
indx_nan
indx_null<- apply(events, 2, function(x) any(is.null(x)))
indx_null

#third, check for clicks_train table----------------------------------------------------

typeof(clicks_train$display_id)
boxplot.stats(clicks_train$display_id)

typeof(clicks_train$ad_id)
boxplot.stats(clicks_train$ad_id)

typeof(clicks_train$clicked)

indx_na<- apply(clicks_train, 2, function(x) any(is.na(x)))
indx_na
indx_inf<- apply(clicks_train, 2, function(x) any(is.infinite(x)))
indx_inf
indx_nan<- apply(clicks_train, 2, function(x) any(is.nan(x)))
indx_nan
indx_null<- apply(clicks_train, 2, function(x) any(is.null(x)))
indx_null



#fourth, check for promoted_content table----------------------------------------------

typeof(promoted_content$ad_id)
boxplot.stats(promoted_content$ad_id)

typeof(promoted_content$document_id)

typeof(promoted_content$campaign_id)
boxplot.stats(promoted_content$ad_id)

typeof(promoted_content$advertiser_id)
boxplot.stats(promoted_content$advertiser_id)

indx_na<- apply(promoted_content, 2, function(x) any(is.na(x)))
indx_na
indx_inf<- apply(promoted_content, 2, function(x) any(is.infinite(x)))
indx_inf
indx_nan<- apply(promoted_content, 2, function(x) any(is.nan(x)))
indx_nan
indx_null<- apply(promoted_content, 2, function(x) any(is.null(x)))
indx_null


#fifth, check for page_views_sample table-----------------------------------------------

typeof(page_views_sample$uuid)
boxplot.stats(page_views_sample$uuid)

typeof(page_views_sample$document_id)
boxplot.stats(page_views_sample$document_id)

typeof(page_views_sample$timestamp)
boxplot.stats(page_views_sample$timestamp)

typeof(page_views_sample$platform)
boxplot.stats(page_views_sample$platform)

typeof(page_views_sample$geo_location)

typeof(page_views_sample$traffic_source)
boxplot.stats(page_views_sample$traffic_source)

indx_na<- apply(page_views_sample, 2, function(x) any(is.na(x)))
indx_na
indx_inf<- apply(page_views_sample, 2, function(x) any(is.infinite(x)))
indx_inf
indx_nan<- apply(page_views_sample, 2, function(x) any(is.nan(x)))
indx_nan
indx_null<- apply(page_views_sample, 2, function(x) any(is.null(x)))
indx_null


#sixth, check for document_entities table------------------------------------------------

typeof(document_entities$document_id)
boxplot.stats(document_entities$document_id)

typeof(document_entities$entity_id)
boxplot.stats(document_entities$entity_id)

typeof(document_entities$confidence_level)
boxplot.stats(document_entities$confidence_level)

indx_na<- apply(document_entities, 2, function(x) any(is.na(x)))
indx_na
indx_inf<- apply(document_entities, 2, function(x) any(is.infinite(x)))
indx_inf
indx_nan<- apply(document_entities, 2, function(x) any(is.nan(x)))
indx_nan
indx_null<- apply(document_entities, 2, function(x) any(is.null(x)))
indx_null

#seventh, check for document_topics table-----------------------------------------------

typeof(document_topics$document_id)
boxplot.stats(document_topics$document_id)

typeof(document_topics$topic_id)
boxplot.stats(document_topics$topic_id)

typeof(document_topics$confidence_level)
boxplot(document_topics$confidence_level)
boxplot.stats(document_topics$confidence_level)

indx_na<- apply(document_topics, 2, function(x) any(is.na(x)))
indx_na
indx_inf<- apply(document_topics, 2, function(x) any(is.infinite(x)))
indx_inf
indx_nan<- apply(document_topics, 2, function(x) any(is.nan(x)))
indx_nan
indx_null<- apply(document_topics, 2, function(x) any(is.null(x)))
indx_null


#eighth, check for document_categories table---------------------------------------------

typeof(document_categories$document_id)
boxplot.stats(document_categories$document_id)

typeof(document_categories$category_id)
boxplot.stats(document_categories$category_id)

typeof(document_categories$confidence_level)
boxplot.stats(document_categories$confidence_level)

indx_na<- apply(document_categories, 2, function(x) any(is.na(x)))
indx_na
indx_inf<- apply(document_categories, 2, function(x) any(is.infinite(x)))
indx_inf
indx_nan<- apply(document_categories, 2, function(x) any(is.nan(x)))
indx_nan
indx_null<- apply(document_categories, 2, function(x) any(is.null(x)))
indx_null

#missingness map test-------------------------------------------------------------------
missmap(page_views_sample,legend = TRUE,col = c("wheat","darkred"))

# ---------------------------------------------------------------------------------------------------------------------------
page_views_sample$platform<-(as.factor(page_views_sample$platform))
page_views_sample$traffic_source<-(as.factor(page_views_sample$traffic_source))

sample_m10<-model.matrix(~platform - 1,data =page_views_sample)
sample_m11<-model.matrix(~traffic_source - 1,data =page_views_sample)

page_views1<-cbind(page_views_sample,sample_m10)
page_views<-cbind(page_views1,sample_m11)

# check for NA's here: No NA values found------------------------------------------------------------------------------------
events_pageviews_na_val <- sqldf("select * from page_views as b")
View(events_pageviews_na_val)

events_pageviews_na <- sqldf("select a.display_id,a.uuid,a.document_id,b.geo_location,b.platform1,b.platform2,b.platform3,b.traffic_source1,b.traffic_source2,b.traffic_source3 from events as a,page_views as b where a.uuid = b.uuid and b.geo_location ='NA'")
View(events_pageviews_na)--------#error is coming

# ----------------------------------------------------------------------------------------------------------------------------
#Joining `events` and `page_views`
events_pageviews <- sqldf("select a.display_id,a.uuid,a.document_id,a.geo_location,b.platform1,b.platform2,b.platform3,b.traffic_source1,b.traffic_source2,b.traffic_source3 from events as a,page_views as b where a.uuid = b.uuid")

#Joining `events_pageviews` and `clicks_train`
events_pageviews_clickstrain <- sqldf("select a.display_id,a.uuid,a.document_id,a.geo_location,a.platform1,a.platform2,a.platform3,a.traffic_source1,a.traffic_source2,a.traffic_source3,b.ad_id,b.clicked from events_pageviews as a,clicks_train as b where a.display_id = b.display_id")

#Joining `events_pageviews_clickstrain` and `promotedcontent`
events_pageviews_clickstrain_promotedcontent <- sqldf("select a.display_id,a.uuid,a.document_id,a.platform1,a.platform2,a.platform3,a.geo_location,a.traffic_source1,a.traffic_source2,a.traffic_source3,a.ad_id,b.campaign_id,b.advertiser_id,a.clicked from events_pageviews_clickstrain as a,promoted_content as b where a.ad_id = b.ad_id")

# ---------------------------------------------------------------------------------------------------------------------------
#Joining `events_pageviews_clickstrain_promotedcontent` and `document_topics`
final_table<- sqldf("select a.display_id,a.uuid,a.document_id,a.platform1,a.platform2,a.platform3,a.geo_location,a.traffic_source1,a.traffic_source2,a.traffic_source3,a.ad_id,b.topic_id,b.confidence_level as topic_conf_level,a.clicked from events_pageviews_clickstrain_promotedcontent as a,document_topics as b where a.document_id = b.document_id")

#Joining `final_table` and `document_entities`
final_table1<- sqldf("select a.display_id,a.uuid,a.document_id,a.platform1,a.platform2,a.platform3,a.geo_location,a.traffic_source1,a.traffic_source2,a.traffic_source3,a.ad_id,a.topic_id,a.topic_conf_level,b.entity_id,b.confidence_level as entity_confidence,a.clicked from final_table as a,document_entities as b where a.document_id = b.document_id ")

#Joining `final_table1` and `document_categories`
final_table2<- sqldf("select a.display_id,a.uuid,a.document_id,a.platform1,a.platform2,a.platform3,a.geo_location,a.traffic_source1,a.traffic_source2,a.traffic_source3,a.ad_id,a.topic_id,a.topic_conf_level,a.entity_id,a.entity_confidence,b.category_id,b.confidence_level as category_confidence,a.clicked from final_table1 as a,document_categories as b where a.document_id = b.document_id ")

# ---------------------------------------------------------------------------------------------------------------------------
#Plotting the correlation matrix for determining dependencies
correlationMatrix <- cor(final_table2[,c(1,3:6,8:13,15:18)])
print(correlationMatrix)
corrplot(correlationMatrix,method="number")

#--------------FEATURE SELECTION-----------------
#--------Rank features by importance----------------------
final_table2$clicked <- as.factor(final_table2$clicked)
control <- trainControl(method="repeatedcv", number=3, repeats=10)
control2<- trainControl(method = "cv", number = 10)
model_rank <- train(clicked~display_id+document_id+ad_id+topic_id+category_id+platform1+platform2+platform3+traffic_source1+traffic_source2+traffic_source3+topic_conf_level+entity_confidence+category_confidence, data=final_table2[,c(1,3:6,8:13,15:18)], method="gbm", preProcess="scale", trControl=control2)
summary(model_rank)


# ----------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX----------------------------
summary(d$clicked)
plot(density(d$clicked))
sd(d$clicked,na.rm=TRUE)		#THIS SECTION's CODE NOT TO BE RUN

#dim(d) #check dimensions
#str(d) #show structure of the data
#sum(d) 
#colnames(d)
#apply(d,2,var) #check the variance accross the variables
#pca =prcomp(d) #applying principal component analysis on data
#par(mar = rep(2, 4)) #plot to show variable importance
#plot(pca) 
#'below code changes the directions of the biplot, if we donot include
#the below two lines the plot will be mirror image to the below one.'
#pca$rotation=-pca$rotation
#pca$x=-pca$x
#biplot (pca , scale =0) #plot pca components using biplot in r
#-----------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX--------------------------


# -------------------------------------------------------------------------------------------------------------------------------------------

d<-final_table2;
trainIndex <- sample(1:nrow(d), 0.5 * nrow(d));
temp <- d[trainIndex,];
summary(d$clicked)

trainIndex <- sample(1:nrow(temp), 0.9 * nrow(temp));
train <- temp[trainIndex,];
test <- temp[-trainIndex,];

#Ratio of original data
table(final_table2$clicked)/nrow(final_table2)

#Ratio of reduced data
table(train$clicked)/nrow(train)
table(test$clicked)/nrow(test)

# -------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------FINDING THE VARIANCE OF ENTIRE DATASET
d.data <- d[,c(1,3:6,8:13,15:17)]		
d.clicked <- d[,18]
d.pca <- prcomp(d.data,center=TRUE,scale.=TRUE)
plot(d.pca,type="l",main="Variance of the attributes in the Dataset")
legend('topright',legend=c('1 - display_id','2 - document_id','3 - platform','4 - traffic_source','5 - ad_id','6 - topic_id','7 - topic_conf_level','8 - entity_confidence','9 - category_id','10 - category_confidence'),box.col=1,bg='white', bty='o');

#g <- ggbiplot(d.pca, obs.scale = 1, var.scale = 1,groups = d.clicked, ellipse = TRUE, circle = TRUE)
#g <- g + scale_color_discrete(name = '')
#g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
#print(g)

#BiPlot of entire Dataset
ggbiplot(d.pca, obs.scale = 0.001, var.scale = 0.001,groups = d.clicked, ellipse = TRUE, circle = TRUE,main="BiPlot of Dataset")
	#https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/

# -------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------- Variable Separation Test Graph ---------------------------------------
plot(test[,c(1,11)], test$clicked, pch=c(2,3), cex.main=1.5, frame.plot=FALSE, col=ifelse(test$clicked==1,2,1),main=paste("Relationship between ad_id and display_id","\n for displaying separation","\n between two classes relative to clicked"))
legend('topleft',legend=c('ad_id(clicked)','ad_id(non-clicked)','display_id(clicked)','display_id(non-clicked)'),pch=c(2,2,3,3),col=c(2,1,2,1),box.col=1,bg='white', bty='o');

# -------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------Linearity test with the help of S V M--------------------------------------------

#MODEL_1 clicked~ad_id+display_id
#MODEL_2 clicked~ad_id+display_id+document_id+category_id+platform2+entity_confidence
#MODEL_3 clicked~ad_id+display_id+document_id+category_id+platform2+entity_confidence+traffic_source2+traffic_source3

#creation of svm model from test data
svm.model <- svm(clicked~ad_id+display_id+document_id+category_id+platform2+entity_confidence+traffic_source2+traffic_source3, train, cost = 100, gamma = 1);

#test data on svm model created above
svm.pred <- predict(svm.model, test[,c(1,3,5,9,10,11,15,16)]);

#rounding values of test results into integers
svm<-as.numeric(as.character(svm.pred));

#calculating accuracy
accuracy_svm <- 100*mean(svm==test[,18]);

#calculating accuracy using confusion matrix
table(pred = svm, true = test[,18]);

#plotting the result  
#plot(x=svm.pred,y=test$clicked,col='red',main=paste('Real vs predicted using SVM','\n Linearity Test'),pch=18,cex=1.5)+ abline(0,1,lwd=2)+ legend('bottomright',legend='NN',pch=18,col='red', bty='n');

cat("Accuracy using a linear SVM is: ",accuracy_svm,"\n")   

#Calculate ROC curve
svm.round <- as.numeric(as.character(svm))
svmPrediction <- prediction(predictions = svm.round, labels = test$clicked)
svmPerformance <- performance(svmPrediction, measure = "tpr", x.measure = "fpr")
plot(svmPerformance, col="red", lwd=3,main=("ROC Curve for SVM Model"))      


# ----------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------N A I V E   B A Y E S-----------------------------

#MODEL_1 clicked~ad_id+display_id
#MODEL_2 clicked~ad_id+display_id+document_id+category_id+platform2+entity_confidence
#MODEL_3 clicked~ad_id+display_id+document_id+category_id+platform2+entity_confidence+traffic_source2+traffic_source3

#Convert the final column in training data
train$clicked <- as.factor(train$clicked)

#Convert the final column in test data
test$clicked <- as.factor(test$clicked)

#Train the Naive Bayes model
modelNB <- naiveBayes(clicked~ad_id+display_id+document_id+category_id+platform2+entity_confidence+traffic_source2+traffic_source3,data = train)

#Fit the Naive Bayes using the created model
predNB <- predict(modelNB,test[,c(1,3,5,9,10,11,15,16)],type="class")

#Create a table for comparing predicted and original results
resultsNB <- table(predNB,test[,18])

#calculating accuracy using confusion matrix
table(pred = predNB, true = test[,18]);

#Calculate the accuracy for the Naive Bayes Model
accuracyNB = sum(diag(resultsNB))/sum(resultsNB)
cat("Accuracy using Naive Bayes is", accuracyNB*100,"\n")

#Calculate ROC curve
predNB.round <- as.numeric(as.character(predNB))
nbPrediction <- prediction(predictions = predNB.round, labels = test$clicked)
nbPerformance <- performance(nbPrediction, measure = "tpr", x.measure = "fpr")
plot(nbPerformance, col="red", lwd=3,main=("ROC Curve for Naive Bayes Model"))

# ---------------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------k N N------------------------------ 
#Making the output variable as a factor 
#final_table2$clicked <- factor(final_table2$clicked)

#Selecting a subset of the variables which will contribute in predicting the output variable
myvars<-c("document_id","platform1","platform2","platform3","traffic_source1","traffic_source2","traffic_source3","ad_id","topic_id","topic_conf_level","entity_confidence","category_id","category_confidence")

#Creating a subset of the selected variables
train.knn <- train[myvars]
test.knn <- test[myvars]

#summary(train.knn)

#Creating a KNN model with K=1
knn.1 <- knn(train.knn,test.knn,train$clicked,k=1)

#Creating a KNN model with K=5
knn.5 <- knn(train.knn,test.knn,train$clicked,k=5)

#Creating a KNN model with K=10
knn.10 <- knn(train.knn,test.knn,train$clicked,k=10)

#Creating a KNN model with K=20
knn.20 <- knn(train.knn,test.knn,train$clicked,k=20)

#calculating the accuracy for k = 1
inter_acc1<-100*sum(test$clicked == knn.1)/100
accuracy_1<-(inter_acc1*100)/nrow(test.knn)
cat("Accuracy using kNN when k=1 is: ",accuracy_1,"\n")

#calculating the accuracy for k = 5
inter_acc5<-100*sum(test$clicked == knn.5)/100
accuracy_5<-(inter_acc5*100)/nrow(test.knn)
cat("Accuracy using kNN when k=5 is: ",accuracy_5,"\n")

#calculating the accuracy for k = 10
inter_acc10<-100*sum(test$clicked == knn.10)/100
accuracy_10<-(inter_acc10*100)/nrow(test.knn)
cat("Accuracy using kNN when k=10 is: ",accuracy_10,"\n")

#calculating the accuracy for k = 20
inter_acc20<-100*sum(test$clicked == knn.20)/100
accuracy_20<-(inter_acc20*100)/nrow(test.knn)
cat("Accuracy using kNN when k=20 is: ",accuracy_20,"\n")

#ROC curve for k=1
knnResults = as.numeric(knn.1) - 1
knnPrediction <- prediction(predictions = knnResults, labels = test$clicked)
knnPerformance <- performance(knnPrediction, measure = "tpr", x.measure = "fpr")
plot(knnPerformance, col="red", lwd=3)

#ROC curve for k=5
knnResults = as.numeric(knn.5) - 1
knnPrediction <- prediction(predictions = knnResults, labels = test$clicked)
knnPerformance <- performance(knnPrediction, measure = "tpr", x.measure = "fpr")
plot(knnPerformance, col="red", lwd=3)

#ROC curve for k=10
knnResults = as.numeric(knn.10) - 1
knnPrediction <- prediction(predictions = knnResults, labels = test$clicked)
knnPerformance <- performance(knnPrediction, measure = "tpr", x.measure = "fpr")
plot(knnPerformance, col="red", lwd=3)

#ROC curve for k=20
knnResults = as.numeric(knn.20) - 1
knnPrediction <- prediction(predictions = knnResults, labels = test$clicked)
knnPerformance <- performance(knnPrediction, measure = "tpr", x.measure = "fpr")
plot(knnPerformance, col="red", lwd=3)


# ------------------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------R A N D O M   F O R E S T----------------

#MODEL_1 clicked~ad_id+display_id
#MODEL_2 clicked~ad_id+display_id+document_id+category_id+platform2+entity_confidence
#MODEL_3 clicked~ad_id+display_id+document_id+category_id+platform2+entity_confidence+traffic_source2+traffic_source3

#names of the attributes
names(train)

#Creating the randomForest model
rf <- randomForest(clicked ~ document_id+platform1+platform2+platform3+traffic_source1+traffic_source2+traffic_source3+ad_id+topic_id+topic_conf_level+entity_confidence+category_id+category_confidence,train,ntree=500,importance=T)

#plot the model
plot(rf)

#check the variable importance
varImpPlot(rf,sort = T,main = "Variable Importance",n.var = 5)

#Calculating the accuracy
prediction<-predict(rf,test[,1:17])
accuracy_rf <- 100*(mean(prediction == test$clicked))
cat("Accuracy using Random Forest is: ",accuracy_rf,"\n")

#ROC curve for RandomForest
prediction.round<-as.numeric(as.character(prediction))
rfPrediction <- prediction(predictions = prediction.round, labels = test$clicked)
rfPerformance <- performance(rfPrediction, measure = "tpr", x.measure = "fpr")
plot(rfPerformance, col="green", lwd=2)

# ----------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------B O O S T I N G----------------------------

#MODEL_1 clicked~ad_id+display_id
#MODEL_2 clicked~ad_id+display_id+document_id+category_id+platform2+entity_confidence
#MODEL_3 clicked~ad_id+display_id+document_id+category_id+platform2+entity_confidence+traffic_source2+traffic_source3

#simple boost tree fitting model
boosting <- train(clicked~ad_id+display_id+document_id+category_id+platform2+entity_confidence+traffic_source2+traffic_source3, method = "gbm", data = train, verbose = F, trControl = trainControl(method = "cv", number = 10))

# out-of-sample errors for testing data
pred_boosting <- predict(boosting, test[,c(1,3,5,9,10,11,15,16)])

pred_b <- as.numeric(as.character(pred_boosting))

#Confusion Matrix
table(pred = pred_b,true = test$clicked)

# summary of final model
plot(boosting)
plot(boosting$finalModel,main="Error in Boosting ")
print(varImp(boosting))

#accuracy
accuracy_boosting <- 100*mean(pred_b==test$clicked);
cat("Accuracy using Boosting for the dataset is: ",accuracy_boosting,"\n")

#boosting iterations
plot(boosting,plotType = "level")

#RMSE-Rsquared graphs
resampleHist((boosting))

#ROC curve for Boosting
bPrediction <- prediction(predictions = pred_b, labels = test$clicked)
bPerformance <- performance(bPrediction, measure = "tpr", x.measure = "fpr")
plot(bPerformance, col="green", lwd=2,main=("ROC Curve for Boosting Model"))

# ---------------------------------------------------------------------------------------------------------------------------------------------------
savehistory(file="20_11_27-11.history")
# ----------------------------------------------------E N D----------------------------------------------------------------
