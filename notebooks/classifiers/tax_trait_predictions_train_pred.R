rm(list=ls())

set.seed(12345)

library("randomForest")
library("caTools")
library ("ROCR")


setwd("~/Documents/VIMSS/ontology/KG-Hub/KG-Microbe/")
node_data <- read.csv("./20210119/merged-kg/merged-kg_edges.tsv", sep="\t",header=T)
edge_data <- read.csv("./20210119/merged-kg/merged-kg_nodes.tsv", sep="\t",header=T)

dim(node_data)
head(node_data) 

node_labels <- as.character(node_data$id)


#virus_host_positive <- read.csv("./link_predict/virus_host__subtract.tsv", row.names=1)
virus_host_positive <- read.csv("./link_predict_kgmicrobe_shape/train_edges__subtract.tsv", row.names=1, header=TRUE, sep=",")
virus_host_positive_labels <- read.csv("./link_predict_kgmicrobe_shape/train_edges__subtract_labels.tsv")
dim(virus_host_positive)
length(virus_host_positive_labels)
head(virus_host_positive)
dimpos <- dim(virus_host_positive)
dimpos


#virus_host_negative <- read.csv("./link_predict/virus_host_NEGATIVE__subtract.tsv", row.names=1)
virus_host_negative <- read.csv("./link_predict_kgmicrobe_shape/train_edges_NEGATIVE__subtract.tsv", row.names=1, header=TRUE, sep=",")
virus_host_negative_labels <- read.csv("./link_predict_kgmicrobe_shape/train_edges_NEGATIVE__subtract_labels.tsv")
dim(virus_host_negative)
sum(is.na(virus_host_negative))
dim(virus_host_negative_labels)
head(virus_host_negative)
dimneg <- dim(virus_host_negative)
head(virus_host_negative)
dimneg
row.names(virus_host_negative) <- virus_host_negative_labels[,1]


virus_host_new <- read.csv("./link_predict_kgmicrobe_shape/train_edges_NEW__subtract.tsv", row.names=1, header=TRUE, sep=",")
virus_host_new_labels <- read.csv("./link_predict_kgmicrobe_shape/train_edges_NEW_subtract_labels.tsv")
dim(virus_host_new)
sum(is.na(virus_host_new))
dim(virus_host_new_labels)
head(virus_host_new)
dimnegnew <- dim(virus_host_new)
head(virus_host_new)
dimnegnew
row.names(virus_host_new) <- virus_host_new_labels[,1]

#print("trimming positive because FEWER NEGATIVE!!!")
#virus_host_positive <- virus_host_positive[1:dimneg[1],]
class(virus_host_positive_labels)
virus_host_positive_labels <- virus_host_positive_labels[1:dimneg[1],]
dim(virus_host_positive)
dim(virus_host_positive_labels)
length(virus_host_positive_labels)
#row.names(virus_host_positive) <- virus_host_positive_labels
dimpos <- dim(virus_host_positive)
dimpos

sum(is.na(virus_host_positive))
sum(is.na(virus_host_negative))

total_train_data <- rbind(virus_host_positive, virus_host_negative)
dim(total_train_data)
head(total_train_data)
sum(is.na(total_train_data))

pos_neg_label <- c(rep(1, dimpos[1]), rep(0, dimneg[1]))
names(pos_neg_label) <- "pos_neg"
total_train_data <- cbind(total_train_data, pos_neg_label)


###shuffle rows here?

done = FALSE
if(!done) {
  sample <- sample.split(total_train_data$pos_neg, SplitRatio = 0.8)
  write.table(sample, file="kgmicrobe_sample.txt", sep="\t")
  train <- subset(total_train_data, sample == TRUE)
  class(train)
  test <- subset(total_train_data, sample == FALSE)
  
  write.table(train, file="kgmicrobe_train.txt", sep="\t")
  write.table(test, file="kgmicrobe_test.txt", sep="\t")
} else {
  sample <- read.csv("kgmicrobe_sample.txt", row.names=1, header=TRUE, sep="\t")
  train <- read.csv("kgmicrobe_train.txt", row.names=1, header=TRUE, sep="\t")
  test <- read.csv("kgmicrobe_test.txt", row.names=1, header=TRUE, sep="\t")
}
dim(sample)
dim(train)
dim(test)

print(row.names(test))

done = FALSE
if(!done) {
  write.table(row.names(test), file="kgmicrobe_edges_labels.txt", sep="\t")
}

#head(test)

#colnames(total_train_data)

train <- data.matrix(train)
dimtrain <- dim(train)
test <- data.matrix(test)

colnames(train)
sum(is.na(train))

###convert response variable to factor for classification
###otherwise random forest regression model
rf_classifier <- randomForest(as.factor(pos_neg_label) ~ ., data=train, ntree=200, importance=TRUE,do.trace=TRUE)#mtry=sqrt(dimtrain[1]),
#rf_classifier <- randomForest(train[,'pos_neg_label'], data=train[, 1:(dimtrain[1]-1)], ntree=10, mtry=sqrt(dimtrain[1]), importance=TRUE,do.trace=TRUE)

rf_classifier
varImpPlot(rf_classifier)

#predict_train <- predict(rf_classifier,train)

prediction_for_table <- predict(rf_classifier,test)
length(prediction_for_table)
dim(test)
class(test)
colnames(test)
table(observed=test[,101],predicted=prediction_for_table)#pos_neg_label


####


y <- test[,'pos_neg_label']#... # logical array of positive / negative cases
predictions <- prediction_for_table # array of predictions
class(predictions)
pred <- prediction(as.numeric(as.character(predictions)), y);

# Recall-Precision curve             
RP.perf <- performance(pred, "prec", "rec");

pdf(file = "virus_host_RF_recall_precision.pdf", width = 11, height = 8)
plot (RP.perf);
dev.off()

# ROC curve
ROC.perf <- performance(pred, "tpr", "fpr");
pdf(file = "virus_host_RF_ROC.pdf", width = 11, height = 8)
plot (ROC.perf);
dev.off()


# ROC area under the curve
auc.tmp <- performance(pred,"auc");
auc <- as.numeric(auc.tmp@y.values)


dim(virus_host_negative)

prediction_for_table2 <- predict(rf_classifier,virus_host_negative)
length(prediction_for_table2)
prediction_for_table2[which(prediction_for_table2 == 1)]
write.table(prediction_for_table2, file="virus_host_predict__new_links_on_negtrain.txt ", sep="\t")


prediction_for_table3 <- predict(rf_classifier,virus_host_positive)
length(prediction_for_table3)
prediction_for_table3[which(prediction_for_table3 == 1)]
write.table(prediction_for_table3, file="virus_host_predict__new_links_on_postrain.txt ", sep="\t")


prediction_for_table4 <- predict(rf_classifier,virus_host_new)
length(prediction_for_table4)
prediction_for_table4[which(prediction_for_table4 == 1)]
sum(prediction_for_table4 == 1)/length(prediction_for_table4)
write.table(prediction_for_table4, file="virus_host_predict__new_links_on_new.txt ", sep="\t")



save.image(file='randomForest_v0.01__IMGVR_sample_extra_v4')

