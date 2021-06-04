rm(list=ls())

library("randomForest")
library("caTools")
library ("ROCR")
library("ggplot2")
library(dplyr)
library(caret)

setwd("~/Documents/VIMSS/ontology/KG-Hub/KG-Microbe/")
node_data <- read.csv("./link_predict_kgmicrobe_shape_80/kgmicrobe_nodes__positive80.tsv", sep="\t",header=T)
edge_data <- read.csv("./link_predict_kgmicrobe_shape_80/kgmicrobe_edges__positive80.tsv", sep="\t",header=T)

dim(node_data)
head(node_data) 

node_labels <- as.character(node_data$id)


#virus_host_positive <- read.csv("./link_predict/virus_host__subtract.tsv", row.names=1)
virus_host_positive <- read.csv("./link_predict_kgmicrobe_shape_80/taxa_trait__subtract.tsv", row.names=1, header=TRUE, sep=",")
virus_host_positive_labels <- read.csv("./link_predict_kgmicrobe_shape_80/taxa_trait__subtract_labels.tsv")
dim(virus_host_positive)
length(virus_host_positive_labels)
head(virus_host_positive)
dimpos <- dim(virus_host_positive)
dimpos

shapedata_split <- strsplit(row.names(virus_host_positive), "__")
shapes <- unlist(shapedata_split)[2*(1:length(shapedata_split))-1]

#virus_host_negative <- read.csv("./link_predict/virus_host_NEGATIVE__subtract.tsv", row.names=1)
virus_host_negative <- read.csv("./link_predict_kgmicrobe_shape_80/taxa_trait_NEGATIVE__subtract.tsv", row.names=1, header=TRUE, sep=",")
virus_host_negative_labels <- read.csv("./link_predict_kgmicrobe_shape_80/taxa_trait_NEGATIVE__subtract_labels.tsv")
dim(virus_host_negative)
sum(is.na(virus_host_negative))
dim(virus_host_negative_labels)
head(virus_host_negative)
dimneg <- dim(virus_host_negative)
head(virus_host_negative)
dimneg
row.names(virus_host_negative) <- virus_host_negative_labels[,1]


virus_host_new <- read.csv("./link_predict_kgmicrobe_shape_80/taxa_trait_NEW__subtract_80.tsv", row.names=1, header=TRUE, sep=",")
virus_host_new_labels <- read.csv("./link_predict_kgmicrobe_shape_80/taxa_trait_NEW_subtract_labels_80.tsv")
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

total_train_data <- virus_host_positive#rbind(virus_host_positive, virus_host_negative)
dim(total_train_data)
head(total_train_data)
sum(is.na(total_train_data))

#pos_neg_label <- c(rep(1, dimpos[1]), rep(0, dimneg[1]))
pos_neg_label <- c(shapes)#, rep("",  dimneg[1]))
names(pos_neg_label) <- "pos_neg"
total_train_data <- cbind(total_train_data, pos_neg_label)



virus_host_test <- read.csv("./link_predict_kgmicrobe_shape/kgmicrobe_test.txt", row.names=1, header=TRUE, sep="\t")
table(virus_host_test$pos_neg_label)
  #virus_host_test_labels <- read.csv("./link_predict_kgmicrobe_shape/IMGVR_sample_extra_test_edges_labels.txt")
dim(virus_host_test)
head(virus_host_test)
#sum(is.na(virus_host_test_labels))
dim(virus_host_test_labels)
#head(virus_host_negative)
dimtest <- dim(virus_host_test)
dimtest
#head(virus_host_negative)
#row.names(virus_host_test) <- virus_host_test_labels[,1]
shapedata2_split <- strsplit(row.names(virus_host_test), "__")
length(shapedata2_split)
shapes2 <- unlist(shapedata2_split)[2*(1:dimtest[1])-1]
#shapes2_index <- match(shapes2, shapes)
length(shapes2)
virus_host_test_orig <- virus_host_test
dim(virus_host_test_orig)

virus_host_test <- virus_host_test[virus_host_test$pos_neg_label == 1,]
dimtest <- dim(virus_host_test)
dimtest
virus_host_test <- virus_host_test[,-dimtest[2]]
head(virus_host_test)
dimtest <- dim(virus_host_test)
dimtest
shapes2_true <- shapes2[virus_host_test_orig$pos_neg_label == 1]


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
head(train)
pos_neg_label_train <- train[ , dim(train)[2]]
train <- train[ , -dim(train)[2]]
head(train)
dimtrain <- dim(train)
#train <- cbind(train, shapes)
#colnames(train)[dim(train)[2]] <-"pos_neg_label"  

#virus_host_test$pos_neg_label <- shapes2
#test <- data.matrix(virus_host_test)
#names(shapes2_true) <- "pos_neg_label"
test <- cbind(virus_host_test, shapes2_true)
colnames(test)[dim(test)[2]] <-"pos_neg_label"  
head(test)

colnames(train)
sum(is.na(train))

###convert response variable to factor for classification
###otherwise random forest regression model
#rf_classifier <- randomForest(as.factor(pos_neg_label_train) ~ ., data=train, ntree=200, importance=TRUE,do.trace=TRUE)#mtry=sqrt(dimtrain[1]),
rf_classifier <- randomForest(as.factor(total_train_data$pos_neg_label) ~ ., data=total_train_data[,-dim(total_train_data)[2]], ntree=200, importance=TRUE,do.trace=TRUE)
#rf_classifier <- randomForest(train[,'pos_neg_label'], data=train[, 1:(dimtrain[1]-1)], ntree=10, mtry=sqrt(dimtrain[1]), importance=TRUE,do.trace=TRUE)

rf_classifier
varImpPlot(rf_classifier)

prediction_for_table_train <- predict(rf_classifier,total_train_data[,-dim(total_train_data)[2]])

cm <- confusionMatrix(factor(prediction_for_table_train), factor(total_train_data[,dim(total_train_data)[2]]), dnn = c("Prediction", "Reference"))

ggplot(as.data.frame(cm$table), aes(Prediction,sort(Reference,decreasing = T), fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#predict_train <- predict(rf_classifier,train)

prediction_for_table <- predict(rf_classifier,test[,-dim(test)[2]])
length(prediction_for_table)
head(test)
dim(test)
class(test)
colnames(test)
table_results <- table(observed=test[,dim(test)[2]],predicted=prediction_for_table)#pos_neg_label
table_results
dim(table_results)

heatmap.2(log(table_results+0.001,2), trace="none", dendrogram = "none")

cm <- confusionMatrix(factor(prediction_for_table), factor(test[,dim(test)[2]]), dnn = c("Prediction", "Reference"))

ggplot(as.data.frame(cm$table), aes(Prediction,sort(Reference,decreasing = T), fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



colnames(table_results) <- row.names(table_results)



save.image(file='randomForest_v0.01__kg_microbe_v4')

