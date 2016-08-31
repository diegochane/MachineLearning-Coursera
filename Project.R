# Download & read data
url.training="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url.test="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(url.training,destfile="training.csv", method = "curl")
download.file(url.test, destfile = "test.csv", method="curl")
training=read.csv("training.csv", na.strings = c("#DIV/0!", "", "NA"))
test=read.csv("test.csv", na.strings = c("#DIV/0!", "", "NA"))
training[,1]=NULL
test[,1]=NULL

# Divide training & validation sets
library (caret)
sample=createDataPartition(y=training$classe, p=0.6, list=F)
validation=training[-sample,]
training=training[sample,]

# Cleanup non-zero variance variables
nearzero=nearZeroVar(training, saveMetrics =F)
training=training[,-nearzero]
validation=validation[,-nearzero]
test=test[,-nearzero]

# Cleanup variables with too many NA's
na.var=sapply(training, function(y) sum(length(which(is.na(y)))))
na.var=data.frame(na.var)
na.var$total=sapply(training,function (y) length(y))
na.var$perc=na.var$na.var/na.var$total

na.var.col=which(na.var$perc>0.3)
training=training[,-na.var.col]
validation=validation[,-na.var.col]
test=test[,-na.var.col]

test[,58]=NULL

# Train models
## Decision trees
library(rpart)
tree.model=rpart(classe ~ ., data=training, method="class")
tree.pred=predict(tree.model,newdata = validation, type="class")
tree.matrix=confusionMatrix(tree.pred,validation$classe)
tree.matrix
plot(tree.matrix$table, main="Decision Tree Confusion Matrix")

## Random Forests
random.model=train(classe~., data=training, method="rf", prox=T, do.trace=T,ntree=300)
random.pred=predict(random.model, newdata=validation)
random.matrix=confusionMatrix(random.pred,validation$classe)
random.matrix
plot(random.matrix$table, main="Random Forest Confusion Matrix")

## Boosting
boosting.model=train(classe~., data=training, method="gbm", verbose=F)
boosting.pred=predict(boosting.model, newdata=validation)
boosting.matrix=confusionMatrix(boosting.pred,validation$classe)
boosting.matrix
plot(boosting.matrix$table, main="Boosting Confusion Matrix")

# Create Predictions
final.pred=predict(random.model,newdata=test)
data.frame(final.pred)
write.csv(final.pred, file="predictions.csv", row.names=F)
