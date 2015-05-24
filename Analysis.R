##Downloading the files
URL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(URL, destfile = "./pml-training.csv")

URL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(URL, destfile = "./pml-testing.csv")


##Creating Dataframe from the downloaded csv files

train_data <- read.csv(file = "./pml-training.csv", na.strings=c("", "NA"))
test_data <- read.csv(file = "./pml-testing.csv", na.strings=c("", "NA"))


dim(train_data)
dim(test_data)

View(train_data)


##Removing the columns with 'NA's

train_data<-train_data[,colSums(is.na(train_data)) == 0]
test_data <-test_data[,colSums(is.na(test_data)) == 0]


##Removing irrelevant columns -  user_name, raw_timestamp_part_1, raw_timestamp_part_,2 cvtd_timestamp, new_window, and  num_window (columns 1 to 7)
train_data   <-train_data[,-c(1:7)]
test_data <-test_data[,-c(1:7)]


head(train_data)

##A Look into the data
plot(x = train_data$classe,main = "Classe within the Training data set")

install.packages("doParallel")
library(randomForest)
##Partion the Dataset
inTrain <- createDataPartition( y= train_data$classe, p = 0.75 , list = FALSE)
trainingSet <- train_data[inTrain,]
testingSet <- train_data[-inTrain,]



cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

trcntrl <- trainControl(classProbs=TRUE,
                     savePredictions=TRUE,
                     allowParallel=TRUE)


trainingModel <- train(classe ~ ., data=trainingSet, method="rf",trControl = trcntrl)


trainingModel



predictedfromtrains <- predict(trainingModel, trainingSet)
confusionMatrix(predictedfromtrains, trainingSet$classe)

varImp(trainingModel)


trainingModel$finalModel


save(trainingModel, file="trainingModel.RData")



load(file="trainingModel.RData", verbose=TRUE)


predictedfromtestV <- predict(trainingModel, testingSet)


confusionMatrix(predictedfromtestV, testingSet$classe)


predictedV <- predict(trainingModel, test_data)
test_data <- cbind(predictedV , test_data)
subset(test_data, select=names(test_data)[grep("belt|[^(fore)]arm|dumbbell|forearm", names(test_data), invert=TRUE)])



pml_write_files = function(x){
    n = length(x)
    path <- "./answers"
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=file.path(path, filename),quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}


pml_write_files(predictedV)

