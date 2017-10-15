library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(knitr)
library(e1071)


###загрузка тренировочных данных и результата по указанным в описании ссылкам 
set.seed(12345)
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
###исключаем из данных нечисленные значения
training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))

######################################################
### dictionary of variables
### myTR - my Training
### myTS - myTS
### TEMP - trainTEMP
### prognosA1 - prediction A1
### prognosB1 - prediction B1
### modelfita1 - model Fit A1
### modelfitb1 - model Fit A1
### gbmAcT -  gbm Accuracy Test
### Trees - variable for plot Decision Trees
### RF - variable for plot Random Forests
######################################################

###Необходимо разделить полученные данные на 2 части, для проведения теста и тренировки, размер определяет р=0,6
inTrain <- createDataPartition(training$classe, p=0.6, list=FALSE)
myTR <- training[inTrain, ]
myTS <- training[-inTrain, ]
###проверим результат разделения, размер выборки:
dim(myTR); dim(myTS)

###########
###Необходимо удалить нулевые переменные функцией nearZeroVar 
###из тренировочных данных
nzv <- nearZeroVar(myTR, saveMetrics=TRUE)
myTR <- myTR[,nzv$nzv==FALSE]
###из тестовых данных
nzv<- nearZeroVar(myTS,saveMetrics=TRUE)
myTS <- myTS[,nzv$nzv==FALSE]
############


###
myTR <- myTR[c(-1)]
#####


######
TEMP <- myTR
for(i in 1:length(myTR)) {
    if( sum( is.na( myTR[, i] ) ) /nrow(myTR) >= .7) {
        for(j in 1:length(TEMP)) {
            if( length( grep(names(myTR[i]), names(TEMP)[j]) ) == 1)  {
                TEMP <- TEMP[ , -j]
            }   
        } 
    }
}

### Возвращаем обратно имя переменной 
myTR <- TEMP
rm(TEMP)



#####
clean1 <- colnames(myTR)
clean2 <- colnames(myTR[, -58])  # удаляем столбец classe
myTS <- myTS[clean1]         # разрешить только переменные myTS которое также есть в  myTR
testing <- testing[clean2]             # азрешить только переменные testing которое также есть в myTR

dim(myTS)

#####


### Поручить данные в одном типе
for (i in 1:length(testing) ) {
    for(j in 1:length(myTR)) {
        if( length( grep(names(myTR[i]), names(testing)[j]) ) == 1)  {
            class(testing[j]) <- class(myTR[i])
        }      
    }      
}

### Чтобы получить тот же класс между testing и myTR 
testing <- rbind(myTR[2, -58] , testing)
testing <- testing[-1,]



######
### Используем модель прогноза "Случайный лес" 
set.seed(12345)
modelfita1 <- rpart(classe ~ ., data=myTR, method="class")
fancyRpartPlot(modelfita1)
######

#####
prognosA1 <- predict(modelfita1, myTS, type = "class")
Trees <- confusionMatrix(prognosA1, myTS$classe)
Trees
#####

plot(Trees$table, col = Trees$byClass, main = paste("Дерево решений: Точность =", round(Trees$overall['Accuracy'], 4)))

### Используем модель прогноза "Случайный лес" 
set.seed(12345)
modelfitb1 <- randomForest(classe ~ ., data=myTR)
prognosB1 <- predict(modelfitb1, myTS, type = "class")
RF <- confusionMatrix(prognosB1, myTS$classe)
RF

plot(modelfitb1)


plot(RF$table, col = Trees$byClass, main = paste("Случайный лес решений: Точность =", round(RF$overall['Accuracy'], 4)))

### Используем модель прогноза "Generalized Boosted Regression" из лекций GBR
set.seed(12345)
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 1)

gbmFit1 <- train(classe ~ ., data=myTR, method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE)


gbmFinMod1 <- gbmFit1$finalModel

gbmPredTest <- predict(gbmFit1, newdata=myTS)
gbmAcT <- confusionMatrix(gbmPredTest, myTS$classe)
gbmAcT



plot(gbmFit1, ylim=c(0.9, 1))


predictionB2 <- predict(modelfitb1, testing, type = "class")

predictionB2

# Запишем тестовый файл для отправки кода