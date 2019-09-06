getwd()
setwd('Desktop/RPI/Sem 2/DA/IMDB/')

#import final Data
library(readr)
movie_ratings <- read_csv("final_data.csv", col_types = cols(genreCluster = col_factor(levels = c("1", "2", "3", "4")), X1 = col_skip(),isAnyOriginal = col_logical()))
colnames(movie_ratings)[11]<-"genreCluster"

#EDA
summary(movie_ratings)
#distribution of runtimes
hist(movie_ratings$runtimeMinutes)
boxplot(movie_ratings$runtimeMinutes, main = "RuntimeMinutes", xlab="runtime minutes")
#Plotting different plots
library(ggplot2)
qplot(movie_ratings$runtimeMinutes,movie_ratings$movieRating,color=movie_ratings$isAnyOriginal)
#not very useful
qplot(movie_ratings$runtimeMinutes,movie_ratings$movieRating,color=movie_ratings$genreCluster)

qplot(movie_ratings$startYear,movie_ratings$movieRating,color=movie_ratings$isAnyOriginal)
qplot(movie_ratings$startYear,movie_ratings$movieRating,color=movie_ratings$genreCluster)
#not very useful

qplot(movie_ratings$numVotes,movie_ratings$movieRating,color=movie_ratings$isAnyOriginal,xlim = c(1,10000), xlab = "Number of Votes", ylab = "Movie Rating",main="Number of Votes vs Movie Rating")+labs(col="isOriginal")
qplot(movie_ratings$numVotes,movie_ratings$movieRating,color=movie_ratings$genreCluster,xlab = "Number of Votes", ylab = "Movie Rating",main="Number of Votes vs Movie Rating")+
  scale_color_discrete(labels= c("Drama","Other","Action","Comedy"),name="Genre")+
  theme(legend.text = element_text(size=20),legend.title = element_text(size=25),
        axis.text = element_text(size=15),axis.title = element_text(size=15),plot.title = element_text(size=20))

#kinda useful

qplot(movie_ratings$actorRating,movie_ratings$movieRating)
qplot(movie_ratings$directorRating,movie_ratings$movieRating)
qplot(movie_ratings$genreCluster,movie_ratings$movieRating)

barplot(table(movie_ratings$isAnyOriginal,movie_ratings$numVotes),xlab="Movie Rating",ylab="Frequency",
        main = "Distribution of Movie ratings for Original & Remake Types",
        col=c("#FF99CC","#33FFFF"),legend.text = c("False","True"),args.legend=list(x="topright",title="isOriginal"))

plot(movie_ratings$isAnyOriginal,movie_ratings$numVotes)


#finding cooreleation between variables
# plotting cor-relations
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], method = 'pearson')
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(movie_ratings[,c(3,5:9)])
head(p.mat[, 1:5])
library(corrplot)
corr = cor(movie_ratings[,c(3,5:9)])
corrplot(corr,method = 'circle',type = 'lower',order='alphabet', p.mat = p.mat, sig.level = 0.1,title = 'Correlation between numerical variables',mar=c(0,0,2,3),tl.cex=1.5,cl.cex = 1.5)

plot(movie_ratings[sample(1:97817,1000),c(3,5:10)])

#Test model - simple lm
sapply(movie_ratings, class)
#99% confidence level, 1% confidence interval, population size 97817, sample size should be 14222
test1<- movie_ratings
test1[,c(3,5:8)] <- scale(test1[,c(3,5:8)]) 
#trainingDataIndex <- sample(1:dim(movie_ratings)[1],14222)
trainingDataIndex <- test1$startYear<1995
trainingData <- test1[trainingDataIndex,]
testData <- test1[!trainingDataIndex,]

#Model
model <- lm(movieRating ~ runtimeMinutes + actorRating + directorRating + genreCluster ,data = trainingData) # trainingData
summary(model)
library(ModelMetrics)
cor(predict(model,trainingData),trainingData$movieRating)
cor(predict(model,testData),testData$movieRating)
rmse(trainingData$movieRating,predict(model,trainingData))
rmse(testData$movieRating,predict(model,testData))
plot(model)
plot(predict(model,testData),testData$movieRating)

#Making svm
library(e1071)
svm.model <- svm(movieRating ~ ., data = trainingData, cost = 100, gamma = 0.01,scale = FALSE,kernel = "radial")
svm.pred <- predict(svm.model, trainingData)
crossprod(svm.pred - trainingData$movieRating) / dim(trainingData)[1]
rmse(trainingData$movieRating,svm.pred)

summary(svm.model)
head(svm.pred)
cor(svm.pred,trainingData$movieRating)

library(kernlab)
svm_model <- ksvm(movieRating ~ .,data = trainingData)


## decision tree
library(rpart)
library(rpart.plot)
ratingTree <- rpart(movieRating ~ .,data = trainingData[,c(3,5:11)],method = "anova")
defaultPar = par()
par(xpd = NA)
rpart.plot(ratingTree,uniform = TRUE)
summary(ratingTree)
print(ratingTree)
printcp(ratingTree)
rsq.rpart(ratingTree)

prunedTree <- prune(ratingTree,cp = 0.01000)
rpart.plot(prunedTree)

cor(predict(ratingTree,trainingData),trainingData$movieRating)
cor(predict(ratingTree,testData),testData$movieRating)

library(randomForest)
ratingForest <- randomForest(movieRating ~ .,data = trainingData[,c(3,5:11)],ntree=100)
print(ratingForest)
importance(ratingForest)
varImpPlot(ratingForest)
plot(ratingForest)
cor(predict(ratingForest,trainingData),trainingData$movieRating)
cor(predict(ratingForest,testData),testData$movieRating)


#K fold validation 
# Define training control
set.seed(123) 
library(caret)
train.control <- trainControl(method = "cv", number = 20,repeats = 3)
# Train the model - Best 600 training points
model <- train(movieRating ~., data = trainingData[,c(3,5:8,11)], method = "svmLinear",trControl = train.control)
# Summarize the results
print(model)
cor(predict(model,trainingData),trainingData$movieRating)
cor(predict(model,testData),testData$movieRating)

names(getModelInfo())
