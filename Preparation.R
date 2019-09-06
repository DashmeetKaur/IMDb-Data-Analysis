#Project 
#set directory
getwd()
setwd('Desktop/RPI/Sem 2/DA/IMDB/')

library(readr)
library(dplyr)

#import datasets - name.basics.tsv file
name_basics <- read_delim("name.basics.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)

#Cleaning name_basics file
#removing rows with no profession details
table(is.na(name_basics$primaryProfession))
#1655808 rows - 18% -- too much??
#removing records with missing primary profession
new_name_basics <- name_basics[!(is.na(name_basics$primaryProfession)),]
table(is.na(new_name_basics$primaryProfession))

#Making a new feature that contains list of profession
new_name_basics <- new_name_basics %>% mutate(professionList = strsplit(primaryProfession, split=','))

#Getting unique professions
profession<-unlist(lapply(new_name_basics$professionList, function(y) unlist(y)))
profession <- unique(profession)
t(t(profession))

#Making new features for professions - actor, director and other
new_name_basics$director <- sapply(new_name_basics$professionList, function(y) ifelse("director" %in% unlist(y),1,0))
new_name_basics$actor <- sapply(new_name_basics$professionList, function(y) ifelse("actor" %in% unlist(y) | "actress" %in% unlist(y),1,0))

#fields to keep
keep = c('nconst','primaryName','birthYear','deathYear','knownForTitles','primaryProfession','director','actor')
new_name_basics <- new_name_basics[,keep]

#saving as tsv file
write_tsv(new_name_basics,'name_basics_cleaned.tsv')
#---------------------------------------------------------------------------------------------
#importing other dataset - title.akas.tsv file
title_akas <- read_delim("title.akas.tsv", "\t", escape_double = FALSE, col_types = cols(isOriginalTitle = col_character()), trim_ws = TRUE)
table(title_akas$titleId,title_akas$ordering)

#Checking null valuess
table(is.na(title_akas$region))/dim(title_akas)[1]
#434397 - 11.94% - too much? - Hypothesis don't have anything to do with region; leave it
table(is.na(title_akas$language))/dim(title_akas)[1]
#3368477 - 92.59% - Very high can't do anything related to language. 
table(is.na(title_akas$isOriginalTitle))/dim(title_akas)[1]
#replacing na values in region and originals with "NA"
title_akas[is.na(title_akas$region),]$region <- "NA"
title_akas[is.na(title_akas$isOriginalTitle),]$isOriginalTitle <- "NA"
#Checkiing for duplicate rows
title_akas <- title_akas[!duplicated(title_akas),]
#A duplicate in case something wrong happens
test <- title_akas

##Function to find the region where the movie/tv show was originally made
findRegion = function(regions,originals) {
  region = NA;
  for(i in length(originals)){
    if(originals[i]=='1'){
      region = regions[i];
      break;
    }
  }
  return(region);
}

#Grouping and finding original region, the number of times it was remade and if it is original
test1 <- test %>% 
                  group_by(titleId,add=TRUE) %>% 
                  do(originalRegion = findRegion(.$region,.$isOriginalTitle),
                  remake = length(unique(.$ordering)),isAnyOriginal = ifelse(sum(as.integer(.$isOriginalTitle))>0,1,0)) 

#Finding out how many titles have originalRegion
dim(test1[test1$originalRegion != 'NA',])
## What??? Only 3 rows have original Region's valid value - that's not at all useful
#checking column datatypes
sapply(test1,class)
#Converting remake and isAnyOriginal into integer type
test1$remake <- as.integer(test1$remake)
test1$isAnyOriginal <- as.integer(test1$isAnyOriginal)

##Dropping the originalRegion Column
#fields to keep
keep = c('titleId','remake','isAnyOriginal')
title_akas_cleaned <- test1[,keep]

#saving as tsv file
write_tsv(title_akas_cleaned,'title_akas_cleaned.tsv')

#---------------------------------------------------------------------------------------------

#importing other datasets
title_basics <- read_delim("title.basics.tsv", "\t", escape_double = FALSE, col_types = cols(startYear = col_character()),trim_ws = TRUE)
#unique types of shows
t(t(unique(title_basics$titleType)))
table(is.na(title_basics$titleType))/dim(title_basics)[1]

#Only considering movie, tvMovie, short, tvEpisode and tvSeries
titleType <- c('movie','tvMovie','short','tvEpisode','tvSeries')
title_basics_small <- title_basics[title_basics$titleType %in% titleType,]
#Missing values percentage of genres & runtimeMinutes
table(is.na(title_basics_small$genres))/dim(title_basics_small)[1]
table(is.na(title_basics_small$runtimeMinutes))/dim(title_basics_small)[1]

#removing missing values for genres & runtimes
new_title_basics <- title_basics_small[!(is.na(title_basics_small$genres)),]
table(is.na(new_title_basics$genres))/dim(title_basics_small)[1]
# new_title_basics <- new_title_basics[!(is.na(new_title_basics$runtimeMinutes)),]
# table(is.na(new_title_basics$runtimeMinutes))/dim(title_basics_small)[1]


#Making a new feature that contains list of genre
new_title_basics <- new_title_basics %>% mutate(genreList = strsplit(genres, split=','))

#Getting unique genre
genre<-unlist(lapply(new_title_basics$genreList, function(y) unlist(y)))
genre <- unique(genre)
t(t(genre))

#Making new features for different genres - categorical to binary conversion
for(i in seq(1,length(genre),1)){
  currentGenre <- genre[i]
  new_title_basics[,currentGenre] <- sapply(new_title_basics$genreList, function(y) ifelse(genre[i] %in% unlist(y),1,0))
}

#fields to keep
keep <- c('tconst','titleType','isAdult','startYear','runtimeMinutes')
keep <- c(keep,genre)
keep
title_basics_cleaned <- new_title_basics[,keep]

#saving as tsv file
write_tsv(title_basics_cleaned,'title_basics_cleaned.tsv')

#---------------------------------------------------------------------------------------------

#importing other datasets
title_crew <- read_delim("title.crew.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)
title_episode <- read_delim("title.episode.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)
title_principals <- read_delim("title.principals.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)
title_ratings <- read_delim("title.ratings.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)

#---------------------------------------------------------------------------------------------
#Univariate Analysis
#---------------------------------------------------------------------------------------------
#rating vs runtime
titles <- read_delim("title_basics_cleaned.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)
titleRatings <- read_delim("title.ratings.tsv", "\t", escape_double = FALSE, trim_ws = TRUE) 

#extracting only movie & tvMovie type titles & runtimeMinutes, tconst & titleType variable
titles_movies <- titles[titles$titleType == 'movie'| titles$titleType == 'tvMovie',c("tconst","titleType","runtimeMinutes","startYear")]
# % of data left
dim(titles_movies)[1]/dim(titles)[1]
# 11% - 550K records
#Finding missing values for the runtimeMinutes
table(is.na(titles_movies$runtimeMinutes))/dim(titles_movies)[1]
# 32.7% missing values
#370K records remaining - 67.3% records remaining
titles_movies <- titles_movies[!(is.na(titles_movies$runtimeMinutes)),]
#merging data
movie_ratings  <- merge(titles_movies,titleRatings,by="tconst")
sapply(movie_ratings, class)
#distribution of runtimes
hist(movie_ratings$runtimeMinutes)
boxplot(movie_ratings$runtimeMinutes, main = "RuntimeMinutes", xlab="runtime minutes")
summary(movie_ratings$runtimeMinutes)
sdRuntime <- sd(movie_ratings$runtimeMinutes)
meanRuntime <- mean(movie_ratings$runtimeMinutes)
#meanRuntime + 3*sdRuntime 
#Removing outliers - 3 standard deviations up or down the mean
movie_ratings_new <- movie_ratings[movie_ratings$runtimeMinutes>60 &
                                   movie_ratings$runtimeMinutes<meanRuntime + 3*sdRuntime,]
#distribution of ratings
hist(movie_ratings_new$averageRating)
boxplot(movie_ratings_new$averageRating)
hist(movie_ratings_new$runtimeMinutes)
#taking sample and plotting 
testSample <- sample(dim(movie_ratings_new)[1],0.05*dim(movie_ratings_new)[1])
test <- movie_ratings_new[testSample,]
#plotting rating vs runtime
plot(test$averageRating,test$runtimeMinutes)
unique(movie_ratings_new$startYear)
#Partitioning data on the basis of start Year
trainingData <- movie_ratings_new[movie_ratings_new$startYear<2014,]
dim(trainingData)[1]/dim(movie_ratings_new)[1]
# 80% of data in training data now
testingData <- setdiff(movie_ratings_new,trainingData)

cor(movie_ratings_new$runtimeMinutes,movie_ratings_new$averageRating)

model <- lm(averageRating ~ 0+runtimeMinutes,data=trainingData)
model
summary(model)
#plot(model)
head(pred)
head(testingData$averageRating)

pred <- predict(model,testingData)
cor(testingData$averageRating,pred)

plot(movie_ratings_new$runtimeMinutes,movie_ratings_new$averageRating)
plot(trainingData$runtimeMinutes,trainingData$averageRating)
abline(model$coefficients,col=3)
#---------------------------------------------------------------------------------------------
#ratings vs genre
titles <- read_delim("title_basics_cleaned.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)
titleRatings <- read_delim("title.ratings.tsv", "\t", escape_double = FALSE, trim_ws = TRUE) 
#extracting only movie & tvMovie type titles & runtimeMinutes, tconst & titleType variable
titles_movies <- titles[titles$titleType == 'movie'| titles$titleType == 'tvMovie',]
# % of data left
dim(titles_movies)[1]/dim(titles)[1]
# 11% - 550K records
#Finding missing values for the runtimeMinutes
table(is.na(titles_movies$runtimeMinutes))/dim(titles_movies)[1]
# 32.7% missing values
#370K records remaining - 67.3% records remaining
titles_movies <- titles_movies[!(is.na(titles_movies$runtimeMinutes)),]
#merging data
movie_ratings  <- merge(titles_movies,titleRatings,by="tconst")
#Checking for missing values
table(is.na(movie_ratings))
sapply(movie_ratings,function(x) table(is.na(x)))
#Start year has 6 missing values - drop those columns
movie_ratings <- movie_ratings[!(is.na(movie_ratings$startYear)),]

sapply(movie_ratings, class)
#Converting genres from numeric to factors
movie_ratings[,c(3,6:33)]<-sapply(movie_ratings[,c(3,6:33)],factor)
sapply(movie_ratings, class)

#distribution of runtimes
hist(movie_ratings$runtimeMinutes)
boxplot(movie_ratings$runtimeMinutes, main = "RuntimeMinutes", xlab="runtime minutes")
summary(movie_ratings$runtimeMinutes)
sdRuntime <- sd(movie_ratings$runtimeMinutes)
meanRuntime <- mean(movie_ratings$runtimeMinutes)
#meanRuntime + 3*sdRuntime 
#Removing outliers - 3 standard deviations up or down the mean
movie_ratings_new <- movie_ratings[movie_ratings$runtimeMinutes>60 &
                                     movie_ratings$runtimeMinutes<meanRuntime + 3*sdRuntime,]
summary(movie_ratings_new)
movie_ratings_new[,6:33] <- lapply(movie_ratings_new[,6:33],factor)

#Partitioning data on the basis of start Year
trainingData <- movie_ratings_new[movie_ratings_new$startYear<2014,]
dim(trainingData)[1]/dim(movie_ratings_new)[1]
# 80% of data in training data now
testingData <- setdiff(movie_ratings_new,trainingData)

#without PCA - decision tree
library(rpart)
library(rpart.plot)
ratingTree <- rpart(averageRating ~ .,data = trainingData[,c(6:34)],method = "anova")
defaultPar = par()
par(xpd = NA)
rpart.plot(ratingTree,uniform = TRUE)
summary(ratingTree)
print(ratingTree)
printcp(ratingTree)
rsq.rpart(ratingTree)
prunedTree <- prune(ratingTree,cp = 0.01000)
rpart.plot(prunedTree)
pred <- predict(ratingTree,testingData[,c(6:33)])
head(pred)
cor(pred,testingData$averageRating)

library(randomForest)
ratingForest <- randomForest(averageRating ~ .,data = data.frame(trainingData[,c(6:34)]),ntree=100)
print(ratingForest)
importance(ratingForest)
varImpPlot(ratingForest)
plot(ratingForest)
pred <- predict(ratingForest,data.frame(testingData[,c(6:33)]))
head(pred)
head(testingData$averageRating)
cor(pred,testingData$averageRating)

#with single tree - correlation 0.45 & with random Forest - correlation 0.48
#not much difference - single tree should work

#with PCA - Not desriable - cuz categories are binomial type.
colnames(movie_ratings_new)
movie_genres <- movie_ratings_new[,c(4,6:34)]
colnames(movie_genres)
movie_genres <- sapply(movie_genres,as.numeric)
movie_genresPCA <- prcomp(movie_genres[,-30])

summary(movie_genresPCA)
pcaData <- data.frame(cbind(movie_genresPCA$x[,c(1,2)],movie_genres[,c('startYear','averageRating')]))
#partitioning data into training & testing
trainPCAdata <- pcaData[pcaData$startYear < 2014,]
testPCAdata <- setdiff(pcaData,trainPCAdata)
#linear regression
model1 <- lm(averageRating ~ .,data = trainPCAdata[,-3])
summary(model1)
pred <- predict(model1,testPCAdata[,1:2])
cor(pred,testPCAdata$averageRating)
### - 0.061

##CLuster categories
sort(unique(movie_ratings_new$startYear))
movie_ratings_new[,6:33] <- lapply(movie_ratings_new[,6:33],factor)
head(movie_ratings_new[,6:33])
sampleSize <- sample(1:205203,2400)
genres <- movie_ratings_new[movie_ratings_new$startYear>=1980,c(6,8:33)]
genres1 <- movie_ratings_new[movie_ratings_new$startYear>=1980,c(1,4:6,8:34)]
genres <- data.frame(lapply(genres,factor))
sapply(genres,class)
#Kmodes algorithm - elbow plot
library(klaR)
wss<-mean(kmodes(genres,1)$withindiff)
for (i in 2:10) wss[i] <- mean(kmodes(genres,i)$withindiff)
plot(1:10, wss, type="b", xlab="Number of Clusters",ylab="Mean of simple matching distance for each cluster",main="Elbow Method for Genres Clustering")
wss
#Kmodes for optimal value of k
k = kmodes(genres,4) 
mean(k$withindiff)
sum(k$withindiff)
t(k$modes)
k$size


genres1 <- cbind(genres1,k$cluster)
#renaming column
colnames(genres1)[32] <- "Cluster"
#writing to file
write.csv(genres1,"Genre_cluster.csv")

barplot(table(genres1$`k$cluster`,genres1$averageRating))
table(genres1$`k$cluster`,genres1$averageRating)
plot(genres1$startYear,genres1$averageRating)

#---------------------------------------------------------------------------------------------
#Calculating Director and Actor popularity - 

title_principals <- read_delim("title.principals.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)
head(title_principals)
unique(title_principals$category)
#Taking only director, actor & actress from category
actors_df <- title_principals[title_principals$category %in% c("actor","actress"),c("tconst","nconst","category")]
head(actors_df)
directors_df <- title_principals[title_principals$category == "director",c("tconst","nconst","category")]
head(directors_df)

#checking missing values
table(is.na(directors_df))
table(is.na(actors_df))

#checking and Removing duplicates
directors_df <- directors_df[!duplicated(directors_df),]
actors_df <- actors_df[!duplicated(actors_df),]

##merging with movies
#Getting ratings for movie &tvmovie type titles
titles <- read_delim("title_basics_cleaned.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)
titleRatings <- read_delim("title.ratings.tsv", "\t", escape_double = FALSE, trim_ws = TRUE) 
#extracting only movie & tvMovie type titles & runtimeMinutes, tconst & titleType variable
titles_movies <- titles[titles$titleType == 'movie'| titles$titleType == 'tvMovie',c("tconst","titleType","startYear","runtimeMinutes")]
# % of data left
dim(titles_movies)[1]/dim(titles)[1]
# 11% - 550K records
#Finding missing values for the runtimeMinutes
table(is.na(titles_movies$runtimeMinutes))/dim(titles_movies)[1]
# 32.7% missing values
#370K records remaining - 67.3% records remaining
titles_movies <- titles_movies[!(is.na(titles_movies$runtimeMinutes)),]
#merging data
movie_ratings  <- merge(titles_movies,titleRatings,by="tconst")
#Checking for missing values
table(is.na(movie_ratings))
sapply(movie_ratings,function(x) table(is.na(x)))

sdRuntime <- sd(movie_ratings$runtimeMinutes)
meanRuntime <- mean(movie_ratings$runtimeMinutes)
#meanRuntime + 3*sdRuntime 
#Removing outliers - 3 standard deviations up or down the mean
movie_ratings_new <- movie_ratings[movie_ratings$runtimeMinutes>60 &
                                     movie_ratings$runtimeMinutes<meanRuntime + 3*sdRuntime,]
summary(movie_ratings_new)

movie_ratings_new <- movie_ratings_new[,c("tconst","titleType","startYear","averageRating")]

#merging with directors data frame
test <- merge(movie_ratings_new,directors_df,by="tconst")

#Partitioning data on the basis of start Year
trainingData <- test[test$startYear<2014,]
dim(trainingData)[1]/dim(test)[1]
# 80% of data in training data now
testingData <- setdiff(test,trainingData)

#getting directors' rating 
director_rating <- trainingData %>% group_by(nconst) %>% summarise(rating = median(averageRating))
write_csv(director_rating,"director_rating.csv")

#getting actor's rating
#merging with directors data frame
test1 <- merge(movie_ratings_new,actors_df,by="tconst")

#Partitioning data on the basis of start Year
trainingData1 <- test1[test1$startYear<2014,]
dim(trainingData1)[1]/dim(test1)[1]
# 80% of data in training data now
testingData1 <- setdiff(test1,trainingData1)

actor_rating <- trainingData1 %>% group_by(nconst) %>% summarise(rating = median(averageRating))
write_csv(actor_rating,"actor_rating.csv")

#merging director rating with training data
newTrainigData <- merge(director_rating,trainingData)
newTestingData <- merge(director_rating,testingData)
#removing missing value from training data
newTrainigData <- newTrainigData[!is.na(newTrainigData$averageRating),]
##testing with linear model - rating vs director rating
model <- lm(averageRating ~ rating, data = newTrainigData)
summary(model)
length(model$fitted.values)
length(newTrainigData$averageRating)
cor(model$fitted.values,newTrainigData$averageRating)
#predicting for future values
pred <- predict(model,newTestingData)
cor(pred,newTestingData$averageRating)
head(pred)
head(newTestingData$averageRating)
plot(trainSample$rating,trainSample$averageRating)
abline(model$coefficients,col=3)
rmse(newTestingData$averageRating,pred)

#Making svm
library(e1071)
library(ModelMetrics)
trainSample <- newTrainigData[sample(dim(newTrainigData)[1],9200),]
svm.model <- svm(averageRating ~ rating, data = trainSample, cost = 100, gamma = 0.01,scale = FALSE,kernel = "polynomial")
newTestingData1 <- newTestingData[!(is.na(newTestingData$startYear)),]
svm.pred <- predict(svm.model, trainSample)
crossprod(svm.pred - newTestingData1$averageRating) / dim(newTestingData1)[1]

rmse(newTestingData1$averageRating,svm.pred)

summary(svm.model)
head(svm.pred)
cor(svm.pred,newTestingData1$averageRating)
plot(newTestingData1$rating,newTestingData1$averageRating)
points(newTestingData1$rating,svm.pred,col="blue")
head(svm.pred)
plot(data = trainSample,averageRating ~ rating)
points(trainSample$rating,svm.pred,col=2)

##Tuning svm
tune.out <- tune(svm,data=trainSample,averageRating~rating,kernel="polynomial",ranges = list(cost = c(0.001,0.1,10,100)),scale=FALSE)
summary(tune.out)


####For actor
#merging director rating with training data
newTrainigData <- merge(actor_rating,trainingData)
newTestingData <- merge(actor_rating,testingData)
#removing missing value from training data
newTrainigData <- newTrainigData[!is.na(newTrainigData$averageRating),]
##testing with linear model - rating vs director rating
model <- lm(averageRating ~ rating, data = newTrainigData)
summary(model)
length(model$fitted.values)
length(newTrainigData$averageRating)
cor(model$fitted.values,newTrainigData$averageRating)
#predicting for future values
pred <- predict(model,newTestingData)
cor(pred,newTestingData$averageRating)
head(pred)
head(newTestingData$averageRating)
plot(trainSample$rating,trainSample$averageRating)
abline(model$coefficients,col=3)
rmse(newTestingData$averageRating,pred)

#Making svm
library(e1071)
library(ModelMetrics)
trainSample <- newTrainigData[sample(dim(newTrainigData)[1],9200),]
svm.model <- svm(averageRating ~ rating, data = trainSample, cost = 100, gamma = 0.01,scale = FALSE,kernel = "polynomial")
newTestingData <- newTestingData[!(is.na(newTestingData$startYear)),]
svm.pred <- predict(svm.model, trainSample)
crossprod(svm.pred - newTestingData$averageRating) / dim(newTestingData)[1]
rmse(newTestingData$averageRating,svm.pred)

summary(svm.model)
head(svm.pred)
cor(svm.pred,newTestingData$averageRating)
plot(newTestingData$rating,newTestingData$averageRating)
points(newTestingData$rating,svm.pred,col="blue")
head(svm.pred)
plot(data = trainSample,averageRating ~ rating)
points(trainSample$rating,svm.pred,col=2)

##Tuning svm
tune.out <- tune(svm,data=trainSample,averageRating~rating,kernel="polynomial",ranges = list(cost = c(0.001,0.1,10,100)),scale=FALSE)
summary(tune.out)