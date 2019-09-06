getwd()
setwd('Desktop/RPI/Sem 2/DA/IMDB/')

###Merging all files to make a single large dataset
library(readr)
titles <- read_delim("title_basics_cleaned.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)
titleRatings <- read_delim("title.ratings.tsv", "\t", escape_double = FALSE, trim_ws = TRUE) 
actorRating <- read_delim("actor_rating.csv",",",escape_double = FALSE, trim_ws = TRUE)
directorRating <- read_delim("director_rating.csv",",",escape_double = FALSE,trim_ws = TRUE)
genres <- read_delim("Genre_cluster.csv",",",escape_double = FALSE,trim_ws = TRUE)
titleOriginal <- read_delim("title_akas_cleaned.tsv","\t",escape_double = FALSE,trim_ws = TRUE)
title_principals <- read_delim("title.principals.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)

##Checking file reading
head(titles)
head(titleRatings)
head(actorRating)
head(directorRating)
head(genres)
head(titleOriginal)
head(title_principals)

##Getting actors & directors for each category
actors_df <- title_principals[title_principals$category %in% c("actor","actress"),c("tconst","nconst","category")]
directors_df <- title_principals[title_principals$category == "director",c("tconst","nconst","category")]

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

#merging data - titles & ratings
movie_ratings  <- merge(titles_movies,titleRatings,by="tconst")
sapply(movie_ratings, class)

#merging actor rating with movie title
actor_ratings <- merge(actorRating,actors_df,by="nconst")
actor_ratings[actor_ratings$tconst == 'tt0612730',]
##A movie may have multiple actors - so taking median rating of all its rating
movie_actor_rating <- actor_ratings %>% group_by(tconst) %>% summarise(actorRating = median(rating))
#merging with main dataset
movie_ratings <- merge(movie_ratings,movie_actor_rating,by='tconst')

#merging director rating with movie title
director_ratings <- merge(directorRating,directors_df,by='nconst')
director_ratings[director_ratings$tconst == 'tt0074147',]
##A movie may have multiple directors - so taking median rating of all its rating
movie_director_rating <- director_ratings %>% group_by(tconst) %>% summarise(directorRating = median(rating))
#merging with main dataset
movie_ratings <- merge(movie_ratings,movie_director_rating,by='tconst')

#merging moive with titleOriginals - isOriginal & number of remake made
movie_ratings <- merge(movie_ratings,titleOriginal,by.x='tconst',by.y = 'titleId')

#merge with genres cluster with movies
head(genres)
#removing first column - not required
genres <- genres[,-1]
#merge data with movie_ratings
movie_ratings <- merge(movie_ratings,genres[,c("tconst","Cluster")])
## 97K records
#save final data
colnames(movie_ratings)[5]<-"movieRating"
write.csv(movie_ratings,"final_data.csv")

###Barplot for different genres
n <- sapply(genres[,5:31], sum)
barplot(sort(n),las=2)
