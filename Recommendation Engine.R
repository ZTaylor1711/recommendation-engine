

#Recommendation Engine for Movies and TV Shows

#Install and load required packages
install.packages("recommenderlab")
install.packages("ggplot2")
install.packages("data.table")
install.packages("reshape2")

library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)


#Set working directory and load movie and rating data
setwd("C:/users/zacht/One_Drive/Recommendation Engine")
movie_data <- read.csv("movies.csv",stringsAsFactors=FALSE)
rating_data <- read.csv("ratings.csv")
str(movie_data)

#pre process data
#one hot encoding to tag each movie with genre
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors=FALSE)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', 
                                        type.convert=TRUE), 
                              stringsAsFactors=FALSE)
colnames(movie_genre2) <- c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")
genre_mat1 <- matrix(0,10330,18)
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre

for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1,] == movie_genre2[index,col])
    genre_mat1[index+1,gen_col] <- 1
  }
}

#Remove first row, which was the genre list
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE) 

#Convert from characters to integers
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col])
}

#Create a search matrix
SearchMatrix <- cbind(movie_data[,1:2], genre_mat2[])

#Create a rating matrix and remove userIds from it
ratingMatrix <- dcast(rating_data, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) 

#Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")

#Create recommender model
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
lapply(recommendation_model, "[[", "description")
recommendation_model$IBCF_realRatingMatrix$parameters

#Exploring similar data.  Collaborative Filtering is based on the idea that if User A and User B had similar interests
#in the past, then they probably would like similar things in the future.  Recommendations on based on the similarity
#between users

#in the generated matrix below, each row and column indicates a user.  Each cell represents the similarity between
#these users
#User Similarity Matrix
similarity_mat <- similarity(ratingMatrix[1:4, ],method = "cosine",which = "users")
as.matrix(similarity_mat)
image(as.matrix(similarity_mat), main = "User's Similarities")

#Item Similarity Matrix
movie_similarity <- similarity(ratingMatrix[, 1:4], method ="cosine", which = "items")
as.matrix(movie_similarity)
image(as.matrix(movie_similarity), main = "Movies similarity")

#Extract the most unique ratings from the similarity matrix
rating_values <- as.vector(ratingMatrix@data)
unique(rating_values)

#Create a Table of Ratings that will display the most unique ratings
Table_of_Ratings <- table(rating_values)

