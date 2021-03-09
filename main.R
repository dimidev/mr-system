# Loading required packages
library(recommenderlab)

library(ggplot2)
library(data.table)
library(reshape2)

# Retrieving the Data
# Retrieve our data from movies.csv into movie_data dataframe and 
# ratings.csv into rating_data.
setwd("./dataset")
movie_data <- read.csv("movies.csv", stringsAsFactors=FALSE)
rating_data <- read.csv("ratings.csv")

# Data Pre-processing
# From the above table, we observe that the userId column, as well as the 
# movieId column, consist of integers. We need to convert the 
# genres present in the movie_data dataframe into a more usable format by 
# the users. In order to do so, we will first create a matrix that comprises 
# of corresponding genres for each of the films.
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors=FALSE)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', type.convert=TRUE), 
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
    gen_col = which(genre_mat1[1,] == movie_genre2[index, col])
    genre_mat1[index+1, gen_col] <- 1
  }
}

#remove first row, which was the genre list
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE)

for (col in 1:ncol(genre_mat2)) {
  #convert from characters to integers
  genre_mat2[,col] <- as.integer(genre_mat2[,col])
} 

# In the next step of Data Pre-processing of R project, we will create a 
# ‘search matrix’ that will allow us to perform an easy search of the films 
# by specifying the genre present in our list.
SearchMatrix <- cbind(movie_data[,1:2], genre_mat2[])

# For our movie recommendation system to make sense of our ratings through 
# recommenderlabs, we have to convert our matrix into a sparse matrix one. 
# This new matrix is of the class ‘realRatingMatrix’.
ratingMatrix <- dcast(rating_data, userId~movieId, value.var = "rating", na.rm=FALSE)

#remove userIds
ratingMatrix <- as.matrix(ratingMatrix[,-1])

#Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")

recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")

lapply(recommendation_model, "[[", "description")
