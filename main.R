# Loading required packages
library(recommenderlab)

library(ggplot2)
library(data.table)
library(reshape2)

# Retrieving the Data
setwd("./dataset/movie_data")
movie_data <- read.csv("movies.csv", stringsAsFactors=FALSE)
rating_data <- read.csv("ratings.csv")
str(movie_data)