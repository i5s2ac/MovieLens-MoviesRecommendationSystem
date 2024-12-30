#Movielens Project 
#Name:Isaac Cyrman Casafont
#Email:Cyr20552@uvg.edu.gt

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggpubr)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


#Data exploration and discovery process

#In the data set given, there was a column named "timestamp", this column provided the date where ratings where given by the user for each movie, however this one was unreadable, so we convert it into a readable one.  
edx$date <- as.POSIXct(edx$timestamp, origin="1970-01-01")
validation$date <- as.POSIXct(validation$timestamp, origin="1970-01-01")

edx$YearRated <- format(edx$date,"%Y")
edx$MonthRated <- format(edx$date,"%m")
edx$DayRated <- format(edx$date,"%d")
validation$YearRated <- format(validation$date,"%Y")
validation$MonthRated <- format(validation$date,"%m")
validation$DayRated <- format(validation$date,"%d")

#After having our dates in a readable format, we have to delete the "timestamp" column in both data sets.
edx <- edx %>% select(userId, movieId, rating, title, genres, YearRated,MonthRated,DayRated)
validation <- validation %>% select(userId, movieId, rating, title, genres, YearRated,MonthRated,DayRated)

#Also in the data set in the column "genres", we had to extract or separate all the genres that have one movie, this was needed for analyzing and modeling purposes. 
edx<-edx %>% separate_rows(genres, sep = "\\|")
validation<-validation %>% separate_rows(genres, sep = "\\|")

#Data Analysis Process

#How many users and movies are different in the data set.
User_Movie_different<-edx%>% summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))


#First of all, we need to create a new data set with the genres separated, grouped by genres and then count them to see how many movie genres are in the data set.
Quantity_Movies_Genres<- edx %>% group_by(genres) %>% count()

#By having the genres separated, we can start analyzing the data more precisely for every movie genre in the data set.

#In the following plot, we are going to see the quantity of movie genres in all the data set.
ggplot(Quantity_Movies_Genres, aes(genres, n)) +
  geom_col(aes(fill = factor(genres))) + ggtitle("Quantity of Movies Genres") + 
  xlab("Genres") + ylab("Quantity")



#For the top 10 genres based on quantity, we need to order the data and then select the first 10 rows.
top_10_genres<-Quantity_Movies_Genres[order(-Quantity_Movies_Genres$n),][1:10,]

#In the following plot, we are going to list the top 10 movies genres in the data set.
ggplot(top_10_genres, aes(genres, n)) +
  geom_col(aes(fill = factor(genres))) + ggtitle("Top 10 Movies Genres") + 
  xlab("Genres") + ylab("Quantity")



#For the top genres by year, we need to create a new data set with the genres separated, grouped by genres and YearRated and then count them to see how many movie genres are listed per year in the data set.
Quantity_Movies_Genres_by_year<- edx %>% group_by(YearRated,genres)  %>% count()

#After creating the data set with the genres counted per year, we are going to order, group again by year and  then select the first/best genre for every year in the data set.
top_genres_by_year<-Quantity_Movies_Genres_by_year[order(-Quantity_Movies_Genres_by_year$n),] %>% group_by(YearRated) %>%  top_n(1)

#In the following plots, we are going to list the best genre by year in the data set.
top_genre_graphic1<-ggplot(top_genres_by_year, aes(YearRated, n)) +
  geom_col(aes(fill = factor(genres))) + ggtitle("Top Genre by year") + 
  xlab("Year") + ylab("Quantity")

top_genre_graphic2 <- ggplot(top_genres_by_year, aes(YearRated, n)) +
  geom_point(aes(colour = factor(genres)),size=2.5) + scale_colour_manual(values = c("red", "blue" )) + geom_line(group=15) + ggtitle("Top Genre by year") + 
  xlab("Year") + ylab("Quantity")

#In the following plot, we are going to join the last two plots for visualization and interpretation purposes with the function ggarrange.
Top_genre_by_year_graphic <- ggarrange(top_genre_graphic1, top_genre_graphic2,labels = c("1", "2"),ncol = 1, nrow = 2)
Top_genre_by_year_graphic


#Now we are going to analyze Movies ratings.


#We are going to create a new data set grouping the movie ratings and then count how many times a rate was given.
Frequency_Movie_Ratings<- edx %>% group_by(rating) %>% count()

#In the following plot, we are going to show the movie rating frequency in the data set. This one will be "Rating vs Times Given".
ggplot(Frequency_Movie_Ratings, aes(rating, n)) +
  geom_col(aes(fill = factor(rating))) + ggtitle("Movie Rating Frequency") + 
  xlab("Rating") + ylab("Times given")

#We are going to create a new data set grouping movies ratings and adding the title of the movies.
Movies_rating_title<- edx %>% group_by(rating) %>%  summarise(title=title)

#In the following table, we will see the top 10 movies by rating given.
top_10_movies<- Movies_rating_title[order(-Movies_rating_title$rating),][1:10,]




#We are going to create a new data set grouped by YearRated and adding the mean per year of the movie ratings.
Mean_ratings_per_year<-edx %>% group_by(YearRated) %>% summarise(Mean_per_year=mean(rating))

#In the following plot, we are going to show the mean of rating per year. This will show us the best year in rating terms. 
p1 <- ggplot(Mean_ratings_per_year, aes(YearRated, Mean_per_year)) +
  geom_point(aes(colour = factor(YearRated)),size=2.5)
Mean_ratings_per_year_graphic1<-p1 + scale_colour_manual(values = c("red", "blue", "green","purple","dark red", "orange", "pink", "yellow", "magenta", "black", "grey", "violet", "turquoise", "dark blue","dark green" )) + geom_line(group=10) + ggtitle("Mean rating per year") + 
  xlab("Year") + ylab("Mean")


#We are going to create a new data set grouped by YearRated and then count how many ratings received every year.
Quantity_MovieRatings_per_year<-edx %>% group_by(YearRated) %>% count()

#In the following plot, we are going to show the quantity of ratings given per year.
p2 <- ggplot(Quantity_MovieRatings_per_year, aes(YearRated, n)) +
  geom_point(aes(colour = factor(YearRated)),size=2.5)
Quantity_MovieRatings_per_year_graphic<-p2 + scale_colour_manual(values = c("red", "blue", "green","purple","dark red", "orange", "pink", "yellow", "magenta", "black", "grey", "violet", "turquoise", "dark blue","dark green" )) + geom_line(group=10) + ggtitle("Quantity of ratings per year ") + 
  xlab("Year") + ylab("Ratings given")

#In the following plot, we are going to join the last two plots for visualization and interpretation purposes with the function ggarrange.
Mean_Quantity_ratings_per_year_graphic <- ggarrange(Mean_ratings_per_year_graphic1, Quantity_MovieRatings_per_year_graphic,labels = c("1", "2"),ncol = 1, nrow = 2)
Mean_Quantity_ratings_per_year_graphic


#Netflix Challenge

#The following function will be used to generate/create models for our recommendation system.
RMSE <- function(true_ratings = NULL, predicted_ratings = NULL) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#FIRST MODEL

#STEP1: We are going to calculate the mean of the ratings in the edx data set.
MeanEdxRating<-mean(edx$rating)

#STEP2: We are going to use the function above so we can generated our first model, for this we will use the actual values (MeanEdxRating) and predicted values (validation$rating). 
RMSE_FISRT_MODEL<-RMSE(MeanEdxRating, validation$rating)

#SECOND MODEL

#STEP1: We are going to calculate the mean of the movies.
MeanEdxRatingMovieID<-edx %>% group_by(movieId) %>% summarize(b_u= mean(rating - MeanEdxRating))

#STEP2: We are going to create/generate a model with the mean of the movies.
RMSE_SECOND_MODEL <- validation %>%
  left_join(MeanEdxRatingMovieID, by='movieId')%>%
  mutate(PREDICT_SECOND_MODEL = MeanEdxRating + b_u) %>% .$PREDICT_SECOND_MODEL 

#STEP3: We are going to use the function defined early above to generate and improve our last model, for this one, we will use the actual values (validation$rating) and predicted values (RMSE_SECOND_MODEL). 
SECOND_MODEL_RESULT<-RMSE(validation$rating,RMSE_SECOND_MODEL)

#STEP4: Show the results.
SECOND_MODEL_RESULT %>% knitr::kable()

#THIRD MODEL

#STEP1: We are going to calculate the mean of the movies and users.
MeanEdxRatingUsers<-edx %>% group_by(userId) %>% left_join(MeanEdxRatingMovieID,by="movieId") %>% summarize(b_u2=mean(rating-MeanEdxRating-b_u))

#STEP2: We are going to create/generate a model with the mean of the movies and users.
RMSE_THIRD_MODEL <- validation %>%
  left_join(MeanEdxRatingMovieID, by='movieId')%>%
  left_join(MeanEdxRatingUsers, by='userId') %>%
  mutate(PREDICT_THIRD_MODEL = MeanEdxRating + b_u + b_u2) %>% .$PREDICT_THIRD_MODEL

#STEP3: We are going to use the function defined early above to generate and improve our last model, for this one, we will use the actual values (validation$rating) and predicted values (RMSE_THIRD_MODEL). 
THIRD_MODEL_RESULT<-RMSE(validation$rating,RMSE_THIRD_MODEL)

#STEP4: Show the results.
THIRD_MODEL_RESULT %>% knitr::kable()

#FOURTH MODEL

#STEP1: We are going to calculate the mean of the movies, users and genres.
MeanEdxRatingGenres<-edx %>% group_by(genres) %>% left_join(MeanEdxRatingMovieID,by="movieId") %>% left_join(MeanEdxRatingUsers,by="userId")%>%  summarize(b_u3=mean(rating-MeanEdxRating-b_u-b_u2))

#STEP2: We are going to create/generate a model with the mean of the movies, users and genres.
RMSE_FOURTH_MODEL <- validation %>%
  left_join(MeanEdxRatingMovieID, by='movieId')%>%
  left_join(MeanEdxRatingUsers, by='userId') %>%
  left_join(MeanEdxRatingGenres, by='genres') %>%
  mutate(PREDICT_FOURTH_MODEL = MeanEdxRating + b_u + b_u2 + b_u3) %>% .$PREDICT_FOURTH_MODEL

#STEP3: We are going to use the function defined early above to generate and improve our last model, for this one, we will use the actual values (validation$rating) and predicted values (RMSE_FOURTH_MODEL). 
FOURTH_MODEL_RESULT<- RMSE(validation$rating,RMSE_FOURTH_MODEL)
FOURTH_MODEL_RESULT

#STEP4: Show the results.
FOURTH_MODEL_RESULT %>% knitr::kable()

#FIFTH MODEL

#STEP1: We are going to calculate the mean of the movies, users, genres and title.
MeanEdxRatingTitles<-edx %>% group_by(title) %>%
  left_join(MeanEdxRatingMovieID,by="movieId") %>% 
  left_join(MeanEdxRatingUsers,by="userId")%>% 
  left_join(MeanEdxRatingGenres,by="genres")%>% 
  summarize(b_u4=mean(rating-MeanEdxRating-b_u-b_u2-b_u3))

#STEP2: We are going to create/generate a model with the mean of the movies, users, genres and titles.
RMSE_FIFTH_MODEL <- validation %>%
  left_join(MeanEdxRatingMovieID, by='movieId')%>%
  left_join(MeanEdxRatingUsers, by='userId') %>%
  left_join(MeanEdxRatingGenres, by='genres') %>%
  left_join(MeanEdxRatingTitles,by="title")%>% 
  mutate(PREDICT_FIFTH_MODEL = MeanEdxRating + b_u + b_u2 + b_u3+b_u4) %>% .$PREDICT_FIFTH_MODEL

#STEP3: We are going to use the function defined early above to generate and improve our last model, for this one, we will use the actual values (validation$rating) and predicted values (RMSE_FIFTH_MODEL). 
FIFTH_MODEL_RESULT<- RMSE(validation$rating,RMSE_FIFTH_MODEL)
FIFTH_MODEL_RESULT

#STEP4: Show the results
FIFTH_MODEL_RESULT %>% knitr::kable()

#Regularization process for the five models

#STEP1:We are going to define a sequence to generate multiple results in the models evaluated.
seq <- seq(0, 35, 0.1)

#FISRT MODEL REGULARIZED (MOVIE MODEL).

#STEP1: We are going to create a function using the sequence above to compute the different values for the predicted ratings.

Regularization_function1 <- sapply(seq, function(x) {
  
#STEP2: We are going to calculate the mean by movieID.
  b_u <- edx %>%
    group_by(movieId) %>%
    summarize(b_u = sum(rating - MeanEdxRating) / (n() + x))
  
#STEP3: We are going to create/generate a model with the mean of the movies.
  predicted_ratings <- validation %>%
    left_join(b_u, by='movieId') %>%
    mutate(pred = MeanEdxRating + b_u) %>%
    pull(pred)

#STEP4: We are going to use the function defined early above to generate and evaluate our first model regularized., for this one, we will use the actual values (validation$rating) and predicted values (predicted_ratings). 
  return(RMSE(validation$rating, predicted_ratings))
  
})

#STEP5: Show the min sequence value that minimize the RMSE of our model.
min_movie <- seq[which.min(Regularization_function1)]
min_movie

#STEP6: Show our min/best value of RMSE.
rmse_regularized_movie <- min(Regularization_function1)
rmse_regularized_movie

#STEP7: Create a data frame with the results and sequence for visualization purposes.
DATA_RMSE_SEQ1<-data.frame(RMSE=Regularization_function1,Seq=seq)

#STEP8: We are going to create a plot with our data frame values to show the linear regression in the model.
MOVIE_MODEL_GRAPHIC<-ggplot(DATA_RMSE_SEQ1,aes(Regularization_function1,seq)) + geom_point() + ggtitle("Regularized Model: MOVIE") + 
  xlab("RMSE") + ylab("Sequence")

MOVIE_MODEL_GRAPHIC

#SECOND MODEL REGULARIZED (MOVIE+USER MODEL).

#STEP1: We are going to create a function using the sequence above to compute the different values for the predicted ratings.
Regularization_function2 <- sapply(seq, function(x) {
  
#STEP2: We are going to calculate the mean by movieID.
  b_u <- edx %>%
    group_by(movieId) %>%
    summarize(b_u = sum(rating - MeanEdxRating) / (n() + x))
  
#STEP3: We are going to calculate the mean by UserID.
  b_u1 <- edx %>%
    left_join(b_u, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u1 = sum(rating - b_u - MeanEdxRating) / (n() + x))

#STEP4: We are going to create/generate a model with the mean of the movieID and UserID.
  predicted_ratings <- validation %>%
    left_join(b_u, by='movieId') %>%
    left_join(b_u1, by='userId') %>%
    mutate(pred = MeanEdxRating + b_u + b_u1 ) %>%
    pull(pred)
  
#STEP5: We are going to use the function defined early above to generate and evaluate our second model regularized, for this one, we will use the actual values (validation$rating) and predicted values (predicted_ratings).  
  return(RMSE(validation$rating, predicted_ratings))
  
})

#STEP6: Show the min sequence value that minimize the RMSE of our model.
min_movie_user <- seq[which.min(Regularization_function2)]
min_movie_user

#STEP7: Show our min/best value of RMSE.
rmse_regularized_movie_user <- min(Regularization_function2)
rmse_regularized_movie_user

#STEP8: Create a data frame with the results and sequence for visualization purposes.
DATA_RMSE_SEQ2<-data.frame(RMSE=Regularization_function2,Seq=seq)

#STEP9: We are going to create a plot with our data frame values to show the linear regression in the model.
MOVIE_USER_MODEL_GRAPHIC<-ggplot(DATA_RMSE_SEQ2,aes(Regularization_function2,seq)) + geom_point() + ggtitle("Regularized Model: MOVIE+USER") + 
  xlab("RMSE") + ylab("Sequence")
MOVIE_USER_MODEL_GRAPHIC

#THIRD MODEL REGULARIZED (MOVIE+USER+GENRE MODEL).

#STEP1: We are going to create a function using the sequence above to compute the different values for the predicted ratings.
Regularization_function3 <- sapply(seq, function(x) {

#STEP2: We are going to calculate the mean by movieID.
   b_u <- edx %>%
    group_by(movieId) %>%
    summarize(b_u = sum(rating - MeanEdxRating) / (n() + x))
   
#STEP3: We are going to calculate the mean by UserID. 
   b_u1 <- edx %>%
    left_join(b_u, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u1 = sum(rating - b_u - MeanEdxRating) / (n() + x))
   
#STEP4: We are going to calculate the mean by Genre.
  b_u2 <- edx %>%
    left_join(b_u, by='movieId') %>%
    left_join(b_u1, by='userId') %>%
    group_by(genres) %>%
    summarize(b_u2 = sum(rating - b_u - MeanEdxRating - b_u1) / (n() + x))

#STEP5: We are going to create/generate a model with the mean of the movieID, UserID and Genres.  
  predicted_ratings <- validation %>%
    left_join(b_u, by='movieId') %>%
    left_join(b_u1, by='userId') %>%
    left_join(b_u2, by='genres') %>%
    mutate(pred = MeanEdxRating + b_u + b_u1 + b_u2 ) %>%
    pull(pred)

#STEP6: We are going to use the function defined early above to generate and evaluate our third model regularized, for this one, we will use the actual values (validation$rating) and predicted values (predicted_ratings).    
  return(RMSE(validation$rating, predicted_ratings))
 
})

#STEP7: Show the min sequence value that minimize the RMSE of our model.
min_movie_user_genres <- seq[which.min(Regularization_function3)]
min_movie_user_genres


#STEP8: Show our min/best value of RMSE.
rmse_regularized_movie_user_genres <- min(Regularization_function3)
rmse_regularized_movie_user_genres

#STEP9: Create a data frame with the results and sequence for visualization purposes.
DATA_RMSE_SEQ3<-data.frame(RMSE=Regularization_function3,Seq=seq)

#STEP10: We are going to create a plot with our data frame values to show the linear regression in the model.
MOVIE_USER_GENRES_MODEL_GRAPHIC<-ggplot(DATA_RMSE_SEQ3,aes(Regularization_function3,seq)) + geom_point() + ggtitle("Regularized Model: MOVIE+USER+GENRES") + 
  xlab("RMSE") + ylab("Sequence")
MOVIE_USER_GENRES_MODEL_GRAPHIC

#THIRD MODEL REGULARIZED (MOVIE+USER+GENRE+TITLE MODEL).

#STEP1: We are going to create a function using the sequence above to compute the different values for the predicted ratings.
Regularization_function4 <- sapply(seq, function(x) {
 
#STEP2: We are going to calculate the mean by movieID. 
  b_u <- edx %>%
    group_by(movieId) %>%
    summarize(b_u = sum(rating - MeanEdxRating) / (n() + x))
  
#STEP3: We are going to calculate the mean by UserID. 
  b_u1 <- edx %>%
    left_join(b_u, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u1 = sum(rating - b_u - MeanEdxRating) / (n() + x))
  
#STEP4: We are going to calculate the mean by Genres
  b_u2 <- edx %>%
    left_join(b_u, by='movieId') %>%
    left_join(b_u1, by='userId') %>%
    group_by(genres) %>%
    summarize(b_u2 = sum(rating - b_u - MeanEdxRating - b_u1) / (n() + x))
  
#STEP5: We are going to calculate the mean by Title.
  b_u3 <- edx %>%
    left_join(b_u, by='movieId') %>%
    left_join(b_u1, by='userId') %>%
    left_join(b_u2,by="genres") %>%
    group_by(title) %>%
    summarize(b_u3 = sum(rating - b_u - MeanEdxRating - b_u1-b_u2) / (n() + x))
  
#STEP6: We are going to create/generate a model with the mean of the movieID, UserID, Genres and Title.  
  predicted_ratings <- validation %>%
    left_join(b_u, by='movieId') %>%
    left_join(b_u1, by='userId') %>%
    left_join(b_u2, by='genres') %>%
    left_join(b_u3, by='title') %>%
    mutate(pred = MeanEdxRating + b_u + b_u1 + b_u2 + b_u3) %>%
    pull(pred)

#STEP7: We are going to use the function defined early above to generate and evaluate our third model regularized, for this one, we will use the actual values (validation$rating) and predicted values (predicted_ratings). 
  return(RMSE(validation$rating, predicted_ratings))
  
})

#STEP8: Show the min sequence value that minimize the RMSE of our model.
min_movie_user_genres_titles <- seq[which.min(Regularization_function4)]
min_movie_user_genres_titles

#STEP9: Show our min/best value of RMSE.
rmse_regularized_movie_user_genres_titles <- min(Regularization_function4)
rmse_regularized_movie_user_genres_titles

#STEP10: Create a data frame with the results and sequence for visualization purposes.
DATA_RMSE_SEQ3<-data.frame(RMSE=Regularization_function4,Seq=seq)

#STEP11: We are going to create a plot with our data frame values to show the linear regression in the model.
MOVIE_USER_GENRES_TITLE_MODEL_GRAPHIC<-ggplot(DATA_RMSE_SEQ3,aes(Regularization_function4,seq)) + geom_point() + ggtitle("Regularized Model: MOVIE+USER+GENRES+TITLES") + 
  xlab("RMSE") + ylab("Sequence")
MOVIE_USER_GENRES_TITLE_MODEL_GRAPHIC

#Results In general

#We are going to create a data frame with all the results.
ALL_RESULTS<- data.frame(
  Model = c("FIRST (NO regularized)","SECOND (NO regularized)","THIRD (NO regularized)","FOURTH (NO regularized)","FIFTH (NO regularized)",
            "SIX (regularized)", "SEVEN (regularized)", "EIGHT(regularized)","NINE (regularized)"),
  RMSE= c( RMSE_FISRT_MODEL,
           SECOND_MODEL_RESULT,
           THIRD_MODEL_RESULT,
           FOURTH_MODEL_RESULT,
           FIFTH_MODEL_RESULT,
           rmse_regularized_movie,
           rmse_regularized_movie_user,
           rmse_regularized_movie_user_genres,
           rmse_regularized_movie_user_genres_titles))

