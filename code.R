###########################################################
#   HarvardX: PH125.9x  Data Science: Capstone            #
#         Movielens recommendation system                 #
###########################################################

# clear console output
cat("\014")

###########################################################
#                        LIBRARIES                        #
###########################################################

# library installations if needed:
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(Matrix)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(pheatmap)) install.packages("pheatmap", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")

# loading libraries
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(Matrix)
library(stringr)
library(pheatmap)
library(corrplot)


###########################################################
# Create edx set, validation set (final hold-out test set)#
#       Code bellow is provided by edX platform           #
###########################################################

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# If data is not downloaded - download it and unzip. If files are already unzipped and in directory, just read them
if (!file.exists("ml-10M100K/ratings.dat") | !file.exists("ml-10M100K/movies.dat")) {
  dl <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  
  ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                   col.names = c("userId", "movieId", "rating", "timestamp"))
  
  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
} else {
  ratings <- fread(text = gsub("::", "\t", readLines("ml-10M100K/ratings.dat")),
                   col.names = c("userId", "movieId", "rating", "timestamp"))
  movies <- str_split_fixed(readLines("ml-10M100K/movies.dat"), "\\::", 3)
}

# name columns
colnames(movies) <- c("movieId", "title", "genres")


# Create Data Frame with movies

# If using R 3.6 or earlier, uncomment following block:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))

# If using R 3.6 or earlier, comment out this statement and use above:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

# Join with rating by movieID
movielens <- left_join(ratings, movies, by = "movieId")


# slice of movielens dataset to edx and validation datasets
# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")        # if using R 3.5 or earlier, use `set.seed(1)`
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

# code above was provided by edX platform

# Dimensions of edx and validation datasets:
dim(edx)
dim(validation)



###########################################################
#                         Analysis                        #
###########################################################

# first look at the data:
class(edx)
head(edx)
str(edx)


# rating analysis
unique(edx$rating)
summary(edx$rating)

# the distribution of different ratings
edx %>% ggplot(aes(rating)) + 
  geom_bar(col = "black") +
  xlab("Rating") + ylab("Count" )+ 
  scale_y_continuous(breaks = seq(0,3*10^6,10^6),
                     labels=c("0","1M","2M","3M")) +
  ggtitle("Distibution of Movie ratings") +
  theme(plot.title = element_text(hjust = 0.5)) 

#Comparing rating types: negative if <= mean, positive if > mean
edx %>%
  mutate(rating_type = if_else(rating > mean(rating), "postitive",
                               "negative")) %>%
  group_by(rating_type) %>%
  count()


#Comparing half star and whole star ratings
edx %>%
  mutate(rating_star = if_else(!rating %%1, "whole_star",
                               "half_star")) %>%
  group_by(rating_star) %>%
  count()

# Movies and users

# the number of unique users and movies in datasets
edx_unique_info <- edx %>% 
  summarise(n_user_unique = n_distinct(userId),
            n_Movie_unique = n_distinct(movieId))
edx_unique_info

# total user/movie combination
edx_unique_info$n_user_unique * edx_unique_info$n_Movie_unique

nrow(edx)

# how many ratings are missing?
paste(round(nrow(edx) / (edx_unique_info$n_user_unique * edx_unique_info$n_Movie_unique) * 100, 1), "%")

# sample 100 users and 100 movies to visualize rated/unrated user-movie combinations
users <- sample(unique(edx$userId), 100)
sample_matrix <- edx %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.)
sample_matrix  %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users", col = gray.colors(n = 2, start = 0, end = 0))
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")
mtext(paste(sum(!is.na(sample_matrix)), 
                " User-movie combinations are rated (", 
                round(sum(!is.na(sample_matrix))/sum(is.na(sample_matrix)) * 100, 1), " %) \n"))
mtext(paste(sum(is.na(sample_matrix)), " User-movie combinations are unrated"))
title("User-Movie combinations", line = 3, font.main = 1)


# remove variables which we don't need anymore
rm(users, edx_unique_info, sample_matrix)

# Movies analysis

# top rated movies
edx %>% group_by(movieId) %>% 
  summarise(avg_rating = mean(rating), title = title) %>% 
  arrange(desc(avg_rating)) %>% 
  distinct() %>%
  ungroup() %>%
  select(-movieId) %>%
  head(10)

# bottom rated movies
edx %>% group_by(movieId) %>% 
  summarise(avg_rating = mean(rating), title = title) %>% 
  arrange(avg_rating) %>% 
  distinct() %>%
  ungroup() %>%
  select(-movieId) %>%
  head(10)

# top 10 best and worst movies are not widely known by it's title

# the distribution of number of ratings for movies
edx %>% group_by(movieId) %>% 
  summarise(number_of_ratings  = n()) %>%
  ggplot(aes(number_of_ratings)) +
  geom_histogram(bins = 100, col = "black") + scale_x_log10() +
  ggtitle("Distribution of movies by no. of rating") +
  theme(plot.title = element_text(hjust = 0.5))

# we can see, that approximately half of movies have less than 100 ratings
# about 125 movies have only one rating
# that can explain our observation of top 10 movies: very high and very low average movie rating can be done based on very few reviews,
# or even one review. This can't be reliable and will be taken in account when building a prediction model.

# Plotting movie distribution by mean rating
edx %>% group_by(movieId) %>% 
  summarise(average_movie_ratings  = mean(rating)) %>%
  ggplot(aes(average_movie_ratings)) +
  geom_histogram(bins = 100,col = "black") +
  geom_vline(xintercept = mean(edx$rating), col = "yellow") +
  ylab("Count of movies") +
  ggtitle("Distribution of movies by mean rating of movies") +
  theme(plot.title = element_text(hjust = 0.5))


# Effect of the number of movie ratings on average rating of this movie
edx %>% group_by(movieId) %>% 
  summarise(number_of_ratings  = n(), avg_rating = mean(rating)) %>% 
  ggplot(aes(number_of_ratings,avg_rating)) +
  geom_point(alpha= 0.2, color = "blue", lwd = 1) + 
  ggtitle("Average rating versus number of movie ratings") +
  geom_smooth(method = "loess", color = "red") + 
  xlab ("Number of movie ratings") +
  ylab("Average of rating") +
  theme(plot.title = element_text(hjust = 0.5)) 

# We can see, that more often rated movies are also have slightly higher average rating
# and smaller standard deviation

# Top 10 movies by number of ratings:
edx %>% group_by(movieId) %>% 
  summarise(number_of_ratings  = n(), avg_rating = mean(rating), title = title) %>% 
  arrange(desc(number_of_ratings)) %>% distinct() %>% head(10)

# Bottom 10 movies by number of ratings:
edx %>% group_by(movieId) %>% 
  summarise(number_of_ratings  = n(), avg_rating = mean(rating), title = title) %>% 
  arrange(number_of_ratings) %>% distinct() %>% head(10)

# Look at these tables confirms, that movies which were rated more often, have higher average rating
# We can see that the movie "Hellhounds on My Trail (1999)" also appeared in top rated movies table as it has average rating 5
# But it is based only on a single rating, which cannot be reliable. We will account it later, during model building and tuning.

# Users analysis

# the distribution of number of ratings for users
edx %>% group_by(userId) %>% 
  summarise(number_of_ratings_by_user  = n()) %>%
  ggplot(aes(number_of_ratings_by_user)) +
  geom_histogram(bins = 100, col = "black") + scale_x_log10()


# Similar to number of ratings of movies, many users rated only few movies
# We can see, that about half of users rated less than approximately 65 movies
# We will also take it in account for building model

# Plotting user distribution by mean rating of user
edx %>% group_by(userId) %>% 
  summarise(average_user_ratings  = mean(rating)) %>%
  ggplot(aes(average_user_ratings)) +
  geom_histogram(bins = 100,col = "black") +
  geom_vline(xintercept = mean(edx$rating), col = "yellow") +
  ylab("Count of movies") +
  ggtitle("Distribution of users by mean rating of user") +
  theme(plot.title = element_text(hjust = 0.5))

# Finding distribution of users by star type
edx %>%
  mutate(rating_star = ifelse(!rating %%1,
                              "whole_star", "half_star")) %>%
  group_by(rating_star) %>%
  summarise(n_users = n_distinct(userId))


# Effect of the number of movies rated by User on average rating by this user (for users who rated 100 or more movies)
edx %>% group_by(userId) %>% 
  filter(n() >= 100) %>%
  summarise(number_of_ratings_by_user  = n(), avg_rating_by_user = mean(rating)) %>% 
  ggplot(aes(number_of_ratings_by_user,avg_rating_by_user)) +
  geom_point(alpha= 0.2, color = "blue", lwd = 1) + 
  ggtitle("Average user rating versus number of ratings by the user") +
  geom_smooth(method = "loess", color = "red") + 
  xlab ("Number of ratings by user") +
  ylab("Average of rating by user") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Top 10 users by number of ratings:
edx %>% group_by(userId) %>% 
  summarise(number_of_ratings_by_user  = n(), avg_rating_by_user = mean(rating)) %>% 
  arrange(desc(number_of_ratings_by_user)) %>% distinct() %>% head(10)

# Bottom 10 users by number of ratings:
edx %>% group_by(userId) %>% 
  summarise(number_of_ratings_by_user  = n(), avg_rating_by_user = mean(rating)) %>% 
  arrange(number_of_ratings_by_user) %>% distinct() %>% head(10)

# Much higher variation among the users who rated less movies, compare to users who rated them a lot
# Average rating of the users who rated many movies tends to be closer to average (3-3.5)


# Year in the title cannot be used for prediction. We need to separate it to own column. 
# Also timestamp as it is is completely uninformative,
# therefore is being replaced by year, month and day of the week.

# Year of the movie, extracted from the title, and rating year, from the timestamp:
edx <- edx %>% 
  mutate(year_released = as.numeric(str_sub(title,-5,-2)), 
         year_rated = year(as_datetime(timestamp)),
         month_rated = month(as_datetime(timestamp)),
         day_rated = weekdays(as_datetime(timestamp))) %>% 
  select(-timestamp)


# Effect of released year of movieId on rating
seperate_year_released <- edx %>% group_by(year_released) %>% 
  summarise(year_released_rating = mean(rating)) %>% arrange(desc(year_released_rating))

seperate_year_released %>% 
  ggplot(aes(year_released,year_released_rating)) + 
  geom_point(alpha= 0.2, color = "blue", lwd = 1) +
  geom_smooth(method = "loess", color = "red") +
  ggtitle("Effect of released year on Rating") +
  xlab("Released year") +
  ylab("Rating") +
  theme(plot.title = element_text(hjust = 0.5))

#Movies released in 1940-1960 have higher average rating 


# Effect of rating year, month and day of the week on average rating

#Year
year_of_rating <- edx %>% group_by(year_rated) %>% 
  summarise(year_rated_rating = mean(rating)) %>% arrange(desc(year_rated_rating))

year_of_rating %>% 
  ggplot(aes(year_rated,year_rated_rating)) + 
  geom_point(alpha= 0.2, color = "blue", lwd = 1) +
  geom_smooth(method = "loess", color = "red") +
  ggtitle("Effect of rated year on Rating") +
  xlab("Rated year") +
  ylab("Rating") +
  theme(plot.title = element_text(hjust = 0.5))

# first year when movie was rated is
min(year_of_rating$year_rated)

#month
month_of_rating <- edx %>% group_by(month_rated) %>% 
  summarise(month_rated_rating = mean(rating)) %>% arrange(desc(month_rated_rating))

month_of_rating %>% 
  ggplot(aes(month_rated,month_rated_rating)) + 
  geom_point(alpha= 0.2, color = "blue", lwd = 1) +
  geom_smooth(method = "loess", color = "red") +
  ggtitle("Effect of rated month on Rating") +
  xlab("Rated month") +
  ylab("Rating") +
  theme(plot.title = element_text(hjust = 0.5))


#day
day_of_rating <- edx %>% group_by(day_rated) %>% 
  summarise(day_rated_rating = mean(rating)) %>% arrange(desc(day_rated_rating))

day_of_rating %>% 
  ggplot(aes(day_rated, day_rated_rating)) + 
  geom_point(color = "blue", lwd = 1) +
  ggtitle("Effect of rated day of the week on Rating") +
  xlab("Rated day of week") +
  ylab("Rating") +
  theme(plot.title = element_text(hjust = 0.5))

# Year and month of rating have effect on it (lower average rating after 2000 and lower rating during summer time)
# DoW just slightly (min 3.5 on Sundays and max 3.53 on Saturdays)

# remove variables which we don't need anymore
rm(seperate_year_released, day_of_rating, month_of_rating, year_of_rating)

# genres analysis

n_distinct(edx$genres)

head(edx$genres, 10)

# We can see, that genres are combined in 797 different groups

# check how many of them are unique
edx %>% group_by(genres) %>%
  summarise(number_of_ratings = n()) %>% arrange(number_of_ratings)%>% head(20)

# as we can see, many genres combinations have very few ratings,
# therefore we will split them to separate genres

# Relation of type of genres on average of rating 
separated_genres <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(number_of_ratings = n(), avg_rating = mean(rating))

separated_genres %>% arrange(desc(avg_rating)) 

# effect of separated genres on rating
separated_genres %>% 
  ggplot(aes(x = reorder(genres, avg_rating), y = avg_rating)) +
  geom_col(color = "black" ) + 
  ggtitle("Average rating versus separated genres") +
  xlab("Genres") + 
  ylab("Average rating") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90 , hjust = 0.5))

# number of separated genres appearing:
separated_genres %>% 
  ggplot(aes(x = reorder(genres, number_of_ratings), y = number_of_ratings)) +
  geom_col(color = "black" ) + 
  ggtitle("Number of separated genres") +
  xlab("Genres") + 
  ylab("Count") +
  scale_y_continuous(breaks = seq(0,3*10^6,10^6),
                     labels=c("0","1M","2M","3M")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90 , hjust = 0.5))

# only one movie without genre
edx %>% filter(genres == "(no genres listed)") %>% group_by(movieId) %>% pull(title) %>% unique()


# do we want to use 797 different genres combinations for our prediction,
# or it makes sense to split genre column to different ones?
# getting list of all genres combinations from the dataset
combined_genres <- unique(edx$genres)

# genres names without "(no genres listed)"
names <- separated_genres$genres[-1]

# create list of dataframes for each genres combinations
genres_columns <- sapply(names, function(name){
  df <- data.frame(name = ifelse(grepl(name, combined_genres), 1, 0))
  
})

# combine them to one dataframe
genres_df <- bind_cols(genres_columns)

# assign correct names
colnames(genres_df) <- names

# check if we can remove some non indicative genres
# build correlation matrix

corrplot(cor(genres_df), method="color", type="upper")

# some meaningful correlation only between genres Children and Animation
# other genres are relatively independent from each other

# remove variables, which will not be used further
rm(genres_columns, separated_genres, combined_genres, names)

###########################################################
#                   Data preparation                      #
###########################################################

# in order to build, train and verify prediction model,
# we need to prepare our dataset
# column timestamp already was replaced by columns 
# "year_rated", "month_rated" and "day_rated"




# our dataset is large enough therefore we can use train-test split (or hold out) approach for cross-validation
# before model building, training and selection we need to split our dataset to train and test subsets:
# test set will be 30% of all the data

test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.3, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

# remove temporary variables
rm(test_index, temp, removed)

# check dimensions 
dim(train_set)
dim(test_set)

###########################################################
#         Model building, training and selection          #
###########################################################

# function to estimate RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
  }


# first model: naive model, predicts always average rating
mu <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu)
naive_rmse
rmse_results <- data.frame(method = "Just the average", RMSE = naive_rmse)

# include movie bias (we saw, that some movies are more popular than others)
# bias is the term for effect
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

movie_avgs %>% ggplot(aes(b_i)) +
  geom_histogram(bins = 10,col = "black") +
  ylab("Count") +
  xlab("Movie bias") +
  ggtitle("Distribution of movies biases") +
  theme(plot.title = element_text(hjust = 0.5))

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results


# include user bias (it is based on our observation, that some users are cranky and others love different movies)
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i))

user_avgs %>% ggplot(aes(b_u)) +
  geom_histogram(bins = 10,col = "black") +
  ylab("Count") +
  xlab("User bias") +
  ggtitle("Distribution of users biases") +
  theme(plot.title = element_text(hjust = 0.5))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effect Model",
                                     RMSE = model_2_rmse ))
rmse_results

# include released year
year_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(year_released) %>% 
  summarize(b_y = mean(rating - mu - b_i - b_u))

year_avgs %>% ggplot(aes(b_y)) +
  geom_histogram(bins = 10,col = "black") +
  ylab("Count") +
  xlab("Released year bias") +
  ggtitle("Distribution of released year biases") +
  theme(plot.title = element_text(hjust = 0.5))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs, by='year_released') %>%
  mutate(pred = mu + b_i + b_u + b_y) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Year Effect Model",
                                     RMSE = model_3_rmse ))
rmse_results

# released year didn't give much improvement


# include genre
genre_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs, by='year_released') %>%
  group_by(genres) %>% 
  summarize(b_g = mean(rating - mu - b_i - b_u - b_y))

genre_avgs %>% ggplot(aes(b_g)) +
  geom_histogram(bins = 30,col = "black") +
  ylab("Count") +
  xlab("Genre bias") +
  ggtitle("Distribution of genres biases") +
  theme(plot.title = element_text(hjust = 0.5))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs, by='year_released') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
  .$pred

model_4_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Year + Genre Effect Model",
                                     RMSE = model_4_rmse ))
rmse_results

# remember that some movies were rated just few times and some users rated only few movies,
# we need to regularize our model by implementing penalty large estimates which are formed using small sample sizes

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_y <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(year_released) %>%
    summarize(b_y = sum(rating - b_i - b_u - mu)/(n()+l))
  
  b_g <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_y, by="year_released") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - b_y - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = "year_released") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  
lambda <- lambdas[which.min(rmses)]
min(rmses)
lambda

# add regularized model

b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))


b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

b_y <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(year_released) %>%
  summarize(b_y = sum(rating - b_i - b_u - mu)/(n()+lambda))

b_g <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_y, by="year_released") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_i - b_u - b_y - mu)/(n()+lambda))


predicted_ratings <- test_set %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_y, by='year_released') %>%
  left_join(b_g, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
  .$pred

model_5_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User + Year + Genre Effect Model",
                                     RMSE = model_5_rmse ))
rmse_results



# different lambda for different biases:
#!!!!!!!!!!!!!! CODE BELOW RUNS FOREVER !!!!!!!!!!!!!!!!!!

# lambda_i <- seq(3, 8, 0.5)
# lambda_u <- seq(3, 8, 0.5)
# lambda_y <- seq(3, 8, 0.5)
# lambda_g <- seq(3, 8, 0.5)
# lambda_grid <- expand.grid(lambda_i, lambda_u, lambda_y, lambda_g)
# colnames(lambda_grid) <- c("lambda_i", "lambda_u", "lambda_y", "lambda_g")
# 
# rmses <- apply(lambda_grid, MARGIN = 1, function(l){
# 
#   mu <- mean(train_set$rating)
#   
#   b_i <- train_set %>% 
#     group_by(movieId) %>%
#     summarize(b_i = as.numeric(sum(rating - mu)/(n()+l["lambda_i"])))
#   
#   b_u <- train_set %>% 
#     left_join(b_i, by="movieId") %>%
#     group_by(userId) %>%
#     summarize(b_u = as.numeric(sum(rating - b_i - mu)/(n()+l["lambda_u"])))
#   
#   b_y <- train_set %>% 
#     left_join(b_i, by="movieId") %>%
#     left_join(b_u, by="userId") %>%
#     group_by(year_released) %>%
#     summarize(b_y = as.numeric(sum(rating - b_i - b_u - mu)/(n()+l["lambda_y"])))
#   
#   b_g <- train_set %>% 
#     left_join(b_i, by="movieId") %>%
#     left_join(b_u, by="userId") %>%
#     left_join(b_y, by="year_released") %>%
#     group_by(genres) %>%
#     summarize(b_g = as.numeric(sum(rating - b_i - b_u - b_y - mu)/(n()+l["lambda_g"])))
#   
#   
#   predicted_ratings <- 
#     test_set %>% 
#     left_join(b_i, by = "movieId") %>%
#     left_join(b_u, by = "userId") %>%
#     left_join(b_y, by = "year_released") %>%
#     left_join(b_g, by = "genres") %>%
#     mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
#     pull(pred)
#   
#   return(RMSE(predicted_ratings, test_set$rating))
# })
# 
# best_lambda_set <- lambda_grid[which.min(rmses)]
# min(rmses)

# find different biases separately:

lambdas <- seq(0, 10, 0.25)
mu <- mean(train_set$rating)

# best lambda for movie effect:
rmses <- sapply(lambdas, function(l){

  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  
lambda_movie <- lambdas[which.min(rmses)]
min(rmses)
lambda_movie


# best lambda for user effect:
rmses <- sapply(lambdas, function(l){
  
  b_u <- train_set %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  
lambda_user<- lambdas[which.min(rmses)]
min(rmses)
lambda_user

lambdas <- seq(47, 52, 0.25)
# best lambda for released year effect:
rmses <- sapply(lambdas, function(l){

  b_y <- train_set %>% 
    group_by(year_released) %>%
    summarize(b_y = sum(rating - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_y, by = "year_released") %>%
    mutate(pred = mu + b_y) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  
lambda_year <- lambdas[which.min(rmses)]
min(rmses)
lambda_year

lambdas <- seq(0, 10, 0.25)
# best lambda for genre effect:
rmses <- sapply(lambdas, function(l){
  
  b_g <- train_set %>% 
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_g) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})


qplot(lambdas, rmses)  
lambda_genre <- lambdas[which.min(rmses)]
min(rmses)
lambda_genre

best_lambda_set <- c(lambda_movie, lambda_user, lambda_year, lambda_genre)

# fit regularized model with best lambdas for each predictor
b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = as.numeric(sum(rating - mu)/(n()+best_lambda_set[1])))


b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = as.numeric(sum(rating - b_i - mu)/(n()+best_lambda_set[2])))

b_y <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(year_released) %>%
  summarize(b_y = as.numeric(sum(rating - b_i - b_u - mu)/(n()+best_lambda_set[3])))

b_g <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_y, by="year_released") %>%
  group_by(genres) %>%
  summarize(b_g = as.numeric(sum(rating - b_i - b_u - b_y - mu)/(n()+best_lambda_set[4])))


predicted_ratings <- test_set %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_y, by='year_released') %>%
  left_join(b_g, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
  .$pred

model_6_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Separately regularized Movie + User + Year + Genre Effect Model",
                                     RMSE = model_6_rmse ))
rmse_results





model_7_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User + Year + Genre + rating year and month effect Model",
                                     RMSE = model_7_rmse ))
rmse_results



# regularisation of all predictors
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){

  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_y <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(year_released) %>%
    summarize(b_y = sum(rating - b_i - b_u - mu)/(n()+l))
  
  b_g <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_y, by="year_released") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - b_y - mu)/(n()+l))
  
  b_ry <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_y, by="year_released") %>%
    left_join(b_g, by="genres") %>%
    group_by(year_rated) %>%
    summarize(b_ry = sum(rating - b_i - b_u - b_y - b_g - mu)/(n()+l))
  
  b_rm <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_y, by="year_released") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_ry, by="year_rated") %>%
    group_by(month_rated) %>%
    summarize(b_rm = sum(rating - b_i - b_u - b_y - b_g - b_ry - mu)/(n()+l))
  
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = "year_released") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_ry, by= "year_rated") %>%
    left_join(b_rm, by= "month_rated") %>%
    mutate(pred = mu + b_i + b_u + b_y + b_g + b_ry + b_rm) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  
lambda <- lambdas[which.min(rmses)]
min(rmses)
lambda


# add regularized model

b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))


b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

b_y <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(year_released) %>%
  summarize(b_y = sum(rating - b_i - b_u - mu)/(n()+lambda))

b_g <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_y, by="year_released") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_i - b_u - b_y - mu)/(n()+lambda))

b_ry <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_y, by="year_released") %>%
  left_join(b_g, by="genres") %>%
  group_by(year_rated) %>%
  summarize(b_ry = sum(rating - b_i - b_u - b_y - b_g - mu)/(n()+l))

b_rm <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_y, by="year_released") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_ry, by="year_rated") %>%
  group_by(month_rated) %>%
  summarize(b_rm = sum(rating - b_i - b_u - b_y - b_g - b_ry - mu)/(n()+l))

predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y, by = "year_released") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_ry, by= "year_rated") %>%
  left_join(b_rm, by= "month_rated") %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g + b_ry + b_rm) %>%
  pull(pred)

model_7_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Regularized Movie + User + Year + Genre + rating year and month effect Model",
                                     RMSE = model_7_rmse ))
rmse_results



# fit with best lambda on a complete edx dataset
b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))


b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

b_y <- edx %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(year_released) %>%
  summarize(b_y = sum(rating - b_i - b_u - mu)/(n()+lambda))

b_g <- edx %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_y, by="year_released") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_i - b_u - b_y - mu)/(n()+lambda))

b_ry <- edx %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_y, by="year_released") %>%
  left_join(b_g, by="genres") %>%
  group_by(year_rated) %>%
  summarize(b_ry = sum(rating - b_i - b_u - b_y - b_g - mu)/(n()+l))

b_rm <- edx %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_y, by="year_released") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_ry, by="year_rated") %>%
  group_by(month_rated) %>%
  summarize(b_rm = sum(rating - b_i - b_u - b_y - b_g - b_ry - mu)/(n()+l))


###########################################################
#            Final test on validation dataset             #
###########################################################

# add columns year_released, year_rated and month_rated from timestamp
validation <- validation %>% 
  mutate(year_released = as.numeric(str_sub(title,-5,-2)), 
         year_rated = year(as_datetime(timestamp)),
         month_rated = month(as_datetime(timestamp)))

# predict ratings in validation set using biases
predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y, by = "year_released") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_ry, by= "year_rated") %>%
  left_join(b_rm, by= "month_rated") %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g + b_ry + b_rm) %>%
  pull(pred)

RMSE(predicted_ratings, validation$rating)

# 0.864114 < 0.8649