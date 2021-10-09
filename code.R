###########################################################
#   HarvardX: PH125.9x  Data Science: Capstone            #
#         Movielens recommendation system                 #
###########################################################


##########################################################
#            READ THIS BEFORE RUN THE CODE!!!            #
#                                                        #
#     1. Check your R version (type "R.version.string")  #
# in console and comment/uncomment code blocks which are #
#            corresponding to your version.              #
# These version dependent code blocks are located in     #
#             lines 72-80 and line 89                    #
#                                                        #
#                                                        #
#       Run complete code with console output            #
#     can take 30-60 minutes, depends on a computer      #
#                                                        #
# Code itself is well commented as code, for theoretical #
#     background please look in report.pdf               #
##########################################################


# clear console output
cat("\014")

###########################################################
#                        LIBRARIES                        #
###########################################################

# library installations if needed:
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(data.table)) install.packages("data.table")
if(!require(lubridate)) install.packages("lubridate")
if(!require(Matrix)) install.packages("lubridate")
if(!require(stringr)) install.packages("stringr")
if(!require(pheatmap)) install.packages("pheatmap")
if(!require(corrplot)) install.packages("corrplot")
if(!require(recosystem)) install.packages("recosystem")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(missMDA)) install.packages("missMDA")

# loading libraries
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(Matrix)
library(stringr)
library(pheatmap)
library(corrplot)
library(recosystem)
library(gridExtra)
library(missMDA)


###########################################################
# Create edx set, validation set (final hold-out test set)#
#       Code bellow is provided by edX platform           #
###########################################################

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# Note: this process could take a couple of minutes

# download data
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)


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

###########################
# first look at the data  #
###########################
class(edx)
head(edx)
str(edx)


# rating analysis
unique(edx$rating)
summary(edx$rating)

# the distribution of different ratings
edx %>% ggplot(aes(rating)) + 
  geom_bar(col = "black") +
  xlab("Rating") + ylab("Count" ) + 
  scale_y_continuous(breaks = seq(0,3*10^6,10^6),
                     labels=c("0","1M","2M","3M")) +
  ggtitle("Distibution of movie ratings") +
  theme(plot.title = element_text(hjust = 0.5)) 

# comparing rating types: negative if <= mean, positive if > mean
edx %>%
  mutate(rating_type = if_else(rating > mean(rating), "postitive",
                               "negative")) %>%
  group_by(rating_type) %>%
  count()


# comparing half star and whole star ratings
edx %>%
  mutate(rating_star = if_else(!rating %%1, "whole_star",
                               "half_star")) %>%
  group_by(rating_star) %>%
  count()


####################
# Movies and users #
####################

# the number of unique users and movies in datasets
edx_unique_info <- edx %>% 
  summarise(n_user_unique = n_distinct(userId),
            n_movie_unique = n_distinct(movieId))
edx_unique_info

# total user/movie combination
edx_unique_info$n_user_unique * edx_unique_info$n_movie_unique

#number of ratings in dataset
nrow(edx)

# how many ratings are missing?
paste(round(nrow(edx) / (edx_unique_info$n_user_unique * edx_unique_info$n_movie_unique) * 100, 1), "%", sep = "")

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
                round(sum(!is.na(sample_matrix))/sum(is.na(sample_matrix)) * 100, 1), "%) \n"))
mtext(paste(sum(is.na(sample_matrix)), " User-movie combinations are unrated"))
title("User-Movie combinations", line = 3, font.main = 1)


# remove variables which we don't need anymore
rm(users, edx_unique_info, sample_matrix)



###################
# Movies analysis #
###################

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
  xlab("Number of ratings") + ylab("Count of movies" ) +
  ggtitle("Distribution of movies by no. of ratings") +
  theme(plot.title = element_text(hjust = 0.5))

# we can see, that approximately half of movies have less than 100 ratings
# about 125 movies have only one rating
# that can explain our observation of top 10 movies: very high and very low average movie rating can be done based on very few reviews,
# or even one review. This can't be reliable and will be taken in account when building a prediction model.

# plotting movie distribution by mean rating
edx %>% group_by(movieId) %>% 
  summarise(average_movie_ratings  = mean(rating)) %>%
  ggplot(aes(average_movie_ratings)) +
  geom_histogram(bins = 100,col = "black") +
  geom_vline(xintercept = mean(edx$rating), col = "yellow") +
  xlab("Average rating") + ylab("Count of movies") +
  ggtitle("Distribution of movies by mean rating of movies") +
  theme(plot.title = element_text(hjust = 0.5))


# effect of the number of movie ratings on average rating of this movie
edx %>% group_by(movieId) %>% 
  summarise(number_of_ratings  = n(), avg_rating = mean(rating)) %>% 
  ggplot(aes(number_of_ratings,avg_rating)) +
  geom_point(alpha= 0.2, color = "blue", lwd = 1) + 
  ggtitle("Average rating versus number of movie ratings") +
  geom_smooth(method = "loess", color = "red") + 
  xlab ("Number of movie ratings") +
  ylab("Average rating") +
  theme(plot.title = element_text(hjust = 0.5)) 

# We can see, that more often rated movies are also have slightly higher average rating
# and smaller standard deviation

# top 10 movies by number of ratings:
edx %>% group_by(movieId) %>% 
  summarise(number_of_ratings  = n(), avg_rating = mean(rating), title = title) %>% 
  arrange(desc(number_of_ratings)) %>% distinct() %>% head(10)

# bottom 10 movies by number of ratings:
edx %>% group_by(movieId) %>% 
  summarise(number_of_ratings  = n(), avg_rating = mean(rating), title = title) %>% 
  arrange(number_of_ratings) %>% distinct() %>% head(10)

# Look at these tables confirms, that movies which were rated more often, have higher average rating
# We can see that the movie "Hellhounds on My Trail (1999)" also appeared in top rated movies table as it has average rating 5
# But it is based only on a single rating, which cannot be reliable. We will account it later, during model building and tuning.


##################
# Users analysis #
##################

# the distribution of number of ratings for users
edx %>% group_by(userId) %>% 
  summarise(number_of_ratings_by_user  = n()) %>%
  ggplot(aes(number_of_ratings_by_user)) +
  geom_histogram(bins = 100, col = "black") + scale_x_log10() +
  xlab("Number of ratings") + ylab("Count of users" ) +
  ggtitle("Distribution of users by no. of ratings") +
  theme(plot.title = element_text(hjust = 0.5))

# Similar to number of ratings of movies, many users rated only few movies
# We can see, that about half of users rated less than approximately 65 movies
# We will also take it in account for building model

# plotting user distribution by mean rating of user
edx %>% group_by(userId) %>% 
  summarise(average_user_ratings  = mean(rating)) %>%
  ggplot(aes(average_user_ratings)) +
  geom_histogram(bins = 100,col = "black") +
  geom_vline(xintercept = mean(edx$rating), col = "yellow") +
  xlab("Average rating") + ylab("Count of users") +
  ggtitle("Distribution of users by mean rating of user") +
  theme(plot.title = element_text(hjust = 0.5))

# finding distribution of users by star type
edx %>%
  mutate(rating_star = ifelse(!rating %%1,
                              "whole_star", "half_star")) %>%
  group_by(rating_star) %>%
  summarise(n_users = n_distinct(userId))


# effect of the number of movies rated by User on average rating by this user (for users who rated 100 or more movies)
edx %>% group_by(userId) %>% 
  filter(n() >= 100) %>%
  summarise(number_of_ratings_by_user  = n(), avg_rating_by_user = mean(rating)) %>% 
  ggplot(aes(number_of_ratings_by_user,avg_rating_by_user)) +
  geom_point(alpha= 0.2, color = "blue", lwd = 1) + 
  ggtitle("Average user rating versus number of ratings by the user") +
  geom_smooth(method = "loess", color = "red") + 
  xlab ("Number of ratings by user") +
  ylab("Average rating by user") +
  theme(plot.title = element_text(hjust = 0.5)) 

# top 10 users by number of ratings:
edx %>% group_by(userId) %>% 
  summarise(number_of_ratings_by_user  = n(), avg_rating_by_user = mean(rating)) %>% 
  arrange(desc(number_of_ratings_by_user)) %>% distinct() %>% head(10)

# bottom 10 users by number of ratings:
edx %>% group_by(userId) %>% 
  summarise(number_of_ratings_by_user  = n(), avg_rating_by_user = mean(rating)) %>% 
  arrange(number_of_ratings_by_user) %>% distinct() %>% head(10)

# Much higher variation among the users who rated less movies, compare to users who rated them a lot
# Average rating of the users who rated many movies tends to be closer to average (3-3.5)


############################
# Year of release analysis #
############################

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

# released year range
min(edx$year_released)
max(edx$year_released)

# how many movies were released each year
year_distr1 <- edx %>% distinct(year_released, movieId) %>% 
                    group_by(year_released) %>%
                    summarise(number_of_movies = n()) %>%
                    ggplot(aes(x = year_released, y = number_of_movies)) +
                    geom_col(col = "black") +
                    xlab("Year of release") + ylab("Count of movies" )+ 
                    ggtitle("Distribution of movies by released year") +
                    theme(plot.title = element_text(hjust = 0.5))

# how many ratings for movies released each year

year_distr2 <- edx %>% 
                  ggplot(aes(year_released)) +
                  geom_bar(col = "black") +
                  xlab("Year of release") + ylab("Count of ratings" ) +
                  scale_y_continuous(breaks = seq(0,8*10^5, 2*10^5),
                                     labels=c("0","200k","400k","600k", "800k")) +
                  ggtitle("Distibution of movie ratings by released year") +
                  theme(plot.title = element_text(hjust = 0.5)) 
  
grid.arrange(year_distr1, year_distr2, nrow=2)

# remove variables which we don't need anymore
rm(year_distr1, year_distr2)

# effect of released year of movieId on rating
seperate_year_released <- edx %>% group_by(year_released) %>% 
  summarise(year_released_rating = mean(rating)) %>% arrange(desc(year_released_rating))


seperate_year_released %>% 
  ggplot(aes(year_released,year_released_rating)) + 
  geom_point(alpha= 0.2, color = "blue", lwd = 1) +
  geom_smooth(method = "loess", color = "red") +
  ggtitle("Effect of released year on rating") +
  xlab("Released year") +
  ylab("Average rating") +
  theme(plot.title = element_text(hjust = 0.5))

# Movies released in 1940-1960 have higher average rating 



########################
# Rating date analysis #
########################

# Effect of rating year, month and day of the week on average rating

# year
year_of_rating <- edx %>% group_by(year_rated) %>% 
  summarise(year_rated_rating = mean(rating)) %>% arrange(desc(year_rated_rating))

year_of_rating %>% 
  ggplot(aes(year_rated,year_rated_rating)) + 
  geom_point(alpha= 0.2, color = "blue", lwd = 1) +
  geom_smooth(method = "loess", color = "red") +
  ggtitle("Effect of rated year on rating") +
  xlab("Rated year") +
  ylab("Average rating") +
  theme(plot.title = element_text(hjust = 0.5))

# first year when movie was rated is
min(year_of_rating$year_rated)

# month
month_of_rating <- edx %>% group_by(month_rated) %>% 
  summarise(month_rated_rating = mean(rating)) %>% arrange(desc(month_rated_rating))

month_of_rating %>% 
  ggplot(aes(month_rated,month_rated_rating)) + 
  geom_point(alpha= 0.2, color = "blue", lwd = 1) +
  geom_smooth(method = "loess", color = "red") +
  ggtitle("Effect of rated month on rating") +
  scale_x_continuous(breaks = seq(1,12)) +
  xlab("Rated month") +
  ylab("Average rating") +
  theme(plot.title = element_text(hjust = 0.5))


# day
day_of_rating <- edx %>% group_by(day_rated) %>% 
  summarise(day_rated_rating = mean(rating))

day_of_rating$day_rated <- ordered(day_of_rating$day_rated, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                         "Friday", "Saturday", "Sunday"))

day_of_rating %>% 
  ggplot(aes(day_rated, day_rated_rating)) + 
  geom_point(color = "blue", lwd = 2) +
  ggtitle("Effect of rated day of the week on rating") +
  xlab("Rated day of week") +
  ylab("Average rating") +
  theme(plot.title = element_text(hjust = 0.5))

# Year and month of rating have effect on it (lower average rating after 2000 and lower rating during summer time)
# DoW just slightly (min 3.5 on Sundays and max 3.53 on Saturdays)

# remove variables which we don't need anymore
rm(seperate_year_released, day_of_rating, month_of_rating, year_of_rating)



###################
# Genres analysis #
###################

n_distinct(edx$genres)

head(edx$genres, 10)

# We can see, that genres are combined in 797 different groups

# check how many of them are unique
edx %>% group_by(genres) %>%
  summarise(number_of_ratings = n()) %>% arrange(number_of_ratings)%>% head(20)

# as we can see, many genres combinations have very few ratings,
# therefore we will split them to separate genres

# relation of type of genres on average of rating 
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

corrplot(cor(genres_df), method="color", number.cex=0.6, type="upper", diag=FALSE,
         mar=c(0,0,1.5 ,0), tl.col="black", addCoef.col = "black")
title("Correlations between genres", line = 3, font.main = 1)
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

# save RMSE
rmse_results <- data.frame(method = "Just the average", RMSE = RMSE(test_set$rating, mu))


###################
#  Linear models  #
###################

# include movie bias (we saw, that some movies are more popular than others)
# bias is the term for effect
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# plotting movies biases
movie_avgs %>% ggplot(aes(b_i)) +
  geom_histogram(bins = 10,col = "black") +
  ylab("Count") +
  xlab("Movie bias") +
  ggtitle("Distribution of movies biases") +
  theme(plot.title = element_text(hjust = 0.5))

# predict on test-set
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

# limiting rating 0.5 - 5.0, as we know real ratings can't be out of this range
predicted_ratings <- ifelse(predicted_ratings > 5, 5, ifelse(predicted_ratings < 0.5, 0.5, predicted_ratings))

# add calculated RMSE to a table, for models comparison selection
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Movie effect model",
                                     RMSE = RMSE(predicted_ratings, test_set$rating) ))
rmse_results


# include user bias (it is based on our observation, that some users are cranky and others love different movies)
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i))

# plotting users biases
user_avgs %>% ggplot(aes(b_u)) +
  geom_histogram(bins = 10,col = "black") +
  ylab("Count") +
  xlab("User bias") +
  ggtitle("Distribution of users biases") +
  theme(plot.title = element_text(hjust = 0.5))

# predict on test-set
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

# limiting rating 0.5 - 5.0, as we know real ratings can't be out of this range
predicted_ratings <- ifelse(predicted_ratings > 5, 5, ifelse(predicted_ratings < 0.5, 0.5, predicted_ratings))

# add calculated RMSE to the table
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Movie + user effect model",
                                     RMSE = RMSE(predicted_ratings, test_set$rating) ))
rmse_results


# include released year
year_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(year_released) %>% 
  summarize(b_y = mean(rating - mu - b_i - b_u))

# plotting years biases
year_avgs %>% ggplot(aes(b_y)) +
  geom_histogram(bins = 10,col = "black") +
  ylab("Count") +
  xlab("Released year bias") +
  ggtitle("Distribution of released year biases") +
  theme(plot.title = element_text(hjust = 0.5))

# predict on test-set
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs, by='year_released') %>%
  mutate(pred = mu + b_i + b_u + b_y) %>%
  .$pred

# limiting rating 0.5 - 5.0, as we know real ratings can't be out of this range
predicted_ratings <- ifelse(predicted_ratings > 5, 5, ifelse(predicted_ratings < 0.5, 0.5, predicted_ratings))

# add calculated RMSE to the table
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Movie + user + year effect model",
                                     RMSE = RMSE(predicted_ratings, test_set$rating) ))
rmse_results

# released year didn't give much improvement


# include genre
genre_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs, by='year_released') %>%
  group_by(genres) %>% 
  summarize(b_g = mean(rating - mu - b_i - b_u - b_y))

# plotting genres biases
genre_avgs %>% ggplot(aes(b_g)) +
  geom_histogram(bins = 30,col = "black") +
  ylab("Count") +
  xlab("Genre bias") +
  ggtitle("Distribution of genres biases") +
  theme(plot.title = element_text(hjust = 0.5))

# predict on test-set
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs, by='year_released') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
  .$pred

# limiting rating 0.5 - 5.0, as we know real ratings can't be out of this range
predicted_ratings <- ifelse(predicted_ratings > 5, 5, ifelse(predicted_ratings < 0.5, 0.5, predicted_ratings))

# add calculated RMSE to the table
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Movie + user + year + genre Effect Model",
                                     RMSE = RMSE(predicted_ratings, test_set$rating) ))
rmse_results

# remember that some movies were rated just few times and some users rated only few movies,
# we need to regularize our model by implementing penalty large estimates which are formed using small sample sizes


# find the best lambda (small sample size penalty)
lambdas <- seq(0, 10, 0.25)

# build models for different lambdas
# train-test set cross validation is used, because dataset is relatively big
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

# plot all lambdas against RMSE
qplot(lambdas, rmses, main = "Different lambda versus RMSE") +
  theme(plot.title = element_text(hjust = 0.5))
# which lambda makes best RMSE
lambda <- lambdas[which.min(rmses)]
min(rmses)
lambda

# add regularized model with the best lambda
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

# predict on test-set
predicted_ratings <- test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y, by = "year_released") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
  .$pred

# limiting rating 0.5 - 5.0, as we know real ratings can't be out of this range
predicted_ratings <- ifelse(predicted_ratings > 5, 5, ifelse(predicted_ratings < 0.5, 0.5, predicted_ratings))

# add calculated RMSE to the table
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Regularized movie + user + year + genre effect model",
                                     RMSE = RMSE(predicted_ratings, test_set$rating) ))
rmse_results


# let's add year and month of rating

# regularization of all predictors
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
    left_join(b_ry, by = "year_rated") %>%
    left_join(b_rm, by = "month_rated") %>%
    mutate(pred = mu + b_i + b_u + b_y + b_g + b_ry + b_rm) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

# plot all lambdas against RMSE
qplot(lambdas, rmses, main = "Different lambda versus RMSE") +
  theme(plot.title = element_text(hjust = 0.5))

# Which lambda makes best RMSE
lambda <- lambdas[which.min(rmses)]
min(rmses)
lambda


# add regularized model with the best lambda
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
  summarize(b_ry = sum(rating - b_i - b_u - b_y - b_g - mu)/(n()+lambda))

b_rm <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_y, by="year_released") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_ry, by="year_rated") %>%
  group_by(month_rated) %>%
  summarize(b_rm = sum(rating - b_i - b_u - b_y - b_g - b_ry - mu)/(n()+lambda))

# predict on test-set
predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y, by = "year_released") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_ry, by = "year_rated") %>%
  left_join(b_rm, by = "month_rated") %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g + b_ry + b_rm) %>%
  pull(pred)

# limiting rating 0.5 - 5.0, as we know real ratings can't be out of this range
predicted_ratings <- ifelse(predicted_ratings > 5, 5, ifelse(predicted_ratings < 0.5, 0.5, predicted_ratings))

# add calculated RMSE to the table
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Regularized movie + user + year + genre + rating year and month effect model",
                                     RMSE = RMSE(predicted_ratings, test_set$rating) ))
rmse_results


# clean variable environment from variables which we will not use anymore
rm(b_g, b_i, b_rm, b_ry, b_u, b_y, genre_avgs, genres_df, movie_avgs, user_avgs, year_avgs)
rm(lambdas, lambda, predicted_ratings, mu, rmses)



################################
# Principal Component Analysis #
################################

# our model Yui = u + bi+ bu + e

# groups of movies have similar rating patterns and groups of users have similar rating patterns as well.
# residuals: rui = yui - bi - bu

# convert the data into a matrix so that each user gets a row, each movie gets a column, and  
# yui is the entry in row u and column i.

# it takes a lot of time, therefore first try on small dataset

# make small dataset with movies which were rated 3000 or more times and users who rated 250 or more movies
train_small <- train_set %>% 
  group_by(movieId) %>%
  filter(n() >= 3000) %>% ungroup() %>% 
  group_by(userId) %>%
  filter(n() >= 250) %>% ungroup()

# convert it to matrix with users in rows, movies in columns ans ratings in cells
y <- train_small %>% 
  select(userId, movieId, rating) %>%
  pivot_wider(names_from = "movieId", values_from = "rating") %>%
  as.matrix()


# add row names and column names
rownames(y)<- y[,1]
y <- y[,-1]


# name columns as movie title, instead of movieId
movie_titles <- train_set %>% 
  select(movieId, title) %>%
  distinct()

# apply to columns
colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])


# convert them to residuals by removing the column and row effects
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))
y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))


# impute dataset with PCA
pca_md <- imputePCA(y, ncp = 3)
y_md <- pca_md$completeObs
# perform Principal Components Analysis on the imputed data set
pca <- prcomp(y_md)

# plot variance explained by each of principal components
ggplot(aes(1:length(pca$sdev), (pca$sdev^2 / sum(pca$sdev^2))*100), data = NULL) + geom_col() +
  scale_y_continuous(name = "% variance explained", limits = c(0,15)) + xlab("PCs") +
  xlim(0, 30) + 
  ggtitle("Variance explained by Principal Components")+
  theme(plot.title = element_text(hjust = 0.5))

# plot cumulative variance explained by principal components
ggplot(aes(1:length(pca$sdev), cumsum(pca$sdev^2 / sum(pca$sdev^2))*100), data = NULL) + 
  geom_point(alpha = 0.5, size = 1) +
  scale_y_continuous(name = "% variance explained", limits = c(0,100)) + xlab("PCs") +
  xlim(0, length(pca$sdev)) + geom_line() +
  ggtitle("Cumulative variance explained by Principal Components")+
  theme(plot.title = element_text(hjust = 0.5))


# visualize the structure on some well-known movies
sample_movies <- c("Bourne", "Star Trek", "Truman Show", "Beauty and the Beast",
                   "Pulp Fiction", "Matrix", "Harry Potter", "Goldfinger", "Mary Poppins", "Jumanji",
                   "Shawshank Redemption", "Piano", "Trainspotting", "Birds", "Toy Story", "Contact")

pca$rotation %>% as.data.frame() %>% mutate(titles = rownames(.)) %>% 
  filter(grepl(paste(sample_movies, collapse='|'), titles)) %>%
  ggplot(aes(PC1, PC2, label = titles)) + geom_point() + ggrepel::geom_text_repel(size = 3) +
  ggtitle("Movies on PC1 and PC2") +
  theme(plot.title = element_text(hjust = 0.5))




# PC1: Sci-Fi/ Fantasy vs. Drama
# PC2: Kid's movies vs. adult movies
# We see the concept behind matrix factorization. We try to explain the matrix (= the ratings) by searching 
# for similarities between users and similarities between movies



# we can see some strong correlations between some movies
# That makes sense, that people who loves Matrix also like Start Trek, e.g.


# remove temporary variables
rm(pca_md, y_md, pca, y, sample_movies, movie_titles, train_small)




####################
# Recosystem model #
####################


# There is recosystem package for R, which uses matrix factorization.
# Recosystem is the wrapper of the 'libmf' library
# LIBMF is an open source tool for approximating an incomplete matrix using the product of two matrices in a latent space. 
# Main features of LIBMF include:
# 
# providing solvers for real-valued matrix factorization, binary matrix factorization, and one-class matrix factorization
# parallel computation in a multi-core machine
# using CPU instructions (e.g., SSE) to accelerate vector operations
# taking less than 20 minutes to converge to a reasonable level on a data set of 1.7B ratings
# cross validation for parameter selection
# supporting disk-level training, which largely reduces the memory usage

# using it we can solve our task based only by userId and MovieId and because it uses low-level CPU instruction,
# we can build a model on an avarage laptop with meaningful time


# create new model object
r = Reco()

# keep only userId, movieId (predictors) and rating (outcome) from the trainset
reco_train <- train_set %>%
  select(userId, movieId, rating)

# create an object of class "DataSource" from the new trainset, to use it in train(), tune() and predict() functions
reco_train <- with(reco_train,
                   data_memory(user_index = userId, item_index = movieId,
                               rating = rating, index1 = TRUE))

# train model with default parameters (first check)
r$train(reco_train, opts = c(niter = 20))

# predict on the testset
# keep only predictors: userId and movieId
reco_test <- test_set %>%
  select(userId, movieId)

# convert to DataSource
reco_test <- with(reco_test,
                  data_memory(user_index = userId, item_index = movieId, index1 = TRUE))

# predict
predicted_ratings <- r$predict(reco_test)

# check RMSE
RMSE(predicted_ratings, test_set$rating)

# Already better result


# cross-validation for optimal parameter
# tune() function support k-fold cross-validation, thus we will use it. 3-fold will be enough.
opts = r$tune(reco_train,
              opts = list(dim = c(5, 10, 20, 50),
                          lrate = c(0.1, 0.2),
                          costp_l1 = c(0), costq_l1 = c(0),
                          costp_l2 = c(0, 0.01, 0.1, 0.3),
                          nthread = 4, niter = 20, nfold = 3))
best_options <- opts$min


# train model with the best parameters
r$train(reco_train, opts = c(best_options, niter = 50))

# predict on test_set
predicted_ratings <- r$predict(reco_test)

# limiting rating 0.5 - 5.0, as we know real ratings can't be out of this range
predicted_ratings <- ifelse(predicted_ratings > 5, 5, ifelse(predicted_ratings < 0.5, 0.5, predicted_ratings))

# add calculated RMSE to the table
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Recosystem matrix factorization model",
                                     RMSE = RMSE(predicted_ratings, test_set$rating) ))
rmse_results

# remove variables, clean environment from variables we don't need
rm(r, opts, reco_test, reco_train, test_set, train_set, predicted_ratings)


# Train recosys model with best parameters on a complete edx dataset

# create new model object
r = Reco()

# keep only userId, movieId (predictors) and rating (outcome) from the edx
reco_edx <- edx %>%
  select(userId, movieId, rating)

# convert to recosystem DataSource object
reco_edx <- with(reco_edx,
                   data_memory(user_index = userId, item_index = movieId,
                               rating = rating, index1 = TRUE))

# train with selected by cross-validation best parameters on a complete edx dataset
r$train(reco_edx, opts = c(best_options, niter = 50))

# r is the our final model


###########################################################
#            Final test on validation dataset             #
###########################################################

# keep only userId and movieId in validation set 
reco_validation <- validation %>%
  select(userId, movieId)

# specify source for recommender system
reco_validation <- with(reco_validation,
                 data_memory(user_index = userId, item_index = movieId, index1 = TRUE))

# predict using our best model
validation_predict <- r$predict(reco_validation)

# limit predictions in range 0.5 - 5.0
validation_predict <- ifelse(validation_predict > 5, 5, ifelse(validation_predict < 0.5, 0.5, validation_predict))

# final RMSE
RMSE(validation_predict, validation$rating)


model_validation_rmse <- RMSE(validation_predict, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Final model on validation (final hold-out) set",
                                     RMSE = model_validation_rmse ))
rmse_results


# 10 random ratings from validation with our prediction:
validation %>% cbind(validation_predict) %>%
  sample_n(10) %>% select(title, rating, validation_predict)


# Most of the time we are not so far from real rating