###########################################################
#                        LIBRARIES                        #
###########################################################

# library installations if needed:
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

# loading libraries
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

###########################################################
# Create edx set, validation set (final hold-out test set)#
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
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

# Join with rating by movieID
movielens <- left_join(ratings, movies, by = "movieId")

# Brief analysis of movielens:
head(movielens)
names(movielens)

# Year in the title cannot be used for prediction. Also timestamp as it is is completely uninformative,
# therefore is being replaced by year, month and day of the week.

# Year of the movie, extracted from the title, and rating year, from the timestamp:
movielens <- movielens %>% 
              mutate(year_released = as.numeric(str_sub(title,-5,-2)), 
                     year_rated = year(as_datetime(timestamp)),
                     month_rated = month(as_datetime(timestamp)),
                     day_rated = weekdays(as_datetime(timestamp))) %>% 
              select(-timestamp)

# Check our dataset after columns modification
head(movielens)
names(movielens)
 
# slice of movielens dataset to edx and validation datasets
# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
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

# Dimentions of edx and validation datasets:
dim(edx)
dim(validation)



###########################################################
#                         Analysis                        #
###########################################################

# the number of unique users and movies in datasets
edx_unique_info <- edx %>% 
  summarise(n_user_edx = n_distinct(userId),
            n_Movie_edx = n_distinct(movieId))
edx_unique_info

# the distribution of different ratings
edx %>% ggplot(aes(rating)) + 
  geom_histogram(binwidth = 0.5, lwd = 1)+
  xlab("Rating") + ylab("Count")+ 
  ggtitle("Distibution of Movie rating") +
  theme(plot.title = element_text(hjust = 0.5)) 

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


# the distribution of number of ratings for movies
edx %>% group_by(movieId) %>% 
  summarise(number_of_ratings  = n()) %>%
  ggplot(aes(number_of_ratings)) +
  geom_histogram(binwidth = 0.5)

# Effect of the number of movie ratings on average rating of this movie
edx %>% group_by(movieId) %>% 
  summarise(number_of_ratings  = n(), avg_rating = mean(rating)) %>% 
  ggplot(aes(number_of_ratings,avg_rating)) +
  geom_point(alpha= 0.2, color = "green", lwd = 1) + 
  ggtitle("Average rating versus number of movie ratings") +
  geom_smooth(method = "loess", color = "yellow") + 
  xlab ("Number of movie ratings") +
  ylab("Average of rating") +
  theme(plot.title = element_text(hjust = 0.5)) 

# We can see, that more often rated movies are also have higher average rating

# Top 10 movies by number of ratings:
edx %>% group_by(movieId) %>% 
  summarise(number_of_ratings  = n(), avg_rating = mean(rating), title = title) %>% 
  arrange(desc(number_of_ratings)) %>% distinct() %>% head(10)

# Bottom 10 movies by number of ratings:
edx %>% group_by(movieId) %>% 
  summarise(number_of_ratings  = n(), avg_rating = mean(rating), title = title) %>% 
  arrange(number_of_ratings) %>% distinct() %>% head(10)

#We can see, that generally, movies which were rated more often, have higher average rating
#People like more popular movies


# the distribution of number of ratings for users
edx %>% group_by(userId) %>% 
  summarise(number_of_ratings_by_user  = n()) %>%
  ggplot(aes(number_of_ratings_by_user)) +
  geom_histogram(binwidth = 0.5)

# Effect of the number of movies rated by User on average rating by this user
edx %>% group_by(userId) %>% 
  summarise(number_of_ratings_by_user  = n(), avg_rating_by_user = mean(rating)) %>% 
  ggplot(aes(number_of_ratings_by_user,avg_rating_by_user)) +
  geom_point(alpha= 0.2, color = "green", lwd = 1) + 
  ggtitle("Average user rating versus number of ratings by the user") +
  geom_smooth(method = "loess", color = "yellow", se = F) + 
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


# Relation of type of genres on average of rating 
single_genres <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(number_of_genres = n(), avg_rating = mean(rating))

single_genres %>% arrange(desc(avg_rating)) 

single_genres %>% 
  arrange(desc(number_of_genres)) %>% 
  ggplot(aes(genres, avg_rating)) +
  geom_col(color = "yellow", fill = "green" , size = 1) + 
  ggtitle("Average of rating versus type of genres") +
  xlab("Genres") + 
  ylab("Average of rating") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90 , hjust = 0.5))

# Effect of released year of movieId on rating
seperate_year_released <- edx %>% group_by(year_released) %>% 
  summarise(year_released_rating = mean(rating)) %>% arrange(desc(year_released_rating))

seperate_year_released %>% 
  ggplot(aes(year_released,year_released_rating)) + 
  geom_point(color = "green", alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  ggtitle("Figure 5-Effect of released year on Rating") +
  xlab("Released year") +
  ylab("Rating") +
  theme(plot.title = element_text(hjust = 0.5))

#Movies in 1940-1960 have higher average rating 


# Effect of rating year, month and day of the week on average rating

#Year
year_of_rating <- edx %>% group_by(year_rated) %>% 
  summarise(year_rated_rating = mean(rating)) %>% arrange(desc(year_rated_rating))

year_of_rating %>% 
  ggplot(aes(year_rated,year_rated_rating)) + 
  geom_point(color = "green", alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  ggtitle("Figure 5-Effect of rated year on Rating") +
  xlab("Rated year") +
  ylab("Rating") +
  theme(plot.title = element_text(hjust = 0.5))

#month
month_of_rating <- edx %>% group_by(month_rated) %>% 
  summarise(month_rated_rating = mean(rating)) %>% arrange(desc(month_rated_rating))

month_of_rating %>% 
  ggplot(aes(month_rated,month_rated_rating)) + 
  geom_point(color = "green", alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  ggtitle("Figure 5-Effect of rated month on Rating") +
  xlab("Rated month") +
  ylab("Rating") +
  theme(plot.title = element_text(hjust = 0.5))


#day
day_of_rating <- edx %>% group_by(day_rated) %>% 
  summarise(day_rated_rating = mean(rating)) %>% arrange(desc(day_rated_rating))

day_of_rating %>% 
  ggplot(aes(day_rated,day_rated_rating)) + 
  geom_point(color = "red", alpha = 1) +
  geom_smooth(method = "loess") +
  ggtitle("Figure 5-Effect of rated day of the week on Rating") +
  xlab("Rated DoW") +
  ylab("Rating") +
  theme(plot.title = element_text(hjust = 0.5))

#Year and month of rating have effect on it (lower average rating after 2000 and lower rating during summer time)
#DoW just slightly
