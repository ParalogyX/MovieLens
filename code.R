###########################################################
#                        LIBRARIES                        #
###########################################################

# library installations if needed:
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(Matrix)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

# loading libraries
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(Matrix)

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

# Dimentions of edx and validation datasets:
dim(edx)
dim(validation)



###########################################################
#                         Analysis                        #
###########################################################

# first look at the data:
head(edx)
str(edx)



# the number of unique users and movies in datasets
edx_unique_info <- edx %>% 
  summarise(n_user_edx = n_distinct(userId),
            n_Movie_edx = n_distinct(movieId))
edx_unique_info





top_rated_movies <- edx %>% count(movieId) %>% top_n(5) %>% pull(movieId)
top_active_users <- edx %>% count(userId) %>% top_n(5) %>% pull(userId)

tab <- edx %>%
  filter(userId %in% top_active_users) %>% 
  filter(movieId %in% keep) %>% 
  select(userId, title, rating) %>% 
  spread(title, rating)

tab



users <- sample(unique(edx$userId), 100)
edx %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")


# If we multiply those two numbers, we get a number around 750 millions, yet our data table has about 9 millions rows. 
# This implies that not every user rated every movie. 
# So we can think of these data as a very large matrix, with users on the rows and movies on the columns, with many empty cells.
# To visualize it we will sample 100 movies and 100 users and make a matrix with a user/movie combination for which we have a rating:
# 
# 100 random users
by_user <- edx %>% pull(userId) %>% unique() %>% sample(size = 100)

# 100 random movies
by_movie <- edx %>% pull(movieId) %>% unique() %>% sample(size = 100)

reduced_edx <- edx %>% filter(movieId %in% by_movie, userId %in% by_user)

tst <- data.frame(by_user, by_movie)

#reduced_edx <- edx %>% filter(movieId %in% by_movie) %>% sample_n(size = 100)# %>% ungroup()


#reduced_edx <- reduced_edx[sample(1:nrow(reduced_edx), 100), ]  %>% ungroup() 



#users in reduced:
reduced_edx_unique_info <- reduced_edx %>% 
  summarise(n_user_edx = n_distinct(userId),
            n_Movie_edx = n_distinct(movieId))
reduced_edx_unique_info


edx %>% filter(movieId %in% c(52666, 37837))
edx %>% filter(movieId %in% c(37837, 52666))

tst <- reduced_edx %>% select(title, userId, rating) %>% pivot_wider(names_from = title, values_from = rating)


#first_movie <- edx %>% group_by(movieId) %>% filter(movieId == by_movie)

RatingMat <- dcast(reduced_edx, userId ~movieId, value.var = "rating")

image(as.matrix(RatingMat), col = hcl.colors(12, "Cork", rev = TRUE))

tst <- data.frame(by_movie, row.names = by_user)

# by_user <- edx %>% group_by(userId)
# tst <- slice_sample(by_user, n = 1)
# by_movie <- tst %>% group_by(movieId)
# tst2 <- slice_sample(by_movie, n = 1)
# 
# 
# 
# gg <- pivot_wider(tst2, id_cols = 'userId', names_from = 'title', values_from = 'rating')
# 
# 
# image(as.matrix(gg))
# 
# 
# gg <- sparseMatrix(i = tst$movieId, j = tst$userId)
# 
# 
# RatingMat <- !is.na(dcast(tst, userId ~movieId, value.var = "rating"))
# image(as.matrix(RatingMat))
# 
# head(RatingMat)

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
  geom_histogram(bins = 100) + scale_x_log10()


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
# We can see that the movie "Hellhounds on My Trail (1999)" also appeared in top rated movies table
# But it is based only on a single rating, which cannot be reliable. We will account it later, during model building and tuning.


# the distribution of number of ratings for users
edx %>% group_by(userId) %>% 
  summarise(number_of_ratings_by_user  = n()) %>%
  ggplot(aes(number_of_ratings_by_user)) +
  geom_histogram(bins = 100) + scale_x_log10()


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
  geom_smooth(method = "loess", se = T) +
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
  geom_smooth(method = "loess", se = T) +
  ggtitle("Figure 5-Effect of rated month on Rating") +
  xlab("Rated month") +
  ylab("Rating") +
  theme(plot.title = element_text(hjust = 0.5))


#day
day_of_rating <- edx %>% group_by(day_rated) %>% 
  summarise(day_rated_rating = mean(rating)) %>% arrange(desc(day_rated_rating))

day_of_rating %>% 
  ggplot(aes(day_rated, day_rated_rating)) + 
  geom_point(color = "red")
  ggtitle("Figure 5-Effect of rated day of the week on Rating") +
  xlab("Rated DoW") +
  ylab("Rating") +
  theme(plot.title = element_text(hjust = 0.5))

#Year and month of rating have effect on it (lower average rating after 2000 and lower rating during summer time)
#DoW just slightly (min 3.5 on Sundays and max 3.53 on Saturdays)


# Relation of type of genres on average of rating 
single_genres <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(number_of_genres = n(), avg_rating = mean(rating))

single_genres %>% arrange(desc(avg_rating)) 

single_genres %>% 
  arrange(desc(number_of_genres)) %>% 
  ggplot(aes(genres, avg_rating)) +
  geom_col(color = "red", fill = "blue" , size = 1) + 
  ggtitle("Average of rating versus type of genres") +
  xlab("Genres") + 
  ylab("Average of rating") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90 , hjust = 0.5))







#change genres to another based on their correlations





# how many moves have only one rating 
edx %>% group_by(movieId) %>%
  filter(n() == 1) %>% nrow()

# top rated movies with more than one rating
edx %>% group_by(movieId) %>% 
  filter(n() > 1) %>% 
  summarise(avg_rating = mean(rating), title = title) %>% 
  arrange(desc(avg_rating)) %>% 
  distinct() %>%
  ungroup() %>%
  select(-movieId) %>%
  head(10)