###########################################################
#                        LIBRARIES                        #
###########################################################

# clear console output
cat("\014")


# library installations if needed:
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(Matrix)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(pheatmap)) install.packages("pheatmap", repos = "http://cran.us.r-project.org")

# loading libraries
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(Matrix)
library(stringr)
library(pheatmap)


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

#Comparing rating types
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
paste(nrow(edx) / (edx_unique_info$n_user_unique * edx_unique_info$n_Movie_unique) * 100, "%")


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
                " User/movie combinations are rated (", 
                round(sum(!is.na(sample_matrix))/sum(is.na(sample_matrix)) * 100, 1), " %) \n"))
mtext(paste(sum(is.na(sample_matrix)), " User/movie combinations are unrated"))
title("User/Movie combinations", line = 3)



rm(users, edx_unique_info)

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


# the distribution of number of ratings for movies
edx %>% group_by(movieId) %>% 
  summarise(number_of_ratings  = n()) %>%
  ggplot(aes(number_of_ratings)) +
  geom_histogram(bins = 100, col = "black") + scale_x_log10() +
  ggtitle("Distribution of movies by no. of rating") +
  theme(plot.title = element_text(hjust = 0.5))


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


# the distribution of number of ratings for users
edx %>% group_by(userId) %>% 
  summarise(number_of_ratings_by_user  = n()) %>%
  ggplot(aes(number_of_ratings_by_user)) +
  geom_histogram(bins = 100, col = "black") + scale_x_log10()



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
  geom_point(color = "red") +
  ggtitle("Figure 5-Effect of rated day of the week on Rating") +
  xlab("Rated DoW") +
  ylab("Rating") +
  theme(plot.title = element_text(hjust = 0.5))

#Year and month of rating have effect on it (lower average rating after 2000 and lower rating during summer time)
#DoW just slightly (min 3.5 on Sundays and max 3.53 on Saturdays)


# genres analysis
n_distinct(edx$genres)

head(edx$genres, 10)

# We can see, that genres are combined in 797 different groups

# Relation of type of genres on average of rating 
single_genres <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(number_of_ratings = n(), avg_rating = mean(rating))

single_genres %>% arrange(desc(avg_rating)) 


single_genres %>% 
  arrange(desc(number_of_genres)) %>% 
  ggplot(aes(genres, avg_rating)) +
  geom_col(color = "black" ) + 
  ggtitle("Average rating versus separated genres") +
  xlab("Genres") + 
  ylab("Average rating") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90 , hjust = 0.5))

edx %>% filter(genres == "(no genres listed)") %>% group_by(movieId) %>% pull(title) %>% unique()

# Try to find combinations which are most common to reduce variability of genres combinations
# We will do it in form of correlation matrix, where numbers in cells shows how many times the specific genre appears in combination with 
# another one

genre_cor <- setNames(data.frame(matrix(ncol = 20, nrow = 20)), single_genres$genres) 
genre_cor[1,1] <- 1

genres_list <- single_genres$genres

genres_combinations <- sapply(genres_list, function(x){
  combination <- edx %>% filter(grepl(x, genres, fixed = TRUE)) %>% select(genres) %>%unique()
  str_count(as.character(combination), genres_list)
})

rownames(genres_combinations) <- genres_list

genres_combinations

heatmap(x = genres_combinations, Colv=NA, Rowv=NA)

pheatmap(genres_combinations, display_numbers = T, color = colorRampPalette(c('white','red'))(100), cluster_rows = F, cluster_cols = F, fontsize_number = 15)



genres_combinations %>% mutate(genre = genres_list)

genres_combinations %>% ggplot(aes(x = ))

action_genres_combinations <- edx %>% filter(grepl(colnames(genre_cor)[2], genres, fixed = TRUE)) %>% select(genres) %>%unique()# %>% slice(1:100)

str_count(as.character(action_genres_combinations), genres_list)



grepl("Comedy", action_genres_combinations, fixed = TRUE)

grepl(colnames(genre_cor)[2], edx$genres %>% filter())


#colnames(genre_cor) <- single_genres$genres
#change genres to another based on their correlations


#Creating genres matrix
mg_mat <- single_genres %>%
  mutate(genre_value = 1) %>%
  pivot_wider(movieId,names_from = genres,values_from=genre_value,
              values_fill = 0,values_fn = mean)


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