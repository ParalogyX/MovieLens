library(tidyverse)


#if dataset is loaded and splitted on edx and validation - do nothong, 
#otherwise call create-sets.R, which will load and prepare dataset for analysis

if(!(exists("edx") & exists("validation"))) {source("create-sets.R")}

#Q1
print(c("Number of rows in edx: ", nrow(edx)), quote = FALSE)
print(c("Number of columns in edx: ", ncol(edx)), quote = FALSE)

#Q2
print(c("Number of zero ratings in edx: ", sum(edx$rating == 0)), quote = FALSE)
print(c("Number of ratings three in edx: ", sum(edx$rating == 3)), quote = FALSE)

#Q3
print(c("Number of different movies in edx: ", length(unique(edx$movieId))), quote = FALSE)

#Q4
print(c("Number of different users in edx: ", length(unique(edx$userId))), quote = FALSE)

#Q5
print(c("Number of Drama movie ratings in edx: ", sum(str_detect(edx$genres, "Drama"))), quote = FALSE)
print(c("Number of Comedy movie ratings in edx: ", sum(str_detect(edx$genres, "Comedy"))), quote = FALSE)
print(c("Number of Thriller movie ratings in edx: ", sum(str_detect(edx$genres, "Thriller"))), quote = FALSE)
print(c("Number of Romance movie ratings in edx: ", sum(str_detect(edx$genres, "Romance"))), quote = FALSE)

#Q6
most_rated_movie_title <- edx %>% group_by(movieId) %>%
                                 mutate(n = n()) %>%
                                 arrange(desc(n)) %>% 
                                  distinct(title) %>% 
                                  head(1) %>% pull(title)
print(c("The greatest number of ratings has the movie: ", most_rated_movie_title), quote = FALSE)


#Q7
five_most_given_ratings <- edx %>% group_by(rating) %>%
  mutate (n = n()) %>%
  arrange(desc(n)) %>% 
  distinct(rating) %>% head(5) %>% pull(rating)
print("Five most given ratings in order from most to least: ", quote = FALSE)
print(five_most_given_ratings)


#Q8
half_star_less_common <- sum(edx$rating != round(edx$rating)) < sum(edx$rating == round(edx$rating))
print(c("Statement that half star ratings are less common than whole star ratings is ", half_star_less_common), quote = FALSE)



