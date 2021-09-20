if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")



library(tidyverse)
library(caret)
library(data.table)
library(lubridate)


#Load data unless it is already loaded
if(!(exists("edx") & exists("validation"))) {source("create-sets.R")}

#look on edx dataset
head(edx)

#how many different movies:
length(unique(edx$movieId))

#how many different users:
length(unique(edx$userId))

#Add data columns which can be valuable:
#Year of the movie, extracted from the title:
edx_prep <- edx %>% mutate(year_released = as.numeric(str_sub(title,-5,-2)), year_rated = year(as_datetime(timestamp))) %>% select(-timestamp)

#Unique genres and dummy them
unique(edx_prep$genres)

single_genres <- edx_prep %>% separate_rows(genres, sep = "\\|") %>% group_by(genres) %>% 
  summarise(number_of_genres = n(), avg_rating = mean(rating))  


#Check if no genre
edx_prep %>% filter(genres == "(no genres listed)")
#only one movie

#how many user-movie combinations which are unrated
sum(is.na(edx$rating))
