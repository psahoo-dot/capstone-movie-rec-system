
##Movie Recommendation system

##Create train and validation sets
##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)
options(digits = 7)
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

## split edx set into two parts: train( 90% of data) and test set(10% of data)

set.seed(1, sample.kind = "Rounding")
options(digits = 7)
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1,list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

## make sure movieId and userId in the test set are also in the train set

test_set <- temp %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

## add rows removed from the temp back into the train set

removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)


rm(test_index, temp, removed)


## find the structure of edx
str(edx)

## find no. of rows and columns of edx data set
dim(edx)

## show the structure and content of the edx data set
head(edx)

##Genres:code below will show different combinations of genres in edx dataset
edx %>%
  group_by(genres) %>%
  summarise(n = n()) %>%
  head()

## Genres: create a table that lists the no. of  movies grouped by the same no. of genres movie is associated with
#showing in descending order

tibble( count = str_count(edx$genres, fixed("|")), genres = edx$genres) %>%
  group_by(count, genres) %>%
  summarise(n = n()) %>%
  arrange(desc(count)) %>%
  head()

## Date :shows the rating period

library(lubridate)
tibble('Start Date' = date(as_datetime(min(edx$timestamp), origin = "1970-01-01")), 
       'End Date' = date(as_datetime(max(edx$timestamp), origin = "1970-01-01"))) %>%
  mutate(Period = duration(max(edx$timestamp) - min(edx$timestamp)))

## shows the rating distribution per year

if(!require(ggthemes)) 
  install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(scales)) 
  install.packages("scales", repos = "http://cran.us.r-project.org")       
edx %>% mutate(year = year(as_datetime(timestamp, origin = "1970-01-01"))) %>%
  ggplot(aes(x = year)) +
  geom_histogram(color = "black") +
  ggtitle("Rating Distribution Per Year") +
  xlab("year") +
  ylab("Number of Ratings") +
  scale_y_continuous(labels = comma)
theme_economist()

## Date: create a table that lists the day with more ratings

edx %>% mutate(date = date(as_datetime(timestamp, origin ="1970-01-01"))) %>%
  group_by(date, title) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)

## Ratings : lists the number of ratings of different values of rating in the scale(0.5 through 5)

edx %>% group_by(rating) %>%
  summarize(count = n())

## Rating: Shows in the plot how 10 possible values of rating are distributed in edx dataset

edx %>% group_by(rating) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line() + 
  geom_point() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10 ^.x))) +
  ggtitle("Rating distribution")  +
  xlab("Rating")   +
  ylab("count") +
  theme_economist() 

##Rating:  in the plot above round values receive more ratings than decimal values

## Movies: shows in the plot how the movies are distributed  in terms of having the number of ratings 

edx %>% group_by(movieId) %>%
  summarise(n = n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "black") +
  scale_x_log10() +
  ggtitle("Movie Distribution") +
  xlab("number of Rating") +
  ylab("Number of movies") +
  theme_economist()

## Movies: The distribution is almost symmetric in the plot 

##Users: table below lists the no. of ratings each user has given

edx %>% group_by(userId) %>%
  summarise(n =n()) %>%
  arrange(n) %>%
  head(15)

## Users : shows in the plot how users are distributed in rating the number of movies.

edx %>% group_by(userId) %>%
  summarise(n =n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "blue") +
  scale_x_log10() +
  ggtitle("Distribution of Users") +
  xlab(" number of Rating ") +
  ylab("Number of Users") +
  theme_economist()
## Users: plot shows the distribution is right skewed

## Heatmap of users and movies (user x movie matrix)

users <- sample(unique(edx$userId), 100)
edx %>% filter(userId %in% users) %>%
  
  select(userId, movieId, rating) %>%
  mutate(rating =1) %>%
  spread(movieId,rating) %>%
  select(sample(ncol(.), 100)) %>%
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,., xlab = "Movies", ylab = "Users") %>%
  abline(h = 0:100 +0.5, v = 0:100 +0.5, col ="grey") %>%
  title("User x Movie Matrix")

## Data Cleaning:only user and movie info will be used to estimate in order to avoid model complexity 
## and less use of computer resources, so train_set and test_ set will be as follows.

train_set <- train_set %>% select(userId, movieId, rating, title)
test_set <- test_set %>% select(userId, movieId, rating, title)

## Random Prediction: evaluate RMSE by Random Prediction using Monte carlo simulation

## create probability of each rating
set.seed(1, sample.kind = "Rounding")
options(digits = 7)

p <- function(x,y) mean(y == x)
rating <- seq(0.5, 5, 0.5)

## Estimate the probability of each rating by monte carlo simulation 

B <- 10^3
M <- replicate(B, {
  s <- sample(train_set$rating, 100, replace = TRUE)
  sapply(rating,p,y = s)
})
prob <- sapply(1:nrow(M),function(x) mean(M[x,]))

## Predict random rating
y_hat_random <- sample(rating, nrow(test_set), replace = TRUE, prob = prob)

## create a table with error results

result <- tibble(Method = "Project Goal", RMSE = 0.86490)
result <- rbind(result, tibble(Method = "Random Prediction",RMSE = RMSE(test_set$rating, y_hat_random)))
result

## LInear Model: initial Prediction with  the mean of observed values of rating(mu)
##1. mean of observed values
mu <- mean(train_set$rating)

##update the error table
result <- rbind(result, tibble(Method ="Mean", RMSE = RMSE(test_set$rating, mu)))

## Show the  RMSE improvement
result

##2. Add the movie Effect(bi)

##Linear Model: movie effects(bi)
bi <- train_set %>% 
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu))
head(bi)

## plot the movie effect to show the distribution of movie effect

if(!require(ggthemes)) 
  install.packages("ggthemes", repos = "http://cran.us.r-project.org")
bi %>% ggplot(aes (x = b_i)) +
  geom_histogram(bins = 10, col = I("white")) +
  ggtitle("Movie Effect Distribution") +
  xlab("Movie Effect") +
  ylab("Count") +
  theme_economist()

##Linear Model: Predict the movie rating with mean + bi

y_hat_bi <-  mu + test_set %>%
  left_join(bi, by = "movieId") %>%
  .$b_i
##Linear Model: Calculate RMSE with mean +bi
result <- rbind(result, tibble(Method = "mean + bi", RMSE = RMSE(test_set$rating, y_hat_bi)))
result

## Linear Model: Include user effect
bu <- train_set %>%
  left_join(bi, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u  = mean(rating - mu - b_i))
head(bu)

## Linear Model : Prediction with movie and user effect
y_hat_bi_bu <-  test_set %>%
  left_join(bi, by = "movieId") %>%
  left_join(bu, by= "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
head(y_hat_bi_bu)

##Linear Model: Calculate RMSE with mean + bi + bu
result <- rbind(result, tibble(Method = "mean + bi + bu", RMSE = RMSE(test_set$rating, y_hat_bi_bu)))
result

##Linear Model: shows in the plot the  distribution of user effect for those users who have 
##rated 100 or more than 100 movies 

train_set %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating)) %>%
  filter (n() >= 100) %>%
  ggplot(aes(x = b_u)) +
  geom_histogram(col = I("Blue")) +
  ggtitle("User Effect Distribution") +
  xlab("User Effect") +
  ylab("Count") +
  theme_economist()

##Check Model Evaluation with the movie effect model even though RMSE is improved
##Check the 10 largest residual differences
train_set %>%
  left_join(bi, by = "movieId") %>%
  mutate(residual = rating -(mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%
  slice(1:10)

## Create a data set that keeps movieid and movie title
movie_titles <- train_set %>%
  select(movieId, title) %>%
  distinct()
## find out top 10 best movies according to our estimate(b_i)
bi %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>%
  select (title) %>%
  head (10)
## find out top 10 worst movies based on our estimate(b_i)
bi %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(b_i) %>%
  select(title) %>%
  head(10)

## Check how often 10 best movies are rated
train_set %>% 
  count(movieId) %>%
  left_join(bi, by = "movieId") %>%
  arrange(desc(b_i)) %>%
  slice(1:10) %>%
  pull(n)

## Regularization: pick the best value of penalty factor lambda that minimizes RMSE using the regularization
##function.

#Define a set of lambda
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
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

## plot lambda vs rmse
qplot(lambdas,rmses)

## pick the best lambda that gives the lowest RMSE
lambda <- lambdas[which.min(rmses)]
lambda


# Then, we calculate the predicted rating using the best parameters 
# achieved from regularization.  
mu <- mean(train_set$rating)

# Movie effect (bi)
b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

# User effect (bu)
b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

# Prediction
y_hat_reg <- test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

result <- rbind(result, tibble(Method = "Regularized b_i, b_u", RMSE = RMSE(test_set$rating, y_hat_reg)))
result

## Matrix Factorization: 

if(!require(recosystem)) 
  install.packages("recosystem", repos = "http://cran.us.r-project.org")
set.seed(123, sample.kind = "Rounding") # This is a randomized algorithm
options(digits = 7)

# Convert the train and test sets into recosystem input format
train_data <-  with(train_set, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))
test_data  <-  with(test_set,  data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))

# Create the model object
r <-  recosystem::Reco()

# Select the best tuning parameters
opts <- r$tune(train_data, opts = list(dim = c(10, 20, 30), 
                                       lrate = c(0.1, 0.2),
                                       costp_l2 = c(0.01, 0.1), 
                                       costq_l2 = c(0.01, 0.1),
                                       nthread  = 4, niter = 10))

# Train the algorithm  
r$train(train_data, opts = c(opts$min, nthread = 4, niter = 20))


# Calculate the predicted values  
y_hat_reco <-  r$predict(test_data, out_memory())
head(y_hat_reco, 10)

result <- bind_rows(result, 
                    tibble(Method = "Matrix Factorization - recosystem", 
                           RMSE = RMSE(test_set$rating, y_hat_reco)))
result

## FINAL VALIDATION( With edx and validation set)

## linear Model with Regularization

mu_edx <- mean(edx$rating)

# Movie effect (bi)
b_i_edx <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_edx)/(n()+lambda))

# User effect (bu)
b_u_edx <- edx %>% 
  left_join(b_i_edx, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu_edx)/(n()+lambda))

# Prediction
y_hat_edx <- validation %>% 
  left_join(b_i_edx, by = "movieId") %>%
  left_join(b_u_edx, by = "userId") %>%
  mutate(pred = mu_edx + b_i + b_u) %>%
  pull(pred)
y_hat_edx
# Update the results table
result <- bind_rows(result, 
                    tibble(Method = "Final Regularization (edx vs validation)", 
                           RMSE = RMSE(validation$rating, y_hat_edx)))
result


## Top 10 best movies
validation %>% 
  left_join(b_i_edx, by = "movieId") %>%
  left_join(b_u_edx, by = "userId") %>% 
  mutate(pred = mu_edx + b_i + b_u) %>% 
  arrange(-pred) %>% 
  group_by(title) %>% 
  select(title) %>%
  head(10)

## top 10 worst movies

validation %>% 
  left_join(b_i_edx, by = "movieId") %>%
  left_join(b_u_edx, by = "userId") %>% 
  mutate(pred = mu_edx + b_i + b_u) %>% 
  arrange(pred) %>% 
  group_by(title) %>% 
  select(title) %>%
  head(10)

## Matrix Factorization with edx and validation datasets

set.seed(1234, sample.kind = "Rounding")

# Convert 'edx' and 'validation' sets to recosystem input format
edx_reco <-  with(edx, data_memory(user_index = userId, 
                                   item_index = movieId, 
                                   rating = rating))
validation_reco  <-  with(validation, data_memory(user_index = userId, 
                                                  item_index = movieId, 
                                                  rating = rating))

# Create the model object
r <-  recosystem::Reco()

# Tune the parameters
opts <-  r$tune(edx_reco, opts = list(dim = c(10, 20, 30), 
                                      lrate = c(0.1, 0.2),
                                      costp_l2 = c(0.01, 0.1), 
                                      costq_l2 = c(0.01, 0.1),
                                      nthread  = 4, niter = 10))

# Train the model
r$train(edx_reco, opts = c(opts$min, nthread = 4, niter = 20))


# Calculate the prediction
y_hat_final_reco <-  r$predict(validation_reco, out_memory())

# Update the result table



result <- bind_rows(result, 
                    tibble(Method = "Final Matrix Factorization - recosystem", 
                           RMSE = RMSE(validation$rating, y_hat_final_reco)))
result
#Top10 best movies

tibble( Title = validation$title, Rating = y_hat_final_reco) %>%
  arrange(-Rating) %>%
  group_by(Title) %>%
  select(Title) %>%
  head(10)


##Worst 10 movies

tibble(title = validation$title, rating = y_hat_final_reco) %>%
  arrange(rating) %>%
  group_by(title) %>%
  select(title)%>%
  head(10)

