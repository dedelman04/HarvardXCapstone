if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(qcc)) install.packages("qcc", repos = "http://cran.us.r-project.org")

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

set.seed(1)
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

#To reset if something messes up
#edx_base <- edx
#validation_base <- validation
#edx <- edx_base
#validation <- validation_base

#Distro of ratings
edx %>% ggplot(aes(x=rating))+geom_histogram(stat="count")

#Distro of ratings with .5 ratings rounded down
edx %>% mutate(gp_rating = floor(rating)) %>%
  ggplot(aes(x=gp_rating))+geom_histogram(stat="count")+
  xlab("rating")

#Mean rating
mu <- mean(edx$rating)

###Seperate genres
#Find the longest genre entry
dim(table(edx$genres))
g <- names(table(edx$genres))
g[which.max(str_length(g))]

#Since the longest has 8, we will split into 8 columns
#(NA will populate where there are not 8 genres)
genres <- c("genre1", "genre2", "genre3", 
            "genre4", "genre5", "genre6",
            "genre7", "genre8")

edx <- edx %>% separate(genres, into=genres, 
                        sep="[|]", fill="right", remove=FALSE, extra="merge")

edx <- edx %>% mutate_at(genres, ~as.factor(.x))

all_genres <- rbind(distinct(data.frame(genre = levels(edx$genre1))),
                    distinct(data.frame(genre = levels(edx$genre2))),
                    distinct(data.frame(genre = levels(edx$genre3))),
                    distinct(data.frame(genre = levels(edx$genre4))),
                    distinct(data.frame(genre = levels(edx$genre5))),
                    distinct(data.frame(genre = levels(edx$genre6))),
                    distinct(data.frame(genre = levels(edx$genre7))),
                    distinct(data.frame(genre = levels(edx$genre8))))

all_genres <- as.character(distinct(all_genres)$genre)

edx <- edx %>% mutate_at(genres, ~as.character(.x))

edx %>% mutate(gp_rating = floor(rating)) %>%
  ggplot(aes(x=gp_rating))+geom_histogram(stat="count")+facet_wrap(~genre1, scales="free_y")


###Create "nostalgia index" - number of years between movie release and review
#Add review year
edx <- edx %>% mutate(review_yr = year(as.Date(((edx$timestamp/60)/60)/24, origin="1970-1-1")))

table(edx$review_yr) %>% as.data.frame() %>% arrange(desc(Freq))

#Plot review year against mean rating, newer reviews first
edx %>% filter(review_yr != 1995 & review_yr != 2009) %>%
  group_by(review_yr) %>% summarize(mean_rt = mean(rating)) %>%
  ggplot(aes(x=review_yr, y=mean_rt))+geom_line()+scale_x_reverse()

    
#Extract movie year from title
edx <- edx %>% separate(title, into=c("ttl", "movie_yr"), sep= -5, 
                        remove=FALSE, fill="right", extra="merge")

edx <- edx %>% mutate(movie_yr = as.numeric(sub(")", "", edx$movie_yr)))
edx <- edx %>% select(-ttl)

range(edx$movie_yr)

#Plot movie year against mean rating, newer movies first
edx %>% group_by(movie_yr) %>% summarize(mean_rt = mean(rating)) %>%
  ggplot(aes(x=movie_yr, y=mean_rt))+geom_line()+scale_x_reverse()+
  geom_smooth(linetype = 2, se=FALSE)

#Calculate "nostaliga index" - years between movie release and review;
#negative numbers are assumed to be typos and will be set to 0
edx <- edx %>% mutate(nostalgia_idx = ifelse(review_yr < movie_yr, 0, review_yr - movie_yr))

edx %>% group_by(nostalgia_idx) %>% summarize(mean_rt = mean(rating)) %>% 
  ggplot(aes(x=nostalgia_idx, y=mean_rt))+geom_line()

#User+genre
#Find genre mean to compare to user/genre means
find_genre_mean <- function(x) {
  tmp <- edx %>% mutate( gr = ifelse (genre1 == x | genre2 == x |
                                        genre3 == x | genre4 == x |
                                        genre5 == x | genre6 == x |
                                        genre7 == x | genre8 == x, x, NA) )
  tmp %>% filter(! is.na(gr)) %>% group_by(gr) %>% summarize(mean_rt = mean(rating)) %>% .$mean_rt
}

gr <- sapply(all_genres, find_genre_mean)
grd <- data.frame(genre=names(gr), mean_rt = gr, stringsAsFactors = FALSE)

#Find user/genre pairs
gu <- rbind(edx %>% select(userId, genre1, rating) %>% mutate(genre = genre1) %>% select(-genre1),
            edx %>% filter(! is.na(genre2)) %>% 
              select(userId, genre2, rating) %>% mutate(genre = genre2) %>% select(-genre2),
            edx %>% filter(! is.na(genre3)) %>% 
              select(userId, genre3, rating) %>% mutate(genre = genre3) %>% select(-genre3),
            edx %>% filter(! is.na(genre4)) %>% 
              select(userId, genre4, rating) %>% mutate(genre = genre4) %>% select(-genre4),
            edx %>% filter(! is.na(genre5)) %>% 
              select(userId, genre5, rating) %>% mutate(genre = genre5) %>% select(-genre5),
            edx %>% filter(! is.na(genre6)) %>% 
              select(userId, genre6, rating) %>% mutate(genre = genre6) %>% select(-genre6),
            edx %>% filter(! is.na(genre7)) %>% 
              select(userId, genre7, rating) %>% mutate(genre = genre7) %>% select(-genre7),
            edx %>% filter(! is.na(genre8)) %>% 
              select(userId, genre8, rating) %>% mutate(genre = genre8) %>% select(-genre8)
)

gu <- gu %>% group_by(userId, genre) %>% summarize(mean_rt = mean(rating))

#User vs User/Genre plots
gu %>% ggplot(aes(x=userId, y=mean_rt))+geom_point()+
  geom_point(data = user, aes(x=userId, y=mu+buser), color="green")+
  geom_hline(aes(yintercept = mean_rt), data=grd, color = "red")+facet_wrap(~genre)

gu %>% ggplot(aes(x=mean_rt))+geom_histogram(binwidth=0.1)+
  geom_vline(aes(xintercept = mean_rt), data=grd, color = "red")+facet_wrap(~genre, scales="free_y")


user %>% ggplot(aes(x=userId, y=buser+mu))+geom_point()+
  geom_hline(aes(yintercept = mu), color = "red")

user %>% ggplot(aes(x=buser+mu))+geom_histogram(binwidth=0.1)


###Do same data transformations to validation set
#Review Year
validation <- validation %>% 
  mutate(review_yr = year(as.Date(((validation$timestamp/60)/60)/24, origin="1970-1-1")))

#Movie Year
validation <- validation %>% separate(title, into=c("ttl", "movie_yr"), sep= -5, 
                        remove=FALSE, fill="right", extra="merge")

validation <- validation %>% 
  mutate(movie_yr = as.numeric(sub(")", "", validation$movie_yr))) %>% 
  select(-ttl)

#Nostalgia Index
validation <- validation %>% 
  mutate(nostalgia_idx = ifelse(review_yr < movie_yr, 0, review_yr - movie_yr))

#Nostalgia factor
validation <- validation %>% mutate(nostalgia_factor = cut(nostalgia_idx, cut_levels, labels=facs))

#Genres
validation <- validation %>% separate(genres, into=genres, 
                        sep="[|]", fill="right", remove=FALSE, extra="merge")


### Modelling
## Naive population mean
mu <- mean(edx$rating)

RMSE <- function(x) {sqrt(mean((x - validation$rating)^2))}

RMSE(mu)

results <- data.frame(model = "Population Mean", RMSE = RMSE(mu))

##Movie bias
#Find the mean effect per movie
movie <- edx %>% group_by(movieId) %>% summarize(bmov = mean(rating-mu))

#Predict by adding the movie effect to the population mean
pred <- validation %>% left_join(movie, by="movieId") %>%
  mutate(pred = ifelse(bmov+mu > 5, 5, bmov+mu)) %>% .$pred

RMSE(pred)

results <- rbind(results, data.frame(model = "Movie Effect", RMSE = RMSE(pred)))

##User bias
#Find the mean effect per user
user <- edx %>% group_by(userId) %>% summarize(buser = mean(rating-mu))

#Predict by adding the user effect to the population mean
pred <- validation %>% left_join(user, by="userId") %>% 
  mutate(pred = ifelse(mu + buser > 5, 5, mu + buser)) %>% .$pred

RMSE(pred)

results <- rbind(results, data.frame(model = "User Effect only", RMSE = RMSE(pred)))

#Predict by adding the user effect to the movie effect algorithm
pred <- validation %>% left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>% mutate(pred = ifelse(mu + bmov + buser > 5, 5,
                                                        mu + bmov + buser)) %>% .$pred

RMSE(pred)

results <- rbind(results, data.frame(model = "Movie Effect + User Effect", RMSE = RMSE(pred)))


##Movie Year bias - NOT PUT IN REPORT
myear <- edx %>% group_by(movie_yr) %>% summarize(bmyear = mean(rating-mu))

pred <- validation %>% 
  left_join(myear, by = "movie_yr") %>%
  mutate(pred = ifelse(mu + bmyear > 5, 5,
                       mu + bmyear)) %>% .$pred

RMSE(pred)

pred <- validation %>% 
  left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>%
  left_join(myear, by = "movie_yr") %>%
  mutate(pred = ifelse(mu + bmov + buser + bmyear > 5, 5,
                       mu + bmov + buser + bmyear)) %>% .$pred

RMSE(pred)

##Review Year bias - NOT PUT IN REPORT
ryear <- edx %>% group_by(review_yr) %>% summarize(bryear = mean(rating-mu))

pred <- validation %>% 
  left_join(ryear, by = "review_yr") %>%
  mutate(pred = ifelse(mu + bryear > 5, 5,
                       mu + bryear)) %>% .$pred

RMSE(pred)

pred <- validation %>% 
  left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>%
  left_join(ryear, by = "review_yr") %>%
  mutate(pred = ifelse(mu + bmov + buser + bryear > 5, 5,
                       mu + bmov + buser + bryear)) %>% .$pred

RMSE(pred)

##Nostalgia index bias
#Just nostalgia index
nidx <- edx %>% group_by(nostalgia_idx) %>% summarize(bnost = mean(rating-mu))

pred <- validation %>% 
  left_join(nidx, by = "nostalgia_idx") %>%
  mutate(pred = ifelse(mu + bnost > 5, 5,
                       mu + bnost)) %>% .$pred

RMSE(pred)

results <- rbind(results, data.frame(model = "Nostalgia Index Bias", RMSE = RMSE(pred)))

#Movie + User + Nostalgia
pred <- validation %>% 
  left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>%
  left_join(nidx, by = "nostalgia_idx") %>%
  mutate(pred = ifelse(mu + bmov + buser + bnost > 5, 5,
                       mu + bmov + buser + bnost)) %>% .$pred

RMSE(pred)

results <- rbind(results, data.frame(model = "Movie + User + Nostalgia", RMSE = RMSE(pred)))

##all three?? - NOT PUT IN REPORT
pred <- validation %>% 
  left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>%
  left_join(myear, by = "movie_yr") %>%
  left_join(ryear, by = "review_yr") %>%
  left_join(nidx, by = "nostalgia_idx") %>%
  mutate(pred = ifelse(mu + bmov + buser + bmyear + bryear + bnost > 5, 5,
                       mu + bmov + buser + bmyear + bryear + bnost)) %>% .$pred

RMSE(pred)

#####Time effects make things worse!!#####

##Genres
find_genre_effect <- function(x) {
  tmp <- edx %>% mutate( gr = ifelse (genre1 == x | genre2 == x |
                                        genre3 == x | genre4 == x |
                                        genre5 == x | genre6 == x |
                                        genre7 == x | genre8 == x, x, NA) )
  tmp %>% filter(! is.na(gr)) %>% group_by(gr) %>% summarize(bgen = mean(rating-mu)) %>% .$bgen
}

gre <- sapply(all_genres, find_genre_effect)
gred <- data.frame(genre=names(gre), bgen = gre, stringsAsFactors = FALSE)

validation <- validation %>%
  left_join(gred, by=c("genre1" = "genre")) %>%
  mutate(bgen1 = bgen) %>% 
  select(-bgen)

validation <- validation %>%
  left_join(gred, by=c("genre2" = "genre")) %>%
  mutate(bgen2 = bgen) %>% 
  select(-bgen)

validation <- validation %>%
  left_join(gred, by=c("genre3" = "genre")) %>%
  mutate(bgen3 = bgen) %>% 
  select(-bgen)

validation <- validation %>%
  left_join(gred, by=c("genre4" = "genre")) %>%
  mutate(bgen4 = bgen) %>% 
  select(-bgen)

validation <- validation %>%
  left_join(gred, by=c("genre5" = "genre")) %>%
  mutate(bgen5 = bgen) %>% 
  select(-bgen)

validation <- validation %>%
  left_join(gred, by=c("genre6" = "genre")) %>%
  mutate(bgen6 = bgen) %>% 
  select(-bgen)

validation <- validation %>%
  left_join(gred, by=c("genre7" = "genre")) %>%
  mutate(bgen7 = bgen) %>% 
  select(-bgen)

validation <- validation %>%
  left_join(gred, by=c("genre8" = "genre")) %>%
  mutate(bgen8 = bgen) %>% 
  select(-bgen)

#Total Genre effect
validation <- validation %>% 
  mutate(total_bgen = ifelse(is.na(bgen1),0,bgen1)+
           ifelse(is.na(bgen2),0,bgen2)+
           ifelse(is.na(bgen3),0,bgen3)+
           ifelse(is.na(bgen4),0,bgen4)+
           ifelse(is.na(bgen5),0,bgen5)+
           ifelse(is.na(bgen6),0,bgen6)+
           ifelse(is.na(bgen7),0,bgen7)+
           ifelse(is.na(bgen8),0,bgen8))

pred <- validation %>%
  left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>%
  mutate(pred = ifelse(mu + bmov + buser+total_bgen > 5, 5,
                       mu + bmov + buser+total_bgen)) %>% .$pred

RMSE(pred)

results <- rbind(results, data.frame(model = "Movie + User + Total Genre", RMSE = RMSE(pred)))

##Mean genre by movie
mean_genre <- validation %>% select(movieId,
                                    bgen1, bgen2, bgen3, bgen4,
                                    bgen5, bgen6, bgen7, bgen8) %>%
  gather(key=effect, value=genre_effect, -movieId)

mean_genre <- mean_genre %>%
  group_by(movieId) %>% summarize(mean_bgen = mean(genre_effect, na.rm=TRUE))

pred <- validation %>%
  left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>%
  left_join(mean_genre, by="movieId") %>%
  mutate(pred = ifelse(mu + bmov + buser + mean_bgen > 5, 5,
                       mu + bmov + buser + mean_bgen)) %>% .$pred

RMSE(pred)

results <- rbind(results, data.frame(model = "Movie + User + Mean Genre", RMSE = RMSE(pred)))

##User+genre effect
gu <- rbind(edx %>% select(userId, genre1, rating) %>% mutate(genre = genre1) %>% select(-genre1),
            edx %>% filter(! is.na(genre2)) %>% 
              select(userId, genre2, rating) %>% mutate(genre = genre2) %>% select(-genre2),
            edx %>% filter(! is.na(genre3)) %>% 
              select(userId, genre3, rating) %>% mutate(genre = genre3) %>% select(-genre3),
            edx %>% filter(! is.na(genre4)) %>% 
              select(userId, genre4, rating) %>% mutate(genre = genre4) %>% select(-genre4),
            edx %>% filter(! is.na(genre5)) %>% 
              select(userId, genre5, rating) %>% mutate(genre = genre5) %>% select(-genre5),
            edx %>% filter(! is.na(genre6)) %>% 
              select(userId, genre6, rating) %>% mutate(genre = genre6) %>% select(-genre6),
            edx %>% filter(! is.na(genre7)) %>% 
              select(userId, genre7, rating) %>% mutate(genre = genre7) %>% select(-genre7),
            edx %>% filter(! is.na(genre8)) %>% 
              select(userId, genre8, rating) %>% mutate(genre = genre8) %>% select(-genre8)
)

gue <- gu %>% group_by(userId, genre) %>% summarize(bgu = mean(rating-mu))


validation <- validation %>%
  left_join(gue, by=c("userId", "genre1" = "genre")) %>%
  mutate(bgu1 = bgu) %>% 
  select(-bgu)

validation <- validation %>%
  left_join(gue, by=c("userId", "genre2" = "genre")) %>%
  mutate(bgu2 = bgu) %>% 
  select(-bgu)

validation <- validation %>%
  left_join(gue, by=c("userId", "genre3" = "genre")) %>%
  mutate(bgu3 = bgu) %>% 
  select(-bgu)

validation <- validation %>%
  left_join(gue, by=c("userId", "genre4" = "genre")) %>%
  mutate(bgu4 = bgu) %>% 
  select(-bgu)

validation <- validation %>%
  left_join(gue, by=c("userId", "genre5" = "genre")) %>%
  mutate(bgu5 = bgu) %>% 
  select(-bgu)

validation <- validation %>%
  left_join(gue, by=c("userId", "genre6" = "genre")) %>%
  mutate(bgu6 = bgu) %>% 
  select(-bgu)

validation <- validation %>%
  left_join(gue, by=c("userId", "genre7" = "genre")) %>%
  mutate(bgu7 = bgu) %>% 
  select(-bgu)

validation <- validation %>%
  left_join(gue, by=c("userId", "genre8" = "genre")) %>%
  mutate(bgu8 = bgu) %>% 
  select(-bgu)

#Total bgu - NOT PUT IN REPORT
validation <- validation %>% 
  mutate(total_bgu = ifelse(is.na(bgu1),0,bgu1)+
           ifelse(is.na(bgu2),0,bgu2)+
           ifelse(is.na(bgu3),0,bgu3)+
           ifelse(is.na(bgu4),0,bgu4)+
           ifelse(is.na(bgu5),0,bgu5)+
           ifelse(is.na(bgu6),0,bgu6)+
           ifelse(is.na(bgu7),0,bgu7)+
           ifelse(is.na(bgu8),0,bgu8))

pred <- validation %>% 
  left_join(movie, by="movieId") %>%
  mutate(pred = ifelse(mu+bmov+total_bgu > 5, 5,
                       mu+bmov+total_bgu)) %>% .$pred

RMSE(pred)

#Mean BGU
mean_gu <- validation %>% select(movieId, userId,
                                 bgu1, bgu2, bgu3, bgu4,
                                 bgu5, bgu6, bgu7, bgu8) %>%
  gather(key=effect, value=gu_effect, -c(movieId, userId))

mean_gu <- mean_gu %>%
  group_by(movieId, userId) %>% 
  summarize(mean_bgu = ifelse(is.na(mean(gu_effect, na.rm = TRUE)), 0, mean(gu_effect, na.rm=TRUE)))

pred <- validation %>%
  left_join(movie, by="movieId") %>%
  left_join(mean_gu, by=c("movieId", "userId")) %>%
  mutate(pred = ifelse(mu + bmov + mean_bgu > 5, 5,
                       mu + bmov + mean_bgu)) %>% .$pred

RMSE(pred)

results <- rbind(results, data.frame(model = "Movie + Mean User/Genre", RMSE = RMSE(pred)))

##Output the predicted ratings
validation <- validation %>% 
  select(-rating) %>% 
  left_join(movie, by="movieId") %>%
  left_join(mean_gu, by=c("movieId", "userId")) %>%
  mutate(pred = ifelse(mu + bmov + mean_bgu > 5, 5,
                       mu + bmov + mean_bgu)) %>%
  select(userId, movieId, timestamp, title, genres, pred)

# Ratings will go into the CSV submission file below:
write.csv(validation,
          "submission.csv", na = "", row.names=FALSE)