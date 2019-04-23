if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

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

####
#movie_yr, review_yr --> weighted "nostalgia" factor
#mean across all genres
#ignore zero variance users?

###Create "nostalgia index" - number of years between movie release and review

#Add review year
edx <- edx %>% mutate(review_yr = year(as.Date(((edx$timestamp/60)/60)/24, origin="1970-1-1")))

#Extract movie year from title
edx <- edx %>% separate(title, into=c("ttl", "movie_yr"), sep= -5, 
                        remove=FALSE, fill="right", extra="merge")

edx <- edx %>% mutate(movie_yr = as.numeric(sub(")", "", edx$movie_yr)))
edx <- edx %>% select(-ttl)

#Calculate years between movie release and review; negative numbers are assumed to be typos
#and will be set to 0
edx <- edx %>% mutate(nostalgia_idx = ifelse(review_yr < movie_yr, 0, review_yr - movie_yr))

edx %>% ggplot(aes(x=nostalgia_idx))+geom_histogram(binwidth=1)+scale_y_log10()

edx %>% group_by(nostalgia_idx) %>% summarize(mean_rt = mean(rating)) %>% 
  ggplot(aes(x=nostalgia_idx, y=mean_rt))+geom_line()
#    geom_vline(xintercept = 15)+
#    geom_vline(xintercept = 30)+
#    geom_vline(xintercept = 45)+
#    geom_vline(xintercept = 60)+
#    geom_vline(xintercept = 75)+
#    geom_vline(xintercept = 60)+
#    geom_vline(xintercept = 70)+

p <- edx %>% group_by(movie_yr) %>% summarize(mean_rt = mean(rating)) %>%
  ggplot(aes(x=movie_yr, y=mean_rt))+geom_line(color="red") #+scale_x_reverse()

edx %>% group_by(movie_yr) %>% summarize(mean_rt = mean(rating)) %>%
  cor()

review <- edx %>% group_by(review_yr) %>% summarize(mean_rt = mean(rating))

p <- p + 
  geom_line(data = edx %>% group_by(review_yr) %>% summarize(mean_rt = mean(rating)),
            mapping = aes(x=review_yr, y=mean_rt),
            color="blue") #+scale_x_reverse()

p <- p +
  geom_line(data = edx %>% group_by(nostalgia_idx) %>%
              summarize(mean_rt = mean(rating)),
            mapping = aes(x=2010-nostalgia_idx, y=mean_rt),
            color = "black")

p + legend()

edx %>% group_by(review_yr) %>% summarize(mean_rt = mean(rating)) %>%
  cor()

edx %>% group_by(nostalgia_idx) %>% summarize(mean_rt = mean(rating)) %>% 
  cor()

edx %>% filter(nostalgia_idx <= 55) %>% 
  group_by(nostalgia_idx) %>% summarize(mean_rt = mean(rating)) %>%
  cor()

edx %>% filter(nostalgia_idx > 55) %>% 
  group_by(nostalgia_idx) %>% summarize(mean_rt = mean(rating)) %>%
  cor()

#Pareto chart
nost <- edx %>% group_by(nostalgia_idx) %>% summarize(n=n()) %>% arrange(nostalgia_idx) %>% .$n
names(nost) <- distinct(data.frame(idx = edx$nostalgia_idx)) %>% arrange(idx) %>% .$idx
pareto.chart(nost, ylab="total ratings", cumperc=seq(0,100, by=5))

#Group nostalgia indices into nostalgia factors
#facs <- c("0-10", "11-20", "21-30", "31-40", "41-50", "51-74", "75+")
#cut_levels <- c(-0.5, 10.5, 20.5, 30.5, 40.5, 50.5, 74.5, 100)
facs <- c("0-20", "21-35", "36-60", "61+")
cut_levels <- c(-0.5, 20.5, 35.5, 60.5, 100)

edx <- edx %>% mutate(nostalgia_factor = cut(nostalgia_idx, cut_levels, labels=facs))

edx %>% group_by(nostalgia_factor) %>% summarize(mean_rt = mean(rating)) %>% 
  ggplot(aes(x=nostalgia_factor, y=mean_rt))+geom_point()

edx %>% ggplot(aes(x=rating))+geom_histogram(bins=5)+facet_wrap(~ nostalgia_factor, scales="free_y")

###Seperate genres
#Find the longest genre entry
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

edx %>% ggplot(aes(x=rating))+geom_histogram(bins=5)+facet_wrap(~genre1, scales="free_y")

#edx <- edx %>% mutate_at(genres, ~ifelse(is.na(.x), "", .x))

edx[edx$genre1=="IMAX",]

###Look for users with little variance ( <= 1) in their ratings

#Only look at users with 10 or more ratings
keep_users <- edx %>% select(userId) %>% group_by(userId) %>% 
  summarize(n=n()) %>% filter(n >= 10) %>% .$userId

#see the distibution of ratings ranges per user
edx %>% filter(userId %in% keep_users) %>% group_by(userId) %>%
  summarize(rt_min = min(rating), rt_max = max(rating)) %>% 
         group_by(rng = rt_max - rt_min) %>% summarize(n=n())

#Note users whose ratings range is <= 1
lowvar_users <- edx %>% group_by(userId) %>%
  summarize(rng = max(rating) - min(rating)) %>% filter(rng <= 1) %>% .$userId

#edx <- edx %>% filter(userId %in% keep_users) %>% filter(! userId %in% rem_lowvar_users )

#validation %>% group_by(userId) %>% summarize(n=n()) %>% 
#  anti_join(y = edx %>% group_by(userId) %>% summarize(n=n()), by="userId")


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

#validation <- validation %>% mutate_at(genres, ~as.factor(.x))
#validation <- validation %>% mutate_at(genres, ~as.character(.x))

## Modelling
mu <- mean(edx$rating)

RMSE <- function(x) {sqrt(mean((x - validation$rating)^2))}

RMSE(mu)

#Movie bias
movie <- edx %>% group_by(movieId) %>% summarize(bmov = mean(rating-mu))

pred <- mu + validation %>% left_join(movie, by="movieId") %>% .$bmov

RMSE(pred)

#sqrt(mean((pred - validation$rating)^2))

#User bias
user <- edx %>% group_by(userId) %>% summarize(buser = mean(rating-mu))

pred <- validation %>% left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>% mutate(pred = mu + bmov + buser) %>% .$pred

RMSE(pred)

#sqrt(mean((pred - validation$rating)^2))

#Movie Year bias
myear <- edx %>% group_by(movie_yr) %>% summarize(bmyear = mean(rating-mu))

pred <- validation %>% 
  left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>%
  left_join(myear, by = "movie_yr") %>%
  mutate(pred = mu + bmov + buser + bmyear) %>% .$pred

RMSE(pred)

#Review Year bias
ryear <- edx %>% group_by(review_yr) %>% summarize(bryear = mean(rating-mu))

pred <- validation %>% 
  left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>%
  left_join(ryear, by = "review_yr") %>%
  mutate(pred = mu + bmov + buser + bryear) %>% .$pred

RMSE(pred)

#Nostalgia index bias
nidx <- edx %>% group_by(nostalgia_idx) %>% summarize(bnost = mean(rating-mu))

pred <- validation %>% 
  left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>%
  left_join(nidx, by = "nostalgia_idx") %>%
  mutate(pred = mu + bmov + buser + bnost) %>% .$pred

RMSE(pred)

#all three??
pred <- validation %>% 
  left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>%
  left_join(myear, by = "movie_yr") %>%
  left_join(ryear, by = "review_yr") %>%
  left_join(nidx, by = "nostalgia_idx") %>%
  mutate(pred = mu + bmov + buser + bmyear + bryear + bnost) %>% .$pred

RMSE(pred)

#Nostalgia factor
nfac <- edx %>% group_by(nostalgia_factor) %>% summarize(bnfac = mean(rating-mu))

pred <- validation %>% 
  left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>%
  left_join(nfac, by = "nostalgia_factor") %>%
  mutate(pred = mu + bmov + buser + bnfac) %>% .$pred

RMSE(pred)

#####Time effects make things worse!!#####

##Genres
find_genre_mean <- function(x) {
  tmp <- edx %>% mutate( gr = ifelse (genre1 == x | genre2 == x |
                             genre3 == x | genre4 == x |
                             genre5 == x | genre6 == x |
                             genre7 == x | genre8 == x, x, NA) )
  tmp %>% filter(! is.na(gr)) %>% group_by(gr) %>% summarize(bgen = mean(rating-mu)) %>% .$bgen
}

gr <- sapply(all_genres, find_genre_mean)
grd <- data.frame(genre=names(gr), bgen = gr, stringsAsFactors = FALSE)

for (i in 1:8) {
  g <- paste("genre", i, sep="")

  validation <- validation %>%
    left_join(gr, by=c(g, "genre")) %>% mutate(grx = pred + bgen)
}

validation <- validation %>%
  left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>%
  mutate(pred = mu + bmov + buser)

