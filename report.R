if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(qcc)) install.packages("qcc", repos = "http://cran.us.r-project.org")

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
  ggplot(aes(x=movie_yr, y=mean_rt))+geom_line()+scale_x_reverse()+geom_smooth(linetype = 2, se=FALSE)

#Calculate years between movie release and review; negative numbers are assumed to be typos
#and will be set to 0
edx <- edx %>% mutate(nostalgia_idx = ifelse(review_yr < movie_yr, 0, review_yr - movie_yr))

edx %>% group_by(nostalgia_idx) %>% summarize(mean_rt = mean(rating)) %>% 
  ggplot(aes(x=nostalgia_idx, y=mean_rt))+geom_line()

###The below were attempted but didn't do much
#Pareto chart
#nost <- edx %>% group_by(nostalgia_idx) %>% summarize(n=n()) %>% arrange(nostalgia_idx) %>% .$n
#names(nost) <- distinct(data.frame(idx = edx$nostalgia_idx)) %>% arrange(idx) %>% .$idx
#pareto.chart(nost, ylab="total ratings", cumperc=seq(0,100, by=5))

#Group nostalgia indices into nostalgia factors
#facs <- c("0-10", "11-20", "21-30", "31-40", "41-50", "51-74", "75+")
#cut_levels <- c(-0.5, 10.5, 20.5, 30.5, 40.5, 50.5, 74.5, 100)
#facs <- c("0-20", "21-35", "36-60", "61+")
#cut_levels <- c(-0.5, 20.5, 35.5, 60.5, 100)
#edx <- edx %>% mutate(nostalgia_factor = cut(nostalgia_idx, cut_levels, labels=facs))

#edx %>% group_by(nostalgia_factor) %>% summarize(mean_rt = mean(rating)) %>% 
#  ggplot(aes(x=nostalgia_factor, y=mean_rt))+geom_point()

#edx %>% ggplot(aes(x=rating))+geom_histogram(bins=5)+facet_wrap(~ nostalgia_factor, scales="free_y")


###Look for users with little variance ( <= 1) in their ratings

#Only look at users with 10 or more ratings
#keep_users <- edx %>% select(userId) %>% group_by(userId) %>% 
#  summarize(n=n()) %>% filter(n >= 10) %>% .$userId

#see the distibution of ratings ranges per user
#edx %>% filter(userId %in% keep_users) %>% group_by(userId) %>%
#  summarize(rt_min = min(rating), rt_max = max(rating)) %>% 
#         group_by(rng = rt_max - rt_min) %>% summarize(n=n())

#Note users whose ratings range is <= 1
#lowvar_users <- edx %>% group_by(userId) %>%
#  summarize(rng = max(rating) - min(rating)) %>% filter(rng <= 1) %>% .$userId

#edx <- edx %>% filter(userId %in% keep_users) %>% filter(! userId %in% rem_lowvar_users )

#validation %>% group_by(userId) %>% summarize(n=n()) %>% 
#  anti_join(y = edx %>% group_by(userId) %>% summarize(n=n()), by="userId")
#####



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
  left_join(user, by="userId") %>% mutate(pred = ifelse(mu + bmov + buser > 5, 5,
                                                        mu + bmov + buser)) %>% .$pred

RMSE(pred)

#sqrt(mean((pred - validation$rating)^2))

#Movie Year bias
myear <- edx %>% group_by(movie_yr) %>% summarize(bmyear = mean(rating-mu))

pred <- validation %>% 
  left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>%
  left_join(myear, by = "movie_yr") %>%
  mutate(pred = ifelse(mu + bmov + buser + bmyear > 5, 5,
                       mu + bmov + buser + bmyear)) %>% .$pred

RMSE(pred)

#Review Year bias
ryear <- edx %>% group_by(review_yr) %>% summarize(bryear = mean(rating-mu))

pred <- validation %>% 
  left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>%
  left_join(ryear, by = "review_yr") %>%
  mutate(pred = ifelse(mu + bmov + buser + bryear > 5, 5,
                       mu + bmov + buser + bryear)) %>% .$pred

RMSE(pred)

#Nostalgia index bias
nidx <- edx %>% group_by(nostalgia_idx) %>% summarize(bnost = mean(rating-mu))

pred <- validation %>% 
  left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>%
  left_join(nidx, by = "nostalgia_idx") %>%
  mutate(pred = ifelse(mu + bmov + buser + bnost > 5, 5,
                       mu + bmov + buser + bnost)) %>% .$pred

RMSE(pred)

#all three??
pred <- validation %>% 
  left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>%
  left_join(myear, by = "movie_yr") %>%
  left_join(ryear, by = "review_yr") %>%
  left_join(nidx, by = "nostalgia_idx") %>%
  mutate(pred = ifelse(mu + bmov + buser + bmyear + bryear + bnost > 5, 5,
                       mu + bmov + buser + bmyear + bryear + bnost)) %>% .$pred

RMSE(pred)

#Nostalgia factor
nfac <- edx %>% group_by(nostalgia_factor) %>% summarize(bnfac = mean(rating-mu))

pred <- validation %>% 
  left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>%
  left_join(nfac, by = "nostalgia_factor") %>%
  mutate(pred = ifelse(mu + bmov + buser + bnfac > 5, 5,
                       mu + bmov + buser + bnfac)) %>% .$pred

RMSE(pred)

#####Time effects make things worse!!#####

##Genres
find_genre_mean <- function(x) {
  tmp <- edx %>% mutate( gr = ifelse (genre1 == x | genre2 == x |
                             genre3 == x | genre4 == x |
                             genre5 == x | genre6 == x |
                             genre7 == x | genre8 == x, x, NA) )
  tmp %>% filter(! is.na(gr)) %>% group_by(gr) %>% summarize(mean_rt = mean(rating)) %>% .$mean_rt
}

gr <- sapply(all_genres, find_genre_mean)
grd <- data.frame(genre=names(gr), bgen = gr, stringsAsFactors = FALSE)

#validation <- validation %>% select(-c(bgen.x, bgen.y, bgen))

validation <- validation %>%
  left_join(grd, by=c("genre1" = "genre")) %>%
  mutate(bgen1 = bgen) %>% 
  select(-bgen)

validation <- validation %>%
  left_join(grd, by=c("genre2" = "genre")) %>%
  mutate(bgen2 = bgen) %>% 
  select(-bgen)

validation <- validation %>%
  left_join(grd, by=c("genre3" = "genre")) %>%
  mutate(bgen3 = bgen) %>% 
  select(-bgen)

validation <- validation %>%
  left_join(grd, by=c("genre4" = "genre")) %>%
  mutate(bgen4 = bgen) %>% 
  select(-bgen)

validation <- validation %>%
  left_join(grd, by=c("genre5" = "genre")) %>%
  mutate(bgen5 = bgen) %>% 
  select(-bgen)

validation <- validation %>%
  left_join(grd, by=c("genre6" = "genre")) %>%
  mutate(bgen6 = bgen) %>% 
  select(-bgen)

validation <- validation %>%
  left_join(grd, by=c("genre7" = "genre")) %>%
  mutate(bgen7 = bgen) %>% 
  select(-bgen)

validation <- validation %>%
  left_join(grd, by=c("genre8" = "genre")) %>%
  mutate(bgen8 = bgen) %>% 
  select(-bgen)

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

mean_genre <- validation %>% select(movieId, userId,
                      bgen1, bgen2, bgen3, bgen4,
                      bgen5, bgen6, bgen7, bgen8) %>%
  gather(key=effect, value=genre_effect, -c(movieId, userId))

mean_genre <- mean_genre %>%
  group_by(movieId, userId) %>% summarize(mean_bgen = mean(genre_effect, na.rm=TRUE))

pred <- validation %>%
  left_join(movie, by="movieId") %>%
  left_join(user, by="userId") %>%
  left_join(mean_genre, by=c("movieId", "userId")) %>%
  mutate(pred = ifelse(mu + bmov + buser + mean_bgen > 5, 5,
                       mu + bmov + buser + mean_bgen)) %>% .$pred

RMSE(pred)

###User+genre effect
gu <- rbind(edx %>% select(userId, genre1, rating) %>%
              mutate(genre = genre1) %>% select(-genre1),
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

gu <- gu %>% group_by(userId, genre) %>% summarize(mean_rt = mean(rating-mu))

gu %>% ggplot(aes(x=userId, y=mean_rt))+geom_point()+
  geom_hline(aes(yintercept = bgen+mu), data=grd, color = "red")+facet_wrap(~genre)

gu %>% ggplot(aes(x=mean_rt))+geom_histogram(binwidth=0.1)+
  geom_vline(aes(xintercept = bgen+mu), data=grd, color = "red")+facet_wrap(~genre, scales="free_y")


user %>% ggplot(aes(x=userId, y=buser+mu))+geom_point()+
  geom_hline(aes(yintercept = mu), color = "red")

user %>% ggplot(aes(x=buser+mu))+geom_histogram(binwidth=0.1)

validation <- validation %>%
  left_join(gu, by=c("userId", "genre1" = "genre")) %>%
  mutate(bgu1 = mean_rt) %>% 
  select(-mean_rt)

validation <- validation %>%
  left_join(gu, by=c("userId", "genre2" = "genre")) %>%
  mutate(bgu2 = mean_rt) %>% 
  select(-mean_rt)

validation <- validation %>%
  left_join(gu, by=c("userId", "genre3" = "genre")) %>%
  mutate(bgu3 = mean_rt) %>% 
  select(-mean_rt)

validation <- validation %>%
  left_join(gu, by=c("userId", "genre4" = "genre")) %>%
  mutate(bgu4 = mean_rt) %>% 
  select(-mean_rt)

validation <- validation %>%
  left_join(gu, by=c("userId", "genre5" = "genre")) %>%
  mutate(bgu5 = mean_rt) %>% 
  select(-mean_rt)

validation <- validation %>%
  left_join(gu, by=c("userId", "genre6" = "genre")) %>%
  mutate(bgu6 = mean_rt) %>% 
  select(-mean_rt)

validation <- validation %>%
  left_join(gu, by=c("userId", "genre7" = "genre")) %>%
  mutate(bgu7 = mean_rt) %>% 
  select(-mean_rt)

validation <- validation %>%
  left_join(gu, by=c("userId", "genre8" = "genre")) %>%
  mutate(bgu8 = mean_rt) %>% 
  select(-mean_rt)

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
