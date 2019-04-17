if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

edx_base <- edx
#edx <- edx_base

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
  ggplot(aes(x=nostalgia_idx, y=mean_rt))+geom_line()+ylim(0,5)

#Group nostalgia indices into nostalgia factors
facs <- c("0-10", "11-20", "21-30", "31-40", "41-50", "50-74", "75+")
cut_levels <- c(-0.5, 10.5, 20.5, 30.5, 40.5, 50.5, 74.5, 100)
edx <- edx %>% mutate(nostalgia_factor = cut(nostalgia_idx, cut_levels, labels=facs))

edx %>% group_by(nostalgia_factor) %>% summarize(mean_rt = mean(rating)) %>% 
  ggplot(aes(x=nostalgia_factor, y=mean_rt))+geom_point()

edx %>% ggplot(aes(x=rating))+geom_histogram(bins=5)+facet_wrap(~ nostalgia_factor, scales="free_y")

###Seperate genres
genres <- c("genre1", "genre2", "genre3", 
            "genre4", "genre5", "genre6",
            "genre7", "genre8")

edx <- edx %>% separate(genres, into=genres, 
                        sep="[|]", fill="right", remove=FALSE, extra="merge")

edx %>% ggplot(aes(x=rating))+geom_histogram(bins=5)+facet_wrap(~genre1, scales="free_y")

#edx <- edx %>% mutate_at(genres, ~ifelse(is.na(.x), "", .x))

edx[edx$genre1=="IMAX",]

###Look for users with little variance in their ratings ("pollyanas" or "debbie downers")
#Median number of ratings per user
median_au <- edx %>% select(userId, rating) %>% group_by(userId) %>% 
  summarize(n=n()) %>% ##group_by(n) %>% summarize(n2 = n()) %>% arrange(desc(n2))
  .$n %>% median()

mean_au <- edx %>% select(userId, rating) %>% group_by(userId) %>% 
  summarize(n=n()) %>% 
  .$n %>% mean()

sd_au <- edx %>% select(userId, rating) %>% group_by(userId) %>% 
  summarize(n=n()) %>% .$n %>% sd()

keep_users <- edx %>% select(userId) %>% group_by(userId) %>% 
  summarize(n=n()) %>% filter(n > 10) %>% .$userId

edx %>% filter(userId %in% keep_users) %>% group_by(userId) %>%
  summarize(rt_min = min(rating), rt_max = max(rating)) %>% 
         group_by(rng = rt_max - rt_min) %>% summarize(n=n())

    ggplot(aes(x=rng))+ geom_histogram(binwidth = 1) 

