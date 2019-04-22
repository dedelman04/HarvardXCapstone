##Q1
str(edx)  #or dim(edx)

##Q2
edx %>% group_by(rating) %>% summarize(cnt = n())

###Official answer code
edx %>% filter(rating == 0) %>% tally()
edx %>% filter(rating == 3) %>% tally()
###

##Q3
mv <- edx %>% group_by(movieId) %>% summarize(cnt = n())

##Q4
usr <- edx %>% group_by(userId) %>% summarize(cnt = n())

#Official for 3 & 4 --> n_distinct(edx$userId)

##Q5
rt <- edx %>% group_by(genres) %>% summarize(cnt = n())
rt %>% filter(genres %like% "Drama") %>% .$cnt %>% sum()
rt %>% filter(genres %like% "Comedy") %>% .$cnt %>% sum()
rt %>% filter(genres %like% "Thriller") %>% .$cnt %>% sum()
rt %>% filter(genres %like% "Romance") %>% .$cnt %>% sum()

##Official answer code
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
#####

##Q6
gn <- edx %>% group_by(title) %>% summarize(cnt = n()) %>% filter(title != "")
gn[which.max(gn$cnt), ]

##Official answer code
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
#####

#Q7
edx %>% group_by(rating) %>% summarize(cnt = n()) %>%
  arrange(desc(cnt)) ##official code

#Visually
edx %>% group_by(rating) %>% summarize(cnt = n()) %>%
  ggplot(aes(x=rating, y=cnt))+geom_line()

edx %>% ggplot(aes(x=rating))+geom_histogram(bins=10)
