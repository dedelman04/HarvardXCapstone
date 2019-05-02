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
