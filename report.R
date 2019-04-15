genres_sep <- edx %>% separate(genres, 
                               into=c("genre1", "genre2", "genre3", 
                                      "genre4", "genre5", "genre6",
                                      "genre7", "genre8"), 
                               sep="[|]", fill="right", remove=FALSE, extra="merge")

genres <- c("genre1", "genre2", "genre3", 
            "genre4", "genre5", "genre6",
            "genre7", "genre8")

genres_sep %>% ggplot(aes(x=rating))+geom_histogram(bins=10)+facet_wrap(~genre1, scales="free_y")

genres_sep <- genres_sep %>% mutate_at(genres, ~ifelse(is.na(.x), "", .x))

genres_sep[genres_sep$genre1=="IMAX",]