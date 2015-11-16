# Return the mean of ratings before and after oscar nominations 
mean_before_after <- function(movies_ratings, year_movie, date_nominations, nominations) {
  before_oscar <- movies_ratings  %>% 
    filter(rating_timestamp < date_nominations)  %>% 
    filter(movie_id %in% filter(nominations, year == year_movie)$cod_imdb)
  
  after_oscar <- movies_ratings  %>% 
    filter(rating_timestamp > date_nominations) %>% 
    filter(movie_id %in% filter(nominations, year == year_movie)$cod_imdb)
  
  group_before_oscar <- group_by(before_oscar, movie_id)
  group_after_oscar <- group_by(after_oscar, movie_id)
  
  before_oscar_mean <- summarise(group_before_oscar, mean(rating))
  colnames(before_oscar_mean) <- c("movie_id", "ratings_before")
  
  after_oscar_mean <- summarise(group_after_oscar, mean(rating))
  colnames(after_oscar_mean) <- c("movie_id", "ratings_after")
  
  oscar <- cbind(before_oscar_mean, after_oscar_mean[,2])
  
  left_join(oscar, nominations, by = c("movie_id" = "cod_imdb"))
}

mean_before <- function(movies_ratings, df_times, year_movie, nominations) {
  df <- data.frame()
  timestamp <- df_times$timestamp
  
  for (date in timestamp){
    before_date <- movies_ratings  %>% 
      filter(rating_timestamp < date) %>%
      filter(movie_id %in% filter(nominations, year == year_movie)$cod_imdb)
    
    group_before <- group_by(before_date, movie_id)
    
    before_mean <- summarise(group_before, mean(rating))
    colnames(before_mean) <- c("movie_id", "ratings")
    
    before_mean$timestamp <- date
    
    df <- rbind(df, before_mean)
  }
  df <- left_join(df, nominations, by = c("movie_id" = "cod_imdb"))
  left_join(df, df_times, by = c("timestamp" = "timestamp"))
}