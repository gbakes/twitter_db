library(RPostgres)
library(tidyverse)
library(magrittr)
library(hrbrthemes)
library(ggseas)

extrafont::font_import("Arial")


tweets_sql <- paste(readLines("sql/tweets.sql"), collapse = "\n")
tweets <- dbGetQuery(con, tweets_sql, stringsAsFactors = FALSE)

batches_sql <- paste(readLines("sql/sql/batches.sql"), collapse = "\n")
batches <- dbGetQuery(con, batches_sql, stringsAsFactors = FALSE)

hash_sql <- paste(readLines("sql/sql/hash.sql"), collapse = "\n")
hash <- dbGetQuery(con, hash_sql,stringsAsFactors = F)

popular_text_sql <- paste(readLines("sql/sql/popular_text.sql"), collapse = "\n")
retweets <- dbGetQuery(con, popular_text_sql,stringsAsFactors = F)


tweeters_sql <- paste(readLines("sql/sql/tweeters.sql"), collapse = "\n")
tweeters <- dbGetQuery(con, tweeters_sql,stringsAsFactors = F)
dbDisconnect(con)

#----------------------- Processing

hash_data_full <-hash %>%
    group_by(hashtag) %>%
    summarise(freq = sum(freq)) %>%
    arrange(desc(freq))

hash_data <- hash_data_full%>%
    slice(1:80)

retweets_data <- retweets %>%
    select(-lang, -rank) %>%
    arrange(desc(observed_retweets)) %>%
    mutate(text = gsub("&amp;", "&", text, fixed = TRUE))
  names(tmp) <- toTitleCase(gsub("_", " ", names(tmp)))

  #----------------define graphics------------------
 # theme_set(theme_dark(base_family = "FreeSans"))
  
  tweeters_plot <- tweeters %>%
      slice(1:20) %>%
      mutate(freq = as.numeric(freq),
             screen_name = fct_reorder(screen_name, freq)) 
  
      ggplot(tweeters_plot,aes(y = screen_name, x = freq, label = round(inv_prop, -1))) +
      geom_text(colour = "grey") +
    labs(x = "Count in sample", y = "")+
        theme_dark()
    

  
  batches_plot <- batches %>%
      select(time_collection_started, tweets_loaded:users_followers_counted) %>%
      gather(variable, value, -time_collection_started) %>%
      mutate(variable = gsub("_loaded$", "", variable),
             variable = gsub("_", " ", variable))

    
    batches_plot$value[is.na(batches_plot$value)] <- 0
    batches_plot <-  na.omit(batches_plot) %>% 
      as_tibble() 
    
      ggplot(batches_plot,aes(x = time_collection_started, y = value)) +
      facet_wrap(~variable, scales = "free_y") +
      geom_line(colour = "lightblue", alpha = 0.5)+
      stat_rollapplyr(width = 24, colour = "white") +
      scale_y_continuous() +
      labs(x = "") +
      ggtitle("Summary of Twitter information sampled since May 2018",
              "White line is 24 hour moving average; blue line is original data")
 
