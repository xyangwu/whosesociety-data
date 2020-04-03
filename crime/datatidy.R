
#### rape crime section -----------------------------------------------------------
judgement_rape <- data.frame(page = seq(1,107,1), stringsAsFactors = F)
judgement_rape$table <- NA
judgement_rape$link <- NA
judgement_rape$number <- NA
judgement_rape$title <- NA
judgement_rape$type <- NA 
judgement_rape$cause <- NA 
judgement_rape$apartment <- NA 
judgement_rape$level <- NA 
judgement_rape$time <- NA

remDr$navigate(url)

judgement_rape_df2 <- cbind.data.frame(do.call(rbind.data.frame, judgement_rape$table),
                                      link = unlist(judgement_rape$link))

names(judgement_rape_df) <- c(judgement_rape_df[1,][1:7], "link")
judgement_rape_df <- judgement_rape_df[-which(judgement_rape_df[,1]=='案号'), ]
rownames(judgement_rape_df) <- 1:nrow(judgement_rape_df)
judgement_rape_df$link <- as.character(judgement_rape_df$link) %>% str_extract_all(., '\\([^&].+\\)') %>% gsub("\\(|\\)|'",'',.)
saveRDS(judgement_rape_df, 'data/judgement_rape_df.rds')
judgement_rape_df$content <- NA

## obscene women ----------------------------------------------------------

## unlist table and link
judgement_obscene_w_df <- cbind.data.frame(do.call(rbind.data.frame, judgement_obscene_w$table),
                                           link = unlist(judgement_obscene_w$link))
names(judgement_obscene_w_df) <- c(judgement_obscene_w_df[1,][1:7], "link")
judgement_obscene_w_df <- judgement_obscene_w_df[-which(judgement_obscene_w_df[,1]=='案号'), ]
rownames(judgement_obscene_w_df) <- 1:nrow(judgement_obscene_w_df)
judgement_obscene_w_df$link <- as.character(judgement_obscene_w_df$link) %>% str_extract_all(., '\\([^&].+\\)') %>% gsub("\\(|\\)|'",'',.)
judgement_obscene_w_df$content <- NA

## child molestation ----------------------------------------------------------
## unlist table and link
judgement_obscene_c_df <- cbind.data.frame(do.call(rbind.data.frame, judgement_obscene_c$table),
                                           link = unlist(judgement_obscene_c$link))
names(judgement_obscene_c_df) <- c(judgement_obscene_c_df[1,][1:7], "link")
judgement_obscene_c_df <- judgement_obscene_c_df[-which(judgement_obscene_c_df[,1]=='案号'), ]
rownames(judgement_obscene_c_df) <- 1:nrow(judgement_obscene_c_df)
judgement_obscene_c_df$link <- as.character(judgement_obscene_c_df$link) %>% str_extract_all(., '\\([^&].+\\)') %>% gsub("\\(|\\)|'",'',.)


## insult crime section ----------------------------------------------------------
## unlist table and link
judgement_insult_df <- cbind.data.frame(do.call(rbind.data.frame, judgement_insult$table),
                                           link = unlist(judgement_insult$link))
names(judgement_insult_df) <- c(judgement_insult_df[1,][1:7], "link")
judgement_insult_df <- judgement_insult_df[-which(judgement_insult_df[,1]=='案号'), ]
rownames(judgement_insult_df) <- 1:nrow(judgement_insult_df)
judgement_insult_df$link <- as.character(judgement_insult_df$link) %>% str_extract_all(., '\\([^&].+\\)') %>% gsub("\\(|\\)|'",'',.)
judgement_insult_df$content <- NA


judgement_rape_df <- rbind.data.frame(judgement_rape_df[1:1833, ], judgement_obscene_w_df, judgement_insult_df, stringsAsFactors = F)
which(is.na(judgement_rape_df$content))[1]
judgement_rape_df[1834,]

