#' ---
#' title: "Peer-graded Assignment: Milestone Report"
#' author: "Frank Chi"
#' date: "`June 18, 2018"
#' output: 
#'   github_document:
#'     toc: true
#' ---
#'
#' ## Introduction
#' This script uses the tidytext (Silge and Robinson 2016) R package applied to 
#' Data Science Capstone - Milestone Report 
#' [Text Mining with R: A Tidy Approach](http://tidytextmining.com/).  
#' 

rm(list = ls())
suppressPackageStartupMessages({
    library(knitr)
    library(ngram)
    library(dplyr)
    library(stringr)
    library(readr)
    library(tidyr)
    library(tidytext)
    library(tm)
    library(ggplot2)
    library(wordcloud)
})
start_time <- Sys.time()

#' ## Data Loading and Summarizing
#+ DataLoading

#' English Repository Files
dsCourse_folder<- "./dsCourse"
rds_repo_folder<- "./dsCourse/repo"
blogs_file   <- "./dsCourse/final/en_US/en_US.blogs.txt"
news_file    <- "./dsCourse/final/en_US/en_US.news.txt"
twitter_file <- "./dsCourse/final/en_US/en_US.twitter.txt"  
sample_pct <- 10 # Data Sampling - 10% 

#' ## Processing the data
#' Note that files already download into `dsCourse` folder

#' Create a repo directory  
if (!file.exists(rds_repo_folder)) {
    dir.create(rds_repo_folder)
}

#' Cleaning text
if (!file.exists(file.path(rds_repo_folder, 'repo_clean_10.rds'))) {

    #' ## Data Sampling and Cleaning
    #+ DataSampling
    set.seed(451013)

    #' Read the data files into tidy dataframes
    #' Note that the `en_US.news` file contained hidden special characters (`Null`) that preventing a full
    #'  file read. These special characters required hand deletion with an editor prior to read full file.
    if (!file.exists(file.path(rds_repo_folder, 'repo_sample.rds'))) {
        blogs<-   readLines(blogs_file,   skipNul = TRUE)
        news<-    readLines(news_file,    skipNul = TRUE)
        twitter<- readLines(twitter_file, skipNul = TRUE)
        
        blogs_sample<-   data_frame(text=blogs)
        news_sample<-    data_frame(text=news)
        twitter_sample<- data_frame(text=twitter)
        
        if (sample_pct > 0 && sample_pct < 100) {
            #+ DataSampling
            blogs_sample <- blogs_sample %>%
                sample_n(., nrow(blogs_sample)*sample_pct/100)
            news_sample <- news_sample %>%
                sample_n(., nrow(news_sample)*sample_pct/100)
            twitter_sample <- twitter_sample %>%
                sample_n(., nrow(twitter_sample)*sample_pct/100)
        }
        
        
        #' Create tidy repository
        repo_sample <- bind_rows(mutate(blogs_sample,   source = "blogs"),
                                 mutate(news_sample,    source = "news"),
                                 mutate(twitter_sample, source = "twitter")) 
        repo_sample$source <- as.factor(repo_sample$source)
        
        saveRDS(repo_sample, file.path(rds_repo_folder, 'repo_sample.rds'))
        rm(blogs, news, twitter, 
           blogs_sample, news_sample, twitter_sample)
    } else {
        repo_sample <- readRDS(file.path(rds_repo_folder, 'repo_sample.rds'))
    }
    
    #' Remove non-alphanumeric characters, url's, and repeated letters(+3x)
    #+ DataCleaning
    replace_reg <- "[^[:alpha:][:space:]]*"
    replace_url <- "http[^[:space:]]*"
    replace_aaa <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"  
    
    #' Clean the sample.
    repo_clean_sample <-  repo_sample %>%
        mutate(text = str_replace_all(text, replace_reg, "")) %>%
        mutate(text = str_replace_all(text, replace_url, "")) %>%
        mutate(text = str_replace_all(text, replace_aaa, "")) %>% 
        mutate(text = iconv(text, "ASCII//TRANSLIT"))
    
    #' Clean up
    saveRDS(repo_clean_sample, file.path(rds_repo_folder, 'repo_clean_10.rds'))
    rm(repo_sample, replace_reg, replace_url, replace_aaa)
} else {
    repo_clean_sample <- readRDS(file.path(rds_repo_folder, 'repo_clean_10.rds'))
}

#' ## Tidytext Transformation
#' - Convert to lower case
#' - Remove punctuation
#' - Remove numbers
#' - Remove stopwords - common parts of speech that are not informative such as a, an, be, of, etc.
#' - Remove domain-specific stopwords
#' - Stemming - reduce words to their word stem
#'   "Fishing", "fished", and "fisher" -> "fish"
#' 
# tidytext_transform<- function(input, lowercase=TRUE, rm_stop_words=TRUE, rm_bad_words=TRUE, rm_number=TRUE) {
#     output <- input
#     rm_words <- NULL
#     
#     if (lowercase == TRUE) {
#         output <- tolower(output)
#     }
#     
#     if(rm_stop_words == TRUE) {
#         data('stop_words')
#         rm_words <- stop_words$word
#     }
#     
#     if (rm_bad_words == TRUE && file.exists(file.path(dsCourse_folder, 'badwords.txt'))) {
#         rm_words <- c(rm_words, 
#                       readLines(file.path(dsCourse_folder, 'badwords.txt'), skipNul = TRUE))
#     }
#     
#     if (!is.null(rm_words)) {
#         output <- removeWords(output, rm_words)
#     }
#     
#     if (rm_number == TRUE) {
#         output = removeNumbers(output)
#     }
#     
#     return(output)
# }


#' Tidytext transformation and create Unigram dataframe 
# if (!file.exists(file.path(rds_repo_folder, 'repo_unigram_10.rds'))) {
#     
#     if (!file.exists(file.path(rds_repo_folder, 'repo_transform_10.rds'))) {
#         repo_clean_sample <- readRDS(file.path(rds_repo_folder, 'repo_clean_10.rds'))
#         repo_clean_sample$text<- 
#             stripWhitespace(
#                 removePunctuation(
#                     tidytext_transform(repo_clean_sample$text),
#                     preserve_intra_word_contractions = TRUE,
#                     preserve_intra_word_dashes = TRUE)
#             )
#         saveRDS(repo_clean_sample, file.path(rds_repo_folder, 'repo_transform_10.rds'))
#     } else {
#         repo_clean_sample <- readRDS(file.path(rds_repo_folder, 'repo_transform_10.rds'))
#     }
#         
#     repo_unigram <- repo_clean_sample %>%
#         unnest_tokens(words, text) 
#     
#     saveRDS(repo_unigram, file.path(rds_repo_folder, 'repo_unigram_10.rds'))
# } else {
#     repo_unigram <- readRDS(file.path(rds_repo_folder, 'repo_unigram_10.rds'))
# }
# rm(repo_clean_sample)

#' Create Unigram freq repo
# if (!file.exists(file.path(rds_repo_folder, 'repo_unigram_10_freq.rds'))) {
#     repo_unigram_freq <- repo_unigram %>%
#         count(words) %>%  
#         mutate(proportion = n / sum(n)) %>%
#         arrange(desc(proportion)) %>%  
#         mutate(coverage = cumsum(proportion)) 
#     
#     saveRDS(repo_unigram_freq, file.path(rds_repo_folder, 'repo_unigram_10_freq.rds'))
# }

end <- Sys.time()

(run_time <- end - start_time)

#' -------------
#'  
#' ## Session info
#+ show-sessionInfo
sessionInfo()       
