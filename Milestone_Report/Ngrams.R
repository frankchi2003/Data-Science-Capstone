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
#' 
#+ startup, echo = FALSE 
rm(list = ls())
suppressPackageStartupMessages({
    library(knitr)
    library(tidytext)
    library(dplyr)
    library(stringr)
    library(tm)
    library(RWeka)
})

start_time <- Sys.time()


#' English Repository Files
dsCourse_folder<- "./dsCourse"
rds_repo_folder<- "./dsCourse/repo"
blogs_file   <- "./dsCourse/final/en_US/en_US.blogs.txt"
news_file    <- "./dsCourse/final/en_US/en_US.news.txt"
twitter_file <- "./dsCourse/final/en_US/en_US.twitter.txt"  
sample_pct <- 10 # Data Sampling - 10% 


#' ## Tidytext Transformation
#' - Convert to lower case
#' - Remove punctuation
#' - Remove numbers
#' - Remove stopwords - common parts of speech that are not informative such as a, an, be, of, etc.
#' - Remove domain-specific stopwords
#' - Stemming - reduce words to their word stem
#'   "Fishing", "fished", and "fisher" -> "fish"
#' 
tidytext_transform<- function(input, lowercase=TRUE, rm_stop_words=TRUE, rm_bad_words=TRUE, rm_number=TRUE) {
    output <- input
    rm_words <- NULL
    
    if (lowercase == TRUE) {
        output <- tolower(output)
    }
    
    if(rm_stop_words == TRUE) {
        data('stop_words')
        rm_words <- stop_words$word
    }
    
    if (rm_bad_words == TRUE && file.exists(file.path(dsCourse_folder, 'badwords.txt'))) {
        rm_words <- c(rm_words, 
                      readLines(file.path(dsCourse_folder, 'badwords.txt'), skipNul = TRUE))
    }
    
    if (!is.null(rm_words)) {
        output <- removeWords(output, rm_words)
    }
    
    if (rm_number == TRUE) {
        output = removeNumbers(output)
    }
    
    return(output)
}

if (!file.exists(file.path(rds_repo_folder, 'repo_transform_10.rds'))) {
    repo_clean_sample <- readRDS(file.path(rds_repo_folder, 'repo_clean_10.rds'))
    repo_clean_sample$text<- 
        stripWhitespace(
            removePunctuation(
                tidytext_transform(repo_clean_sample$text),
                preserve_intra_word_contractions = TRUE,
                preserve_intra_word_dashes = TRUE)
        )
    saveRDS(repo_clean_sample, file.path(rds_repo_folder, 'repo_transform_10.rds'))
}

#' ## Create Ngram by using `unnest_tokens`
repo_Ngrams <- function(ngrams = 2) {
    if (ngrams > 1 && ngrams < 10) {
        rds <- ifelse (ngrams == 2, 'repo_bigram_10.rds', 
                       ifelse(ngrams == 3, 'repo_trigram_10.rds',
                              ifelse(ngrams == 4, 'repo_quadrgram_10.rds',
                                     ifelse(ngrams == 5, 'repo_quintgram_10.rds',
                                            ifelse(ngrams == 6, 'repo_sexagram_10.rds',
                                                   ifelse(ngrams == 7, 'repo_septuagram_10.rds',
                                                          ifelse(ngrams == 8, 'repo_octogram_10.rds',
                                                                 ifelse(ngrams == 9, 'repo_nonagram_10.rds',
                                                                        'repo_bigram_10.rds'))))))))
        rdsfreq <- ifelse (ngrams == 2, 'repo_bigram_freq_10.rds', 
                       ifelse(ngrams == 3, 'repo_trigram_freq_10.rds',
                              ifelse(ngrams == 4, 'repo_quadrgram_freq_10.rds',
                                     ifelse(ngrams == 5, 'repo_quintgram_freq_10.rds',
                                            ifelse(ngrams == 6, 'repo_sexagram_freq_10.rds',
                                                   ifelse(ngrams == 7, 'repo_septuagram_freq_10.rds',
                                                          ifelse(ngrams == 8, 'repo_octogram_freq_10.rds',
                                                                 ifelse(ngrams == 9, 'repo_nonagram_freq_10.rds',
                                                                        'repo_bigram_freq_10.rds'))))))))
    } else {
        rds <- 'repo_unigram_10.rds'
        rdsfreq <- 'repo_unigram_freq_10.rds'
    }
    
    if (!file.exists(file.path(rds_repo_folder, rds))) {
        repo_clean_sample <- readRDS(file.path(rds_repo_folder, 'repo_transform_10.rds'))
        if (ngrams > 1 && ngrams < 10) {
            Ngram <- repo_clean_sample %>% 
                unnest_tokens(words, text, token = "ngrams", n = ngrams)
        } else {
            Ngram <- repo_clean_sample %>% 
                unnest_tokens(words, text)
        }
        saveRDS(Ngram, file.path(rds_repo_folder, rds))
    } else {
        Ngram <- readRDS(file.path(rds_repo_folder, rds))
    }
    
    if (!file.exists(file.path(rds_repo_folder, rdsfreq))) {
        Ngram_freq <- Ngram %>%
            count(words) %>%  
            mutate(proportion = n / sum(n)) %>%
            arrange(desc(proportion)) %>%  
            mutate(coverage = cumsum(proportion)) 
        saveRDS(Ngram_freq, file.path(rds_repo_folder, rdsfreq))
    } else {
        Ngram_freq <- readRDS(file.path(rds_repo_folder, rdsfreq))
    }
    
    return(list(ngram=Ngram, freq=Ngram_freq))
}


end <- Sys.time()

(run_time <- end - start_time)

#' -------------
#'  
#' ## Session info
#+ show-sessionInfo
sessionInfo()       
