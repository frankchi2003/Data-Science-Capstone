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
suppressPackageStartupMessages({
    library(knitr)
    library(ngram)
    library(dplyr)
})  
start <- Sys.time()

#' English Repository Files
dsCourse_folder<- "./dsCourse"
rds_repo_folder<- "./dsCourse/repo"
blogs_file   <- "./dsCourse/final/en_US/en_US.blogs.txt"
news_file    <- "./dsCourse/final/en_US/en_US.news.txt"
twitter_file <- "./dsCourse/final/en_US/en_US.twitter.txt"  
sample_pct <- 10 # Data Sampling - 10% 


#' ## Download and explore the data
#'
#' Create a data directory  
if (!file.exists(dsCourse_folder)) {
    dir.create(dsCourse_folder)
}

#' Create a repo directory  
if (!file.exists(rds_repo_folder)) {
    dir.create(rds_repo_folder)
}

#' Download the data
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if (!file.exists('coursera-swiftkey.zip')){
    download.file(url, dest='coursera-swiftkey.zip', mode='wb') 
}

#' Unzip the data
# place holder of coursera-swiftkey zip files: ./dsCourse/final
if (!file.exists(file.path(dsCourse_folder,'final'))) {
    unzip ("coursera-swiftkey.zip", exdir = dsCourse_folder)
}

#' Read English Files
blogs_file   <- file.path(dsCourse_folder,'final/en_US/en_US.blogs.txt')
news_file    <- file.path(dsCourse_folder,'final/en_US/en_US.news.txt')
twitter_file <- file.path(dsCourse_folder,'final/en_US/en_US.twitter.txt')  

#' ### Build Summary repo
if(!file.exists(file.path(rds_repo_folder, 'repo_summary.rds'))) {
    #' File Sizes (Mb)
    blogs_size   <- file.size(blogs_file) / (2^20)
    news_size    <- file.size(news_file) / (2^20)
    twitter_size <- file.size(twitter_file) / (2^20)
    
    #' Read the data files
    #' Note that the `en_US.news` file contained hidden special characters (`Null`) that preventing a full file 
    #' read. These special characters required hand deletion with an editor prior to read file.
    blogs   <- readLines(blogs_file, skipNul = TRUE)
    news    <- readLines(news_file, skipNul = TRUE)
    twitter <- readLines(twitter_file, skipNul = TRUE) 
    
    #' Number of Lines per file
    blogs_lines   <- length(blogs)
    news_lines    <- length(news)
    twitter_lines <- length(twitter)
    total_lines   <- blogs_lines + news_lines + twitter_lines
    
    #' Distibution of characters per line, by file
    blogs_nchar   <- nchar(blogs)
    news_nchar    <- nchar(news)
    twitter_nchar <- nchar(twitter)

    repo_dist <- list(blogs_nchar,
                      news_nchar, 
                      twitter_nchar )
        
    #' Total characters per file
    blogs_nchar_total   <- sum(blogs_nchar)
    news_nchar_total    <- sum(news_nchar)
    twitter_nchar_total <- sum(twitter_nchar)
    
    #' Total words per file
    blogs_words <- wordcount(blogs, sep = " ")
    news_words  <- wordcount(news,  sep = " ")
    twitter_words <- wordcount(news, sep = " ")
    
    #' Create summary of file stats
    repo_summary <- data.frame(f_names = c("blogs", "news", "twitter"),
                               f_size  = c(blogs_size, news_size, twitter_size),
                               f_lines = c(blogs_lines, news_lines, twitter_lines),
                               f_words = c(blogs_words, news_words, twitter_words),
                               f_nchar = c(blogs_nchar_total, news_nchar_total, twitter_nchar_total)
    )
    repo_summary <- repo_summary %>% 
        mutate(pct_lines = round(f_lines/sum(f_lines)*100, 2)) %>%
        mutate(pct_words = round(f_words/sum(f_words)*100, 2)) %>%
        mutate(pct_nchar = round(f_nchar/sum(f_nchar)*100, 2))
    
    #' Save Summary repo
    saveRDS(repo_summary, file = file.path(rds_repo_folder, 'repo_summary.rds') )
    saveRDS(repo_dist, file = file.path(rds_repo_folder, 'repo_dist.rds') )
}

end <- Sys.time()
(ellapsed <- end - start)

#' -------------
#'  
#' ## Session info
sessionInfo()
