################################################################################
# Extract content and meta information from downloaded blog articles
#
# Author: Stefan Schliebs
# Created: 2020-02-24 08:46:20
################################################################################


library(dplyr)
library(purrr)
library(rvest)
library(stringr)
library(logging)



# Config ------------------------------------------------------------------

# input and output directory
DIR_RAW_PAGES <- "data/raw-pages"
DIR_CLEANED_PAGES <- "data/cleaned-pages"

F_COMBINED_OUTPUT <- "data/r-blogger-blogs.rds"

# set up logging
basicConfig()



# Process articles --------------------------------------------------------

f_articles <- list.files(DIR_RAW_PAGES, pattern = ".*\\.html", full.names = TRUE) #%>% head(10)

for (f_article in f_articles) {
  loginfo("Processing '%s'", f_article)
  
  tryCatch(
    p <- read_html(f_article),
    error = function(e) {
      logerror("Could not process page '%s': %s", f_article, e)
    }
  )
  
  # extract blog title
  blog_title <- 
    p %>% 
    html_node("#leftcontent h1") %>% 
    html_text(trim = TRUE)
  
  # extract publication date
  publication_date <- 
    p %>% 
    html_node(".meta .date") %>% 
    html_text() %>% 
    as.Date(format = "%B %d, %Y")

  # extract author
  blog_author <- 
    p %>% 
    html_node(".meta a") %>% 
    html_text(trim = TRUE)
  
  # extract origin blog
  origin_blog <- 
    p %>% 
    html_node(".entry div strong a") %>% 
    html_text(trim = TRUE)
  
  # extract origin blog url
  origin_blog_url <- 
    p %>% 
    html_node(".entry div strong a") %>% 
    html_attr("href")
  
  # Extract raw content
  # NOTE: The format of the blog integration seems to have changed during the last 
  # decade. The logic to extract the blog content is therefore a bit bloated.
  # browser()
  
  # remove everyting that is not text content from the entire document
  p %>% 
    html_nodes("pre, table, img, script, svg, iframe, br, hr, noscript, style") %>%  
    xml_remove()
  
  # grab everything in the main container
  # this will only work for some articles
  blog_content_raw <- 
    p %>% 
    html_nodes("div.main-container") %>% 
    html_text(trim = TRUE)
  
  # try another approach to extract the content if the previous step failed
  if (length(blog_content_raw) == 0) {
    blog_content_raw <- 
      p %>%
      html_nodes("aside ~ *") %>% 
      html_text(trim = TRUE)
  }

  # fix encoding, remove punctuation, numbers and new line characters  
  blog_content <- 
    blog_content_raw %>% 
    toString() %>% 
    iconv("UTF8", "ASCII", sub = " ") %>% 
    str_replace_all("[[:punct:]]|[[:digit:]]|\r|\n|\t", " ") %>% 
    
    str_remove_all("ShareTweet.*$")
    
    # # remove the javascript code block at the end of every article
    # str_remove_all("var vglnk .*")

  if (is.null(blog_content)) {
    browser()
    stop("Deletion logic failed")
  }
  
  # make a tibble and export to disk
  f_out <- 
    stringr::str_replace(f_article, "html$", "rds") %>% 
    basename() %>% 
    file.path(DIR_CLEANED_PAGES, .)
  
  # save every article as a one line tibble
  tibble(
    blog_title, publication_date, blog_author, origin_blog, origin_blog_url, blog_content
  ) %>% 
    saveRDS(f_out)
}



# Combine exported files into one big one ---------------------------------

# get a list of all exported files
f_data_files <- list.files(DIR_CLEANED_PAGES, pattern = ".*\\.rds", full.names = TRUE)

# each file is a tibble with one row, concatenate all tibbles into a single one
d_combined <- map_df(f_data_files, readRDS)

# export to disk
loginfo("Exporting data to '%s'", F_COMBINED_OUTPUT)
saveRDS(d_combined, file = F_COMBINED_OUTPUT)
loginfo("Done")
