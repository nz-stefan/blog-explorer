################################################################################
# Load data from RSS feed
#
# Author: Stefan Schliebs
# Created: 2020-03-23 08:43:10
################################################################################


library(dplyr)
library(rvest)
library(logging)
library(stringr)
library(purrr)




# Config ------------------------------------------------------------------

URL_RSS_FEED <- "http://feeds.feedburner.com/RBloggers?format=xml"

# setup logging
basicConfig()



# Process feed ------------------------------------------------------------

rss_feed <- read_xml(URL_RSS_FEED)

# extract blog title
blog_title <- 
  rss_feed %>% 
  xml_nodes("item") %>% 
  xml_nodes("title") %>% 
  xml_text()

# extract blog title
publication_date <- 
  rss_feed %>% 
  xml_nodes("item") %>% 
  xml_nodes("pubDate") %>% 
  xml_text() %>% 
  as.Date(format = "%a, %d %b %Y")

# extract blog author
blog_author <- 
  rss_feed %>% 
  xml_nodes("item") %>% 
  xml_nodes("dc\\:creator") %>% 
  xml_text()

# extract original blog URL
origin_blog_url <- 
  rss_feed %>% 
  xml_nodes("guid") %>% 
  xml_text()

blog_content <- 
  rss_feed %>% 
  xml_nodes("content\\:encoded") %>% 
  xml_text()


# original blog name and blog content come from the actual "payload" of the RSS feed
d_blog_content <- map_df(blog_content, function(s) {
  # browser()
  
  tryCatch(
    p <- read_html(s),
    error = function(e) {
      logerror("Could not process page '%s': %s", s[1:100], e)
    }
  )
  
  # extract origin blog
  origin_blog <-
    map(p %>% html_nodes('div') %>% html_text(), function(node) {
      if (str_detect(node, "^To leave a comment for the author")) {
        str_remove_all(node, "To leave a comment for the author, please follow the link and comment on their blog:") %>% 
          str_trim() %>% 
          str_remove_all("\\.")
      }
    }) %>% 
    unlist()
  
  # Extract raw content

  # remove everyting that is not text content from the entire document
  p %>% 
    html_nodes("pre, table, img, script, svg, iframe, br, hr, noscript, style") %>%  
    xml_remove()
  
  # remove the leading (and ending) div elements that "envelope" the content
  p %>% 
    html_nodes(xpath = "/html/body/div[1 or 2]") %>% 
    xml_remove()

  # fix encoding, remove punctuation, numbers and new line characters  
  blog_content <- 
    p %>% 
    html_text(trim = TRUE) %>% 
    toString() %>% 
    iconv("UTF8", "ASCII", sub = " ") %>% 
    str_replace_all("[[:punct:]]|[[:digit:]]|\r|\n|\t", " ")
  
  if (is.null(blog_content)) {
    browser()
    stop("Deletion logic failed")
  }
  
  # pack up response into a list
  list(
    origin_blog = origin_blog, 
    blog_content = blog_content
  )
})

# combine everything into a single frame
d_combined <- 
  d_blog_content %>% 
  mutate(blog_title, blog_author, publication_date, origin_blog_url) %>% 
  select(blog_title, publication_date, blog_author, origin_blog, origin_blog_url, blog_content)



