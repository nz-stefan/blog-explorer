################################################################################
# Download raw html pages from r-blogger
#
# Author: Stefan Schliebs
# Created: 2020-02-19 08:26:43
################################################################################


library(rvest)
library(logging)
library(stringr)



# Config ------------------------------------------------------------------

F_URLS <- "data/urls-to-download.csv"
DIR_OUTPUT <- "data/raw-pages"

basicConfig()



# Download pages ----------------------------------------------------------

# load URLS downloaded through src/download-rbloggers.R
d_urls <- read.csv(F_URLS, stringsAsFactors = FALSE)

# loop over all urls and download them one by one
counter <- 0
for (link in d_urls$urls) {
  counter <- counter + 1
  loginfo("Processing URL #%d (%s)", counter, link)
  
  # prepare output file name
  f_dest <- sprintf("%s/%s.html", DIR_OUTPUT, str_remove_all(link, ".*\\.com/|/$"))
  # browser()
  
  # download page
  res <- tryCatch(
    p <- read_html(link),
    error = function(e) {
      logerror("Could not download page '%s': %s", link, e)
    }
  )
  
  if (inherits(res, "error")) next

  # we can decrease the storage size of blogs considerable, if we only keep the content div
  # of each blog page
  res <- tryCatch(
    blog_content <- 
      p %>% 
      html_node("#leftcontent") %>% 
      as.character(),
    error = function(e) {
      logerror("Could not parse page '%s': %s", link, e)
    }
  )
  if (inherits(res, "error")) next
  
  # to process the data after download we wrap the blog content into a valid HTML document
  blog_content_html <- 
    sprintf("<html><body>%s</body></html>", blog_content)
  
  # store blog to disk
  sink(file = f_dest)
  cat(blog_content_html)
  sink()
  
  loginfo("Stored data into '%s'", f_dest)

  # be gentle on the server
  # Sys.sleep(1)
}

loginfo("Done")

