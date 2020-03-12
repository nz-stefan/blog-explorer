################################################################################
# Download URLs to blog articles from r-bloggers.com
#
# Author: Stefan Schliebs
# Created: 2020-02-17 09:18:28
################################################################################


library(rvest)
library(logging)
library(stringr)

`%>%` <- magrittr::`%>%`



# Config ------------------------------------------------------------------

URL_R_BLOGGERS <- "https://www.r-bloggers.com"

basicConfig()



# Collect pages -----------------------------------------------------------

# create a sequence of year-month combinations representing r-blogger archives
d_archive <- rbind(
  data.frame(month = c("01"), year = 2020),
  expand.grid(month = sprintf("%02d", 12:1), year = 2019:2010)
)

# list to store all blog urls
urls_to_download <- list()

# loop over all archives
for(i in seq(nrow(d_archive))) {
  # unpack current archive to process
  current_month <- d_archive$month[i]
  current_year <- d_archive$year[i]
  
  # each archive has several pages each containing the abstracts of up to 10 blogs
  current_page <- 1
  
  # loop over all archive pages
  while(TRUE) {
    # construct url based on archive and archive page
    current_url <- sprintf("%s/%d/%s/page/%d", URL_R_BLOGGERS, current_year, current_month, current_page)
    
    # try downloading current page
    loginfo("Processing url '%s'", current_url)
    tryCatch(
      p <- read_html(current_url),
      error = function(e) {
        logerror("Could not process url '%s'", current_url)
        break
      }
    )
    
    # unpack links to full article
    urls <- 
      p %>% 
      html_nodes("div .entry .more-link") %>% 
      html_attr("href")
    loginfo("Found %d new urls", length(urls))
    
    urls_to_download <- append(urls_to_download, urls)
    loginfo("Number of downloaded urls so far: %d", length(urls_to_download))
    
    # save intermediate results to disk
    d_cleaned_urls <- data.frame(
      urls = urls_to_download %>% unlist(),
      stringsAsFactors = FALSE
    )
    write.csv(d_cleaned_urls, file = "data/urls-to-download.csv", row.names = FALSE)
    
    
    # move on to next page
    current_page <- 
      p %>% 
      html_node(".current") %>% 
      html_text() %>% 
      as.integer()
    
    max_page <- 
      p %>% 
      html_nodes(".pages") %>% 
      html_text() %>% 
      str_extract("\\d+$") %>% 
      as.integer()
    
    if (current_page == max_page) {
      loginfo("Completed archive %d/%s", current_year, current_month)
      break
    } else {
      current_page <- current_page + 1 
    } 
    
    # be gentle with the server
    Sys.sleep(1)
  }
}

loginfo("Done")
