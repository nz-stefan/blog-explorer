################################################################################
# Server logic of the Shiny app
#
# Author: Stefan Schliebs
# Created: 2020-03-13 09:21:18
################################################################################


server <- function(input, output, session) {
  
  d_data_model <- reactive({ readRDS("data/app-data-model.rds") })
  
  callModule(count_icon, "blog_counter", d_data_model, "n_blogs")
  callModule(count_icon, "author_counter", d_data_model, "n_authors")
  callModule(count_icon, "word_counter", d_data_model, "n_words")
  callModule(count_icon, "page_counter", d_data_model, "n_book_pages")
  
  # output$lala <- renderUI({
  #   HTML('<h3 class="count-to font-alt" data-countto="12345"></h3>')
  # })
}