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

  callModule(insights_charts, "insights_charts", d_data_model)
  
  callModule(topics_graph, "topics_graph", d_data_model)
  
  
  output$last_refresh <- renderUI({
    last_refresh_formatted <- strftime(d_data_model()$last_refresh, format = "%d %b %Y")
    
    HTML(glue("Last data refresh occurred on <strong>{ last_refresh_formatted }</strong>."))
  })
  
  
  output$sometest <- renderText(input$mybutton)
  
}