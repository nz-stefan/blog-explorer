################################################################################
# Shiny module topics_table
#
# Author: Stefan Schliebs
# Created: 2020-03-28 07:50:27
################################################################################



# Module UI ---------------------------------------------------------------

topics_table_ui <- function(id) {
  ns <- NS(id)

  fluidRow(uiOutput(ns("topic_list")))
  
  # fluidRow(
  #   column(width = 12, DTOutput(ns("table")))
  # )
}



# Module logic ------------------------------------------------------------

topics_table <- function(input, output, session, d_data_model) {
  ns <- session$ns

  HTML_TEMPLATE_COLUMN <- '<div class="col-sm-4"><div class="widget"><h5 class="widget-title font-alt">%s</h5><ul>%s</ul></div></div>\n'
  
  output$topic_list <- renderUI({
    req(d_data_model())

    # determine how to split topics across columns in the HTML output    
    nb_chunks <- 3
    
    # compute number of topics per column
    topics_per_chunk <- d_data_model()$n_topics / nb_chunks
    
    # use some dplyr foo to split topics into columns, add headlines to each column
    # and wrap each topic into HTML list items
    d_data_model()$d_top_terms %>% 
      arrange(topic, desc(beta)) %>% 
      group_by(topic) %>% 
      top_n(5, beta) %>% 
      summarise(terms = paste(term, collapse = ", ")) %>% 
      mutate(html = sprintf('<li style="font-size: 10px">%s</li>', terms)) %>% 
      mutate(chunk = sort(rep(seq(1, nb_chunks), topics_per_chunk))) %>% 
      split(.$chunk) %>% 
      purrr::map_chr(function(d_chunk) {
        d_chunk %>% 
          summarise(
            html = sprintf(
              HTML_TEMPLATE_COLUMN, 
              paste("Topic", (chunk[1] - 1) * topics_per_chunk + 1, "to",  chunk[1] * topics_per_chunk), 
              paste(html, collapse = "\n")
            )
          ) %>% 
          pull(html)
      }) %>%
      HTML()
  })

  # d_topics <- reactive({
  #   req(d_data_model())
  #   
  #   d_data_model()$d_top_terms %>% 
  #     group_by(topic) %>% 
  #     top_n(5, beta) %>% 
  #     ungroup() %>% 
  #     arrange(topic, desc(beta)) %>% 
  #     select(topic, term) %>% 
  #     split(.$topic, .$term) %>% 
  #     purrr::map(~ .x$term) %>% 
  #     bind_cols()    
  # })
  #   
  # output$table <- renderDT({
  #   req(d_topics())
  # 
  #   # use inline CSS styling to format the table headers
  #   d <- d_topics() %>% setNames(sprintf('<span style="color: #eee; width: 100px; font-size: 8px">%s</span>', names(d_topics())))
  #   
  #   d %>% 
  #     datatable(
  #       options = list(dom = "t", scrollX = TRUE, ordering = FALSE), 
  #       rownames = FALSE, escape = FALSE
  #     ) %>% 
  #     formatStyle(names(d), backgroundColor = "rgb(88, 88, 88, 0.90)", color = "#EEE")
  #     
  # })
}
