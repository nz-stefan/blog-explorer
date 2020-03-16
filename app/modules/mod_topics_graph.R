################################################################################
# Shiny module topics_graph
#
# Author: Stefan Schliebs
# Created: 2020-03-14 08:48:46
################################################################################


# Module constants --------------------------------------------------------

# F_COUNT_ICON_TEMPLATE <- "www/modules/count_icon/index.html"


# Module UI ---------------------------------------------------------------

topics_graph_ui <- function(id) {
  ns <- NS(id)
  
  material_card(
    fluidRow(
      column(
        width = 7,
        visNetworkOutput(ns("graph"), width = "100%", height = "450px"),
        style = "border-right: silver 1px solid"
      ),
      column(
        width = 5, 
        echarts4rOutput(ns("topic_timeseries_plot"), height = "240px"),
        echarts4rOutput(ns("topic_wordcloud"), height = "260px")
      )
    )
  )
}



# Module logic ------------------------------------------------------------

topics_graph <- function(input, output, session, d_data_model) {
  ns <- session$ns
  

  # Topic correlation network -----------------------------------------------

  output$graph <- renderVisNetwork({
    req(d_data_model())
    
    visNetwork(
      nodes = d_data_model()$d_nodes, 
      edges = d_data_model()$d_edges
    ) %>%
      
      # configure layouting algorithm
      visPhysics(
        solver = "barnesHut",
        barnesHut = list(gravitationalConstant = -8000, damping = 0.05, centralGravity = 0.5)
      ) %>%
      
      # allow clicking on the nodes, but disble the dropdown menu to select nodes from
      visOptions(nodesIdSelection = list(enabled = TRUE, style = "visibility: hidden"))
  })
  

  
  # Topic details panel -----------------------------------------------------

  # plot the topic proportions ovr time
  output$topic_timeseries_plot <- renderEcharts4r({
    req(input$graph_selected)
    
    # extract the node that was clicked on from the Shiny input
    selected_topic <- as.integer(input$graph_selected)
    
    # plot the topic proportions over time
    d_data_model()$d_estimated_effect %>% 
      filter(topic == selected_topic) %>% 
      
      # to plot the confidence envelope in echarts a lower and a height number is required
      mutate(height = ci_upper - ci_lower) %>% 
      
      # make chart
      e_charts(x = publication_date) %>% 
      e_line(estimate, symbol = "none", name = "Expected Topic Proportion") %>%
      e_band(ci_lower, height) %>% 
      e_tooltip(trigger = "axis") %>%
      e_legend(show = FALSE) %>%
      e_title(NULL, "Expected Topic Proportion") %>% 
      e_theme("walden")    # also good: westeros, auritus, walden
  })
  
  # wordcloud of most frequent terms in the selected topic
  output$topic_wordcloud <- renderEcharts4r({
    req(input$graph_selected)
    
    d_data_model()$d_top_terms %>% 
      filter(topic == input$graph_selected) %>% 
      e_charts() %>% 
      e_cloud(term, beta, shape = "circle", sizeRange = c(10, 48), width = "90%", height = "90%", rotationRange = c(0, 0)) %>% 
      # e_title(NULL, "Word frequencies") %>% 
      e_theme("walden")
  })
    
}  
