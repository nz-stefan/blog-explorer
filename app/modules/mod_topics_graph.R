################################################################################
# Shiny module topics_graph
#
# Author: Stefan Schliebs
# Created: 2020-03-14 08:48:46
################################################################################


# Module UI ---------------------------------------------------------------

topics_graph_ui <- function(id) {
  ns <- NS(id)
  
  # material_card(
    fluidRow(
      # column(width = 12, echarts4rOutput(ns("topic_prevalence_plot"), height = "200px")),
      column(
        width = 4, 
        echarts4rOutput(ns("topic_prevalence_plot"), height = "200px"),
        echarts4rOutput(ns("topic_timeseries_plot"), height = "200px"),
        echarts4rOutput(ns("topic_wordcloud"), height = "220px"),
        style = "background-color: rgba(34, 34, 34, 0.9)"
      ),
      column(
        width = 8,
        div(
          uiOutput(ns("headline_section")),
          style = "height: 90px"
        ),
        div(
          class = "graph-loading-bar",
          style = "margin-top: 180px",
          id = ns("loading-bar"),
          div(id = ns("loading-text"), class = "graph-loading-text", "Select a package"),
          div(
            id = ns("loading-bar-background"), class = "graph-loading-bar-background",
            div(id = ns("progress-bar"), class = "graph-progress-bar")
          )
        ),
        div(
          visNetworkOutput(ns("graph"), width = "100%", height = "500px"),
          style = "margin-top: -180px"
        ),
        style = "background-color: #eee"
      )
    )
  # )
}



# Module logic ------------------------------------------------------------

topics_graph <- function(input, output, session, d_data_model) {
  ns <- session$ns


  # Headline section --------------------------------------------------------

  output$headline_section <- renderUI({
    if (! isTruthy(selected_topic())) {
      h4("Click on a node in the graph!", class = "font-alt", style = "text-align: center")
    } else {
      freq_terms <- d_data_model()$d_top_terms %>% 
        filter(topic == selected_topic()) %>% 
        top_n(5, beta) %>% 
        arrange(desc(beta)) %>% 
        pull(term) %>% 
        paste(collapse = ", ")
      
      topic_rank <- d_topic_prevalence() %>% 
        mutate(rank = rank(desc(gamma), ties.method = "first")) %>% 
        filter(topic == selected_topic()) %>% 
        pull(rank)
      
      tagList(
        h4(paste("Topic", selected_topic()), class = "font-alt"),
        p(
          "Most frequent terms: ", freq_terms, br(),
          "Overall topic rank: ", topic_rank, "/", d_data_model()$n_topics
        )
      )
    }
  })  

  
  
  # Topic correlation network -----------------------------------------------

  # javascript function to control progress bar during network layouting
  progress_js_func <- function() {
    glue(
      "function(params) {{",
      "  var maxWidth = 100;",
      "  var minWidth = 20;",
      "  var widthFactor = params.iterations / params.total;",
      "  var width = Math.max(minWidth, maxWidth * widthFactor);",
      "  document.getElementById('{ns('loading-bar')}').style.visibility = 'visible';",
      "  document.getElementById('{ns('progress-bar')}').style.width = width + 'px';",
      "  document.getElementById('{ns('loading-text')}').innerHTML = 'loading - ' + Math.round(widthFactor*100) + '%';",
      "}}"
    )
  }
  
  # javascript function to switch off physics engine after the maximum layout iterations have finished
  stabilised_js_func <- function() {
    glue(
      "function() {{",
      "  document.getElementById('{ns('progress-bar')}').style.width = '100px';",
      "  document.getElementById('{ns('loading-text')}').innerHTML = 'loading - 100%';",
      "  document.getElementById('graph{ns('graph')}').chart.stopSimulation();",
      "  document.getElementById('{ns('loading-bar')}').style.visibility = 'hidden';",
      "  Shiny.onInputChange('{ns('graph_stabilised')}', 1);",
      "}}"
    )
  }
  
  output$graph <- renderVisNetwork({
    req(d_data_model())

    visNetwork(
      nodes = 
        d_data_model()$d_nodes %>% 
        mutate(
          color.highlight.background = "firebrick", 
          color.highlight.border = "firebrick",
          color.border = color,
          font.color.highlight = "firebrick"
        ) %>%
        rename(color.background = color),
      edges = d_data_model()$d_edges
    ) %>%
      
      visNodes(chosen = list(label = htmlwidgets::JS("function(values, id, selected, hovering){values.color='firebrick'}"))) %>% 

      # use straight edges to improve rendering performance
      visEdges(smooth = FALSE, color = list(opacity = 0.5)) %>%

      # configure layouting algorithm
      visPhysics(
        solver = "forceAtlas2Based",
        timestep = 1,
        minVelocity = 1,
        maxVelocity = 30,
        forceAtlas2Based = list(gravitationalConstant = -800, damping = 1),
        stabilization = list(iterations = 600, updateInterval = 10),
        adaptiveTimestep = TRUE
      ) %>% 
    
      # allow clicking on the nodes, but disble the dropdown menu to select nodes from
      visOptions(nodesIdSelection = list(enabled = TRUE, style = "visibility: hidden")) %>% 
      
      # connect custom javascript functions to network events 
      visEvents(
        stabilizationProgress = progress_js_func(),
        stabilizationIterationsDone = stabilised_js_func()
      )
  })
  
  observeEvent(input$graph_stabilised, {
    req(input$graph_stabilised)
    
    # select a random top 10 topic to pre-select when the network graph finished building
    node_id <- d_topic_prevalence()$topic[sample(1:10, 1)]
    visNetworkProxy(ns("graph")) %>% 
      visFocus(node_id, scale = 0.04) %>% 
      visSelectNodes(node_id)
  })
  
  # extract the node that was clicked on from the Shiny input
  selected_topic <- reactive({
    if (! isTruthy(input$graph_selected)) return(NULL)
    
    d_data_model()$d_nodes %>% 
      filter(id == as.integer(input$graph_selected)) %>% 
      pull(topic)
  })
  
  
  
  # Topic details panel -----------------------------------------------------

  # plot the topic proportions over time
  output$topic_timeseries_plot <- renderEcharts4r({
    # show an empty chart if no node was clicked on
    if (is.null(selected_topic())) {
      d_data_model()$d_estimated_effect %>%
        filter(topic == 1) %>%
        e_charts(x = publication_date) %>%
        e_title(NULL, "Expected Topic Proportion", left = "center") %>%
        e_theme("walden")    # also good: westeros, auritus, walden
    } else {
      # plot the topic proportions over time
      d_data_model()$d_estimated_effect %>%
        filter(topic == selected_topic()) %>%
        
        # to plot the confidence envelope in echarts a lower and a height number is required
        mutate(
          estimate = round(estimate, 4),
          height = ci_upper - ci_lower
        ) %>%
        # make chart
        e_charts(x = publication_date) %>%
        e_line(estimate, symbol = "none", name = "Expected Topic Proportion") %>%
        e_band(ci_lower, height, color = c("#888888", "#888888")) %>%
        e_tooltip(
          trigger = "axis",
          formatter = htmlwidgets::JS(
            "function(params){return(params[0].value[0]+'<br />Expected Topic Proportion: '+params[0].value[1])}"
          )
        ) %>%
        e_legend(show = FALSE) %>%
        e_title(NULL, "Expected Topic Proportion", left = "center") %>%
        e_x_axis(splitLine = list(show = FALSE)) %>%
        e_y_axis(
          splitLine = list(show = FALSE), 
          formatter = e_axis_formatter("percent", digits = 0), 
          min = 0, max = 0.06, splitNumber = 4,
          axisLabel = list(margin = 3)
        ) %>% 
        e_theme("walden")    # also good: westeros, auritus, walden
    }
  })
  
  # wordcloud of most frequent terms in the selected topic
  output$topic_wordcloud <- renderEcharts4r({
    # plot an empty chart if no node was clicked on
    if (is.null(selected_topic())) {
      d_data_model()$d_top_terms %>% 
        filter(topic == 1) %>% 
        e_charts(x = topic) %>% 
        e_title(subtext = "Word frequencies", left = "center") %>% 
        e_theme("walden")
    } else {
      d_data_model()$d_top_terms %>% 
        filter(topic == selected_topic()) %>% 
        e_charts() %>% 
        e_cloud(term, beta, shape = "circle", sizeRange = c(10, 48), width = "90%", height = "70%", rotationRange = c(0, 0)) %>% 
        e_title(subtext = "Word frequencies", left = "center") %>% 
        e_theme("walden")
    }
  })


  # Topic prevalence plot ---------------------------------------------------

  d_topic_prevalence <- reactive({
    d_data_model()$d_gamma_terms %>% 
      filter(top_word) %>% 
      mutate(
        topic_name = paste("Topic", topic),
        topic_name = reorder(topic_name, -gamma),
        gamma = round(gamma, 4)
      )
    
  })
  
  output$topic_prevalence_plot <- renderEcharts4r({

    if (! isTruthy(selected_topic())) {
      p <- 
        d_topic_prevalence() %>% 
        e_chart(x = topic_name) %>% 
        e_bar(gamma, name = "Topic prevalence gamma", stack = "grp", color = "#888888") %>% 
        e_title(subtext = "Topic prevalence", left = "center")
        # e_tooltip()
    } else {
      p <-
        d_topic_prevalence() %>% 
        mutate(
          group = ifelse(topic == selected_topic(), "selected", "notselected")
        ) %>%
        select(topic_name, gamma, group) %>% 
        spread(group, gamma, fill = 0) %>% 
        e_chart(x = topic_name) %>% 
        e_bar(selected, name = "Topic prevalence gamma", stack = "grp", color = "#3fb1e3") %>% 
        e_bar(notselected, name = "Topic prevalence gamma", stack = "grp", color = "#888888") %>% 
        e_title(subtext = paste0("Topic prevalence of topic ", selected_topic()), left = "center")
    }
    
    p %>% 
      e_tooltip(
        formatter = htmlwidgets::JS(
          "function(params){return(params.value[0]+'<br />Topic prevalence gamma: '+params.value[1])}"
        )
      ) %>% 
      e_legend(show = FALSE) %>%
      e_x_axis(splitLine = list(show = FALSE), show = FALSE) %>%
      e_y_axis(splitLine = list(show = FALSE), show = FALSE) %>% 
      e_theme("walden")    # also good: westeros, auritus, walden
  })  
  
  observeEvent(input$topic_prevalence_plot_clicked_row, {
    
    node_id <- d_topic_prevalence()$topic[input$topic_prevalence_plot_clicked_row]

    visNetworkProxy(ns("graph")) %>% 
      visFocus(node_id, scale = 0.1) %>% 
      visSelectNodes(node_id)
  })
}  
