################################################################################
# Shiny module insights_charts
#
# Author: Stefan Schliebs
# Created: 2020-03-16 09:13:12
################################################################################


# Module constants --------------------------------------------------------

# F_COUNT_ICON_TEMPLATE <- "www/modules/count_icon/index.html"


# Module UI ---------------------------------------------------------------

insights_charts_ui <- function(id, icon = "icon-wallet", icon_text = "") {
  ns <- NS(id)
  
  fluidRow(
    column(width = 12, echarts4rOutput(ns("plot_monthly_blogs"), height = "200px")),
    column(width = 4, echarts4rOutput(ns("plot_day_of_week"), height = "300px")),
    column(width = 4, echarts4rOutput(ns("plot_n_words"), height = "300px")),
    column(width = 4, echarts4rOutput(ns("plot_acc_author"), height = "300px")),
  )
  
}



# Module logic ------------------------------------------------------------

insights_charts <- function(input, output, session, d_data_model) {
  output$plot_monthly_blogs <- renderEcharts4r({
    req(d_data_model())
    # browser()
    
    d_data_model()$d_monthly_blogs %>%
      e_charts(publication_month) %>%
      e_bar(n_docs, name = "Number of blogs", smooth = TRUE, symbol = "none") %>%
      e_legend(show = FALSE) %>%
      e_tooltip(trigger = "axis") %>%
      e_title(subtext = "Hover mouse over bars", subtextStyle = list(align = "center")) %>%
      e_x_axis(splitLine = list(show = FALSE)) %>%
      e_y_axis(splitLine = list(show = FALSE)) %>%
      # e_datazoom(type = "slider", toolbox = FALSE, bottom = -5) %>%  
      # e_toolbox_feature("dataZoom", title = list(zoom = "zoom", back = "back")) %>%
      e_theme("walden")    # also good: westeros, auritus, walden
  })
  
  output$plot_day_of_week <- renderEcharts4r({
    req(d_data_model())

    d_data_model()$d_day_of_week %>%
      mutate(day_of_week = as.character(day_of_week)) %>% 
      e_charts(day_of_week) %>% 
      e_bar(n_docs, name = "Number of blogs", stack = "grp") %>% 
      e_legend(show = FALSE) %>%
      e_tooltip(trigger = "axis") %>%
      e_title(subtext = "Number of blogs by day of week") %>% 
      e_x_axis(splitLine = list(show = FALSE)) %>%
      e_y_axis(splitLine = list(show = FALSE)) %>%
      e_theme("walden")    # also good: westeros, auritus, walden
  })
  
  output$plot_n_words <- renderEcharts4r({
    req(d_data_model())

    d_data_model()$d_avg_words %>%
      mutate(height = quant_75 - quant_25) %>% 
      e_charts(publication_month) %>%
      e_line(median_words, name = "Number of words", smooth = TRUE, symbol = "none") %>%
      e_band(quant_25, height) %>% 
      e_legend(show = FALSE) %>%
      e_tooltip(trigger = "axis") %>%
      e_title(subtext = "Number of words per blog (stopwords removed)") %>% 
      e_x_axis(splitLine = list(show = FALSE)) %>%
      e_y_axis(splitLine = list(show = FALSE)) %>%
      e_theme("walden")    # also good: westeros, auritus, walden
  })

  output$plot_acc_author <- renderEcharts4r({
    req(d_data_model())
    
    d_data_model()$d_monthly_blogs %>%
      e_charts(publication_month) %>%
      e_line(n_authors, name = "Number of authors", smooth = TRUE, symbol = "none") %>%
      e_legend(show = FALSE) %>%
      e_tooltip(trigger = "axis") %>%
      e_title(subtext = "Number of monthly authors") %>% 
      e_x_axis(splitLine = list(show = FALSE)) %>%
      e_y_axis(splitLine = list(show = FALSE)) %>%
      e_theme("walden")    # also good: westeros, auritus, walden
  })
}  
