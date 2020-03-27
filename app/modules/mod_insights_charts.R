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
    column(width = 4, echarts4rOutput(ns("plot_n_words"), height = "250px")),
    column(width = 4, echarts4rOutput(ns("plot_day_of_week"), height = "250px")),
    column(width = 4, echarts4rOutput(ns("plot_acc_author"), height = "250px")),
  )
  
}



# Module logic ------------------------------------------------------------

insights_charts <- function(input, output, session, d_data_model) {
  
  output$plot_monthly_blogs <- renderEcharts4r({
    req(d_data_model())

    d_data_model()$d_monthly_blogs %>%
      e_charts(publication_month) %>%
      e_bar(n_docs, name = "# blogs", smooth = TRUE, symbol = "none") %>%
      e_legend(show = FALSE) %>%
      e_tooltip(trigger = "axis") %>%
      e_title(subtext = "# monthly blogs", left = "center") %>%
      e_x_axis(splitLine = list(show = FALSE)) %>%
      e_y_axis(splitLine = list(show = FALSE), show = FALSE) %>%
      # e_datazoom(type = "slider", toolbox = FALSE, bottom = -5) %>%  
      # e_toolbox_feature("dataZoom", title = list(zoom = "zoom", back = "back")) %>%
      e_theme("walden")    # also good: westeros, auritus, walden
  })
  
  output$plot_day_of_week <- renderEcharts4r({
    req(d_data_model())

    d_data_model()$d_day_of_week %>%
      mutate(day_of_week = as.character(day_of_week)) %>% 
      e_charts(day_of_week) %>% 
      e_bar(n_docs, name = "# of blogs by weekday", stack = "grp") %>% 
      e_legend(show = FALSE) %>%
      e_tooltip(trigger = "axis") %>%
      e_title(subtext = "Number of blogs by day of week", left = "center") %>% 
      e_x_axis(splitLine = list(show = FALSE)) %>%
      e_y_axis(splitLine = list(show = FALSE), show = FALSE) %>%
      e_theme("walden")    # also good: westeros, auritus, walden
  })
  
  output$plot_n_words <- renderEcharts4r({
    req(d_data_model())

    d_data_model()$d_avg_words %>%
      mutate(height = quant_75 - quant_25) %>% 
      e_charts(publication_month) %>%
      e_line(median_words, name = "# words", smooth = TRUE, symbol = "none") %>%
      e_band(
        quant_25, height, smooth = c(TRUE, TRUE), name = list("25th quantile", "75th quantile"), 
        color = c("#888888", "#888888"), symbol = c("none", "none")
      ) %>% 
      e_legend(show = FALSE) %>%
      e_tooltip(trigger = "axis", formatter = e_tooltip_pointer_formatter("decimal", 0)) %>%
      e_title(subtext = "# unique non-stop words per blog", left = "center") %>% 
      e_x_axis(splitLine = list(show = FALSE)) %>%
      e_y_axis(splitLine = list(show = FALSE), show = FALSE) %>%
      e_theme("walden")    # also good: westeros, auritus, walden
  })

  output$plot_acc_author <- renderEcharts4r({
    req(d_data_model())
    
    d_data_model()$d_monthly_blogs %>%
      e_charts(publication_month) %>%
      e_line(n_authors, name = "#  authors", smooth = TRUE, symbol = "none") %>%
      e_legend(show = FALSE) %>%
      e_tooltip(trigger = "axis") %>%
      e_title(subtext = "# monthly authors", left = "center") %>% 
      e_x_axis(splitLine = list(show = FALSE)) %>%
      e_y_axis(splitLine = list(show = FALSE), show = FALSE, axisLabel = list(margin = 0)) %>%
      e_theme("walden")    # also good: westeros, auritus, walden
  })
}  
