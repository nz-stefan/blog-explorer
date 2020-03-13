################################################################################
# Shiny module count_icon
#
# Author: Stefan Schliebs
# Created: 2020-03-13 09:58:15
################################################################################


# Module constants --------------------------------------------------------

F_COUNT_ICON_TEMPLATE <- "www/modules/count_icon/index.html"


# Module UI ---------------------------------------------------------------

count_icon_ui <- function(id, icon = "icon-wallet", icon_text = "") {
  ns <- NS(id)
  
  htmlTemplate(
    filename = F_COUNT_ICON_TEMPLATE,
    icon = icon,
    count_to = uiOutput(ns("count_to")),
    icon_text = icon_text
  )
}



# Module logic ------------------------------------------------------------

count_icon <- function(input, output, session, d_data_model, value) {
  output$count_to <- renderUI({
    HTML(sprintf('<h3 class="count-to font-alt" data-countto="%d"></h3>', d_data_model()[[value]]))
  })
}  
