################################################################################
# UI of the Shiny app
#
# Author: Stefan Schliebs
# Created: 2020-03-04 09:34:50
################################################################################


htmlTemplate(
  filename = "www/index.html",
  blog_counter = count_icon_ui("blog_counter", icon = "icon-browser", icon_text = "Articles"),
  author_counter = count_icon_ui("author_counter", icon = "icon-profile-female", icon_text = "Authors"),
  word_counter = count_icon_ui("word_counter", icon = "icon-search", icon_text = "Words"),
  page_counter = count_icon_ui("page_counter", icon = "icon-book-open", icon_text = "Equivalent Book Pages"),
  insights_charts = insights_charts_ui("insights_charts"),
  last_refresh = uiOutput("last_refresh"),
  topics_graph = topics_graph_ui("topics_graph"),
  topics_table = topics_table_ui("topics_table")

  # box_packages_new_month = pretty_value_box_ui("packages-new-month", icon_name = "cubes"),
  # box_packages_updated_month = pretty_value_box_ui("packages-updated-month", background_color = "#7ab885", icon_name = "cubes"),
  #
  # box_packages_new_year = pretty_value_box_ui("packages-new-year", icon_name = "cubes"),
  # box_packages_updated_year = pretty_value_box_ui("packages-updated-year", background_color = "#7ab885", icon_name = "cubes"),
  #
  # header_ui = uiOutput("header_ui") %>% withSpinner(size = 0.5, proxy.height = "50px", type = 6, color = "#FFFFFF"),
  #
  # package_chart = package_chart_ui("package_chart"),
  #
  # featured_packages = featured_packages_ui("featured_packages"),
  #
  # dependency_network = graph_network_ui("dependency_network")
)