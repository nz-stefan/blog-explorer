################################################################################
# Entrypoint of the Shiny app
#
# Author: Stefan Schliebs
# Created: 2020-03-04 09:30:40
################################################################################


library(shiny)
library(dplyr)
library(tidyr)
library(echarts4r)
library(visNetwork)
library(glue)



# Utilities ---------------------------------------------------------------

source("utils/ui-utils.R")



# Modules -----------------------------------------------------------------

source("modules/mod_count_icon.R")
source("modules/mod_topics_graph.R")
source("modules/mod_insights_charts.R")
source("modules/mod_topics_table.R")
