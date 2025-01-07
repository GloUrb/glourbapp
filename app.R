#remotes::install_github("lvaudor/glourbi")
library(shiny)
library(glourbi)

shinyApp(ui=glourbapp:::app_ui,server=glourbapp:::app_server)
#
