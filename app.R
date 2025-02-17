#remotes::install_github("lvaudor/glourbi")
library(shiny)
library(glourbi)

options("shiny.port" = 3841, "shiny.host" = "0.0.0.0", "golem.app.prod" = TRUE)

shinyApp(ui=glourbapp:::app_ui,server=glourbapp:::app_server)
#
