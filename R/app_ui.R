#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
    # Your application UI logic

  tagList(
    # Inject custom CSS into the app for tab styling
    tags$style(
      HTML("
      /* Hover effect */
      .nav-pills > li > a:hover {
        background-color: #E3EBF7; /* Background color on hover */
      }

      /* Help-guide button placement */
      .help-btn {
          position: absolute;
          right: 18px;
          top: 10px;
          z-index: 1000; /* Make sure it's above other UI elements */
        }

      .navbar {
        position: relative;
      }
    ")
    ),

  # Define the help button outside of navbarPage, but position it inside with CSS
      div(class = "help-btn",
          mod_help_ui("mod_help_1") # Use the cicerone UI module here
      ),
      navbarPage(
          title=
            img(
              src = "www/GloUrb_wide.png",
              height = 35,
              width = 100
            ),
          id="main_menu_tab",
          tabPanel("Global",icon = icon("globe"),mod_global_ui("mod_global_1")),
          tabPanel("Surface Water",icon = icon("droplet"),mod_GSW_ui("mod_GSW_1")),
          tabPanel("Discourses",icon=icon("comment"),mod_discourses_ui("mod_discourses_1")),
          tabPanel("OpenStreetMap",icon=icon("map"),mod_OSM_ui("mod_OSM_1")),
          tabPanel("Doc",icon=icon("circle-info"),
                   HTML(paste0("<p> To consult the documentation associated to this app",
                               "please check <a href='https://glourb.github.io/glourbdoc/' ",
                               "target='_blank'>",
                               "this site</a>."))),
          theme = bslib::bs_theme(version = 5, bootswatch = "united",
          primary = "#308F10",
          #secondary="#009CDE"
          #primary = "#000000",
          secondary="#909090"
          )
        )#navbarPage
)
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "GloUrb"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
