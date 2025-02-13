#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # set database connection
  conn <- glourbi::connect_to_glourb()


  r_val <- reactiveValues(
    # UI
    main_menu_tab=NULL, # selected tab in main menu
    sub_menu_tab=NULL
  )

  # Your application server logic
  mod_global_server("mod_global_1")
  mod_OSM_server("mod_OSM_1", conn)
  mod_discourses_server("mod_discourses_1",conn,r_val)
  mod_GSW_server("mod_GSW_1",conn)
  #mod_WDWP_server("mod_WDWP_1")
  mod_help_server("mod_help_1",r_val)

  # navbarPage identifier
  observeEvent(input$main_menu_tab, {
    r_val$main_menu_tab = input$main_menu_tab
  })

  # disconnect database when closing session
  onStop(function() {
    print(conn)
    if (!is.null(conn)) {
      DBI::dbDisconnect(conn)
    }
  })
}
